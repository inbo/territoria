
<!-- README.md is generated from README.Rmd. Please edit that file -->

# territoria

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://lifecycle.r-lib.org/articles/figures/lifecycle-stable.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![License](https://img.shields.io/badge/license-GPL--3-blue.svg?style=flat)](https://www.gnu.org/licenses/gpl-3.0.html)
![GitHub](https://img.shields.io/github/license/inbo/territoria) [![R
build
status](https://github.com/inbo/territoria/workflows/check%20package%20on%20main/badge.svg)](https://github.com/inbo/territoria/actions)
[![Codecov test
coverage](https://codecov.io/gh/inbo/territoria/branch/main/graph/badge.svg)](https://app.codecov.io/gh/inbo/territoria?branch=main)
![r-universe
name](https://inbo.r-universe.dev/badges/:name?color=c04384)
![r-universe package](https://inbo.r-universe.dev/badges/territoria)
![GitHub code size in
bytes](https://img.shields.io/github/languages/code-size/inbo/territoria.svg)
![GitHub repo
size](https://img.shields.io/github/repo-size/inbo/territoria.svg)
<!-- badges: end -->

The goal of `territoria` is to cluster observations from different
breeding bird surveys into territoria.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("inbo/territoria")
```

## Example

We start by simulating some observations. We need for every observation
their `x` and `y` coordinates in a projected coordinate system. `survey`
is an integer id for every survey. A survey is a unique combination of
an area and date. `status` is an integer indication the breeding status.
A higher value assume more certainty about breeding. Set this to a
constant value if you don’t distinct between different certainties. In
this example we use three classes: `1`, `2` and `3`.

``` r
library(territoria)
set.seed(20210806)
obs <- simulate_observations()
names(obs)
#> [1] "observations" "centroids"
summary(obs$centroids)
#>        x                 y          
#>  Min.   :  45.51   Min.   :  36.32  
#>  1st Qu.: 544.72   1st Qu.: 946.10  
#>  Median : 927.92   Median :1246.24  
#>  Mean   : 955.15   Mean   :1224.73  
#>  3rd Qu.:1349.57   3rd Qu.:1669.61  
#>  Max.   :1897.31   Max.   :1963.82
summary(obs$observations)
#>        x                 y               survey         status     
#>  Min.   : -19.55   Min.   : -14.75   Min.   :1.00   Min.   :1.000  
#>  1st Qu.: 550.47   1st Qu.: 910.56   1st Qu.:1.75   1st Qu.:2.000  
#>  Median : 899.91   Median :1272.53   Median :2.50   Median :2.000  
#>  Mean   : 946.40   Mean   :1238.04   Mean   :2.50   Mean   :2.096  
#>  3rd Qu.:1362.48   3rd Qu.:1764.88   3rd Qu.:3.25   3rd Qu.:3.000  
#>  Max.   :2012.23   Max.   :2082.18   Max.   :4.00   Max.   :3.000  
#>   observed             id        
#>  Mode :logical   Min.   :  1.00  
#>  FALSE:44        1st Qu.: 26.75  
#>  TRUE :60        Median : 52.50  
#>                  Mean   : 52.50  
#>                  3rd Qu.: 78.25  
#>                  Max.   :104.00
obs <- obs$observations[obs$observations$observed, ]
```

Once we have a data.frame with the observations, we connect to a SQLite
database and import the observations. This assigns every observation to
its own cluster.

``` r
conn <- connect_db()
import_observations(observations = obs, conn = conn, max_dist = 336)
result <- get_cluster(conn = conn)
nrow(result$observations) == nrow(result$cluster)
#> [1] TRUE
```

Next, we need to calculate the distance matrix. This is not the full
distance matrix. We omit all irrelevant distances, e.g. between
observations from the same survey or with a distance larger than twice
the maximum cluster distance.

``` r
distance_matrix(conn = conn, max_dist = 366)
```

Now we can start the clustering. The clustering takes into account all
observations with a `status` greater than or equal to the set status.

``` r
cluster_observation(conn = conn, status = 3, max_dist = 336)
#> ......
result3 <- get_cluster(conn = conn)
nrow(result3$observations) > nrow(result3$cluster)
#> [1] TRUE
```

Repeat the clustering for every status level. Note that skipping levels
implies that we combine them with the lower level.

``` r
cluster_observation(conn = conn, status = 1, max_dist = 336)
#> ......................
result1 <- get_cluster(conn = conn)
nrow(result1$observations) > nrow(result1$cluster)
#> [1] TRUE
nrow(result3$cluster) > nrow(result1$cluster)
#> [1] TRUE
summary(result1$observations)
#>        id               x                 y               survey     
#>  Min.   :  2.00   Min.   : -19.55   Min.   :  40.03   Min.   :1.000  
#>  1st Qu.: 22.75   1st Qu.: 550.47   1st Qu.: 976.80   1st Qu.:1.000  
#>  Median : 53.50   Median : 921.68   Median :1282.60   Median :3.000  
#>  Mean   : 50.32   Mean   : 931.40   Mean   :1283.44   Mean   :2.417  
#>  3rd Qu.: 72.25   3rd Qu.:1309.80   3rd Qu.:1798.25   3rd Qu.:3.000  
#>  Max.   :103.00   Max.   :1884.38   Max.   :2082.18   Max.   :4.000  
#>      status         cluster     
#>  Min.   :1.000   Min.   : 2.00  
#>  1st Qu.:2.000   1st Qu.:10.00  
#>  Median :2.000   Median :17.00  
#>  Mean   :2.033   Mean   :21.33  
#>  3rd Qu.:2.250   3rd Qu.:23.00  
#>  Max.   :3.000   Max.   :77.00
summary(result1$cluster)
#>     cluster          n_obs         max_status      centroid_x     
#>  Min.   : 2.00   Min.   :1.000   Min.   :1.000   Min.   :  19.02  
#>  1st Qu.:10.50   1st Qu.:2.000   1st Qu.:2.000   1st Qu.: 490.94  
#>  Median :19.00   Median :3.000   Median :2.000   Median : 920.00  
#>  Mean   :24.61   Mean   :2.609   Mean   :2.391   Mean   : 915.14  
#>  3rd Qu.:26.50   3rd Qu.:3.000   3rd Qu.:3.000   3rd Qu.:1316.24  
#>  Max.   :77.00   Max.   :4.000   Max.   :3.000   Max.   :1800.93  
#>    centroid_y     
#>  Min.   :  74.42  
#>  1st Qu.: 992.81  
#>  Median :1280.92  
#>  Mean   :1292.35  
#>  3rd Qu.:1829.07  
#>  Max.   :2036.60
```
