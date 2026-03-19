# territoria 0.1.0

* `import_observations()` splits surveys with clearly distinct groups of
  observations into multiple surveys.
  This is done by clustering the observations and checking if the clusters are
  too far apart geographically.
  It also requires to provide a user and region id.
* `edge_distribution()` return the distribution of the length of the edges of a Delaunay
  triangulation on the observations per survey.
* Add `unlikely_survey_area()`, `unlikely_status()` and
  `unlikely_edge_distribution()` to detect surveys which are unlikely done
  according to the sampling protocol.
* `distance_matrix()` ignores observations from unlikely surveys.
  Hence `cluster_observation()` will ignore them during the clustering and
  `get_cluster()` ignores them.
* `get_total()` returns the total number of individuals per region based on the
  clustering.
* `get_unlikely_summary()` and `get_unlikely_observation()` return the unlikely
  surveys and observations.
* Update [`checklist`](https://inbo.github.io/checklist/) machinery.

# territoria 0.0.3

* Update [`checklist`](https://inbo.github.io/checklist/) machinery.
* Release action works with multiple lines in a message.

# territoria 0.0.2

## Breaking changes

* `import_observations()` requires an id for every record.

## User visible changes

* `get_cluster()` returns the id of the observations.
  This allows the user to rematch the observations with other data.
* `simulate_observation()` gains an id variable.
* `cluster_observation()` gains a `reset` argument.

## Improvements

* `cluster_observation()` is much faster.
  Clustering tens of thousands observations now takes only minutes instead of
  hours.
* `import_observations()` returns an error when the diagonal of the bounding
  box is smaller than `max_dist`.
  This should catch situations when providing coordinates in decimal degrees
  instead of projected coordinates.

# territoria 0.0.1

* Added `import_observations()`.
* Added `distance_matrix()`.
* Added `cluster_observation()`.
* Added `get_cluster()`.

# territoria 0.0.0

* Added `simulate_observations()`.
