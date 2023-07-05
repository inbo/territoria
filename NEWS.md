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
