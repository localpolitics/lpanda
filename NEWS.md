# lpanda 0.1.0

* Initial CRAN submission.

## New functions

* `prepare_network_data()` - creates network edgelists and node attributes
  for candidates, lists, continuity, parties, cores and elections.
* `plot_continuity()` - visualizes the continuity of candidate lists across
  elections as a time-based diagram using igraph package.

## New data

* Municipalities:
  - `Doubice_DC_cz`
  - `Jilove_DC_cz`
* Example data:
  - `sample_data`
* Variants for tests:
  - `sample_diff_varnames`
  - `sample_binary_values`
  - `sample_no_continuity`
  - `sample_no_crossing`
