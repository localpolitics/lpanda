# lpanda (development version)

## Changes
* Renamed sample datasets used for testing:
  - `sample_different_varnames` (formerly `sample_diff_varnames`). 
  - `sample_no_switching` (formerly `sample_no_crossing`).

## New data
* Added new sample dataset for testing:
  - `sample_no_pluralism` - variant of `sample_data` with only one candidate
    list per election.

## Documentation
* `help("lpanda")` / `?lpanda` now display basic package information.
* Updated documentation for sample datasets.

# lpanda 0.1.1 (2025-08-26)

## Bug fixes
* `plot_continuity()` now always restores graphical parameters (even if
  an error occurs).

# lpanda 0.1.0 (2025-08-18)

* Initial CRAN submission.

## New features
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
