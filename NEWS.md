# lpanda 0.2.0 (2025-11-28)

## Changes
* Renamed sample datasets used for testing:
  - `sample_different_varnames` (formerly `sample_diff_varnames`). 
  - `sample_no_switching` (formerly `sample_no_crossing`).

## New features
* `plot_continuity()` gains a new argument, `show_candidate_networks`. It adds
  an additional bottom panel visualising candidate network snapshots for each
  selected election. The snapshots highlight candidates running in the focal
  election, show earlier candidates as smaller background nodes, and reflect
  group affiliations (e.g., parties or cores) through node colours and candidate
  lists through group boundaries around the nodes.

## New data
* Added municipalities:
  - `Bublava_SO_cz`
  - `Cernosice_PZ_cz`
  - `Dasnice_SO_cz`
  - `Horomerice_PZ_cz`
  - `Hradce_CB_cz`
  - `Kamenna_CB_cz`
  - `Nebanice_CH_cz`
  - `Potucky_KV_cz`
  - `Prameny_CH_cz`
  - `Roztoky_PZ_cz`
* New sample dataset for testing:
  - `sample_no_pluralism` - variant of `sample_data` with only one candidate
    list per election.

## Improvements
* `plot_continuity()` no longer requires grouping information (e.g., parties) 
  when marking a specific candidate via `mark = c("candidate", "<name>")`. 
  The function now highlights the candidate and their lists even in ungrouped 
  datasets, avoiding unnecessary community detection.

## Bug fixes
* Fixed crashes and encoding issues on non-UTF-8 systems caused by data
  containing diacritics.
  - Added automatic ASCII fallback for non-UTF-8 systems and locales.
  - `plot_continuity()` now detects the UTF-8 capability of the current
    graphics device. It automatically switches to ASCII mode when running in
    non-interactive or CI environments, or when the device cannot render UTF-8.
  - Added global options:
    - `lpanda.text_encoding` — `"auto"` (default) | `"utf8"` | `"ascii"`
    - `lpanda.plot_family` — `"sans"` (default)
    - `lpanda.force_ascii` — `FALSE` (default)
  - Text encoding can also be specified as a `text_encoding` argument via `...`
    in plotting functions.
* `plot_continuity()` now properly resets the graphics layout to prevent
  occasional overlapping plots.

## Documentation
* `help("lpanda")` / `?lpanda` now provide basic package information, including
  an overview of the basic workflow, main functions, datasets, and references.
* Added pkgdown website: <https://localpolitics.github.io/lpanda/>.
* Updated documentation for municipalities and sample datasets.

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
