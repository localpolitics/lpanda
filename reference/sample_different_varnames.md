# Sample Dataset with Some Different Variable Names

A variant of
[`sample_data`](https://localpolitics.github.io/lpanda/reference/sample_data.md)
in which some variables have different names (`list_name` becomes
`party` and `elected` becomes `seat`). This is useful for testing
robustness of input handling.

## Usage

``` r
sample_different_varnames
```

## Format

A data frame with 18 rows and 5 variables (same structure as
[`sample_data`](https://localpolitics.github.io/lpanda/reference/sample_data.md)).

## Source

Fictitious data

## Examples

``` r
# Basic inspection
str(sample_different_varnames)
#> tibble [18 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ elections: num [1:18] 14 14 14 18 18 18 18 18 18 18 ...
#>  $ candidate: chr [1:18] "c01" "c02" "c03" "c01" ...
#>  $ party    : chr [1:18] "A" "A" "A" "A" ...
#>  $ seat     : logi [1:18] TRUE TRUE TRUE TRUE FALSE TRUE ...
#>  $ mayor    : logi [1:18] TRUE FALSE FALSE FALSE FALSE TRUE ...

# Quick continuity diagram (basic and unformatted version)
net <- prepare_network_data(sample_different_varnames,
                                      input_variable_map = list(list_name = "party",
                                                                elected = "seat"),
                                      verbose = FALSE,
                                      skip_groups = TRUE)
plot_continuity(net)
```
