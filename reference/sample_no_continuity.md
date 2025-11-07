# Sample Dataset Without Continuity Between Elections

A variant of
[`sample_data`](https://localpolitics.github.io/lpanda/reference/sample_data.md)
in which no candidate appears in more than one election. This breaks the
continuity between elections, making the dataset useful for testing
whether network-building functions correctly handle cases with no
longitudinal links across candidate lists.

## Usage

``` r
sample_no_continuity
```

## Format

A data frame with 15 rows and 5 variables (same structure as
[`sample_data`](https://localpolitics.github.io/lpanda/reference/sample_data.md)).

## Source

Fictitious data

## Examples

``` r
# Basic inspection
str(sample_no_continuity)
#> tibble [15 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ elections: num [1:15] 14 14 14 18 18 18 18 18 18 18 ...
#>  $ candidate: chr [1:15] "c01" "c02" "c03" "c04" ...
#>  $ list_name: chr [1:15] "A" "A" "A" "A" ...
#>  $ elected  : logi [1:15] TRUE TRUE TRUE TRUE FALSE FALSE ...
#>  $ mayor    : logi [1:15] TRUE FALSE FALSE FALSE FALSE FALSE ...

# Quick continuity diagram (basic and unformatted version)
plot_continuity(sample_no_continuity)
```
