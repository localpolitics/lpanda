# Sample Dataset Without Candidate Switching

A variant of
[`sample_data`](https://localpolitics.github.io/lpanda/reference/sample_data.md)
in which candidates may run in multiple elections, but always remain
within the same political group. In other words, they never switch
between candidate list clusters, which makes the dataset useful for
testing continuity logic under stable group membership (verifying that
no cross-group transitions occur).

## Usage

``` r
sample_no_switching
```

## Format

A data frame with 15 rows and 5 variables (same structure as
[`sample_data`](https://localpolitics.github.io/lpanda/reference/sample_data.md)).

## Source

Fictitious data

## Examples

``` r
# Basic inspection
str(sample_no_switching)
#> tibble [15 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ elections: num [1:15] 14 14 14 18 18 18 18 18 18 18 ...
#>  $ candidate: chr [1:15] "c01" "c02" "c03" "c01" ...
#>  $ list_name: chr [1:15] "A" "A" "A" "A" ...
#>  $ elected  : logi [1:15] TRUE TRUE TRUE TRUE FALSE FALSE ...
#>  $ mayor    : logi [1:15] TRUE FALSE FALSE FALSE FALSE FALSE ...

# Quick continuity diagram (basic and unformatted version)
plot_continuity(sample_no_switching)
```
