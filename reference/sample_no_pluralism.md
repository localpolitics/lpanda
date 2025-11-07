# Sample Dataset Without Party Pluralism (only one candidate list per election)

A variant of
[`sample_data`](https://localpolitics.github.io/lpanda/reference/sample_data.md)
in which only one candidate list is running in each election. This
removes party pluralism from the party system, making the dataset useful
for testing functions under non-competitive conditions.

## Usage

``` r
sample_no_pluralism
```

## Format

A data frame with 9 rows and 5 variables (same structure as
[`sample_data`](https://localpolitics.github.io/lpanda/reference/sample_data.md)).

## Source

Fictitious data

## Examples

``` r
# Basic inspection
str(sample_no_pluralism)
#> tibble [9 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ elections: num [1:9] 14 14 14 18 18 18 22 22 22
#>  $ candidate: chr [1:9] "c01" "c02" "c03" "c01" ...
#>  $ list_name: chr [1:9] "A" "A" "A" "B" ...
#>  $ elected  : logi [1:9] TRUE TRUE TRUE TRUE TRUE TRUE ...
#>  $ mayor    : logi [1:9] TRUE FALSE FALSE FALSE TRUE FALSE ...

# Quick continuity diagram (basic and unformatted version)
plot_continuity(sample_no_pluralism)
```
