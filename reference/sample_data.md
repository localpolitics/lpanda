# Simple Sample Dataset

Basic fictitious dataset simulating election results.

## Usage

``` r
sample_data
```

## Format

A data frame with 18 rows and 5 variables:

- elections:

  Election identifier (numeric)

- candidate:

  Candidate identifier (character)

- list_name:

  Candidate list name (character)

- elected:

  Logical; `TRUE` if the candidate was elected

- mayor:

  Logical; `TRUE` if the candidate became mayor

## Source

Fictitious data

## Examples

``` r
# Basic inspection
str(sample_data)
#> tibble [18 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ elections: num [1:18] 14 14 14 18 18 18 18 18 18 18 ...
#>  $ candidate: chr [1:18] "c01" "c02" "c03" "c01" ...
#>  $ list_name: chr [1:18] "A" "A" "A" "A" ...
#>  $ elected  : logi [1:18] TRUE TRUE TRUE TRUE FALSE TRUE ...
#>  $ mayor    : logi [1:18] TRUE FALSE FALSE FALSE FALSE TRUE ...

# Quick continuity diagram (basic and unformatted version)
plot_continuity(sample_data)
```
