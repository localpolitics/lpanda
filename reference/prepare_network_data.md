# Prepare Network Data for LPANDA

Transforms time series data of local election results into a set of
network data for use in Local Political Actor Network Diachronic
Analysis (LPANDA). The function constructs a bipartite network
(candidate – candidate list), its projected one-mode networks (candidate
– candidate and list – list), a continuity graph (linking candidate
lists between adjacent elections), and an elections network (its node
attributes can serve as electoral statistics). It also detects parties
(as clusters of candidate lists based on community detection applied to
the bipartite network) and constructs their network.

## Usage

``` r
prepare_network_data(df, input_variable_map = list(), verbose = TRUE, ...)
```

## Arguments

- df:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) containing data
  from elections, with one row per candidate. The function also accepts
  a single election, though diachronic outputs will then be empty or
  trivial. See the *Expected structure of input data* section for the
  expected data format and required variables.

- input_variable_map:

  A [list](https://rdrr.io/r/base/list.html) mapping variable names in
  `df` that differ from the expected ones:  
    
  `elections` = **unique** election identifiers
  ([numeric](https://rdrr.io/r/base/numeric.html)),  
  `candidate` = candidate's name used as a **unique** identifier
  ([character](https://rdrr.io/r/base/character.html)),  
  `list_name` = name of the candidate list
  ([character](https://rdrr.io/r/base/character.html)),  
  `list_pos` = candidate's position on the list
  ([numeric](https://rdrr.io/r/base/numeric.html)),  
  `pref_votes` = preferential votes received by the candidate
  ([numeric](https://rdrr.io/r/base/numeric.html)),  
  `list_votes` = \* total votes received by the candidate list
  ([numeric](https://rdrr.io/r/base/numeric.html)),  
  `elected` = whether the candidate was elected
  ([logical](https://rdrr.io/r/base/logical.html)),  
  `nom_party` = party that nominated the candidate
  ([character](https://rdrr.io/r/base/character.html)),  
  `pol_affil` = declared political affiliation of the candidate
  ([character](https://rdrr.io/r/base/character.html)),  
  `mayor` = whether the councillor became mayor
  ([logical](https://rdrr.io/r/base/logical.html)),  
  `dep_mayor` = whether the councillor became deputy mayor
  ([logical](https://rdrr.io/r/base/logical.html)),  
  `board` = whether the councillor became a member of the executive
  board ([logical](https://rdrr.io/r/base/logical.html)),  
  `gov_support` = whether the councillor supported the executive body
  ([logical](https://rdrr.io/r/base/logical.html)),  
  `elig_voters` = \* number of eligible voters
  ([numeric](https://rdrr.io/r/base/numeric.html)),  
  `ballots_cast` = \* number of ballots cast
  ([numeric](https://rdrr.io/r/base/numeric.html)),  
  `const_size` = \* size of the constituency (number of seats)
  ([numeric](https://rdrr.io/r/base/numeric.html))  
    
  \* Variables marked with an asterisk should appear only once per
  election and constituency — in the row of any **one** candidate
  running in that specific elections and constituency.  
    
  See the *Expected input data structure* section to find out how to use
  it.

- verbose:

  Logical, default TRUE. If FALSE, suppresses informative messages.

- ...:

  Optional arguments reserved for internal development, experimental
  features and future extensions, such as `include_cores` (logical,
  default FALSE). Not intended for standard use yet (behavior may change
  without notice). Unknown keys in ... are ignored.

## Value

A [list](https://rdrr.io/r/base/list.html) of network data objects for
diachronic analysis using LPANDA or other social network analysis tools.
Each component contains `edgelist` (data.frame of edges) and `node_attr`
(data.frame of node attributes). The exact set of columns depends on the
input and may evolve. See *Output data structure* for a description of
the returned object.

## Note

Cores are currently experimental, as the conversion of their definition
into code is still being sought, and may be subject to change in future
versions. It is not yet intended for standard use in analyses and
academic studies, since their calculation is not yet comprehensive, so
the cores' network structure will not appear in the standard output
network data unless explicitly called with the `include_cores = TRUE`
parameter. Use with caution, their interpretation is highly
questionable.

## Expected structure of input data

The input data frame (`df`) **must** include at least the election
identifiers (year\[.month\]), candidates' names (uniquely identifying
individuals), and list names. Other variables are optional. If variable
names in the dataset differ from the expected ones, they should be
specified in the `input_variable_map` as a named
[list](https://rdrr.io/r/base/list.html) (only differing names need to
be listed).

*Just in case - a named list is a list where each element has a name
(the expected variable name) and a value (the actual name used in your
data frame), for example:
`list(list_name = "party", elected = "seat", list_votes = "votes_total")`.*

Examples of expected and acceptable values in `df`:

- `elections` (required): Election identifier in the format
  YY\[YY\]\[.MM\]: e.g., 94 \| 02 \| 1998 \| "2024" \| 2022.11

- `candidate` (required): e.g., "John Doe" \| "John Smith (5)" \| "Jane
  Doe, jr."

- `list_name` (required): *for independent candidates, you can use:*
  e.g., "John Smith, Independent Candidate" \| "J.S., IND."

- `list_pos`, `pref_votes`, `list_votes`: must be
  [numeric](https://rdrr.io/r/base/numeric.html)

- `elected`, `mayor`, `dep_mayor`, `board`, `gov_support`: 1 \| "0" \| T
  \| "F" \| "TRUE" \| FALSE (non‑logical inputs will be coerced to
  logical).

- `nom_party`: *for independent candidates, you can use:* "IND" \|
  "Independent Candidate"

- `pol_affil`: *for independent candidates, you can use:* "non-partisan"

- `elig_voters`, `ballots_cast`, `const_size`: A
  [numeric](https://rdrr.io/r/base/numeric.html) that should appear only
  once in any candidate row within a given election and constituency

If `pref_votes` are present but `list_votes` are not, the function
assumes a voting system where list votes are calculated by summing the
preferential votes of candidates on the list.

If `const_size` is missing, it will be estimated based on the number of
elected candidates (if available).

For the purposes of analysis, a new variable `list_id` (class
[character](https://rdrr.io/r/base/character.html)) is added to the
internally processed copy of `df` and carried to the output. It uniquely
identifies each candidate list in a given election (combining
`list_name` and `elections`), e.g., *Besti Flokkurinn (2010)*, *SNP
(2019)*, or *"John Smith (5), IND. (2022.11)"*. This variable serves as
a **key identifier** in LPANDA for tracking candidate lists across
elections and constructing network relations.

## Output data structure

The returned object is a named [list](https://rdrr.io/r/base/list.html)
with up to seven network objects:

- `bipartite`: bipartite network (candidates-lists).

- `candidates`: projected candidate–candidate network.

- `lists`: projected list–list network (directed by election order).

- `continuity`: filtered version of `lists` network (edges of adjacent
  elections only).

- `parties`: network of detected party clusters (via community detection
  applied on `bipartite` network).

- (`cores`): higher-level clusters of `parties`. Cores are currently
  experimental and will not appear in the standard output network data.
  See **Note**.

- `elections`: inter-election candidate flow and election-level stats

Each object is a list with two components:

- `edgelist`: a [data.frame](https://rdrr.io/r/base/data.frame.html)
  representing network edges

- `node_attr`: a [data.frame](https://rdrr.io/r/base/data.frame.html)
  with attributes for each node

For example, `...$candidates$edgelist` contains edges between
individuals who appeared on the same candidate list, and
`...$elections$node_attr` includes several election statistics (e.g.,
number of candidates, distributed seats, plurality index, voter turnout
for each election, etc.).

## Examples

``` r
data(sample_different_varnames, package = "lpanda")
df <- sample_different_varnames
str(df) # different variable names: "party" and "seat"
#> tibble [18 × 5] (S3: tbl_df/tbl/data.frame)
#>  $ elections: num [1:18] 14 14 14 18 18 18 18 18 18 18 ...
#>  $ candidate: chr [1:18] "c01" "c02" "c03" "c01" ...
#>  $ party    : chr [1:18] "A" "A" "A" "A" ...
#>  $ seat     : logi [1:18] TRUE TRUE TRUE TRUE FALSE TRUE ...
#>  $ mayor    : logi [1:18] TRUE FALSE FALSE FALSE FALSE TRUE ...
input_variable_map <- list(list_name = "party", elected = "seat")
# \donttest{
netdata <- prepare_network_data(df, input_variable_map, verbose = FALSE)
str(netdata, vec.len = 1)
#> List of 6
#>  $ bipartite :List of 2
#>   ..$ edgelist :'data.frame':    18 obs. of  5 variables:
#>   .. ..$ from     : chr [1:18] "c01" ...
#>   .. ..$ to       : chr [1:18] "A (14)" ...
#>   .. ..$ elections: num [1:18] 14 14 ...
#>   .. ..$ elected  : logi [1:18] TRUE ...
#>   .. ..$ mayor    : logi [1:18] TRUE ...
#>   ..$ node_attr:'data.frame':    16 obs. of  20 variables:
#>   .. ..$ vertices         : chr [1:16] "c01" ...
#>   .. ..$ type             : logi [1:16] FALSE ...
#>   .. ..$ is_list          : logi [1:16] FALSE ...
#>   .. ..$ is_candidate     : logi [1:16] TRUE ...
#>   .. ..$ c_abbr           : chr [1:16] "c01" ...
#>   .. ..$ c_initials       : chr [1:16] "c01" ...
#>   .. ..$ c_candidacy      : int [1:16] 3 2 ...
#>   .. ..$ c_candidacy_ratio: num [1:16] 1 ...
#>   .. ..$ c_seats          : int [1:16] 2 1 ...
#>   .. ..$ c_seats_ratio    : num [1:16] 0.667 ...
#>   .. ..$ c_mayor          : int [1:16] 1 0 ...
#>   .. ..$ l_abbr           : chr [1:16] NA ...
#>   .. ..$ l_list_name      : chr [1:16] NA ...
#>   .. ..$ l_elections      : num [1:16] NA NA ...
#>   .. ..$ l_cands          : int [1:16] NA NA ...
#>   .. ..$ l_seats_n        : int [1:16] NA NA ...
#>   .. ..$ l_seats_pct      : num [1:16] NA NA ...
#>   .. ..$ l_success        : chr [1:16] NA ...
#>   .. ..$ l_mayor          : int [1:16] NA NA ...
#>   .. ..$ party            : num [1:16] 1 1 ...
#>  $ candidates:List of 2
#>   ..$ edgelist :'data.frame':    15 obs. of  3 variables:
#>   .. ..$ from  : chr [1:15] "c01" ...
#>   .. ..$ to    : chr [1:15] "c02" ...
#>   .. ..$ weight: num [1:15] 2 1 ...
#>   ..$ node_attr:'data.frame':    10 obs. of  10 variables:
#>   .. ..$ vertices       : chr [1:10] "c01" ...
#>   .. ..$ is_isolate     : logi [1:10] FALSE ...
#>   .. ..$ abbr           : chr [1:10] "c01" ...
#>   .. ..$ initials       : chr [1:10] "c01" ...
#>   .. ..$ candidacy      : int [1:10] 3 2 ...
#>   .. ..$ candidacy_ratio: num [1:10] 1 ...
#>   .. ..$ seats          : int [1:10] 2 1 ...
#>   .. ..$ seats_ratio    : num [1:10] 0.667 ...
#>   .. ..$ mayor          : int [1:10] 1 0 ...
#>   .. ..$ party          : num [1:10] 1 1 ...
#>  $ lists     :List of 2
#>   ..$ edgelist :'data.frame':    7 obs. of  3 variables:
#>   .. ..$ from  : chr [1:7] "A (14)" ...
#>   .. ..$ to    : chr [1:7] "A (18)" ...
#>   .. ..$ weight: num [1:7] 2 1 ...
#>   ..$ node_attr:'data.frame':    6 obs. of  11 variables:
#>   .. ..$ vertices  : chr [1:6] "A (14)" ...
#>   .. ..$ is_isolate: logi [1:6] FALSE ...
#>   .. ..$ abbr      : chr [1:6] "A" ...
#>   .. ..$ list_name : chr [1:6] "A" ...
#>   .. ..$ elections : num [1:6] 14 18 ...
#>   .. ..$ cands     : int [1:6] 3 3 ...
#>   .. ..$ seats_n   : int [1:6] 3 1 ...
#>   .. ..$ seats_pct : num [1:6] 100 ...
#>   .. ..$ success   : chr [1:6] "3/3" ...
#>   .. ..$ mayor     : int [1:6] 1 0 ...
#>   .. ..$ party     : num [1:6] 1 1 ...
#>  $ continuity:List of 2
#>   ..$ edgelist :'data.frame':    5 obs. of  3 variables:
#>   .. ..$ from  : chr [1:5] "A (14)" ...
#>   .. ..$ to    : chr [1:5] "A (18)" ...
#>   .. ..$ weight: num [1:5] 2 1 ...
#>   ..$ node_attr:'data.frame':    6 obs. of  11 variables:
#>   .. ..$ vertices  : chr [1:6] "A (14)" ...
#>   .. ..$ is_isolate: logi [1:6] FALSE ...
#>   .. ..$ abbr      : chr [1:6] "A" ...
#>   .. ..$ list_name : chr [1:6] "A" ...
#>   .. ..$ elections : num [1:6] 14 18 ...
#>   .. ..$ cands     : int [1:6] 3 3 ...
#>   .. ..$ seats_n   : int [1:6] 3 1 ...
#>   .. ..$ seats_pct : num [1:6] 100 ...
#>   .. ..$ success   : chr [1:6] "3/3" ...
#>   .. ..$ mayor     : int [1:6] 1 0 ...
#>   .. ..$ party     : num [1:6] 1 1 ...
#>  $ parties   :List of 2
#>   ..$ edgelist :'data.frame':    2 obs. of  3 variables:
#>   .. ..$ from  : num [1:2] 1 2
#>   .. ..$ to    : num [1:2] 3 3
#>   .. ..$ weight: num [1:2] 2 1
#>   ..$ node_attr:'data.frame':    3 obs. of  11 variables:
#>   .. ..$ vertices           : num [1:3] 1 2 ...
#>   .. ..$ is_isolate         : logi [1:3] FALSE ...
#>   .. ..$ lists              : chr [1:3] "A (14) | A (18) | A (22)" ...
#>   .. ..$ comp_by_lists      : chr [1:3] "100% A (14, 18, 22)" ...
#>   .. ..$ comp_by_nom_parties: logi [1:3] NA ...
#>   .. ..$ comp_by_pol_affil  : logi [1:3] NA ...
#>   .. ..$ group_label        : chr [1:3] "A (100%)" ...
#>   .. ..$ elections          : chr [1:3] "14, 18, 22" ...
#>   .. ..$ cands              : int [1:3] 5 3 ...
#>   .. ..$ seats_n            : int [1:3] 5 1 ...
#>   .. ..$ mayor              : int [1:3] 1 0 ...
#>  $ elections :List of 2
#>   ..$ edgelist :'data.frame':    3 obs. of  3 variables:
#>   .. ..$ from  : num [1:3] 14 14 ...
#>   .. ..$ to    : num [1:3] 18 22 ...
#>   .. ..$ weight: int [1:3] 3 2 ...
#>   ..$ node_attr:'data.frame':    3 obs. of  7 variables:
#>   .. ..$ vertices  : num [1:3] 14 18 ...
#>   .. ..$ is_isolate: logi [1:3] FALSE ...
#>   .. ..$ cands     : int [1:3] 3 9 ...
#>   .. ..$ seats     : num [1:3] 3 3 ...
#>   .. ..$ elected   : int [1:3] 3 3 ...
#>   .. ..$ lists     : int [1:3] 1 3 ...
#>   .. ..$ plurality : num [1:3] 1 3 ...
# }
```
