# Municipal Election Data: Dasnice (SO, CZ)

A dataset containing individual-level candidacy records from municipal
elections in the municipality of Dasnice (district Sokolov, Czech
Republic).

## Usage

``` r
Dasnice_SO_cz
```

## Format

An object of class `data.frame` with 81 rows and 14 columns.

## Source

The dataset was compiled primarily from official election results
published by the Czech Statistical Office. Additional contextual or
verification information (such as post-election roles) was obtained from
publicly available municipal records.

- [Czech Statistical Office](https://www.volby.cz/index_en.htm)

- [Municipality website](https://www.dasnice.eu/)

## Details

|                               |                                                                  |
|-------------------------------|------------------------------------------------------------------|
| **Dataset overview:**         |                                                                  |
| Municipality:                 | Dasnice                                                          |
| District:                     | Sokolov                                                          |
| Country:                      | Czech Republic                                                   |
| Number of elections:          | 10                                                               |
| Elections covered:            | 1994, 1998, 2002, 2006, 2010, 2014, 2015.09, 2016.04, 2018, 2022 |
| Number of candidacies (rows): | 81                                                               |
| Note:                         | [Municipality website](https://www.dasnice.eu/)                  |

## Description of variables

|              |                                            |
|--------------|--------------------------------------------|
| **Variable** | **Description**                            |
| elections    | Election identifiers (numeric)             |
| candidate    | Candidate's full name (character)          |
| list_name    | Name of the candidate list (character)     |
| list_pos     | Candidate's position on the list (numeric) |
| pref_votes   | Number of preferential votes (numeric)     |
| elected      | Logical; TRUE if candidate was elected     |
| nom_party    | Nominating party (character)               |
| pol_affil    | Political affiliation (character)          |
| mayor        | TRUE if elected mayor                      |
| dep_mayor    | TRUE if elected deputy mayor               |
| board        | TRUE if member of the executive board      |
| gov_support  | TRUE if supported the local government     |
| elig_voters  | Number of eligible voters (numeric)        |
| ballots_cast | Number of ballots cast (numeric)           |

Each record describes one candidate's run for office, including their
candidate list affiliation, position on the list, nominating party,
political affiliation, number of preferential votes, and whether they
were elected or held specific positions (mayor, deputy mayor, member of
the executive body).

The dataset also includes contextual election-level information, such as
the number of eligible voters and ballots cast, which can be used to
calculate voter turnout and related indicators. These variables appear
only once per election and constituency (they may be stored in a single
candidate row for that election/constituency)

## References

- Krpalkova, S. (2024). *Permanentni opakovani komunalnich voleb:
  zablokovane obce?* (Permanent repetition of municipal election:
  blocked municipalities?). Dissertation thesis. Charles University.
  \[[Full
  text](https://dspace.cuni.cz/handle/20.500.11956/191991?locale-attribute=en)\]

## Examples

``` r
# Basic inspection
str(Dasnice_SO_cz)
#> 'data.frame':    81 obs. of  14 variables:
#>  $ elections   : num  1994 1994 1994 1994 1994 ...
#>  $ candidate   : chr  "Cserge Koloman" "Springl Martin" "Forejtek Stanislav" "Hettová Ivana" ...
#>  $ list_name   : chr  "SNK" "SNK" "SNK" "SNK" ...
#>  $ list_pos    : int  1 2 3 4 1 1 1 2 3 4 ...
#>  $ pref_votes  : int  50 38 57 37 42 42 38 38 34 36 ...
#>  $ elected     : int  1 1 1 0 1 1 1 1 1 1 ...
#>  $ nom_party   : chr  "NK" "NK" "NK" "NK" ...
#>  $ pol_affil   : chr  "BEZPP" "BEZPP" "BEZPP" "BEZPP" ...
#>  $ mayor       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ dep_mayor   : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ board       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ gov_support : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ elig_voters : int  230 NA NA NA NA NA 250 NA NA NA ...
#>  $ ballots_cast: int  80 NA NA NA NA NA 43 NA NA NA ...

# Example of a basic continuity diagram (unformatted version)
plot_continuity(Dasnice_SO_cz, elections = "2010-")
```
