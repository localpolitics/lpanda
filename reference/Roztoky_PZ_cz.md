# Municipal Election Data: Roztoky (PZ, CZ)

A dataset containing individual-level candidacy records from municipal
elections in the municipality of Roztoky (district Praha-zapad, Czech
Republic).

## Usage

``` r
Roztoky_PZ_cz
```

## Format

An object of class `data.frame` with 1079 rows and 14 columns.

## Source

The dataset was compiled primarily from official election results
published by the Czech Statistical Office. Additional contextual or
verification information (such as post-election roles) was obtained from
publicly available municipal records and interviews with local political
representatives.

- [Czech Statistical Office](https://www.volby.cz/index_en.htm)

- [Municipality website](https://www.roztoky.cz/)

## Details

|                               |                                                 |
|-------------------------------|-------------------------------------------------|
| **Dataset overview:**         |                                                 |
| Municipality:                 | Roztoky                                         |
| District:                     | Praha-zapad                                     |
| Country:                      | Czech Republic                                  |
| Number of elections:          | 8                                               |
| Elections covered:            | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022  |
| Number of candidacies (rows): | 1079                                            |
| Note:                         | [Municipality website](https://www.roztoky.cz/) |

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

- Kubalek, M., & Bubenicek, V. (2012). Charakter lokalni politiky v
  suburbannim politickem prostoru (The Nature of Local Politics in
  Suburban Political Space). *Acta Politologica*, 4(3), 284-305. \[[Full
  text](https://acpo.fsv.cuni.cz/ACPONEN-50-version1-acpo_2012_03_05.pdf)\]

## Examples

``` r
# Basic inspection
str(Roztoky_PZ_cz)
#> 'data.frame':    1079 obs. of  14 variables:
#>  $ elections   : int  1994 1994 1994 1994 1994 1994 1994 1994 1994 1994 ...
#>  $ candidate   : chr  "Kantor Ladislav" "Tříska Petr" "Svoboda Miroslav" "Kinský Michal" ...
#>  $ list_name   : chr  "ODA" "ODA" "ODA" "ODA" ...
#>  $ list_pos    : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ pref_votes  : int  438 897 590 275 295 357 256 229 319 232 ...
#>  $ elected     : int  0 1 1 0 0 0 0 0 0 0 ...
#>  $ nom_party   : chr  "ODA" "ODA" "ODA" "ODA" ...
#>  $ pol_affil   : chr  "ODA" "BEZPP" "BEZPP" "BEZPP" ...
#>  $ mayor       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ dep_mayor   : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ board       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ gov_support : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ elig_voters : int  4631 NA NA NA NA NA NA NA NA NA ...
#>  $ ballots_cast: int  3181 NA NA NA NA NA NA NA NA NA ...

# Example of a basic continuity diagram (unformatted version)
plot_continuity(Roztoky_PZ_cz, elections = "2002-")
```
