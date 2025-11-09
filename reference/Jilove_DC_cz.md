# Municipal Election Data: Jilove (DC, CZ)

A dataset containing individual-level candidacy records from municipal
elections in the municipality of Jilove (district Decin, Czech
Republic).

## Usage

``` r
Jilove_DC_cz
```

## Format

An object of class `data.frame` with 745 rows and 14 columns.

## Source

The dataset was compiled primarily from official election results
published by the Czech Statistical Office. Additional contextual or
verification information (such as post-election roles) was obtained from
publicly available municipal records and diploma thesis cited below.

- [Czech Statistical Office](https://www.volby.cz/index_en.htm)

- [Municipality website](https://www.mujilove.cz/)

- Pohlreich, D. (2023). *Vyvoj mistni samospravy ve meste Jilove* (The
  Development of Local Government in the Municipality of Jilove).
  Diploma thesis. Czech University of Life Sciences Prague. \[[Full
  text](https://theses.cz/id/e46aac/?lang=en)\]

## Details

|                               |                                                  |
|-------------------------------|--------------------------------------------------|
| **Dataset overview:**         |                                                  |
| Municipality:                 | Jilove                                           |
| District:                     | Decin                                            |
| Country:                      | Czech Republic                                   |
| Number of elections:          | 8                                                |
| Elections covered:            | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022   |
| Number of candidacies (rows): | 745                                              |
| Note:                         | [Municipality website](https://www.mujilove.cz/) |

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

- Pohlreich, D. (2023). *Vyvoj mistni samospravy ve meste Jilove* (The
  Development of Local Government in the Municipality of Jilove).
  Diploma thesis. Czech University of Life Sciences Prague. \[[Full
  text](https://theses.cz/id/e46aac/?lang=en)\]

## Examples

``` r
# Basic inspection
str(Jilove_DC_cz)
#> 'data.frame':    745 obs. of  14 variables:
#>  $ elections   : int  1994 1994 1994 1994 1994 1994 1994 1994 1994 1994 ...
#>  $ candidate   : chr  "Frejvald Jiří" "Váňo Josef" "Zenkl František" "Vrba František" ...
#>  $ list_name   : chr  "KSČM" "KSČM" "KSČM" "KSČM" ...
#>  $ list_pos    : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ pref_votes  : int  568 493 426 486 410 443 415 412 402 430 ...
#>  $ elected     : int  1 1 0 1 0 0 0 0 0 0 ...
#>  $ nom_party   : chr  "KSČM" "KSČM" "KSČM" "KSČM" ...
#>  $ pol_affil   : chr  "KSČM" "KSČM" "KSČM" "BEZPP" ...
#>  $ mayor       : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ dep_mayor   : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ board       : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ gov_support : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ elig_voters : int  3712 NA NA NA NA NA NA NA NA NA ...
#>  $ ballots_cast: int  1978 NA NA NA NA NA NA NA NA NA ...

# Example of a basic continuity diagram (unformatted version)
plot_continuity(Jilove_DC_cz, elections = "1994-2010")
```
