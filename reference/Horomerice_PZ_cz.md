# Municipal Election Data: Horomerice (PZ, CZ)

A dataset containing individual-level candidacy records from municipal
elections in the municipality of Horomerice (district Praha-zapad, Czech
Republic).

## Usage

``` r
Horomerice_PZ_cz
```

## Format

An object of class `data.frame` with 438 rows and 14 columns.

## Source

The dataset was compiled primarily from official election results
published by the Czech Statistical Office. Additional contextual or
verification information (such as post-election roles) was obtained from
publicly available municipal records.

- [Czech Statistical Office](https://www.volby.cz/index_en.htm)

- [Municipality website](https://www.horomerice.cz/)

- Kadlecova, S. (2013). *Lokalni stranicky system v obci Horomerice*
  (Local Party System in the Municipality of Horomerice). Bachelor
  thesis. Czech University of Life Sciences Prague. \[[Full
  text](https://theses.cz/id/9eylkt/?lang=en)\]

- Bares, M. (2009). *Spolecensky a politicky zivot v obci Horomerice*
  (The Public Life and the Political Process in Horomerice). Bachelor
  thesis. Czech University of Life Sciences Prague. \[[Full
  text](https://theses.cz/id/8bgzon/?lang=en)\]

## Details

|                               |                                                    |
|-------------------------------|----------------------------------------------------|
| **Dataset overview:**         |                                                    |
| Municipality:                 | Horomerice                                         |
| District:                     | Praha-zapad                                        |
| Country:                      | Czech Republic                                     |
| Number of elections:          | 8                                                  |
| Elections covered:            | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022     |
| Number of candidacies (rows): | 438                                                |
| Note:                         | [Municipality website](https://www.horomerice.cz/) |

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

- Vobecka, J., & Kostelecky, T. (Eds.) (2007). *Politicke dusledky
  suburbanizace.* (Political Consequences of Suburbanization). Prague:
  Institute of Sociology of the Czech Academy of Sciences. \[[Full
  text](https://www.soc.cas.cz/images/drupal/publikace/259_ss_07_08.pdf)\]

- Kadlecova, S. (2013). *Lokalni stranicky system v obci Horomerice*
  (Local Party System in the Municipality of Horomerice). Bachelor
  thesis. Czech University of Life Sciences Prague. \[[Full
  text](https://theses.cz/id/9eylkt/?lang=en)\]

- Maxa, D. (2024). *Formovani organu mistni samospravy v obci
  Horomerice.* (The process of Setting Up the Local Government in the
  Municipality of Horomerice). Bachelor thesis. Czech University of Life
  Sciences Prague. \[[Full text](https://theses.cz/id/bfxe93/?lang=en)\]

- Novakova, N. (2025). *Politicke aspekty rizeni a rozvoje vybrane obce*
  *(pripadova studie obce Horomerice)* (Political Aspects of Management
  and the Development of the Chosen Municipality (Case Study of
  Horomerice). Bachelor thesis. Czech University of Life Sciences
  Prague. \[[Full text](https://theses.cz/id/2wmh4o/?lang=en)\]

## Examples

``` r
# Basic inspection
str(Horomerice_PZ_cz)
#> 'data.frame':    438 obs. of  14 variables:
#>  $ elections   : int  1994 1994 1994 1994 1994 1994 1994 1994 1994 1994 ...
#>  $ candidate   : chr  "Horák Jiří" "Popelka Ladislav" "Abelová Jana" "Jirásková Marie" ...
#>  $ list_name   : chr  "KSČM" "KSČM" "KSČM" "KSČM" ...
#>  $ list_pos    : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ pref_votes  : int  338 225 234 154 242 138 142 120 187 127 ...
#>  $ elected     : int  1 0 1 0 1 0 0 0 0 0 ...
#>  $ nom_party   : chr  "NK" "KSČM" "NK" "KSČM" ...
#>  $ pol_affil   : chr  "BEZPP" "KSČM" "BEZPP" "KSČM" ...
#>  $ mayor       : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ dep_mayor   : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ board       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ gov_support : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ elig_voters : int  1308 NA NA NA NA NA NA NA NA NA ...
#>  $ ballots_cast: int  956 NA NA NA NA NA NA NA NA NA ...

# Example of a basic continuity diagram (unformatted version)
plot_continuity(Horomerice_PZ_cz, elections = "2010-")
```
