# Municipal Election Data: Potucky (KV, CZ)

A dataset containing individual-level candidacy records from municipal
elections in the municipality of Potucky (district Karlovy Vary, Czech
Republic).

## Usage

``` r
Potucky_KV_cz
```

## Format

An object of class `data.frame` with 130 rows and 14 columns.

## Source

The dataset was compiled primarily from official election results
published by the Czech Statistical Office. Additional contextual or
verification information (such as post-election roles) was obtained from
publicly available municipal records.

- [Czech Statistical Office](https://www.volby.cz/index_en.htm)

- [Municipality website](https://www.potucky-obec.cz/)

## Details

|                               |                                                      |
|-------------------------------|------------------------------------------------------|
| **Dataset overview:**         |                                                      |
| Municipality:                 | Potucky                                              |
| District:                     | Karlovy Vary                                         |
| Country:                      | Czech Republic                                       |
| Number of elections:          | 8                                                    |
| Elections covered:            | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022       |
| Number of candidacies (rows): | 130                                                  |
| Note:                         | [Municipality website](https://www.potucky-obec.cz/) |

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

- Hornek, J. (2017). Komunalni politika v malych bohatych obcich v Ceske
  republice: politicke souboje o zastupitelska kresla ci vsestranna
  kooperace ve prospech obci? (Local Government in Small Rich
  Municipalities in the Czech Republic: Political Battles for the Chairs
  of Representatives or Versatile Cooperation for the Benefits of
  Municipalities?). *Scientia et Societas*, 13(4), 115-143. \[[Full
  text](https://www.sets.cz/index.php/ojs/issue/view/19)\]

## Examples

``` r
# Basic inspection
str(Potucky_KV_cz)
#> 'data.frame':    130 obs. of  14 variables:
#>  $ elections   : int  1994 1994 1994 1994 1994 1994 1994 1994 1994 1994 ...
#>  $ candidate   : chr  "Schneider Milan" "Zuvač František" "Jukl Jozef" "Rauch Vladimír" ...
#>  $ list_name   : chr  "Milan Schneider, NK" "František Zuvač, NK" "Jozef Jukl, NK" "Vladimír Rauch, NK" ...
#>  $ list_pos    : int  1 1 1 1 1 2 3 4 5 6 ...
#>  $ pref_votes  : int  56 51 56 89 80 26 85 35 38 142 ...
#>  $ elected     : int  0 0 0 1 1 0 1 0 0 1 ...
#>  $ nom_party   : chr  "NK" "NK" "NK" "NK" ...
#>  $ pol_affil   : chr  "BEZPP" "BEZPP" "BEZPP" "BEZPP" ...
#>  $ mayor       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ dep_mayor   : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ board       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ gov_support : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ elig_voters : int  246 NA NA NA NA NA NA NA NA NA ...
#>  $ ballots_cast: int  168 NA NA NA NA NA NA NA NA NA ...

# Example of a basic continuity diagram (unformatted version)
plot_continuity(Potucky_KV_cz, elections = "2010-", lists = "elected")
```
