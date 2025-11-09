# Municipal Election Data: Kamenna (CB, CZ)

A dataset containing individual-level candidacy records from municipal
elections in the municipality of Kamenna (district Ceske Budejovice,
Czech Republic).

## Usage

``` r
Kamenna_CB_cz
```

## Format

An object of class `data.frame` with 178 rows and 14 columns.

## Source

The dataset was compiled primarily from official election results
published by the Czech Statistical Office. Additional contextual or
verification information (such as post-election roles) was obtained from
publicly available municipal records and the master thesis cited below.

- [Czech Statistical Office](https://www.volby.cz/index_en.htm)

- [Municipality website](https://www.oukamenna.cz/)

- Kotaskova, S. (2012). *Politicky proces v obci Kamenna* (The Political
  Process in the Municipality of Kamenna). Master thesis. Czech
  University of Life Sciences Prague. \[[Full
  text](https://theses.cz/id/5kd433/?lang=en)\]

## Details

|                               |                                                   |
|-------------------------------|---------------------------------------------------|
| **Dataset overview:**         |                                                   |
| Municipality:                 | Kamenna                                           |
| District:                     | Ceske Budejovice                                  |
| Country:                      | Czech Republic                                    |
| Number of elections:          | 8                                                 |
| Elections covered:            | 1998, 2002, 2006, 2010, 2011, 2014, 2018, 2022    |
| Number of candidacies (rows): | 178                                               |
| Note:                         | [Municipality website](https://www.oukamenna.cz/) |

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

- Kotaskova, S. K. (2016). Cleavages and political pluralism in the
  small municipality in Czech Republic. *Global Journal of Business,
  Economics* *and Management: Current Issues*, 5(2), 63-69. \[[Full
  text](https://doi.org/10.18844/gjbem.v5i2.369)\]

- Kotaskova, S. (2012). *Politicky proces v obci Kamenna* (The Political
  Process in the Municipality of Kamenna). Master thesis. Czech
  University of Life Sciences Prague. \[[Full
  text](https://theses.cz/id/5kd433/?lang=en)\]

- Kotaskova, S. (2010). *Analyza lokalniho stranickeho systemu v obci
  Kamenna* (Analysis of the Local Party System in the Kamenna
  Municipality). Bachelor thesis. Czech University of Life Sciences
  Prague. \[[Full text](https://theses.cz/id/iorq3v/?lang=en)\]

## Examples

``` r
# Basic inspection
str(Kamenna_CB_cz)
#> 'data.frame':    178 obs. of  14 variables:
#>  $ elections   : int  1998 1998 1998 1998 1998 1998 1998 1998 1998 1998 ...
#>  $ candidate   : chr  "Mikeš Václav" "Holý Václav" "Říha Václav" "Jaroš Jaroslav (1)" ...
#>  $ list_name   : chr  "SNK 1" "SNK 1" "SNK 1" "SNK 1" ...
#>  $ list_pos    : int  1 2 3 4 5 6 1 2 3 4 ...
#>  $ pref_votes  : int  74 81 84 78 91 67 64 38 22 21 ...
#>  $ elected     : int  1 1 1 1 1 0 1 1 0 0 ...
#>  $ nom_party   : chr  "NK" "NK" "NK" "NK" ...
#>  $ pol_affil   : chr  "BEZPP" "BEZPP" "BEZPP" "BEZPP" ...
#>  $ mayor       : int  0 1 0 0 0 0 0 0 0 0 ...
#>  $ dep_mayor   : int  0 0 1 0 0 0 0 0 0 0 ...
#>  $ board       : int  0 0 0 0 0 0 0 0 0 0 ...
#>  $ gov_support : int  1 1 1 1 1 0 0 0 0 0 ...
#>  $ elig_voters : int  179 NA NA NA NA NA NA NA NA NA ...
#>  $ ballots_cast: int  106 NA NA NA NA NA NA NA NA NA ...

# Example of a basic continuity diagram (unformatted version)
plot_continuity(Kamenna_CB_cz, elections = "2002-")
```
