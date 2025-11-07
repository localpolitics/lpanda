# Municipal Election Data: Hradce (CB, CZ)

A dataset containing individual-level candidacy records from municipal
elections in the municipality of Hradce (district Ceske Budejovice,
Czech Republic).

## Usage

``` r
Hradce_CB_cz
```

## Format

An object of class `data.frame` with 103 rows and 14 columns.

## Source

The dataset was compiled primarily from official election results
published by the Czech Statistical Office. Additional contextual or
verification information (such as post-election roles) was obtained from
publicly available municipal records.

- [Czech Statistical Office](https://www.volby.cz/index_en.htm)

- [Municipality website](https://www.hradce.cz/)

## Details

|                               |                                                            |
|-------------------------------|------------------------------------------------------------|
| **Dataset overview:**         |                                                            |
| Municipality:                 | Hradce                                                     |
| District:                     | Ceske Budejovice                                           |
| Country:                      | Czech Republic                                             |
| Number of elections:          | 10                                                         |
| Elections covered:            | 1994, 1998, 2002, 2006, 2010, 2014, 2016, 2017, 2018, 2022 |
| Number of candidacies (rows): | 103                                                        |
| Note:                         | [Municipality website](https://www.hradce.cz/)             |

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

Each record describes one candidate’s run for office, including their
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
str(Hradce_CB_cz)
#> 'data.frame':    103 obs. of  14 variables:
#>  $ elections   : int  1994 1994 1994 1994 1994 1994 1994 1994 1994 1998 ...
#>  $ candidate   : chr  "Bártová Ludmila" "Andrle František" "Dána Jiří" "Šafránek František" ...
#>  $ list_name   : chr  "Ludmila Bártová, NK" "František Andrle, NK" "Jiří Dána, NK" "František Šafránek, NK" ...
#>  $ list_pos    : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ pref_votes  : int  14 18 15 16 5 18 4 12 16 13 ...
#>  $ elected     : int  0 1 1 1 0 1 0 0 1 1 ...
#>  $ nom_party   : chr  "NK" "NK" "NK" "NK" ...
#>  $ pol_affil   : chr  "BEZPP" "BEZPP" "BEZPP" "BEZPP" ...
#>  $ mayor       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ dep_mayor   : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ board       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ gov_support : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ elig_voters : int  31 NA NA NA NA NA NA NA NA 29 ...
#>  $ ballots_cast: int  25 NA NA NA NA NA NA NA NA 23 ...

# Quick continuity diagram (basic and unformatted version)
plot_continuity(Hradce_CB_cz, elections = "2006-")
```
