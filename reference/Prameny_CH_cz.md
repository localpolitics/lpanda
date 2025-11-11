# Municipal Election Data: Prameny (CH, CZ)

A dataset containing individual-level candidacy records from municipal
elections in the municipality of Prameny (district Cheb, Czech
Republic).

## Usage

``` r
Prameny_CH_cz
```

## Format

An object of class `data.frame` with 117 rows and 14 columns.

## Source

The dataset was compiled primarily from official election results
published by the Czech Statistical Office. Additional contextual or
verification information (such as post-election roles) was obtained from
publicly available municipal records.

- [Czech Statistical Office](https://www.volby.cz/index_en.htm)

- [Municipality website](https://www.pramenyobec.cz/)

## Details

|                               |                                                                        |
|-------------------------------|------------------------------------------------------------------------|
| **Dataset overview:**         |                                                                        |
| Municipality:                 | Prameny                                                                |
| District:                     | Cheb                                                                   |
| Country:                      | Czech Republic                                                         |
| Number of elections:          | 11                                                                     |
| Elections covered:            | 1994, 1998, 2002, 2006, 2009, 2012, 2014, 2015, 2018.01, 2018.10, 2022 |
| Number of candidacies (rows): | 117                                                                    |
| Note:                         | [Municipality website](https://www.pramenyobec.cz/)                    |

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

- Hornek, J. (2022). *Zhroucene obce v Ceske republice* (Failed
  Municipalities in the Czech Republic). Dissertation thesis. Charles
  University. \[[Full
  text](https://dspace.cuni.cz/handle/20.500.11956/177784?locale-attribute=en)\]

- Hornek, J., & Juptner, P. (2020). Endangered Municipalities? Case
  Study of Three Small and Critically Indebted Czech Municipalities.
  *NISPAcee Journal of Public Administration and Policy*, 13(1), 35-59.
  \[[Full text](https://doi.org/10.2478/nispa-2020-0002)\]

- Hornek, J. (2019). Endangered European Municipalities: A Systematic
  Outline of the Problem and Its Political Impact. *Politics in Central
  Europe*, 15(2), 219-256. \[[Full
  text](https://doi.org/10.2478/pce-2019-0016)\]

- Hornek, J. (2016). *Politicke dopady zadluzovani malych obci v Ceske
  republice* (Political Impacts of Indebtedness of Small Municipalities
  in the Czech Republic) \[[Publisher
  link](https://karolinum.cz/en/books/hornek-politicke-dopady-zadluzovani-malych-obci-v-ceske-republice-24996)\]

- Hornek, J. (2014). *Politicke dopady zadluzovani malych obci v CR*
  (Financing of Small Municipalities in the Czech Republic and its
  Political Impact). Master thesis. \[[Full
  text](https://dspace.cuni.cz/handle/20.500.11956/67813?locale-attribute=en)\]

## Examples

``` r
# Basic inspection
str(Prameny_CH_cz)
#> 'data.frame':    117 obs. of  14 variables:
#>  $ elections   : num  1994 1994 1994 1994 1994 ...
#>  $ candidate   : chr  "Baroch František" "Brenner Václav" "Šplíchalová Vlasta" "Bartůněk Jiří" ...
#>  $ list_name   : chr  "František Baroch, NK" "Václav Brenner, NK" "Vlasta Šplíchalová, NK" "Jiří Bartůněk, NK" ...
#>  $ list_pos    : int  1 1 1 1 1 1 1 1 1 1 ...
#>  $ pref_votes  : int  8 58 40 51 52 48 47 26 30 30 ...
#>  $ elected     : int  0 1 1 1 1 1 1 1 1 1 ...
#>  $ nom_party   : chr  "NK" "NK" "NK" "NK" ...
#>  $ pol_affil   : chr  "BEZPP" "BEZPP" "BEZPP" "BEZPP" ...
#>  $ mayor       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ dep_mayor   : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ board       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ gov_support : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ elig_voters : int  181 NA NA NA NA NA NA 89 NA NA ...
#>  $ ballots_cast: int  166 NA NA NA NA NA NA 36 NA NA ...

# Example of a basic continuity diagram (unformatted version)
plot_continuity(Prameny_CH_cz, elections = "2012-")
```
