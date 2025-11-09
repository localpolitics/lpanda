# Municipal Election Data: Cernosice (PZ, CZ)

A dataset containing individual-level candidacy records from municipal
elections in the municipality of Cernosice (district Praha-zapad, Czech
Republic).

## Usage

``` r
Cernosice_PZ_cz
```

## Format

An object of class `data.frame` with 971 rows and 14 columns.

## Source

The dataset was compiled primarily from official election results
published by the Czech Statistical Office. Additional contextual or
verification information (such as post-election roles) was obtained from
publicly available municipal records.

- [Czech Statistical Office](https://www.volby.cz/index_en.htm),

- [Municipality website](https://www.mestocernosice.cz/)

## Details

|                               |                                                        |
|-------------------------------|--------------------------------------------------------|
| **Dataset overview:**         |                                                        |
| Municipality:                 | Cernosice                                              |
| District:                     | Praha-zapad                                            |
| Country:                      | Czech Republic                                         |
| Number of elections:          | 8                                                      |
| Elections covered:            | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022         |
| Number of candidacies (rows): | 971                                                    |
| Note:                         | [Municipality website](https://www.mestocernosice.cz/) |

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

- Muller, K. B. (2018). *Dobre vladnuti ve verejnem nezajmu. Lokalni
  politicke* *elity jako klicovi akteri demokratizace?* (Good Governance
  in Public Disinterest. Local Political Elites as Key Actors of
  Democratization?). Prague: Sociologicke nakladatelstvi (SLON).
  \[[Publisher
  link](https://karolinum.cz/en/books/muller-dobre-vladnuti-ve-verejnem-nezajmu-25018)\]

- Muller, K. B. (2018). Dobre vladnuti a jeho akteri v postkomunistickem
  happyvillu aneb jak rozhodovat v zajmu tech, kteri nejsou slyset (Good
  governance and its actors in a post-communist happyville. How to
  govern in favour of those who remain salient). *Acta Politologica*
  10(1), 57-85. \[[Full
  text](https://www.researchgate.net/publication/323280769_Dobre_vladnuti_a_jeho_akteri_v_postkomunistickem_happyvillu_aneb_jak_rozhodovat_v_zajmu_tech_kteri_nejsou_slyset)\]

- Formackova, M. (2013). Vyvoj mistni samospravy v obci Cernosice (The
  Development of Local Government in the Municipality of Cernosice).
  Master thesis. Czech University of Life Sciences Prague. \[[Full
  text](https://theses.cz/id/l5jf3r/?lang=en)\]

- Hausmannova, H. (2011). *Obcanska spolecnost a politicke elity na
  komunalni urovni* (Civil Society and Political Elites on Local Level).
  Bachelor thesis. Prague University of Economics and Business. \[[Full
  text](https://vskp.vse.cz/english/27042)\]

## Examples

``` r
# Basic inspection
str(Cernosice_PZ_cz)
#> 'data.frame':    971 obs. of  14 variables:
#>  $ elections   : int  1994 1994 1994 1994 1994 1994 1994 1994 1994 1994 ...
#>  $ candidate   : chr  "Benešová Marta" "Králík Miroslav" "Šabata Aleš" "Prskavec Jan" ...
#>  $ list_name   : chr  "D92" "D92" "D92" "D92" ...
#>  $ list_pos    : int  1 2 3 4 5 6 7 8 9 10 ...
#>  $ pref_votes  : int  987 943 1056 818 965 764 702 692 750 982 ...
#>  $ elected     : int  1 1 1 0 1 0 0 0 0 1 ...
#>  $ nom_party   : chr  "NK" "NK" "NK" "NK" ...
#>  $ pol_affil   : chr  "BEZPP" "BEZPP" "BEZPP" "BEZPP" ...
#>  $ mayor       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ dep_mayor   : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ board       : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ gov_support : int  NA NA NA NA NA NA NA NA NA NA ...
#>  $ elig_voters : int  3345 NA NA NA NA NA NA NA NA NA ...
#>  $ ballots_cast: int  2425 NA NA NA NA NA NA NA NA NA ...

# Example of a basic continuity diagram (unformatted version)
plot_continuity(Cernosice_PZ_cz, elections = "2010-")
```
