# --------------------------------------------------------------------------- #
# ============================= Jednotlive datasety ===========================
# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Doubice (DC, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Doubice (district Decin, Czech Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:              | Doubice |
#' |District:                  | Decin  |
#' |Country:                   | Czech Republic |
#' |Number of elections:       | 11 |
#' |Elections covered:         | 1993, 1994, 1998, 2002, 2006, 2007, 2010, 2014, 2015, 2018, 2022|
#' |Number of candidacies (rows): | 151 |
#' |Note:                         | [Municipality website](https://oudoubice.cz/) |
#'
#' @section Description of variables:
#' |**Variable** | **Description** |
#' |---------------------------|--------|
#' |elections|Election identifiers (numeric)|
#' |candidate|Candidate's full name (character)|
#' |list_name|Name of the candidate list (character)|
#' |list_pos|Candidate's position on the list (numeric)|
#' |pref_votes|Number of preferential votes (numeric)|
#' |elected|Logical; TRUE if candidate was elected|
#' |nom_party|Nominating party (character)|
#' |pol_affil|Political affiliation (character)|
#' |mayor|TRUE if elected mayor|
#' |dep_mayor|TRUE if elected deputy mayor|
#' |board|TRUE if member of the executive board|
#' |gov_support|TRUE if supported the local government|
#' |elig_voters|Number of eligible voters (numeric)|
#' |ballots_cast|Number of ballots cast (numeric)|
#'
#' Each record describes one candidate’s run for office, including their candidate
#' list affiliation, position on the list, nominating party, political affiliation,
#' number of preferential votes, and whether they were elected or held specific
#' positions (mayor, deputy mayor, member of the executive body).
#'
#' The dataset also includes contextual election-level information, such as the
#' number of eligible voters and ballots cast, which can be used to calculate
#' voter turnout and related indicators. These variables appear only once per
#' election and constituency (they may be stored in a single candidate row for
#' that election/constituency)
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records and relevant academic works listed below.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://oudoubice.cz/)
#' - Bubenicek, V. (2009). Doubice. In Cmejrek, J. et al.,
#'   *Participace obcanu na verejnem zivote venkovskych obci CR*
#'   (Citizens' Participation in the Public Life of Rural Municipalities in the Czech Republic).
#'   Prague: Kernberg Publishing.
#'
#' @references
#' - Bubenicek, V. (2010). *Lokalni modely demokracie v malych obcich CR*
#'   (Local Models of Democracy in Small Municipalities). Dissertation thesis.
#'   Czech University of Life Sciences Prague.
#'   [[Full text](https://www.researchgate.net/publication/323573647_Lokalni_modely_demokracie_v_malych_obcich_CR)]
#' - Bubenicek, V., & Kubalek, M. (2010). Konfliktni linie v malych obcich
#'   (Cleavages in Small Municipalities). *Acta Politologica*, 2(3), 30-45.
#'   [[Full text](https://www.researchgate.net/publication/323573475_Konfliktni_linie_v_malych_obcich)]
#' - Cmejrek, J., Bubenicek, V., & Copik, J. (2010). *Demokracie v lokalnim politickem prostoru*
#'   (Democracy in Local Political Area). Prague: Grada.
#'   [[Publisher link](https://www.grada.cz/demokracie-v-lokalnim-politickem-prostoru-5880/)]
#' - Cmejrek, J. et al. (2009). *Participace obcanu na verejnem zivote venkovskych obci CR*
#'   (Citizens' Participation in the Public Life of Rural Municipalities in the Czech Republic).
#'   Prague: Kernberg Publishing.
#' - Bubenicek, V., Copik, J., Hajny, P., Kopriva, R., & Neumanova, T. (Eds.) (2005).
#'   *Obce jako akteri politickeho procesu: komunitni studie regionalnich politickych systemu*
#'   *a problematika metodiky jejich zpracovani* (Municipalities as Actors of the Political
#'   Process: Case Studies of Regional Political Systems and Methodology of Their Elaboration).
#'   Prague: FEM CZU Prague.
#'
#' @examples
#' # Basic inspection
#' str(Doubice_DC_cz)
#'
#' # Quick continuity diagram
#' \donttest{plot_continuity(Doubice_DC_cz)}
"Doubice_DC_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Bublava (SO, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Bublava (district Sokolov, Czech Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Bublava |
#' |District:                   | Sokolov |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 8 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022 |
#' |Number of candidacies (rows): | 193 |
#' |Note:                         | [Municipality website](https://www.obecbublava.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records.
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.obecbublava.cz/)
#'
#' @references
#' - Hornek, J. (2022). *Zhroucene obce v Ceske republice* (Failed Municipalities in the Czech Republic).
#'   Dissertation thesis. Charles University.
#'   [[Full text](https://dspace.cuni.cz/handle/20.500.11956/177784?locale-attribute=en)]
#' - Hornek, J., & Juptner, P. (2020). Endangered Municipalities? Case Study of Three Small
#'   and Critically Indebted Czech Municipalities.
#'   *NISPAcee Journal of Public Administration and Policy*, 13(1), 35-59.
#'   [[Full text](https://doi.org/10.2478/nispa-2020-0002)]
#' - Hornek, J. (2016). *Politicke dopady zadluzovani malych obci v Ceske republice*
#'   (Political Impacts of Indebtedness of Small Municipalities in the Czech Republic)
#'   [[Publisher link](https://karolinum.cz/en/books/hornek-politicke-dopady-zadluzovani-malych-obci-v-ceske-republice-24996)]
#' - Hornek, J. (2014). *Politicke dopady zadluzovani malych obci v CR*
#'   (Financing of Small Municipalities in the Czech Republic and its Political Impact).
#'   Master thesis. [[Full text](https://dspace.cuni.cz/handle/20.500.11956/67813?locale-attribute=en)]
#'
#' @examples
#' # Basic inspection
#' str(Bublava_SO_cz)
#'
#' # Quick continuity diagram
#' \donttest{plot_continuity(Bublava_SO_cz)}
"Bublava_SO_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Dasnice (SO, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Dasnice (district Sokolov, Czech Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Dasnice |
#' |District:                   | Sokolov |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 10 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2010, 2014, 2015.09, 2016.04, 2018, 2022 |
#' |Number of candidacies (rows): | 81 |
#' |Note:                         | [Municipality website](https://www.dasnice.eu/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source [Czech Statistical Office](https://www.volby.cz/index_en.htm),
#' [Municipality website](https://www.dasnice.eu/),
#' [Dissertation thesis](https://dspace.cuni.cz/handle/20.500.11956/191991?locale-attribute=en)
"Dasnice_SO_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Hradce (CB, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Hradce (district Ceske Budejovice, Czech
#' Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Hradce |
#' |District:                   | Ceske Budejovice |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 10 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2010, 2014, 2016, 2017, 2018, 2022 |
#' |Number of candidacies (rows): | 103 |
#' |Note:                         | [Municipality website](https://www.hradce.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source [Czech Statistical Office](https://www.volby.cz/index_en.htm),
#' [Municipality website](https://www.hradce.cz/),
#' [Dissertation thesis](https://dspace.cuni.cz/handle/20.500.11956/191991?locale-attribute=en)
"Hradce_CB_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Jilove (DC, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Jilove (district Decin, Czech Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Jilove |
#' |District:                   | Decin  |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 8 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022|
#' |Number of candidacies (rows): | 745|
#' |Note:                         | [Municipality website](https://www.mujilove.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source [Czech Statistical Office](https://www.volby.cz/index_en.htm),
#' [Municipality website](https://www.mujilove.cz/),
#' [Student thesis](https://theses.cz/id/e46aac/?lang=en)
"Jilove_DC_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Roztoky (PZ, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Roztoky (district Praha-zapad, Czech Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Roztoky |
#' |District:                   | Praha-zapad |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 8 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022 |
#' |Number of candidacies (rows): | 1079 |
#' |Note:                         | [Municipality website](https://www.roztoky.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source [Czech Statistical Office](https://www.volby.cz/index_en.htm),
#' [Municipality website](https://www.roztoky.cz/),
#' [Acta Politologica article](https://www.researchgate.net/publication/323573707_Charakter_lokalni_politiky_v_suburbannim_politickem_prostoru)
"Roztoky_PZ_cz"

# --------------------------------------------------------------------------- #