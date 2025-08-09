# --------------------------------------------------------------------------- #
# ============================= Jednotlive datasety ===========================
# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Doubice (DC, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Doubice (district Decin, Czech republic).
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
#' voter turnout and related indicators.
#'
#' @source [Czech Statistical Office](https://www.volby.cz/index_en.htm),
#' [Municipality website](https://oudoubice.cz/),
#' [Acta Politologica article](https://www.researchgate.net/publication/323573475_Konfliktni_linie_v_malych_obcich)
"Doubice_DC_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Jilove (DC, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Jilove (district Decin, Czech republic).
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
