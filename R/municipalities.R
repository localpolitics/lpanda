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
#' Each record describes one candidate's run for office, including their candidate
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
#' records and interviews with local political representatives.
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
#'   [[Full text](https://www.pef.czu.cz/dl/46317)]
#' - Bubenicek, V., & Kubalek, M. (2010). Konfliktni linie v malych obcich
#'   (Cleavages in Small Municipalities). *Acta Politologica*, 2(3), 30-45.
#'   [[Full text](https://acpo.fsv.cuni.cz/ACPONEN-48-version1-acpo_2010_03_04.pdf)]
#' - Cmejrek, J., Bubenicek, V., & Copik, J. (2010). *Demokracie v lokalnim politickem prostoru*
#'   (Democracy in Local Political Area). Prague: Grada.
#'   [[Publisher link](https://www.grada.cz/demokracie-v-lokalnim-politickem-prostoru-5880/)]
#' - Cmejrek, J. et al. (2009). *Participace obcanu na verejnem zivote venkovskych obci CR*
#'   (Citizens' Participation in the Public Life of Rural Municipalities in the Czech Republic).
#'   Prague: Kernberg Publishing.
#' - Bubenicek, V. (2009). Aplikace indexu plurality na lokalni politicke urovni
#'   (The Application of the Plurality Index in the Local Politics). In
#'   Svatos, M., Lostak, M., & Zuzak, R. (Eds.) *Sbornik praci z mezinarodni vedecke*
#'   *konference Agrarni perspektivy XVIII. Strategie pro budoucnost.* Prague: FEM CZU Prague.
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
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Doubice_DC_cz, elections = "2010-")}
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
#'   [[Full text](https://reference-global.com/article/10.2478/nispa-2020-0002)]
#' - Hornek, J. (2016). *Politicke dopady zadluzovani malych obci v Ceske republice*
#'   (Political Impacts of Indebtedness of Small Municipalities in the Czech Republic).
#'   Prague: Sociologicke nakladatelstvi (SLON).
#'   [[Publisher link](https://karolinum.cz/en/books/hornek-politicke-dopady-zadluzovani-malych-obci-v-ceske-republice-24996)]
#' - Hornek, J. (2014). *Politicke dopady zadluzovani malych obci v CR*
#'   (Financing of Small Municipalities in the Czech Republic and its Political Impact).
#'   Master thesis. Charles University.
#'   [[Full text](https://dspace.cuni.cz/handle/20.500.11956/67813?locale-attribute=en)]
#'
#' @examples
#' # Basic inspection
#' str(Bublava_SO_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Bublava_SO_cz, elections = "2006-")}
"Bublava_SO_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Cernosice (PZ, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Cernosice (district Praha-zapad, Czech
#' Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Cernosice |
#' |District:                   | Praha-zapad  |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 8 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022 |
#' |Number of candidacies (rows): | 971 |
#' |Note:                         | [Municipality website](https://www.mestocernosice.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm),
#' - [Municipality website](https://www.mestocernosice.cz/)
#'
#' @references
#' - Muller, K. B. (2018). *Dobre vladnuti ve verejnem nezajmu. Lokalni politicke*
#'   *elity jako klicovi akteri demokratizace?* (Good Governance in Public Disinterest.
#'   Local Political Elites as Key Actors of Democratization?).
#'   Prague: Sociologicke nakladatelstvi (SLON).
#'   [[Publisher link](https://karolinum.cz/en/books/muller-dobre-vladnuti-ve-verejnem-nezajmu-25018)]
#' - Muller, K. B. (2018). Dobre vladnuti a jeho akteri v postkomunistickem
#'   happyvillu aneb jak rozhodovat v zajmu tech, kteri nejsou slyset (Good
#'   governance and its actors in a post-communist happyville. How to govern in
#'   favour of those who remain salient). *Acta Politologica* 10(1), 57-85.
#'   [[Full text](https://acpo.fsv.cuni.cz/ACPONEN-56-version1-acpo_2018_01_04.pdf)]
#' - Formackova, M. (2013). Vyvoj mistni samospravy v obci Cernosice (The Development
#'   of Local Government in the Municipality of Cernosice). Master thesis.
#'   Czech University of Life Sciences Prague.
#'   [[Full text](https://theses.cz/id/l5jf3r/?lang=en)]
#' - Hausmannova, H. (2011). *Obcanska spolecnost a politicke elity na komunalni urovni*
#'   (Civil Society and Political Elites on Local Level). Bachelor thesis.
#'   Prague University of Economics and Business.
#'   [[Full text](https://vskp.vse.cz/english/27042)]
#'
#' @examples
#' # Basic inspection
#' str(Cernosice_PZ_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Cernosice_PZ_cz, elections = "2010-")}
"Cernosice_PZ_cz"

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
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.dasnice.eu/)
#'
#' @references
#' - Krpalkova, S. (2024). *Permanentni opakovani komunalnich voleb: zablokovane obce?*
#'   (Permanent repetition of municipal election: blocked municipalities?).
#'   Dissertation thesis. Charles University.
#'   [[Full text](https://dspace.cuni.cz/handle/20.500.11956/191991?locale-attribute=en)]
#'
#' @examples
#' # Basic inspection
#' str(Dasnice_SO_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Dasnice_SO_cz, elections = "2010-")}
"Dasnice_SO_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Horomerice (PZ, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Horomerice (district Praha-zapad, Czech
#' Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Horomerice |
#' |District:                   | Praha-zapad |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 8 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022 |
#' |Number of candidacies (rows): | 438 |
#' |Note:                         | [Municipality website](https://www.horomerice.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.horomerice.cz/)
#' - Kadlecova, S. (2013). *Lokalni stranicky system v obci Horomerice*
#'   (Local Party System in the Municipality of Horomerice).
#'   Bachelor thesis. Czech University of Life Sciences Prague.
#'   [[Full text](https://theses.cz/id/9eylkt/?lang=en)]
#' - Bares, M. (2009). *Spolecensky a politicky zivot v obci Horomerice*
#'   (The Public Life and the Political Process in Horomerice).
#'   Bachelor thesis. Czech University of Life Sciences Prague.
#'   [[Full text](https://theses.cz/id/8bgzon/?lang=en)]
#'
#' @references
#' - Vobecka, J., & Kostelecky, T. (Eds.) (2007). *Politicke dusledky suburbanizace.*
#'   (Political Consequences of Suburbanization). Prague: Institute of Sociology
#'   of the Czech Academy of Sciences.
#'   [[Full text](https://www.soc.cas.cz/images/drupal/publikace/259_ss_07_08.pdf)]
#' - Kadlecova, S. (2013). *Lokalni stranicky system v obci Horomerice*
#'   (Local Party System in the Municipality of Horomerice).
#'   Bachelor thesis. Czech University of Life Sciences Prague.
#'   [[Full text](https://theses.cz/id/9eylkt/?lang=en)]
#' - Maxa, D. (2024). *Formovani organu mistni samospravy v obci Horomerice.*
#'   (The process of Setting Up the Local Government in the Municipality of
#'   Horomerice). Bachelor thesis. Czech University of Life Sciences Prague.
#'   [[Full text](https://theses.cz/id/bfxe93/?lang=en)]
#' - Novakova, N. (2025). *Politicke aspekty rizeni a rozvoje vybrane obce*
#'   *(pripadova studie obce Horomerice)* (Political Aspects of Management
#'   and the Development of the Chosen Municipality (Case Study of Horomerice).
#'   Bachelor thesis. Czech University of Life Sciences Prague.
#'   [[Full text](https://theses.cz/id/2wmh4o/?lang=en)]
#'
#' @examples
#' # Basic inspection
#' str(Horomerice_PZ_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Horomerice_PZ_cz, elections = "2010-")}
"Horomerice_PZ_cz"

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
#' |Note:                         | [Municipality website](https://www.hradce.cz/obec/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records.
#' 
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.hradce.cz/obec/)
#'
#' @references
#' - Krpalkova, S. (2024). *Permanentni opakovani komunalnich voleb: zablokovane obce?*
#'   (Permanent repetition of municipal election: blocked municipalities?).
#'   Dissertation thesis. Charles University.
#'   [[Full text](https://dspace.cuni.cz/handle/20.500.11956/191991?locale-attribute=en)]
#'
#' @examples
#' # Basic inspection
#' str(Hradce_CB_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Hradce_CB_cz, elections = "2006-")}
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
#' |Note:                         | [Municipality website](https://www.mujilove.cz/mujilove) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records and diploma thesis cited below.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.mujilove.cz/mujilove)
#' - Pohlreich, D. (2023). *Vyvoj mistni samospravy ve meste Jilove*
#'   (The Development of Local Government in the Municipality of Jilove).
#'   Diploma thesis. Czech University of Life Sciences Prague.
#'   [[Full text](https://theses.cz/id/e46aac/?lang=en)]
#' 
#' @references
#' - Pohlreich, D. (2023). *Vyvoj mistni samospravy ve meste Jilove*
#'   (The Development of Local Government in the Municipality of Jilove).
#'   Diploma thesis. Czech University of Life Sciences Prague.
#'   [[Full text](https://theses.cz/id/e46aac/?lang=en)]
#'   
#' @examples
#' # Basic inspection
#' str(Jilove_DC_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Jilove_DC_cz, elections = "1994-2010")}
"Jilove_DC_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Kamenna (CB, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Kamenna (district Ceske Budejovice, Czech
#' Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Kamenna |
#' |District:                   | Ceske Budejovice  |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 8 |
#' |Elections covered:          | 1998, 2002, 2006, 2010, 2011, 2014, 2018, 2022|
#' |Number of candidacies (rows): | 178 |
#' |Note:                         | [Municipality website](https://www.oukamenna.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records and the master thesis cited below.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.oukamenna.cz/)
#' - Kotaskova, S. (2012). *Politicky proces v obci Kamenna* (The Political
#'   Process in the Municipality of Kamenna). Master thesis. Czech University
#'   of Life Sciences Prague. [[Full text](https://theses.cz/id/5kd433/?lang=en)]
#'
#' @references
#' - Kotaskova, S. K. (2016). Cleavages and political pluralism in the small
#'   municipality in Czech Republic. *Global Journal of Business, Economics*
#'   *and Management: Current Issues*, 5(2), 63-69.
#' - Kotaskova, S. (2012). *Politicky proces v obci Kamenna* (The Political
#'   Process in the Municipality of Kamenna). Master thesis. Czech University
#'   of Life Sciences Prague. [[Full text](https://theses.cz/id/5kd433/?lang=en)]
#' - Kotaskova, S. (2010). *Analyza lokalniho stranickeho systemu v obci Kamenna*
#'   (Analysis of the Local Party System in the Kamenna Municipality). Bachelor
#'   thesis. Czech University of Life Sciences Prague.
#'   [[Full text](https://theses.cz/id/iorq3v/?lang=en)]
#'
#' @examples
#' # Basic inspection
#' str(Kamenna_CB_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Kamenna_CB_cz, elections = "2002-")}
"Kamenna_CB_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Nebanice (CH, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Nebanice (district Cheb, Czech Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Nebanice |
#' |District:                   | Cheb  |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 9 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2019, 2022 |
#' |Number of candidacies (rows): | 136 |
#' |Note:                         | [Municipality website](https://www.nebanice.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.nebanice.cz/)
#'
#' @references
#' - Hornek, J., & Juptner, P. (2020). Endangered Municipalities? Case Study of Three Small
#'   and Critically Indebted Czech Municipalities.
#'   *NISPAcee Journal of Public Administration and Policy*, 13(1), 35-59.
#'   [[Full text](https://reference-global.com/article/10.2478/nispa-2020-0002)]
#' - Hornek, J. (2016). *Politicke dopady zadluzovani malych obci v Ceske republice*
#'   (Political Impacts of Indebtedness of Small Municipalities in the Czech Republic).
#'   Prague: Sociologicke nakladatelstvi (SLON).
#'   [[Publisher link](https://karolinum.cz/en/books/hornek-politicke-dopady-zadluzovani-malych-obci-v-ceske-republice-24996)]
#' - Hornek, J. (2014). *Politicke dopady zadluzovani malych obci v CR*
#'   (Financing of Small Municipalities in the Czech Republic and its Political Impact).
#'   Master thesis. Charles University.
#'   [[Full text](https://dspace.cuni.cz/handle/20.500.11956/67813?locale-attribute=en)]
#'
#' @examples
#' # Basic inspection
#' str(Nebanice_CH_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Nebanice_CH_cz, elections = "2010-")}
"Nebanice_CH_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Potucky (KV, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Potucky (district Karlovy Vary, Czech
#' Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Potucky |
#' |District:                   | Karlovy Vary |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 8 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022 |
#' |Number of candidacies (rows): | 130 |
#' |Note:                         | [Municipality website](https://www.potucky-obec.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.potucky-obec.cz/)
#'
#' @references
#' - Hornek, J. (2017). Komunalni politika v malych bohatych obcich v Ceske republice:
#'   politicke souboje o zastupitelska kresla ci vsestranna kooperace ve prospech obci?
#'   (Local Government in Small Rich Municipalities in the Czech Republic: Political
#'   Battles for the Chairs of Representatives or Versatile Cooperation for the Benefits
#'   of Municipalities?). *Scientia et Societas*, 13(4), 115-143.
#'   [[Full text](https://www.sets.cz/index.php/ojs/issue/view/19)]
#'
#' @examples
#' # Basic inspection
#' str(Potucky_KV_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Potucky_KV_cz, elections = "2010-", lists = "elected")}
"Potucky_KV_cz"

# --------------------------------------------------------------------------- #

#' @title Municipal Election Data: Prameny (CH, CZ)
#'
#' @description
#' A dataset containing individual-level candidacy records from municipal
#' elections in the municipality of Prameny (district Cheb, Czech Republic).
#'
#' @details
#' |**Dataset overview:** ||
#' |---------------------------|--------|
#' |Municipality:               | Prameny |
#' |District:                   | Cheb |
#' |Country:                    | Czech Republic |
#' |Number of elections:        | 11 |
#' |Elections covered:          | 1994, 1998, 2002, 2006, 2009, 2012, 2014, 2015, 2018.01, 2018.10, 2022 |
#' |Number of candidacies (rows): | 117 |
#' |Note:                         | [Municipality website](https://www.pramenyobec.cz/) |
#'
#' @inheritSection Doubice_DC_cz Description of variables
#'
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.pramenyobec.cz/)
#'
#' @references
#' - Hornek, J. (2022). *Zhroucene obce v Ceske republice* (Failed Municipalities in the Czech Republic).
#'   Dissertation thesis. Charles University.
#'   [[Full text](https://dspace.cuni.cz/handle/20.500.11956/177784?locale-attribute=en)]
#' - Hornek, J., & Juptner, P. (2020). Endangered Municipalities? Case Study of Three Small
#'   and Critically Indebted Czech Municipalities.
#'   *NISPAcee Journal of Public Administration and Policy*, 13(1), 35-59.
#'   [[Full text](https://reference-global.com/article/10.2478/nispa-2020-0002)]
#' - Hornek, J. (2019). Endangered European Municipalities: A Systematic Outline
#'   of the Problem and Its Political Impact. *Politics in Central Europe*, 15(2), 219-256.
#'   [[Full text](https://reference-global.com/article/10.2478/pce-2019-0016)]
#' - Hornek, J. (2016). *Politicke dopady zadluzovani malych obci v Ceske republice*
#'   (Political Impacts of Indebtedness of Small Municipalities in the Czech Republic)
#'   [[Publisher link](https://karolinum.cz/en/books/hornek-politicke-dopady-zadluzovani-malych-obci-v-ceske-republice-24996)]
#' - Hornek, J. (2014). *Politicke dopady zadluzovani malych obci v CR*
#'   (Financing of Small Municipalities in the Czech Republic and its Political Impact).
#'   Master thesis. [[Full text](https://dspace.cuni.cz/handle/20.500.11956/67813?locale-attribute=en)]
#'
#' @examples
#' # Basic inspection
#' str(Prameny_CH_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Prameny_CH_cz, elections = "2012-")}
"Prameny_CH_cz"

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
#' @source
#' The dataset was compiled primarily from official election results published by
#' the Czech Statistical Office. Additional contextual or verification information
#' (such as post-election roles) was obtained from publicly available municipal
#' records and interviews with local political representatives.
#'
#' - [Czech Statistical Office](https://www.volby.cz/index_en.htm)
#' - [Municipality website](https://www.roztoky.cz/)
#'
#' @references
#' - Kubalek, M., & Bubenicek, V. (2012). Charakter lokalni politiky v suburbannim
#'   politickem prostoru (The Nature of Local Politics in Suburban Political Space).
#'   *Acta Politologica*, 4(3), 284-305.
#'   [[Full text](https://acpo.fsv.cuni.cz/ACPONEN-50-version1-acpo_2012_03_05.pdf)]
#'
#' @examples
#' # Basic inspection
#' str(Roztoky_PZ_cz)
#'
#' # Example of a basic continuity diagram (unformatted version)
#' \donttest{plot_continuity(Roztoky_PZ_cz, elections = "2002-")}
"Roztoky_PZ_cz"

# --------------------------------------------------------------------------- #