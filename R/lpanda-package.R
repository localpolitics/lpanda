#' @title lpanda: Local Political Actor Network Diachronic Analysis Tools
#'
#' @section Main functions:
#' The main user-facing functions of `lpanda` are:
#'
#' - \code{\link{prepare_network_data}}() - prepares network objects from
#'                                          municipal election results.
#' - \code{\link{plot_continuity}}() - creates continuity diagrams of local
#'                                     political actors' candidacies over time.
#'
#' Together, these functions facilitate diachronic analysis of local political
#' actor networks and the evolution of local party systems, particularly in
#' small municipalities.
#'
#'
#' @section Basic workflow:
#' The typical workflow in `lpanda` centres on *continuity diagrams* that trace
#' candidacies of local political actors across multiple elections.
#'
#' To create a continuity diagram of candidacies of local political actors,
#' it is necessary to prepare election data containing at least unique names
#' of candidates, unique names of their candidate lists and the years of the
#' elections. In case of same names, they need to be distinguished, e.g., by
#' adding numbers after the names of candidates (for example "Doe Jane (2)"
#' or "Smith John, Jr.") or candidate lists (for example "Independents 3").
#'
#' For a basic continuity diagram, you can directly pass the data into the
#' \code{\link{plot_continuity}}() function. However, for deeper analysis,
#' it is recommended to first process the basic election data using
#' \code{\link{prepare_network_data}}(), which creates a list of network
#' objects, and only then experiment with the various parameters offered
#' by \code{\link{plot_continuity}}().
#'
#'
#' @section Municipality datasets:
#' The `lpanda` package contains several datasets of municipal election results
#' that have been the objects of case studies in various publications focused
#' on Czech local politics. The datasets are compiled from publicly available
#' data, and in some cases supplemented by findings from field research. They
#' can be used to reproduce continuity diagrams published in studies referenced
#' in the documentation of the datasets, for their deeper analysis, or simply
#' to experiment with the workflow.
#'
#' List of municipality datasets:
#' - \code{\link{Bublava_SO_cz}}
#' - \code{\link{Cernosice_PZ_cz}}
#' - \code{\link{Dasnice_SO_cz}}
#' - \code{\link{Doubice_DC_cz}}
#' - \code{\link{Horomerice_PZ_cz}}
#' - \code{\link{Hradce_CB_cz}}
#' - \code{\link{Jilove_DC_cz}}
#' - \code{\link{Kamenna_CB_cz}}
#' - \code{\link{Nebanice_CH_cz}}
#' - \code{\link{Potucky_KV_cz}}
#' - \code{\link{Prameny_CH_cz}}
#' - \code{\link{Roztoky_PZ_cz}}
#'
#'
#' @references
#' The continuity diagram approach implemented in `lpanda` builds on earlier
#' attempts to visualise candidate movements ("defections") between political
#' parties across elections. Over time, these visualisations evolved into
#' a more general method for analysing the continuity of local political
#' actors' candidacies. The method has been progressively formalised and
#' systematised so that it can be reused across different case studies.
#'
#' For an overview of the development of this approach and its applications
#' in Czech local politics research, see the selected studies below, as well
#' as the publications listed in the documentation of the individual datasets.
#'
#' Bubenicek, V. (2010). *Lokalni modely demokracie v malych obcich CR*
#' (Local Models of Democracy in Small Municipalities). Dissertation thesis.
#' Czech University of Life Sciences Prague.
#' [[Full text](https://www.researchgate.net/publication/323573647_Lokalni_modely_demokracie_v_malych_obcich_CR)]
#'
#' Bubenicek, V., & Kubalek, M. (2010). Konfliktni linie v malych obcich
#' (Cleavages in Small Municipalities). *Acta Politologica*, 2(3), 30-45.
#' [[Full text](https://www.researchgate.net/publication/323573475_Konfliktni_linie_v_malych_obcich)]
#'
#' Cmejrek, J., Bubenicek, V., & Copik, J. (2010). *Demokracie v lokalnim politickem prostoru*
#' (Democracy in Local Political Area). Prague: Grada.
#' [[Publisher link](https://www.grada.cz/demokracie-v-lokalnim-politickem-prostoru-5880/)]
#'
#' Kubalek, M., & Bubenicek, V. (2012). Charakter lokalni politiky v suburbannim
#' politickem prostoru (The Nature of Local Politics in Suburban Political Space).
#' *Acta Politologica*, 4(3), 284-305.
#' [[Full text](https://www.researchgate.net/publication/323573707_Charakter_lokalni_politiky_v_suburbannim_politickem_prostoru)]
#'
#' Kotaskova, S. K. (2016). Cleavages and political pluralism in the small
#' municipality in Czech Republic. *Global Journal of Business, Economics and Management: Current Issues*, 5(2), 63-69.
#' [[Full text](https://doi.org/10.18844/gjbem.v5i2.369)]
#'
#' Hornek, J. (2022). *Zhroucene obce v Ceske republice* (Failed Municipalities in the Czech Republic).
#' Dissertation thesis. Charles University.
#' [[Full text](https://dspace.cuni.cz/handle/20.500.11956/177784?locale-attribute=en)]
#'
#' Krpalkova, S. (2024). *Permanentni opakovani komunalnich voleb: zablokovane obce?*
#' (Permanent Repetition of Municipal Election: Blocked Municipalities?).
#' Dissertation thesis. Charles University.
#' [[Full text](https://dspace.cuni.cz/handle/20.500.11956/191991?locale-attribute=en)]
#'
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom magrittr %>%
## usethis namespace: end
NULL
