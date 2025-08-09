#' @title Visualization of Candidacy Continuity Diagram
#'
#' @description
#' Visualizes the continuity of candidacies over time, illustrating the evolution
#' of the local party system through a network of candidate lists linked by
#' candidate transitions across elections.
#'
#' @param netdata A named list created by \code{\link{prepare_network_data}}
#'                containing the continuity network data. Alternatively,
#'                a \link[base]{data.frame} can also be used, but is recommended
#'                only for quick or exploratory plotting of a basic continuity
#'                diagram.
#'
#' @param mark Character or character vector. Specifies which type of group
#'             should be visually distinguished in the diagram. Options include
#'             "parties", "cores", or c("candidate", "candidate name"). Defaults
#'             to NULL (no group highlighting). See *Details* and *Examples* for
#'             usage.
#'
#' @param separate_groups Logical. If TRUE, groups of candidate lists are plotted
#'                        in separate rows on the y-axis, improving clarity for
#'                        group-level analysis. See *Details*.
#'
#' @param lists Character. Candidate lists to be included in the plot. Either
#'              "all" (default) or "elected" to include only lists with at least
#'              one elected candidate (councillor).
#'
#' @param elections Character or character vector. Filters the range of elections
#'                  to be shown in the diagram. By default (`NULL`), all available
#'                  elections in the `netdata` object are included. You can specify:
#'                  individual elections (e.g., `"1994", "2022"`), ranges (e.g.,
#'                  `"2002-"`, `"-2010"`, `"1994-2010"`) or combinations of both
#'                  (e.g., `"-1998, 2002, 2003.11, 2018-"`). See *Details* and
#'                  *Examples* for more information and usage.
#'
#' @param show_elections_between Logical. If TRUE (default), the plot includes
#'                               all election periods between those selected via
#'                               the `elections` argument, even if no candidate
#'                               lists are present for those years because of the
#'                               selection. This is especially useful when
#'                               visualizing groups that did not run in every
#'                               election — empty columns help preserve the visual
#'                               continuity of timelines. Setting this to FALSE
#'                               will omit those gaps. Recommended to keep TRUE
#'                               when analyzing individual groups or when filtering
#'                               only a subset of elections.
#'
#' @param parties Integer or character vector. Filters the so-called political
#'                parties, i.e., groups of candidate lists identified via community
#'                detection (see \code{\link{prepare_network_data}}). Use this
#'                to display only selected parties, for example: \code{parties = c(1, 3, 5)}.
#'                Party IDs can be found in the network data object under
#'                \code{netdata$parties$node_attr$vertices}.
#'
#' @param links Character. Determines which links between candidate lists are
#'              plotted. `"continuity"` (default) includes only connections between
#'              *adjacent* elections. `"all"` includes links across any elections.
#'              This option is mainly useful when analyzing a selection of
#'              non-consecutive elections.
#'
#' @param order_lists_by Character. Sorts candidate lists within each election
#'                       vertically. Options are: "votes" (default) or "seats".
#'                       If `separate_groups = TRUE`, sorting is applied within
#'                       each group.
#'
#' @param order_groups_by Character vector. Used when `separate_groups = TRUE`.
#'                        Specifies the order of groups on the y-axis. Options:
#'                        "elections", "votes", "seats", or "none". Multiple
#'                        criteria can be provided in order of priority. To display
#'                        groups in the order they are listed in `netdata`, use
#'                        "none" or NULL. See *Details* for more information.
#'
#' @param personalization Logical. If TRUE, appends the coefficient of variation
#'                        of preferential votes to the candidate list name. See
#'                        *Details* for interpretation. Default is FALSE.
#'
#' @param coloured Logical. Specifies whether candidate lists of different groups
#'                 should be distinguished in colour (TRUE, default) or in grayscale
#'                 when using the `mark` argument. Ignored if `group_colours` is
#'                 provided.
#'
#' @param group_colours A character vector of colour values (e.g., hex codes or
#'                      R colour names). Custom colours for marked groups. To
#'                      maintain the same colours when displaying the diagram
#'                      repeatedly, the number of colours (elements in the vector)
#'                      must match the number of all identified groups, even if
#'                      only a subset is shown. If NULL (default), the function
#'                      will select the most appropriate colour palette.
#'
#' @param show_legend Logical. Whether to display the legend (only applies when
#'                    groups are marked). Default is TRUE.
#'
#' @param plot_title Character. Title displayed above the diagram. Default is
#'                   NULL (no title).
#'
#' @param ... Additional technical arguments passed internally, primarily for
#'            testing and improving the diagram display.
#'
#' @details
#' ## Recommendation about using the raw data
#' For more advanced use, especially when identifying political parties or
#' analyzing system stability, it is recommended to first process the election
#' data using \code{\link{prepare_network_data}}. This function builds the
#' necessary network structures and attributes also for groups of candidate lists
#' that sometimes takes few minutes but you would need to do it only once. Using
#' raw data frames as input in case of \code{plot_continuity} is intended mainly
#' for quick and basic visualizations, without the group identification.
#'
#'
#' ## Usage of `mark` argument
#' A central feature of this function is the `mark` argument, which allows highlighting
#' of specific groups in the diagram. The most common options are `"parties"` or
#' `"cores"`, referring to communities of candidate lists detected through community
#' detection.
#'
#' When using `mark = "parties"` or `"cores"`, you can further specify which groups
#' to highlight visually by adding their IDs (e.g., \code{mark = c("parties", 2, 5)}).
#' Party and core IDs are available in \code{netdata$parties$node_attr$vertices}
#' or \code{netdata$cores$node_attr$vertices}.
#'
#' You can also highlight individual candidates by using \code{mark = c("candidate",
#' "Candidate Name")}, which will highlight the candidate lists on which the person
#' has appeared in colours of the candidate lists' groups.
#'
#' You may combine the `mark` argument with group separation, and filtering.
#'
#'
#' ## Groups separation
#' The `separate_groups` argument improves diagram readability by placing each
#' group on its own line. This is particularly helpful when analyzing continuity,
#' volatility, and structural reproduction of the party system.
#'
#'
#' ## Elections filtering
#' Filtering elections using the `elections` argument is useful when dealing with
#' many elections that may not fit into a single figure in a report or publication.
#' In such cases, you can split the diagram into two parts (e.g., one with
#' `elections = "-2002"` and one with `elections = "2002-"`, so that the links
#' between the elections adjacent to the 2002 elections are not lost) and stack
#' them vertically.
#'
#' When selecting non-consecutive elections, it is **strongly recommended** to set
#' \code{links = "all"} to retain meaningful connections between candidate lists
#' across time. Otherwise, continuity may appear broken due to missing
#' intermediate elections.
#'
#' For a meaningful continuity analysis, include at least two elections.
#'
#'
#' ## About `order_groups_by` argument
#' The `order_groups_by` argument is relevant only when `separate_groups = TRUE`.
#' You can sort groups by `"elections"`, `"votes"`, `"seats"`, or `"none"` (the
#' original order in the data). If multiple criteria are provided (e.g.,
#' \code{c("votes", "elections")}), they are applied in priority order. The criteria
#' of `"votes"` and `"seats"` will sort the groups according to the value of
#' the given criterion. The `"elections"` criterion ranks groups based on their
#' participation in the most recent election and falls back recursively to earlier
#' ones in case of ties.
#'
#'
#' ## About `personalization` argument
#' The `personalization` option appends the coefficient of variation of preferential
#' votes to the name of each candidate list. A lower value may indicate a party's
#' electoral program voting, while higher variability may suggest a personalized
#' choice (for example, where voters support a prominent individual rather than
#' the whole candidate list). In the case of a limited number of preferential votes,
#' such an interpretation may be debatable and should therefore be used with caution.
#'
#' @note
#' The `mark = "cores"` option is currently experimental, as the conversion
#' of their definition into code is still being sought, and may be subject to
#' change in future versions. Use with caution.
#'
#' @returns \code{NULL}, invisibly. Called for its side effect: plotting the
#'          continuity diagram.
#'
#' @export
#'
#' @importFrom igraph V E graph_from_data_frame make_clusters membership
#' @importFrom igraph communities crossing
#' @importFrom stats setNames
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom grDevices colorRampPalette adjustcolor
#' @importFrom graphics par strwidth layout segments text mtext points legend
#' @importFrom scales grey_pal
#' @importFrom utils head tail
#'
#' @examples
#' data(sample_data, package = "lpanda")
#'
#' # basic continuity diagram
#' plot_continuity(sample_data)
#'
#' # preparing network data
#' netdata <- prepare_network_data(sample_data, verbose = FALSE, quick = TRUE)
#'
#' # highlighting groups
#' plot_continuity(netdata, mark = "parties")
#' plot_continuity(netdata, mark = c("parties", 3), order_lists = "seats")
#' plot_continuity(netdata, mark = "parties", separate_groups = TRUE, show_legend = FALSE)
#'
#' # candidate tracking
#' plot_continuity(netdata, mark = c("candidate", "c03"))
#'
#' # filtering elections and parties
#' plot_continuity(netdata, mark = "parties", elections = "18-")
#' plot_continuity(netdata, elections = c(14, 22), links = "all", show_elections_between = FALSE)
#' plot_continuity(netdata, parties = 1)
#'
plot_continuity <- function(netdata,
                            mark = NULL, # mark group of "parties" / "cores" / "candidate" / NULL = none
                            separate_groups = FALSE, # each party in one line in the plot
                            lists = c("all", "elected"), # elected = council only
                            elections = NULL, # e.g., "2002-", "1994-2010" or "1994,2022"
                            show_elections_between = TRUE, # lepsi TRUE tr. pri filtraci jedne skupiny
                            parties = NULL, # e.g., c(1, 3, 5)
                            links = c("continuity", "all"), # edges from continuity/lists network
                            order_lists_by = c("votes", "seats"), # vertical order
                            order_groups_by = c("elections", "votes", "seats"), # votes / seats / elections progression / NULL or "none" (only if separate_groups = TRUE)
                            personalization = FALSE, # TRUE if $coef_var is available
                            coloured = TRUE,
                            group_colours = c(), # user's own colours for groups
                            show_legend = TRUE, # show legend? (just if mark is not NULL)
                            plot_title = NULL,
                            ...
) {
  
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # ============ Kontrola dat a identifikace dostupnych promennych ============
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  # ------------------------------------------------------------------------- #
  # Kontrola netdata:
  # ------------------------------------------------------------------------- #

  if (inherits(netdata, "list")) {
    
    if (!all(c("continuity", "elections") %in% names(netdata))) {
      stop(paste0("The network data required to display the continuity diagram\n",
                  "is missing in the input data."))
    }
    
  } else if (inherits(netdata, "data.frame")) {
    
    if (is.null(mark) && separate_groups == FALSE) {
      
      # only for displaying quick diagrams, so all messages and information are
      # suppressed (but not warnings)
      netdata <- suppressMessages(prepare_network_data(netdata,
                                                       skip_groups = TRUE,
                                                       verbose = FALSE));
    } else {
      netdata <- prepare_network_data(netdata);
    }; # konec IF-ELSE pro pripravu dat s nebo bez skupin
    
  } else {
    
    stop(paste("Input network data ('netdata') must be a list or dataframe. ",
               "Unsupported input type: ", class(netdata), ".", sep = ""));
    
  }; # konec IF-ELSE IF-ELSE pro kontrolu vstupnich dat
  
  
  # ------------------------------------------------------------------------- #
  # Extrahovani argumentu z "..."
  # ------------------------------------------------------------------------- #
  
  args <- list(...);
  
  allowed_args <- c("cores",
                    "axis_distances",
                    "margins",
                    "do_not_print_to_console");
  
  extra_args <- setdiff(names(args), allowed_args);
  if (length(extra_args) > 0) {
    warning("Unknown arguments in '...': ", paste(extra_args, collapse = ", "), ".")
  };
  
  # ------------------------------------------------------------------------- #
  # Kontrola jednotlivych argumentu ze zadani (jak explicitnich, tak z "...")
  # ------------------------------------------------------------------------- #
  
  links <- match.arg(links);
  network <- if (links == "all" && "lists" %in% names(netdata)) "lists" else "continuity";
  
  # --- #
  
  lists <- match.arg(lists);
  
  # --- #
  
  if (!is.null(mark)) {
    
    if (tolower(mark[1]) %in% c("party")) mark[1] <- "parties";
    if (tolower(mark[1]) %in% c("candidate", "c", "cand")) mark[1] <- "candidates";
    
    mark[1] <- match.arg(mark[1], choices = c("parties", "candidates", "cores", "none"));
    
    if (mark[1] == "candidates") show_legend <- FALSE;
    
    if (!is.null(netdata[[mark[1]]])) {
      if (length(mark) > 1) {
        mark <- c(mark[1],
                  mark[-1][mark[-1] %in% netdata[[mark[1]]]$node_attr$vertices])

        if (mark[1] == "candidates" && length(mark) > 1) {
          mark <- mark[1:2]
        } else if (mark[1] == "candidates" && length(mark) == 1) {
          mark <- NULL;
        } # konec IF-ELSE IF pro kontrolu "candidates"
        
      } else if (mark[1] == "candidates" && length(mark) == 1) {
        mark <- NULL
      } else mark <- mark[1]
      
    } else {
      mark <- NULL;
    } # konec IF-ELSE pro spravny format promenne mark
  } # konec IF pro kontrolu mark, pokud neni NULL
  
  # --- #
  
  order_lists_by <- match.arg(order_lists_by);
  
  # --- #
  
  if (!is.null(order_groups_by)) {
    order_groups_by <- tolower(order_groups_by);
    if (any(c("none", "no", "n") %in% order_groups_by)) {
      order_groups_by <- NULL;
    } else {
      order_groups_by <- match.arg(order_groups_by, several.ok = TRUE);
      order_groups_by <- unique(order_groups_by);
    } # konec IF-ELSE identifikujici obsah argumentu
  } # konec IF pro urceni obsahu argumentu order_groups_by
  
  # --- #
  
  cores <- args$cores;
  
  # --- #
  
  axis_distances <- args$axis_distances;
  
  if (is.null(axis_distances) || !is.list(axis_distances)) {

    axis_distances <- list(x = 0.6, y = 0.3, gr = 0.4);
    
  } else {
    if (is.null(axis_distances$x) || !is.numeric(axis_distances$x)) axis_distances$x <- 0.6;
    if (is.null(axis_distances$y) || !is.numeric(axis_distances$y)) axis_distances$y <- 0.3;
    if (is.null(axis_distances$gr) || !is.numeric(axis_distances$gr)) axis_distances$gr <- 0.4;
  } # konec IF-ELSE pro nastaveni hodnot do axis_distances
  
  # --- #
  
  margins <- args$margins;
  
  if (is.null(margins) || !is.numeric(margins) || length(margins) != 4) {
    margins <- c(2,0,3,0);
  } # konec IF pro nastaveni hodnot margins
  
  # --- #
  
  plot_title <- plot_title;
  
  # --- #
  
  do_not_print_to_console <- isTRUE(args$do_not_print_to_console);
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # ============================= Vytvoreni grafu =============================
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  ## Filtrace ====

  elections <- select_elections(netdata$elections$node_attr$vertices,
                                filter = elections);
  
  lists <- if (lists[1] == "elected" && "seats_n" %in% names(netdata[[network]]$node_attr)) {
    netdata[[network]]$node_attr$vertices[netdata[[network]]$node_attr$seats_n > 0]
  } else {
    netdata[[network]]$node_attr$vertices;
  }; # konec IF-ELSE pro vybrani zahrnutych kandidatnich listin
  
  parties <- if (is.null(parties) || !any(parties %in% netdata$parties$node_attr$vertices)) {
    netdata$parties$node_attr$vertices
  } else {
    netdata$parties$node_attr$vertices[netdata$parties$node_attr$vertices %in%
                                         parties]
  } # konec IF-ELSE pro filtraci stran
  
  cores <- if (is.null(cores) || !any(cores %in% netdata$cores$node_attr$vertices)) {
    netdata$cores$node_attr$vertices
  } else {
    netdata$cores$node_attr$vertices[netdata$cores$node_attr$vertices %in%
                                       cores];
  } # konec IF-ELSE pro filtraci stran
  
  # ------------------------------------------------------------------------- #
  
  nodes <- netdata[[network]]$node_attr;
  nodes <- nodes[nodes$elections %in% elections &
                   nodes$vertices %in% lists &
                   if (is.null(parties)) rep(TRUE, nrow(nodes)) else nodes$party %in% parties &
                   if (is.null(cores)) rep(TRUE, nrow(nodes)) else nodes$core %in% cores, ];
  edges <- netdata[[network]]$edgelist;
  edges <- edges[edges$from %in% nodes$vertices & edges$to %in% nodes$vertices,];
  
  if (!is.null(mark[1])) {
    if (mark[1] == "cores" && !is.null(cores)) {
      group_membership <- stats::setNames(nodes$core, nodes$vertices);
    } else if (!is.null(parties)) {
      group_membership <- stats::setNames(nodes$party, nodes$vertices);
    } else {
      group_membership <- NULL;
    }; # konec IF-ELSE IF-ELSE pro group_membership podle mark
  } else if (separate_groups && !is.null(parties)) {
    group_membership <- stats::setNames(nodes$party, nodes$vertices);
  } else {
    group_membership <- NULL;
  }; # konec IF-ELSE pro vyplneni group_membership pokud je/neni zadano mark
  
  pocet.skupin <- length(unique(group_membership));
  
  pocet.skupin.celkem <- if (!is.null(mark[1])) {
    if (mark[1] == "cores" && !is.null(cores)) {
      length(unique(netdata[[network]]$node_attr$core));
    } else length(unique(netdata[[network]]$node_attr$party));
  } else length(unique(netdata[[network]]$node_attr$party));
  
  # ------------------------------------------------------------------------- #
  
  ## igraph objects =====
  g <- igraph::graph_from_data_frame(d = edges, vertices = nodes, directed = TRUE);
  skupiny <- if ((is.null(mark) && separate_groups == FALSE) || is.null(group_membership)) {
    NULL
  } else {
    igraph::make_clusters(graph = g, membership = group_membership);
  }; # konec IF pro skupiny = NULL, kdyz mark = NULL
  
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # ========================== Vytvoreni koordinatu ===========================
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  included_elections <- netdata$elections$node_attr$vertices;
  
  if (show_elections_between) {
    included_elections <- included_elections[
      included_elections >= utils::head(sort(elections),1) &
        included_elections <= utils::tail(sort(elections),1)];
  } else {
    included_elections <- sort(unique(nodes$elections));
  } # konec IF-ELSE pro zarazeni do diagramu prazdna volebni obdobi mezi vybranymi
  # volbami nebo ne
  
  koordinaty <- matrix(nrow = nrow(nodes),
                       ncol = 2,
                       dimnames = list(igraph::V(g)$name,c("x","y")));
  
  ## ========================== Priprava osy x ================================
  
  pocet.voleb <- length(included_elections);
  rozsah.x    <- axis_distances$x * (pocet.voleb - 1) / 2;
  
  osa.x <- seq(-rozsah.x, rozsah.x, length.out = pocet.voleb);
  
  ## =============== Priprava osy y a doplneni koordinatu =====================
  
  poradi.skupin <- if (pocet.skupin > 0) 1:pocet.skupin else NULL;
  
  # ------------------------------------------------------------------------- #
  
  if (separate_groups && !is.null(skupiny)) {
    
    ### Oddelene skupiny =====

    mezery.osy.y <- c();
    
    #### Poradi skupin =====
    
    skupiny.k.serazeni <- if (!is.null(mark)) {
      
      switch(mark[1],
             "parties"    = list(parties = nodes$party),
             "candidates" = list(parties = nodes$party),
             "cores"      = list(cores   = nodes$core),
             list(parties = nodes$party));
    } else list(parties = nodes$party);
    
    if (!is.null(order_groups_by) && !is.null(skupiny.k.serazeni[[1]])) {
      
      tab.poradi.skupin <- data.frame(group = sort(unique(skupiny.k.serazeni[[1]])),
                                      elections = NA,
                                      votes = NA,
                                      seats = NA);
      
      tab.poradi.skupin <- tab.poradi.skupin[,c("group", order_groups_by)]
      
      if ("elections" %in% order_groups_by) {
        poradi.voleb <- data.frame(group = tab.poradi.skupin$group);
        poradi.voleb <- dplyr::left_join(poradi.voleb,
                                         netdata[[names(skupiny.k.serazeni)[1]]]$node_attr[,c("vertices",
                                                                                              "elections")],
                                         by = c("group" = "vertices"));
        
        poradi.tmp <- lapply(strsplit(poradi.voleb$elections, split = ", "),
                             function(x) sort(x, decreasing = TRUE));
        poradi.voleb$obracene <- unlist(lapply(poradi.tmp,
                                               function(x) paste(x, collapse = ", ")));
        
        tab.poradi.skupin$elections <- rank(poradi.voleb$obracene, ties.method = "min")
        
      } else tab.poradi.skupin$elections <- NA; # konec IF-ELSE pro serazeni podle voleb
      
      if ("votes" %in% order_groups_by) {
        if ("votes_n" %in% names(netdata[[names(skupiny.k.serazeni)[1]]]$node_attr)) {
          poradi.hlasu <- netdata[[names(skupiny.k.serazeni)[1]]]$node_attr$votes_n[
            netdata[[names(skupiny.k.serazeni)[1]]]$node_attr$vertices %in% tab.poradi.skupin$group];
          tab.poradi.skupin$votes <- rank(poradi.hlasu, ties.method = "min");
        } else tab.poradi.skupin$votes <- NA;
      } else tab.poradi.skupin$votes <- NA; # konec IF-ELSE pro serazeni podle
      # poctu hlasu
      
      if ("seats" %in% order_groups_by) {
        if ("seats_n" %in% names(netdata[[names(skupiny.k.serazeni)[1]]]$node_attr)) {
          poradi.mandatu <- netdata[[names(skupiny.k.serazeni)[1]]]$node_attr$seats_n[
            netdata[[names(skupiny.k.serazeni)[1]]]$node_attr$vertices %in% tab.poradi.skupin$group];
          tab.poradi.skupin$seats <- rank(poradi.mandatu, ties.method = "min");
        } else tab.poradi.skupin$seats <- NA;
      } else tab.poradi.skupin$seats <- NA; # konec IF-ELSE pro serazeni podle
      # poctu mandatu
      
      # --- #
      
      poradi.skupin <- order(tab.poradi.skupin[,2],
                             tab.poradi.skupin[,3],
                             tab.poradi.skupin[,4],
                             decreasing = TRUE);
    } else {
      
      poradi.skupin <- 1:pocet.skupin;
      
    } # konec IF-ELSE pro pripadne serazeni skupin podle zadani
    
    # ----------------------------------------------------------------------- #
    
    for (skupina in poradi.skupin) {
      
      listiny.skupiny <- skupiny[[skupina]];
      
      for (i in 1:pocet.voleb) {
        
        listiny.voleb <- listiny.skupiny[which(
          V(g)$elections[V(g)$name %in% listiny.skupiny] == included_elections[i])];
        
        if (length(listiny.voleb) != 0) {
          
          pocet.listin <- length(listiny.voleb);
          rozsah.y     <- axis_distances$y * (pocet.listin - 1) / 2;
          
          osa.y <- seq(rozsah.y, -rozsah.y, length.out = pocet.listin);
          
          #### Poradi listin ---------------------------------------------------
          
          if (order_lists_by == "seats" && "seats_n" %in% names(nodes)) {
            listiny.voleb <- listiny.voleb[order(V(g)$seats_n[V(g)$name %in% listiny.voleb],
                                                 decreasing = TRUE)];
          } else if (order_lists_by == "votes" && "votes_n" %in% names(nodes)) {
            listiny.voleb <- listiny.voleb[order(V(g)$votes_n[V(g)$name %in% listiny.voleb],
                                                 decreasing = TRUE)];
          } # konec IF-ELSE IF pro serazeni listin
          
          #### Vyplneni koordinatu ---------------------------------------------

          for (listina in listiny.voleb) koordinaty[listina,] <-
              c(osa.x[i], osa.y[which(listiny.voleb == listina)]);
          
        } # konec IF pro zjisteni, jestli v danych volbach vubec nejaka stran kandidovala
      } # konec vnejsiho FOR loopu pro pripravu osy y a vyplneni koordinatu pro skupinu voleb
      
      # --- #
      
      if (which(skupina == poradi.skupin) > 1) {
        
        koordinaty[listiny.skupiny,"y"] <-
          (koordinaty[listiny.skupiny,"y"] - max(koordinaty[listiny.skupiny,"y"])) + prvni.pozice.na.ose.y;
        
      } # konec IF pro zmenu koordinatu pro dalsi skupiny (od druhe skupiny)
      
      prvni.pozice.na.ose.y <- min(koordinaty[listiny.skupiny,"y"]) - (axis_distances$gr);
      
      if (skupina > 1 & skupina != pocet.skupin) mezery.osy.y <-
        c(mezery.osy.y, prvni.pozice.na.ose.y + axis_distances$y);
      
    } # konec FOR loopu prochazejici jednotlive (cele) skupiny
    
  } else {
    
    ### Neoddelene skupiny =====
    
    for (i in 1:pocet.voleb) {
      
      pozice.listin <- which(V(g)$elections == included_elections[i]);
      
      if (length(pozice.listin) != 0) {
        
        pocet.listin  <- length(pozice.listin);
        rozsah.y     <- axis_distances$y * (pocet.listin - 1) / 2
        
        osa.y <- seq(rozsah.y, -rozsah.y, length.out = pocet.listin)
        
        ### Poradi listin -----------------------------------------------------
        
        if (order_lists_by == "seats" && "seats_n" %in% names(nodes)) {
          pozice.listin <- pozice.listin[order(V(g)$seats_n[pozice.listin],
                                               decreasing = TRUE)];
        } else if (order_lists_by == "votes" && "votes_n" %in% names(nodes)) {
          pozice.listin <- pozice.listin[order(V(g)$votes_n[pozice.listin],
                                               decreasing = TRUE)];
        } # konec IF-ELSE IF pro serazeni listin
        
        
        ### Vyplneni koordinatu -----------------------------------------------
        
        for (j in 1:pocet.listin) koordinaty[pozice.listin[j],] <- c(osa.x[i], osa.y[j]);
        
      } # konec IF pro zjisteni, jestli v danych volbach vubec nejaka listina kandidovala
    } # konec vnejsiho FOR loopu pro pripravu osy y a vyplneni koordinatu
    
  }; # konec IF pro stanoveni koordinatu
  
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # ==================== Uprava nazvu kandidatnich listin =====================
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  upravene.nazvy.listin <- V(g)$abbr;
  
  upravene.nazvy.listin <- unlist(lapply(upravene.nazvy.listin,
                                         function (nazev) {
                                           ifelse(nchar(nazev) > 9,
                                                  paste0(substring(nazev,1,8), "."),
                                                  nazev)
                                         })) # konec lapply()
  
  if ("success" %in% names(nodes)) {
    upravene.nazvy.listin <- paste0(upravene.nazvy.listin, "\n(", V(g)$success, ")");
  } # konec IF pro pridani uspesnosti kandidatni listiny
  
  if (personalization & "coef_var" %in% names(nodes)) {
    upravene.nazvy.listin <- paste(gsub(")", "|", upravene.nazvy.listin),
                                   round(V(g)$coef_var * 100,0), "%)", sep = "")
  } # konec IF pro pridani personalizace
  
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # =========================== Zobrazeni diagramu ============================
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  if (show_legend && !is.null(mark) && !is.null(skupiny)) {
    
    if (mark[1] == "cores" & !is.null(cores)) {
      okno.legendy <- ifelse(any(
        nchar(netdata$cores$node_attr$group_label[unique(skupiny$membership)]) >
          (15 + 6)), 2, 1);
    } else {
      okno.legendy <- ifelse(any(
        nchar(netdata$parties$node_attr$group_label[unique(skupiny$membership)]) >
          (15 + 6)), 2, 1);
    } # konec IF-ELSE pro vyber velikosti okna pro legendu v mrizce
    
    graphics::par(mar = margins,
                  mfrow = c(1, 3 * pocet.voleb + okno.legendy));
    mrizka <- graphics::layout(matrix(c(rep(1, pocet.voleb + 2 * okno.legendy),
                                        rep(2, okno.legendy)),
                                      ncol  = pocet.voleb + 3 * okno.legendy,
                                      byrow = TRUE));
  } else {
    graphics::par(mar = margins,
                  mfrow = c(1, 1));
  } # konec IF-ELSE pro nastaveni zobrazeni pri vyberu legendy
  
  # ------------------------------------------------------------------------- #
  
  scale_node_size <- function(koordinaty,
                              x_range = c(2, 10),
                              y_range = c(1, 20),
                              width_range = c(20, 45),
                              height_range = c(15, 35)) {
    
    xrange <- diff(range(koordinaty[,1]))
    yrange <- diff(range(koordinaty[,2]))
    
    inv_rescale <- function(val, from, to) {
      val <- min(max(val, from[1]), from[2])
      prop <- (val - from[1]) / (from[2] - from[1])
      to[1] + prop * (to[2] - to[1])
    }
    
    list(
      width = inv_rescale(xrange, from = x_range, to = width_range),
      height = inv_rescale(yrange, from = y_range, to = height_range)
    )
  } # konec vnitrni funkce scale_node_size()
  
  sizes <- scale_node_size(koordinaty);
  
  # ------------------------------------------------------------------------- #
  
  if (!is.null(mark[1]) && !is.null(skupiny)) {
    
    if (!is.null(group_colours) &
        length(group_colours) != pocet.skupin.celkem) {
      warning(paste0("The number of colours in the input does not match the ",
                     "number of groups. A custom colour palette will be created.\n",
                     "NB: According to the logic of uniform display when entering ",
                     "repeatedly, the number of colours must match the number ",
                     "of all groups without filtering."))
      group_colours <- NULL;}
    
    if (coloured && is.null(group_colours)) {
      
      paleta.barev <- "Paired"; # puvodne: "Pastel1"

      if (pocet.skupin.celkem <= RColorBrewer::brewer.pal.info[paleta.barev, "maxcolors"]) {
        if (pocet.skupin.celkem < 3) {
          barvy <- RColorBrewer::brewer.pal(3, paleta.barev);
          barvy <- barvy[1:pocet.skupin.celkem];
        } else {
          barvy <- RColorBrewer::brewer.pal(pocet.skupin.celkem, paleta.barev);
        } # konec IF-ELSE pro vyber barev v ramci moznych barev
        
      } else {
        
        barvy <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[paleta.barev, "maxcolors"], paleta.barev);
        barvy <- grDevices::colorRampPalette(barvy)(pocet.skupin.celkem);
      } # konec IF-ELSE pro nastaveni barev
      
    } else if (!coloured && is.null(group_colours)) { # sedive
      
      barvy <- scales::grey_pal(0.5,0.9)(pocet.skupin.celkem);
      
    } else {
      
      barvy <- group_colours;
      
    } # konec IF-ELSE IF-ELSE pro nastaveni barevnosti skupin, pokud nebyly zadany
    
    # ----------------------------------------------------------------------- #
    
    barvy.clenu.skupiny <- stats::setNames(barvy[igraph::membership(skupiny)],
                                           names(membership(skupiny)));
    
    # ----------------------------------------------------------------------- #
    
    if (length(mark) > 1) {
      
      if (mark[1] == "candidates") {
        
        mark.groups <- NULL;

        bp.el <- netdata$bipartite$edgelist[,c("from", "to")];
        barvy.clenu.skupiny[!names(barvy.clenu.skupiny) %in%
                              bp.el$to[bp.el$from == mark[2]]] <- NA;
        plot_title <- mark[2];
        
      } else {
        groups.to.highlight <- mark[2:length(mark)][mark[2:length(mark)] %in%
                                                      names(communities(skupiny))];
        
        if (length(groups.to.highlight) >= 1) {
          mark.groups <- igraph::communities(skupiny)[groups.to.highlight]
        } else {
          mark.groups <- NULL;
        } # IF-ELSE kontrola vyberu skupin pro zobrazeni
      }; # konec IF-ELSE pro urceni skupin, ktere maji byt oznaceny
      
    } else {
      mark.groups <- NULL;
    } # IF-ELSE pro oznaceni skupin v grafu
    
    # ----------------------------------------------------------------------- #
    
    plot(x = skupiny,
         y = g,
         main = plot_title,
         vertex.shape = "rectangle",
         vertex.size = sizes$width,
         vertex.size2 = sizes$height,
         vertex.frame.color = "black",
         vertex.label = upravene.nazvy.listin,
         vertex.label.color = "black",
         vertex.label.cex = sizes$height * ifelse(show_legend, 0.045, 0.035) * ifelse(separate_groups, 1, 1.5),
         col = barvy.clenu.skupiny,
         edge.color = ifelse(igraph::crossing(skupiny, g), "black", "black"),
         edge.label = ifelse(igraph::E(g)$weight == 1, NA, paste("\n", igraph::E(g)$weight, sep = "")), # zobrazi se cislo jen, kdyz > 1
         edge.label.color = "black",
         edge.lty = ifelse(igraph::crossing(skupiny, g), 2, 1),
         edge.label.cex = sizes$height * ifelse(show_legend, 0.05, 0.04) * ifelse(separate_groups, 1, 1.5),
         edge.width = log(igraph::E(g)$weight)+1, # neni idealni, ale...
         edge.arrow.size = 0.2,
         mark.groups = mark.groups,
         mark.shape = 1.5,
         mark.border = "black",
         mark.col = if (!is.null(mark.groups)) {
           if (coloured) {
             grDevices::adjustcolor(barvy[sort(as.numeric(names(mark.groups)))],
                                    alpha.f = 0.5)
           } else {
             rev(grDevices::adjustcolor(barvy[sort(as.numeric(names(mark.groups)))],
                                        alpha.f = 0.5))
           } # konec IF-ELSE pokud by nebyly zadane mark.groups (zpusobylo by to varovani)
         } else NULL,
         xlim = range(koordinaty[,"x"]) + c(-0.2, 0.2),
         ylim = range(koordinaty[,"y"]) + c(-0.2, 0.8),
         asp = 0,
         layout = koordinaty,
         rescale = FALSE,
         axes = FALSE
    );
    
  } else {
    
    plot(g,
         main = plot_title,
         vertex.shape = "rectangle",
         vertex.size = sizes$width,
         vertex.size2 = sizes$height,
         vertex.frame.color = "black",
         vertex.label = upravene.nazvy.listin,
         vertex.label.color = "black",
         vertex.label.cex = 0.035 * sizes$height * ifelse(separate_groups, 0.8, 1.5),
         vertex.color = "white",
         edge.label = ifelse(igraph::E(g)$weight == 1, NA, paste0("\n", igraph::E(g)$weight)),
         edge.label.color = "black",
         edge.label.cex = 0.035 * sizes$height * ifelse(separate_groups, 0.8, 1.5),
         edge.width = log(igraph::E(g)$weight)+1, # neni idealni, ale...
         edge.arrow.size = 0.2,
         xlim = range(koordinaty[,"x"]) + c(-0.2, 0.2),
         ylim = range(koordinaty[,"y"]) + c(-0.2, 0.8),
         asp = 0,
         layout = koordinaty,
         rescale = FALSE,
         axes = FALSE
    );
  }; # konec IF-ELSE pro zobrazeni s nebo bez skupin
  
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # =========================== Doplneni diagramu =============================
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  pozice.carek <- c(min(osa.x) - axis_distances$x / 2,
                    unique(osa.x) + axis_distances$x / 2)
  
  graphics::segments(
    x0 = pozice.carek,
    x1 = pozice.carek,
    y0 = min(koordinaty[,"y"])-0.2,
    y1 = max(koordinaty[,"y"])+ifelse(separate_groups, 0.85, 0.45),
    lty = "dotted");
  
  nadpisy.voleb1 <- included_elections;
  
  statistiky.voleb <- netdata$elections$node_attr;
  statistiky.voleb <- statistiky.voleb[statistiky.voleb$vertices %in% included_elections, ];
  
  nadpisy.voleb2 <- "";
  
  if ("reg_voters" %in% names(statistiky.voleb)) {
    nadpisy.voleb2 <- paste0(nadpisy.voleb2, "voters: ", statistiky.voleb$reg_voters);
  }
  
  if ("plurality" %in% names(statistiky.voleb)) {
    nadpisy.voleb2 <- paste0(nadpisy.voleb2, "\n","ip: ",
                             format(round(statistiky.voleb$plurality, 1),
                                    nsmall = 1));
  }
  
  posun.na.ose.y <- ifelse(separate_groups,
                           ifelse(length(parties) < 3, 1.4, 1.5),
                           1);
  
  zaklad.y    <- max(koordinaty[,"y"]) + 0.3 * posun.na.ose.y;
  radek.nad.y <- 0.18 * posun.na.ose.y;
  
  graphics::text(
    x = osa.x,
    y = zaklad.y + radek.nad.y,
    labels = nadpisy.voleb1,
    cex = sizes$width * ifelse(!is.null(mark[1]) & show_legend, 0.09, 0.06));
  
  if (length(nadpisy.voleb2) > 0) {
    graphics::text(
      x = osa.x,
      y = zaklad.y,
      labels = nadpisy.voleb2,
      cex = sizes$width * ifelse(!is.null(mark[1]) & show_legend, 0.05, 0.04));
  } # konec IF pro pripad, ze by chybely statistiky pro nadpisy.voleb2
  
  
  # --- #
  
  if (!do_not_print_to_console && !is.null(skupiny)) {
    
    if (mark[1] == "cores" & !is.null(cores)) {
      cat("Cores: ", pocet.skupin, "\n", sep = "");
      cat(paste0(netdata$cores$node_attr$vertices[sort(unique(nodes$core))], ": ",
                 netdata$cores$node_attr$group_label[sort(unique(nodes$core))]),
          sep = "\n");
    } else {
      cat("Parties: ", pocet.skupin, "\n", sep = "");
      cat(paste0(netdata$parties$node_attr$vertices[sort(unique(nodes$party))], ": ",
                 netdata$parties$node_attr$group_label[sort(unique(nodes$party))]),
          sep = "\n");
      
    } # konec IF-ELSE pro vyber vyctu jader nebo stran
  } # konec IF pro vypis slozeni skupin
  
  # --- #
  
  if ("mayor" %in% names(nodes)) {
    starostove <- V(g)$mayor >= 1
    graphics::points(
      x = koordinaty[,"x"][starostove] + 0.004 * sizes$width,
      y = koordinaty[,"y"][starostove] + 0.003 * sizes$height,
      pch = 19,
      col = "black",
      cex = 1)
  } # konec IF pro vlozeni starostu
  
  if ("dep_mayors" %in% names(nodes)) {
    mistostarostove <- V(g)$dep_mayors >= 1
    graphics::points(
      x = koordinaty[,"x"][mistostarostove] + 0.004 * sizes$width,
      y = koordinaty[,"y"][mistostarostove] + 0.001 * sizes$height,
      pch = 1,
      col = "black",
      cex = 1)
  } # konec IF pro vlozeni mistostarostu
  
  if ("gov_support" %in% names(nodes)) {
    podpora_vlady <- V(g)$gov_support;
    graphics::points(
      x = koordinaty[,"x"][podpora_vlady] + 0.004 * sizes$width,
      y = koordinaty[,"y"][podpora_vlady] - 0.003 * sizes$height,
      pch = 15,
      col = "black",
      cex = 1)
  } # konec IF pro vlozeni podpory vlady
  
  # ------------------------------------------------------------------------- #
  
  ## legenda zobrazena v prazdnem plotu vpravo ====
  if (show_legend && !is.null(mark)) {
    
    graphics::par(mar = c(0,0,8,0));
    
    legend_labels <- if (mark[1] == "cores" & !is.null(cores)) {
      
      # odstraneni zavorky s procentem pomoci regularniho vyrazu:
      # " "           = mezera
      # "\\(" a "\\)" = zavorky (musi se escapovat)
      # "\\d+"        = jedna nebo vice cislic
      # "%"           = znak procenta
      
      gsub(" \\(\\d+%\\)", "",
           netdata$cores$node_attr$group_label[sort(unique(nodes$core))][poradi.skupin]);
      
    } else {
      
      gsub(" \\(\\d+%\\)", "",
           netdata$parties$node_attr$group_label[sort(unique(nodes$party))][poradi.skupin]);
    } # konec IF pro vyber textu v legende podle typu skupin
    
    max_label_width <- max(graphics::strwidth(legend_labels, units = "figure", cex = 1));
    legend_cex <- min(1, 0.9 / max_label_width);
  
    plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1);
    
    graphics::legend("topleft", legend = legend_labels, cex = legend_cex,
                     bty = 'n', x.intersp = 0.3, y.intersp = 0.6,
                     fill = barvy[as.numeric(names(igraph::communities(skupiny)))][poradi.skupin]);
    
    graphics::mtext(paste0(toupper(substr(mark[1], 1, 1)),
                           substr(mark[1], 2, nchar(mark[1])), ":"),
                    side = 3, at = 0.085, adj = 0, line = 0, cex = legend_cex + 0.1)
    
  } # konec IF pro zobrazeni legendy
} # konec fce plot_continuity()
