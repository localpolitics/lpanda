#' @title Prepare Network Data for LPANDA
#'
#' @description
#' Transforms time series data of local election results into a set of network
#' data for use in Local Political Actor Network Diachronic Analysis (LPANDA).
#' The function constructs a bipartite network (candidate – candidate list), its
#' projected one-mode networks (candidate – candidate and list – list), a continuity
#' graph (linking candidate lists between adjacent elections), and an elections
#' network (its node attributes can serve as electoral statistics). It also
#' detects parties (as clusters of candidate lists based on community detection
#' applied to the bipartite network) and constructs their network.
#'
#' @param df A \link[base]{data.frame} containing data from elections, with one
#'           row per candidate. The function also accepts a single election,
#'           though diachronic outputs will then be empty or trivial. See the
#'           *Expected structure of input data* section for the expected data
#'           format and required variables.
#'
#' @param input_variable_map A \link[base]{list} mapping variable names in `df`
#'                           that differ from the expected ones:\cr \cr
#'                           `elections`    = **unique** election identifiers
#'                                            (\link[base]{numeric}),\cr
#'                           `candidate`    = candidate's name used as a **unique**
#'                                            identifier (\link[base]{character}),\cr
#'                           `list_name`    = name of the candidate list
#'                                            (\link[base]{character}),\cr
#'                           `list_pos`     = candidate's position on the list
#'                                            (\link[base]{numeric}),\cr
#'                           `pref_votes`   = preferential votes received by the
#'                                            candidate (\link[base]{numeric}),\cr
#'                           `list_votes`   = * total votes received by the candidate
#'                                            list (\link[base]{numeric}),\cr
#'                           `elected`      = whether the candidate was elected
#'                                            (\link[base]{logical}),\cr
#'                           `nom_party`    = party that nominated the candidate
#'                                            (\link[base]{character}),\cr
#'                           `pol_affil`    = declared political affiliation of the candidate
#'                                            (\link[base]{character}),\cr
#'                           `mayor`        = whether the councillor became mayor
#'                                            (\link[base]{logical}),\cr
#'                           `dep_mayor`    = whether the councillor became deputy mayor
#'                                            (\link[base]{logical}),\cr
#'                           `board`        = whether the councillor became a member
#'                                            of the executive board (\link[base]{logical}),\cr
#'                           `gov_support`  = whether the councillor supported
#'                                            the executive body (\link[base]{logical}),\cr
#'                           `elig_voters`  = * number of eligible voters
#'                                            (\link[base]{numeric}),\cr
#'                           `ballots_cast` = * number of ballots cast
#'                                            (\link[base]{numeric}),\cr
#'                           `const_size`   = * size of the constituency (number of seats)
#'                                            (\link[base]{numeric})\cr \cr
#'                           * Variables marked with an asterisk should appear
#'                           only once per election and constituency — in the
#'                           row of any **one** candidate running in that specific
#'                           elections and constituency.\cr \cr
#'                           See the *Expected input data structure* section
#'                           to find out how to use it.
#'
#' @param verbose Logical, default TRUE. If FALSE, suppresses informative messages.
#'
#' @param ...  Optional arguments reserved for internal development, experimental
#'             features and future extensions, such as `include_cores` (logical,
#'             default FALSE). Not intended for standard use yet (behavior may
#'             change without notice). Unknown keys in ... are ignored.
#'
#' @returns A \link[base]{list} of network data objects for diachronic analysis
#' using LPANDA or other social network analysis tools. Each component contains
#' \code{edgelist} (data.frame of edges) and \code{node_attr} (data.frame of node
#' attributes). The exact set of columns depends on the input and may evolve. See
#' *Output data structure* for a description of the returned object.
#'
#'
#' @section Expected structure of input data:
#' The input data frame (`df`) **must** include at least the election identifiers
#' (year\[.month\]), candidates' names (uniquely identifying individuals), and
#' list names. Other variables are optional. If variable names in the dataset
#' differ from the expected ones, they should be specified in the `input_variable_map`
#' as a named \link[base]{list} (only differing names need to be listed).
#'
#' *Just in case - a named list is a list where each element has a name (the
#' expected variable name) and a value (the actual name used in your data frame),
#' for example: `list(list_name = "party", elected = "seat", list_votes = "votes_total")`.*
#'
#' Examples of expected and acceptable values in `df`:
#' - `elections` (required): Election identifier in the format YY\[YY\]\[.MM\]:
#'                           e.g., 94 | 02 | 1998 | "2024" | 2022.11
#' - `candidate` (required): e.g., "John Doe" | "John Smith (5)" | "Jane Doe, jr."
#' - `list_name` (required): *for independent candidates, you can use:*
#'                           e.g., "John Smith, Independent Candidate" | "J.S., IND."
#' - `list_pos`, `pref_votes`, `list_votes`: must be \link[base]{numeric}
#' - `elected`, `mayor`, `dep_mayor`, `board`, `gov_support`: 1 | "0" | T | "F" | "TRUE" | FALSE
#'   (non‑logical inputs will be coerced to logical).
#' - `nom_party`: *for independent candidates, you can use:* "IND" | "Independent Candidate"
#' - `pol_affil`: *for independent candidates, you can use:* "non-partisan"
#' - `elig_voters`, `ballots_cast`, `const_size`: A \link[base]{numeric} that
#'    should appear only once in any candidate row within a given election and constituency
#'
#' If `pref_votes` are present but `list_votes` are not, the function assumes
#' a voting system where list votes are calculated by summing the preferential
#' votes of candidates on the list.
#'
#' If `const_size` is missing, it will be estimated based on the number of
#' elected candidates (if available).
#'
#' For the purposes of analysis, a new variable `list_id` (class \link[base]{character})
#' is added to the internally processed copy of `df` and carried to the output.
#' It uniquely identifies each candidate list in a given election (combining
#' `list_name` and `elections`), e.g., *Besti Flokkurinn (2010)*, *SNP (2019)*,
#' or *"John Smith (5), IND. (2022.11)"*. This variable serves as a **key identifier**
#' in LPANDA for tracking candidate lists across elections and constructing
#' network relations.
#'
#'
#' @section Output data structure:
#' The returned object is a named \link[base]{list} with up to seven network
#' objects:
#'
#' - `bipartite`: bipartite network (candidates-lists).
#' - `candidates`: projected candidate–candidate network.
#' - `lists`: projected list–list network (directed by election order).
#' - `continuity`: filtered version of `lists` network (edges of adjacent elections only).
#' - `parties`: network of detected party clusters (via community detection applied
#'              on `bipartite` network).
#' - (`cores`): higher-level clusters of `parties`. Cores are currently experimental
#'              and will not appear in the standard output network data. See **Note**
#'              below.
#' - `elections`: inter-election candidate flow and election-level stats
#'
#' Each object is a list with two components:
#' - `edgelist`: a \link[base]{data.frame} representing network edges
#' - `node_attr`: a \link[base]{data.frame} with attributes for each node
#'
#' For example, `...$candidates$edgelist` contains edges between individuals
#' who appeared on the same candidate list, and `...$elections$node_attr`
#' includes several election statistics (e.g., number of candidates, distributed
#' seats, plurality index, voter turnout for each election, etc.).
#'
#' @note
#' Cores are currently experimental, as the conversion of their definition into
#' code is still being sought, and may be subject to change in future versions.
#' It is not yet intended for standard use in analyses and academic studies, since
#' their calculation is not yet comprehensive, so the cores' network structure
#' will not appear in the standard output network data unless explicitly called
#' with the `include_cores = TRUE` parameter. Use with caution, their interpretation
#' is highly questionable.
#'
#' @export
#'
#' @importFrom igraph V graph_from_data_frame as_data_frame bipartite_projection
#' @importFrom igraph cluster_optimal cluster_edge_betweenness cluster_louvain
#' @importFrom igraph cluster_walktrap cluster_leading_eigen cluster_fast_greedy
#' @importFrom igraph cluster_label_prop cluster_infomap cluster_spinglass
#' @importFrom igraph modularity is_directed as_undirected edge_attr_names
#' @importFrom igraph components vcount induced_subgraph make_clusters
#' @importFrom stats aggregate sd setNames
#' @importFrom dplyr group_by summarise left_join n_distinct rename_with select
#' @importFrom dplyr filter mutate rename
#' @importFrom utils tail txtProgressBar setTxtProgressBar
#'
#' @examples
#' data(sample_diff_varnames, package = "lpanda")
#' df <- sample_diff_varnames
#' str(df) # different variable names: "party" and "seat"
#' input_variable_map <- list(list_name = "party", elected = "seat")
#' \donttest{
#' netdata <- prepare_network_data(df, input_variable_map, verbose = FALSE)
#' str(netdata, vec.len = 1)
#' }
#'
prepare_network_data <- function(df,
                                 input_variable_map = list(),
                                 verbose = TRUE,
                                 ...) {
  
  if (verbose) cat("==========================================================\n",
                   "Preparing Network Data for LPANDA\n",
                   "==========================================================\n",
                   sep = "");
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # ========== Identifikace experimentalnich a internich promennych ===========
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  args <- list(...);
  
  allowed_args <- c("skip_groups",
                    "include_cores",
                    "core_type",
                    "use_optimal",
                    "repeat_community_detection",
                    "quick");
  
  extra_args <- setdiff(names(args), allowed_args);
  if (length(extra_args) > 0) {
    warning("Unknown arguments in '...': ", paste(extra_args, collapse = ", "), ".")
  };
  
  skip_groups <- isTRUE(args$skip_groups);
  
  include_cores <- isTRUE(args$include_cores & !skip_groups);
  if ("include_cores" %in% names(args)) {
    message("Note: core computation is experimental and may change \nin future ",
            "versions.")
  };
  
  core_type <- if (!is.null(args$core_type)) args$core_type else 1;
  if (!core_type %in% 1:4) {
    warning("Unknown 'core_type', using default = 1.");
    core_type <- 1
  };
  
  pouzit.optimal <- isTRUE(args$use_optimal);
  if (pouzit.optimal) {
    warning("'igraph::cluster_optimal()' is computationally expensive\nand should ",
            "only be used for small networks.")
  };
  
  kolik.vypoctu <- if (is.numeric(args$repeat_community_detection) &&
                       args$repeat_community_detection >= 1
  ) as.integer(args$repeat_community_detection) else 10;
  
  if (isTRUE(args$quick)) kolik.vypoctu <- 1;
  
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # ======== Uprava/kontrola dat a identifikace dostupnych promennych =========
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  if (verbose) cat("\n",
                   "1/ Checking the usability of input data and modifying it for\n",
                   "   conversion to network data.\n", sep = "")
  
  df <- if (verbose) {
    prepare_input_data(df, input_variable_map);
  } else {
    suppressMessages(prepare_input_data(df, input_variable_map));
  };
  
  # ------------------------------------------------------------------------- #
  
  required_cols    <- variables_meta$var_name[variables_meta$required == TRUE
                                              & variables_meta$role == "input"];
  all_allowed_cols <- variables_meta$var_name;
  
  ## 1. Kontrola pritomnosti povinnych promennych (sloupcu v df) ====
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(paste0("Missing required column(s): ", paste(missing_cols, collapse = ", "),
                ". Please check the 'input_variable_map' argument for possible errors."))
  } # konec IF pro kontrolu chybejicich promennych (sloupcu)
  
  ## 2. Kontrola, ze povinne sloupce maji data (zadna NA nebo prazdna pole) ====
  empty_required <- required_cols[sapply(df[required_cols], function(col) any(is.na(col) | col == ""))]
  if (length(empty_required) > 0) {
    stop(paste0("Required column(s) contain missing or empty values: ",
                paste(empty_required, collapse = ", ")));
  } # konec IF pro kontrolu chybejicich dat v povinnych promennych
  
  # ------------------------------------------------------------------------- #
  
  ## 3. Identifikace dostupnych promennych ====
  available_cols <- intersect(all_allowed_cols, names(df))
  
  unknown_cols <- setdiff(names(df), all_allowed_cols)
  if (length(unknown_cols) > 0) {
    warning(paste0("Unknown column(s) detected and will be removed: ",
                   paste(unknown_cols, collapse = ", "),
                   ". If these are relevant, check 'input_variable_map' argument ",
                   "for possible errors."))
    df <- df[, setdiff(names(df), unknown_cols)]
  } # konec IF
  
  if (verbose) cat("   Done.\n")
  
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # ======= Edgelisty, edge-atributy, iniciace node-atributu a skupiny ========
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  convert_lists_edgelist_to_digraph <- function(el) {
    
    if (nrow(el) == 0) return(el);

    for (i in 1:nrow(el)) {
      from.to                <- el[i, 1:2];
      volby.from.to          <- c(df$elections[match(from.to[1], df$list_id)],
                                  df$elections[match(from.to[2], df$list_id)])
      poradi                 <- order(as.numeric(volby.from.to)); # numeric used just in case
      el[i, 1:2] <- from.to[poradi];
    }; # konec FOR loopu pro serazeni poradi uzlu dle casu
    
    el <- el[!duplicated(el),];
    el <- el[order(el$from, el$to),];
    
    return(el)
    
  } # konec fce convert_to_diagraph()
  
  
  # ------------------------------------------------------------------------- #

  df <- df[order(df$elections, df$list_id, df$candidate),];
  
  
  ## ========================= 1. Bipartite network ===========================
  
  if (verbose) cat("2/ Creating an edgelist of a bimodal network. ");
  
  bp.E.attr <- available_cols[available_cols %in%
                                variables_meta$var_name[variables_meta$bp.E.attr]]
  
  bp.edgelist <- data.frame(df[,c("candidate", "list_id", bp.E.attr)]);
  
  names(bp.edgelist)[names(bp.edgelist) == "candidate"] <- "from";
  names(bp.edgelist)[names(bp.edgelist) == "list_id"]   <- "to";
  
  bp.node.attr <- data.frame(vertices = c(sort(unique(df$candidate)),
                                          sort(unique(df$list_id))));
  
  bp.node.attr$type <- bp.node.attr$vertices %in% df$list_id;
  
  # ------------------------------------------------------------------------- #
  
  bp.node.attr$is_list      <- bp.node.attr$type;
  bp.node.attr$is_candidate <- !bp.node.attr$type;
  
  if (verbose) cat("Done.\n");
  
  
  ## ======================= 2. Bipartite projection ==========================

  if (verbose) cat("3/ Projecting a bimodal network ");
  
  bp.net <- igraph::graph_from_data_frame(bp.edgelist, directed = FALSE,
                                          vertices = bp.node.attr);
  
  projekce <- igraph::bipartite_projection(bp.net);
  
  if (verbose) cat("and building edgelists for\n   the following networks: ");
  
  ## ========================= 3. cands.edgelist ==============================
  
  if (verbose) cat("candidates, ");
  
  cands.edgelist <- igraph::as_data_frame(projekce$proj1);
  cands.node.attr <- data.frame(vertices = sort(igraph::V(projekce$proj1)$name));
  cands.node.attr$is_isolate <- !(cands.node.attr$vertices %in% unique(c(cands.edgelist$from,
                                                                         cands.edgelist$to)));
  
  ## ========================= 4. lists.edgelist ==============================
  
  if (verbose) cat("candidate lists,\n", sep = "");
  
  lists.edgelist <- igraph::as_data_frame(projekce$proj2);
  lists.node.attr <- data.frame(vertices = sort(igraph::V(projekce$proj2)$name));
  lists.node.attr$is_isolate <- !(lists.node.attr$vertices %in% unique(c(lists.edgelist$from,
                                                                         lists.edgelist$to)));
  lists.edgelist <- convert_lists_edgelist_to_digraph(lists.edgelist);
  
  ## ======================= 5. continuity.edgelist ===========================
  
  if (verbose) cat("   continuity ");
  
  roky.voleb <- sort(unique(df$elections));
  
  from <- df$elections[match(lists.edgelist$from, df$list_id)];
  to   <- df$elections[match(lists.edgelist$to, df$list_id)];
  
  from <- match(from, roky.voleb);
  to   <- match(to, roky.voleb);
  
  jsou.za.sebou <- (to - from) == 1;
  continuity.edgelist <- lists.edgelist[jsou.za.sebou,];
  
  ## ======================== 6. elections.edgelist ===========================

  if (verbose) cat("and elections. ");
  
  if (nrow(lists.edgelist) == 0) {
    
    elections.edgelist <- lists.edgelist;
    
  } else {
    
    elections.edgelist <- lists.edgelist[rep(1:nrow(lists.edgelist),
                                             lists.edgelist$weight),
                                         c("from", "to")];
    
    elections.edgelist$from <- df$elections[match(elections.edgelist$from, df$list_id)];
    elections.edgelist$to   <- df$elections[match(elections.edgelist$to, df$list_id)];
    
    elections.edgelist <- stats::aggregate(x = list(weight = rep(1, nrow(elections.edgelist))),
                                           by = list(from = elections.edgelist$from,
                                                     to = elections.edgelist$to),
                                           FUN = length);
    
  } # konec IF-ELSE pro pripad, kdy by kandidoval kazde volby nekdo jiny
  
  elections.node.attr <- data.frame(vertices = sort(unique(df$elections)));
  elections.node.attr$is_isolate <- !(elections.node.attr$vertices %in% unique(c(elections.edgelist$from,
                                                                                 elections.edgelist$to)));
  
  if (verbose) cat("Done.\n");
  
  
  ## ======================= 7. Community detection ===========================
  
  if (!skip_groups) {
    
    detect_groups <- function(net,
                              pouzit.optimal,
                              kolik.vypoctu) {

      detect_by_components <- function(g, clustering_function, ...) {
        
        komponenty <- igraph::components(g);
        clenstvi_celkem <- stats::setNames(rep(NA, igraph::vcount(g)), V(g)$name);
        
        max_cislo_skupiny <- 0;
        
        for (x in 1:komponenty$no) {
          uzly_komponenty <- which(komponenty$membership == x);
          subg <- igraph::induced_subgraph(g, uzly_komponenty);
          cl   <- clustering_function(subg, ...);
          clenstvi_celkem[uzly_komponenty] <- cl$membership + max_cislo_skupiny;
          max_cislo_skupiny <- max(clenstvi_celkem, na.rm = TRUE);
        } # konec FOR loopu
        
        vysledek <- igraph::make_clusters(graph = g,
                                          membership = clenstvi_celkem,
                                          algorithm = cl$algorithm);
        vysledek$names <- names(clenstvi_celkem);
        
        return(vysledek)
      } # konec fce detect_by_components()
      
      # ----------------------------------------------------------------------- #
      
      net.props <- list(directed   = igraph::is_directed(net),
                        weighted   = "weight" %in% igraph::edge_attr_names(net),
                        components = igraph::components(net)$no);
      
      if (net.props$directed) {
        net.undir <- igraph::as_undirected(net, mode = "collapse");
      } else {
        net.undir <- net;
      } # konec IF-ELSE pro prevedeni orientovane site na neorientovanou
      
      # ----------------------------------------------------------------------- #
      
      skupiny <- list();
      
      if (pouzit.optimal &
          length(igraph::V(net)) <= 85) {
        
        if (verbose) cat("Trying 'optimal': ... ", sep = "");
        skupiny$optimal <- igraph::cluster_optimal(net);
        if (verbose) cat("Done.\n");
        
      } else {
        
        if (verbose) {
          pb <- utils::txtProgressBar(min = 0, max = kolik.vypoctu * 8,
                                      style = 3, width = 50, char = "\u25A0");
          krok <- 0;
        }
        
        for (i in 1:kolik.vypoctu) {
          
          # GIRVAN-NEWMAN algoritmus (edge_betweenness)
          
          # Algorithm                                   | dir.| weighted | comp.
          # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          # Edge-betweenness: cluster_edge_betweenness  |  T  |    T     |  T
          
          # NB: weights = NA, protoze: "...modularity treats edge weights as
          # similarities while edge betwenness treats them as distances"
          nazev <- paste("eb_", i, sep = "");
          tryCatch(skupiny[[nazev]] <- igraph::cluster_edge_betweenness(net,
                                                                        weights = NA),
                   error = function(e) NULL);
          if (verbose) {krok <- krok + 1; utils::setTxtProgressBar(pb, krok);};
          
          # LOUVAIN algoritmus (podle univerzity v Lovani, Belgie)
          # vhodne pro vazena i binarni data, ale neni vhodny pro orientovane vazby
          
          # Algorithm                                   | dir.| weighted | comp.
          # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          # Louvain: cluster_louvain                    |  F  |    T     |  T
          
          nazev <- paste("lv_", i, sep = "");
          tryCatch(skupiny[[nazev]] <- igraph::cluster_louvain(net.undir),
                   error = function(e) NULL);
          if (verbose) {krok <- krok + 1; utils::setTxtProgressBar(pb, krok);};
          
          # WALKTRAP algoritmus ("past na prochazky")
          # vhodny pro vazena i binarni data, ale ne pro orientovane vazby
          
          # Algorithm                                   | dir.| weighted | comp.
          # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          # Walktrap: cluster_walktrap                  |  F  |    T     |  F
          
          nazev <- paste("wt_", i, sep = "");
          
          # cluster_walktrap funguje i s nespojenymi sitemi, i kdyz by nemel, podle
          # Luke (2015), proto nepouzivam detect_by_components()
          tryCatch(skupiny[[nazev]] <- igraph::cluster_walktrap(net.undir),
                   error = function(e) NULL);
          if (verbose) {krok <- krok + 1; utils::setTxtProgressBar(pb, krok);};
          
          # Leading eigenvector
          
          # Algorithm                                   | dir.| weighted | comp.
          # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          # Leading eigenvector: cluster_leading_eigen  |  F  |    F     |  T
          
          nazev <- paste("ev_", i, sep = "");
          tryCatch(skupiny[[nazev]] <- igraph::cluster_leading_eigen(net.undir,
                                                                     weights = NA),
                   error = function(e) NULL);
          if (verbose) {krok <- krok + 1; utils::setTxtProgressBar(pb, krok);};
          
          # Fast-greedy
          
          # Algorithm                                   | dir.| weighted | comp.
          # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          # Fast-greedy: cluster_fast_greedy            |  F  |    T     |  T
          
          nazev <- paste("fg_", i, sep = "");
          tryCatch(skupiny[[nazev]] <- igraph::cluster_fast_greedy(net.undir),
                   error = function(e) NULL);
          if (verbose) {krok <- krok + 1; utils::setTxtProgressBar(pb, krok);};
          
          # Label propagation
          
          # Algorithm                                   | dir.| weighted | comp.
          # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          # Label propagation: cluster_label_prop       |  F  |    T     |  F
          
          nazev <- paste("lp_", i, sep = "");
          
          # podobne jako cluster_walktrap, i cluster_label_prop funguje s nespo-
          # jenymi sitemi, i kdyz by nemel, podle Luke (2015), proto nepouzivam
          # detect_by_components()
          tryCatch(skupiny[[nazev]] <- igraph::cluster_label_prop(net.undir),
                   error = function(e) NULL);
          if (verbose) {krok <- krok + 1; utils::setTxtProgressBar(pb, krok);};
          
          # InfoMAP
          
          # Algorithm                                   | dir.| weighted | comp.
          # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          # InfoMAP: cluster_infomap                    |  T  |    T     |  T
          
          nazev <- paste("im_", i, sep = "");
          tryCatch(skupiny[[nazev]] <- igraph::cluster_infomap(net),
                   error = function(e) NULL);
          if (verbose) {krok <- krok + 1; utils::setTxtProgressBar(pb, krok);};
          
          # Spinglass - nefunguje s componenty
          
          # Algorithm                                   | dir.| weighted | comp.
          # - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          # Spinglass: cluster_spinglass                |  F  |    T     |  F
          
          nazev <- paste("sg_", i, sep = "");
          tryCatch(skupiny[[nazev]] <- detect_by_components(net.undir,
                                                            igraph::cluster_spinglass),
                   error = function(e) NULL);
          if (verbose) {krok <- krok + 1; utils::setTxtProgressBar(pb, krok);};
          
        } # konec FOR loopu pro identifikaci skupin bez OPTIMAL
        
        if (verbose) close(pb);
        
      } # konec IF-ELSE pro identifikaci jader
      
      if (verbose) cat("   Selected algorithm: ");
      
      modularita <- unlist(lapply(skupiny, function(x) igraph::modularity(net, x$membership)))
      modularita <- modularita[order(modularita, decreasing = TRUE)];
      
      skupiny <- skupiny[[names(modularita[1])]];
      
      if (verbose) cat(names(modularita[1]), " (modularity ",
                       round(modularita[1] * 100, 2), " %).\n", sep = "");
      
      vysledna.tabulka <- data.frame(vertices = skupiny$names,
                                     group    = skupiny$membership);
      return(vysledna.tabulka);
    } # konec fce detect_groups()
    
    
    # ------------------------------------------------------------------------- #
    
    convert_vertices_to_group <- function(edgelist,
                                          node_group,
                                          group_type = "party",
                                          aggregate = TRUE,
                                          drop_loops = TRUE) {
      
      if (nrow(edgelist) == 0) return(edgelist)
      
      name_to_group <- stats::setNames(node_group[[group_type]], node_group$vertices);
      edgelist$from <- name_to_group[edgelist$from];
      edgelist$to   <- name_to_group[edgelist$to];
      
      if (aggregate) edgelist <- stats::aggregate(weight ~ from + to, data = edgelist, FUN = sum);
      
      if (drop_loops) edgelist <- edgelist[edgelist$from != edgelist$to, ];
      
      return(edgelist)
    } # konec fce convert_vertices_to_group()
    
    
    # ------------------------------------------------------------------------- #
    
    skupiny.tab <- data.frame(vertices = bp.node.attr$vertices);
    
    # ------------------------------------------------------------------------- #
    
    
    ### ========================== 7a. party.edgelist =============================
    
    if (verbose) cat("4/ Detecting 'political parties' (may take a while):\n");
    
    vysledna.tabulka <- detect_groups(net = bp.net,
                                      pouzit.optimal, # zatim defaultne vypnuto
                                      kolik.vypoctu);
    names(vysledna.tabulka)[names(vysledna.tabulka) == "group"] <- "party";
    
    skupiny.tab <- dplyr::left_join(skupiny.tab, vysledna.tabulka,
                                    by = "vertices");
    
    df <- dplyr::left_join(df, skupiny.tab[skupiny.tab$vertices %in% df$candidate,
                                           c("vertices", "party")],
                           by = c("candidate" = "vertices"));
    
    # ------------------------------------------------------------------------- #
    
    if (verbose) cat("   Creating an edgelist of political party network. ");
    
    party.edgelist <- convert_vertices_to_group(edgelist = lists.edgelist,
                                                node_group = skupiny.tab,
                                                group_type = "party",
                                                aggregate = TRUE,
                                                drop_loops = TRUE);
    
    party.node.attr <- data.frame(vertices = sort(unique(skupiny.tab$party)));
    party.node.attr$is_isolate <- !(party.node.attr$vertices %in% unique(c(party.edgelist$from,
                                                                           party.edgelist$to)));
    
    party.node.attr$lists <- NA;
    listiny.vsechny <- skupiny.tab[skupiny.tab$vertices %in% lists.node.attr$vertices,]
    for (party in party.node.attr$vertices) {
      listiny.strany <- listiny.vsechny$vertices[listiny.vsechny$party == party];
      party.node.attr$lists[party.node.attr$vertices == party] <-
        paste(listiny.strany, collapse = " | ")
    } # konec FOR loopu pro vyplneni listin stran
    
    # ------------------------------------------------------------------------- #
    
    if (verbose) cat("Done.\n");
    
    
    ## ========================== 7b. core.edgelist =============================
    
    if (include_cores) {
      
      if (verbose) cat("5/ Detecting 'cores':\n");

      multi.bp.edgelist <- cbind(bp.edgelist[,c("from", "to")],
                                 data.frame(layer = "candidate",
                                            elections = bp.edgelist$elections));
      
      if (all(c("elected", "gov_support") %in% bp.E.attr)) {
        
        multi.bp.edgelist$layer[bp.edgelist$elected]  <- "councillor";

        for (volby in sort(unique(bp.edgelist$elections))) {
          
          tabulka <- bp.edgelist[bp.edgelist$elections == volby &
                                   bp.edgelist$elected == TRUE &
                                   bp.edgelist$gov_support == TRUE &
                                   !is.na(bp.edgelist$gov_support), ];
          
          koalice.str  <- unique(tabulka[, "to"]);
          koalice.kand <- tabulka[,"from"];
          
          if (length(koalice.str) > 1) {
            for (kandidat in koalice.kand) {
              strana.kandidata <- tabulka[tabulka$from == kandidat, "to"];
              el.kandidata <- data.frame(from = kandidat,
                                         to = koalice.str[koalice.str != strana.kandidata],
                                         layer = "coalition",
                                         elections = volby);
              multi.bp.edgelist <- rbind(multi.bp.edgelist, el.kandidata);
            } # konec FOR loopu prochazejici jednotlive kandidaty koalice
          } # konec IF pro kontrolu existence dat pro koalice, a ze je ve vlade
          # vice nez jedna strana
        } # konec FOR loopu prochazejici jednotlive volby
        
        multi.bp.edgelist <- multi.bp.edgelist[order(multi.bp.edgelist$elections,
                                                     multi.bp.edgelist$from),];
      } # konec IF pro doplneni edgelistu bimodalni site o koalicni vztahy
      
      
      #### ---------------------------------------------------------------------- #
      #### core_type 1                                                         ====
      #### ---------------------------------------------------------------------- #
      
      if (core_type == 1) {
        
        multi.bp.net <- igraph::graph_from_data_frame(multi.bp.edgelist,
                                                      directed = FALSE,
                                                      vertices = bp.node.attr);
        
        vysledna.tabulka <- detect_groups(net = multi.bp.net,
                                          pouzit.optimal,
                                          kolik.vypoctu);
        names(vysledna.tabulka)[names(vysledna.tabulka) == "group"] <- "core";
        
        # ----------------------------------------------------------------------- #
        
        skupiny.tab <- dplyr::left_join(skupiny.tab, vysledna.tabulka,
                                        by = "vertices");
        
        df <- dplyr::left_join(df, skupiny.tab[skupiny.tab$vertices %in% df$candidate,
                                               c("vertices", "core")],
                               by = c("candidate" = "vertices"));
        
        # ----------------------------------------------------------------------- #
        
        if (verbose) cat("   Creating an edgelist of core network. ");
        
        core.lists.net <- igraph::bipartite_projection(multi.bp.net, which = "true");
        core.lists.edgelist <- as_data_frame(core.lists.net);
        core.lists.edgelist <- convert_lists_edgelist_to_digraph(core.lists.edgelist);
        
        core.edgelist <- convert_vertices_to_group(edgelist = core.lists.edgelist,
                                                   node_group = skupiny.tab,
                                                   group_type = "core",
                                                   aggregate = TRUE,
                                                   drop_loops = TRUE);
      }; # konec IF core_type == 1
      
      
      #### ---------------------------------------------------------------------- #
      ## core_type 2                                                         ====
      #### ---------------------------------------------------------------------- #
      
      if (core_type == 2) {
        
        multi.bp.net <- igraph::graph_from_data_frame(multi.bp.edgelist,
                                                      directed = FALSE,
                                                      vertices = bp.node.attr);
        
        core.lists.net <- igraph::bipartite_projection(multi.bp.net, which = "true");
        core.lists.edgelist <- as_data_frame(core.lists.net);
        core.lists.edgelist <- convert_lists_edgelist_to_digraph(core.lists.edgelist);
        core.party.edgelist <- convert_vertices_to_group(edgelist = core.lists.edgelist,
                                                         node_group = skupiny.tab,
                                                         group_type = "party",
                                                         aggregate = TRUE,
                                                         drop_loops = TRUE);

        core.party.net <- igraph::graph_from_data_frame(core.party.edgelist,
                                                        directed = TRUE,
                                                        vertices = party.node.attr);
        
        vysledna.tabulka <- detect_groups(net = core.party.net,
                                          pouzit.optimal,
                                          kolik.vypoctu);
        names(vysledna.tabulka)[names(vysledna.tabulka) == "group"] <- "core";
        
        # ----------------------------------------------------------------------- #
        
        vysledna.tabulka$vertices <- as.numeric(vysledna.tabulka$vertices);
        
        skupiny.tab <- dplyr::left_join(skupiny.tab, vysledna.tabulka,
                                        by = c("party" = "vertices"));
        
        df <- dplyr::left_join(df, skupiny.tab[skupiny.tab$vertices %in% df$candidate,
                                               c("vertices", "core")],
                               by = c("candidate" = "vertices"));
        
        # ----------------------------------------------------------------------- #
        
        if (verbose) cat("   Creating an edgelist of core network. ");
        
        core.edgelist <- convert_vertices_to_group(edgelist = core.party.edgelist,
                                                   node_group = data.frame(vertices = skupiny.tab$party,
                                                                           core = skupiny.tab$core),
                                                   group_type = "core",
                                                   aggregate = TRUE,
                                                   drop_loops = TRUE);
      }; # konec IF core_type == 2
      
      
      #### ---------------------------------------------------------------------- #
      ## core_type 3                                                         ====
      #### ---------------------------------------------------------------------- #
      
      if (core_type == 3) {
        
        coalition.edgelist <- multi.bp.edgelist[multi.bp.edgelist$layer == "coalition",];
        coalition.nodes    <- bp.node.attr[bp.node.attr$vertices %in% c(coalition.edgelist$from,
                                                                        coalition.edgelist$to),]
        
        multi.bp.net <- igraph::graph_from_data_frame(coalition.edgelist,
                                                      directed = FALSE,
                                                      vertices = coalition.nodes);
        
        core.lists.net <- igraph::bipartite_projection(multi.bp.net, which = "true");
        core.lists.edgelist <- as_data_frame(core.lists.net);
        core.lists.edgelist <- convert_lists_edgelist_to_digraph(core.lists.edgelist);

        core.party.edgelist <- convert_vertices_to_group(edgelist = core.lists.edgelist,
                                                         node_group = skupiny.tab,
                                                         group_type = "party",
                                                         aggregate = TRUE,
                                                         drop_loops = TRUE);
        
        core.party.edgelist <- rbind(core.party.edgelist, party.edgelist)
        core.party.edgelist <- stats::aggregate(weight ~ from + to,
                                                data = core.party.edgelist,
                                                FUN = sum);
        core.party.edgelist <- core.party.edgelist[core.party.edgelist$from !=
                                                     core.party.edgelist$to, ];
        core.party.net <- igraph::graph_from_data_frame(core.party.edgelist,
                                                        directed = TRUE,
                                                        vertices = party.node.attr);
        
        vysledna.tabulka <- detect_groups(net = core.party.net,
                                          pouzit.optimal,
                                          kolik.vypoctu);
        names(vysledna.tabulka)[names(vysledna.tabulka) == "group"] <- "core";
        
        # ----------------------------------------------------------------------- #
        
        vysledna.tabulka$vertices <- as.numeric(vysledna.tabulka$vertices);
        skupiny.tab <- dplyr::left_join(skupiny.tab, vysledna.tabulka,
                                        by = c("party" = "vertices"));
        
        df <- dplyr::left_join(df, skupiny.tab[skupiny.tab$vertices %in% df$candidate,
                                               c("vertices", "core")],
                               by = c("candidate" = "vertices"));
        
        # ----------------------------------------------------------------------- #
        
        if (verbose) cat("   Creating an edgelist of core network. ");
        
        core.edgelist <- convert_vertices_to_group(edgelist = core.party.edgelist,
                                                   node_group = data.frame(vertices = skupiny.tab$party,
                                                                           core = skupiny.tab$core),
                                                   group_type = "core",
                                                   aggregate = TRUE,
                                                   drop_loops = TRUE);
      }; # konec IF core_type == 3
      
      
      #### ---------------------------------------------------------------------- #
      ## core_type 4                                                         ====
      #### ---------------------------------------------------------------------- #
      
      if (core_type == 4) {
        
        multi.bp.net <- igraph::graph_from_data_frame(multi.bp.edgelist,
                                                      directed = FALSE,
                                                      vertices = bp.node.attr);
        
        core.cands.net <- igraph::bipartite_projection(multi.bp.net, which = "false");
        core.cands.edgelist <- as_data_frame(core.cands.net);
        core.cands.edgelist <- stats::aggregate(weight ~ from + to,
                                                data = core.cands.edgelist,
                                                FUN = sum);
        core.cands.edgelist <- core.cands.edgelist[core.cands.edgelist$from !=
                                                     core.cands.edgelist$to, ];
        core.cands.net <- igraph::graph_from_data_frame(core.cands.edgelist,
                                                        directed = FALSE,
                                                        vertices = bp.node.attr[bp.node.attr$is_candidate,]);
        
        vysledna.tabulka <- detect_groups(net = core.cands.net,
                                          pouzit.optimal,
                                          kolik.vypoctu);
        names(vysledna.tabulka)[names(vysledna.tabulka) == "group"] <- "core";
        
        # ----------------------------------------------------------------------- #
        
        get_major_core <- function(core_vector) {
          core_vector <- core_vector[!is.na(core_vector)];
          if (length(core_vector) == 0) return(NA);
          core_tab <- table(core_vector)
          return(as.integer(names(core_tab)[which.max(core_tab)]))
        } # konec fce get_major_core()
        
        vysledna.tmp <- vysledna.tabulka %>%
          dplyr::mutate(from = .data$vertices) %>%
          dplyr::select(.data$from, .data$core)
        
        bp.with.core <- bp.edgelist %>%
          dplyr::left_join(vysledna.tmp, by = "from")
        
        
        if ("elected" %in% names(bp.edgelist)) {
          
          core_from_elected <- bp.with.core %>%
            dplyr::filter(!is.na(.data$elected) & .data$elected == TRUE) %>%
            dplyr::group_by(.data$to) %>%
            dplyr::summarise(core = get_major_core(.data$core), .groups = "drop")
          
          core_from_all <- bp.with.core %>%
            dplyr::group_by(.data$to) %>%
            dplyr::summarise(core = get_major_core(.data$core), .groups = "drop")
          
          list_core_assignment <- core_from_all %>%
            dplyr::left_join(core_from_elected, by = "to", suffix = c("_all", "_elected")) %>%
            dplyr::mutate(core_final = ifelse(!is.na(.data$core_elected),
                                              .data$core_elected,
                                              .data$core_all)) %>%
            dplyr::mutate(vertices = .data$to, core = .data$core_final) %>%
            dplyr::select(.data$vertices, .data$core)
          
        } else {
          
          list_core_assignment <- bp.with.core %>%
            dplyr::group_by(.data$to) %>%
            dplyr::summarise(core = get_major_core(.data$core), .groups = "drop") %>%
            dplyr::mutate(vertices = .data$to) %>%
            dplyr::select(.data$vertices, .data$core)
          
        } # konec IF-ELSE pro zjisteni core pro kandidatni listiny
        
        # ----------------------------------------------------------------------- #
        
        vysledna.tabulka <- rbind(vysledna.tabulka, list_core_assignment)
        skupiny.tab <- dplyr::left_join(skupiny.tab, vysledna.tabulka,
                                        by = "vertices");
        
        df <- dplyr::left_join(df, skupiny.tab[skupiny.tab$vertices %in% df$candidate,
                                               c("vertices", "core")],
                               by = c("candidate" = "vertices"));
        
        # ----------------------------------------------------------------------- #
        
        if (verbose) cat("   Creating an edgelist of core network. ");
        
        core.lists.net <- igraph::bipartite_projection(multi.bp.net, which = "true");
        core.lists.edgelist <- as_data_frame(core.lists.net);
        core.lists.edgelist <- convert_lists_edgelist_to_digraph(core.lists.edgelist);
        
        core.edgelist <- convert_vertices_to_group(edgelist = core.lists.edgelist,
                                                   node_group = skupiny.tab,
                                                   group_type = "core",
                                                   aggregate = TRUE,
                                                   drop_loops = TRUE);
        
      }; # konec IF core_type == 4
      
      # ------------------------------------------------------------------------- #
      
      core.node.attr <- data.frame(vertices = sort(unique(skupiny.tab$core)));
      core.node.attr$is_isolate <- !(core.node.attr$vertices %in% unique(c(core.edgelist$from,
                                                                           core.edgelist$to)));

      if (verbose) cat("Done.\n");
      
    } # konec IF pro zahrnuti detekce jader
    
    # ----------------------------------------------------------------------- #
    
  } # konec IF pro detekci skupin (skip_groups)
  
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # ========================= Atributy uzlu - doplneni ========================
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  if (verbose) {
    if (skip_groups) {
      cat("4/ Creating attribution data for networks: ");
    } else if (include_cores) {
      cat("6/ Creating attribution data for networks: ");
    } else {
      cat("5/ Creating attribution data for networks: ");
    } # konec IF-ELSE pro ocislovani postupu pri zahrnuti detekce jader
  }; # konec IF pro ne/zobrazeni informace o postupu
  
  # ------------------------------------------------------------------------- #
  
  summarise_text_share <- function(x) {
    tbl <- sort(table(x), decreasing = TRUE);
    share <- prop.table(tbl) * 100;
    return(paste(paste0(round(share), "% ", names(share)), collapse = ", "));
  }; # konec fce summarise_text_share()
  
  # ------------------------------------------------------------------------- #
  
  summarise_text_share_elections <- function(x, elections) {
    tmp <- data.frame(group = x, year = elections);
    tbl <- table(tmp$group);
    share <- prop.table(tbl) * 100;
    group_ordered <- names(sort(share, decreasing = TRUE));
    
    labels <- vapply(group_ordered, function(g) {
      years <- sort(unique(tmp$year[tmp$group == g]));
      years_str <- paste(years, collapse = ", ");
      pct <- round(share[g]);
      paste0(pct, "% ", g, " (", years_str, ")");
    }, character(1));
    return(paste(labels, collapse = ", "));
  }; # konec fce summarise_text_share_elections()
  
  # ------------------------------------------------------------------------- #
  
  generate_group_labels <- function(group.data,
                                    id_col = "vertices",
                                    include_pct = TRUE,
                                    max_diff = 1,
                                    min_share = 5) {
    
    # vnitrni funkce pro rozkouskovani slozeni skupiny
    parse_composition_with_context <- function(x) {
      if (is.na(x) || x == "") return(data.frame(name = character(),
                                                 share = numeric(),
                                                 share_all = numeric()))
      
      parts <- unlist(strsplit(x, ",\\s*"))
      parsed <- lapply(parts, function(p) {
        m <- regmatches(p, regexec("([0-9.]+)%\\s*(.+)", p))[[1]]
        if (length(m) == 3) {
          name_clean <- gsub("\\s*\\(.*$", "", m[3])
          name_clean <- trimws(name_clean)
          share_val <- as.numeric(m[2])
          return(data.frame(name = name_clean, share = share_val))
        } else {
          name_guess <- gsub("\\s*\\(.*$", "", p)
          name_guess <- trimws(name_guess)
          return(data.frame(name = name_guess, share = NA))
        }
      })
      
      parts_df <- do.call(rbind, parsed)
      parts_df <- parts_df[!is.na(parts_df$name), ]
      
      if (all(is.na(parts_df$share))) {
        parts_df$share <- rep(100 / nrow(parts_df), nrow(parts_df))
      }
      
      if (any(is.na(parts_df$share)) && any(!is.na(parts_df$share))) {
        parts_df <- parts_df[!is.na(parts_df$share), ]
      }
      
      parts_df$share_all <- parts_df$share
      
      indep_labels <- c("NK", "BEZPP", "IND", "IND.", "non-partisan", "Independent Candidate")
      parts_df <- parts_df[!tolower(parts_df$name) %in% tolower(indep_labels), ]
      parts_df <- parts_df[parts_df$share_all >= min_share, ]
      parts_df <- parts_df[order(-parts_df$share_all), ]
      
      return(parts_df)
    }; # konec vnitrni funkce parse_composition_with_context
    
    group.data$group_label <- apply(group.data, 1, function(row) {
      nom_comp   <- parse_composition_with_context(if ("comp_by_nom_parties" %in% names(row)) row[["comp_by_nom_parties"]] else NA);
      lists_comp <- parse_composition_with_context(if ("comp_by_lists" %in% names(row)) row[["comp_by_lists"]] else NA);
      affil_comp <- parse_composition_with_context(if ("comp_by_pol_affil" %in% names(row)) row[["comp_by_pol_affil"]] else NA);
      
      create_label <- function(comp) {
        if (nrow(comp) == 0) return(NA)
        n1 <- comp$name[1]
        p1 <- comp$share_all[1]
        if (nrow(comp) >= 2 && abs(p1 - comp$share_all[2]) <= max_diff) {
          n2 <- comp$name[2]
          p2 <- comp$share_all[2]
          if (include_pct) {
            return(paste0(n1, " + ", n2, " (", round(p1), "% + ", round(p2), "%)"))
          } else {
            return(paste0(n1, " + ", n2))
          }
        } else {
          if (include_pct) {
            return(paste0(n1, " (", round(p1), "%)"))
          } else {
            return(n1)
          }
        }
      }; # konec vnitrni fce create_label()
      
      nom_label   <- create_label(nom_comp)
      list_label  <- create_label(lists_comp)
      affil_label <- create_label(affil_comp)
      
      if (!is.na(nom_comp$name[1])) {
        nom_name <- nom_comp$name[1]
        list_name <- if (nrow(lists_comp) > 0) lists_comp$name[1] else NA
        nom_pct <- nom_comp$share_all[1]
        if (!is.na(list_name) && nom_name != list_name) {
          label <- paste0(nom_name, " [", list_name, "]")
          if (include_pct) label <- paste0(label, " (", round(nom_pct), "%)")
          return(label)
        } else {
          return(nom_label)
        }
      }
      
      if (!is.na(list_label)) return(list_label)
      if (!is.na(affil_label)) return(affil_label)
      
      return("Independent group")
    })
    
    return(group.data)
  }; # konec fce generate_group_labels()
  
  
  ## ======================== 1. elections.node.attr ==========================
  
  if (verbose) cat("elections, ");
  
  elections_attr <- df %>%
    group_by(.data$elections) %>%
    summarise(
      cands = n_distinct(.data$candidate),
      seats = if ("const_size" %in% names(df)) sum(.data$const_size, na.rm = TRUE) else NA,
      elected = if ("elected" %in% names(df)) sum(.data$elected, na.rm = TRUE) else NA,
      lists = n_distinct(.data$list_id),
      reg_voters = if ("elig_voters" %in% names(df)) sum(.data$elig_voters, na.rm = TRUE) else NA,
      ballots = if ("ballots_cast" %in% names(df)) sum(.data$ballots_cast, na.rm = TRUE) else NA,
      turnout = if (all(c("elig_voters", "ballots_cast") %in% names(df))) sum(.data$ballots_cast, na.rm = TRUE) /
        sum(.data$elig_voters, na.rm = TRUE) else NA,
      votes = if ("list_votes" %in% names(df)) sum(.data$list_votes, na.rm = TRUE) else NA,
      plurality = if ("const_size" %in% names(df)) n_distinct(.data$candidate) /
        sum(.data$const_size, na.rm = TRUE) else NA,
      cand_density = if ("elig_voters" %in% names(df)) n_distinct(.data$candidate) /
        sum(.data$elig_voters, na.rm = TRUE) else NA,
      board = if ("board" %in% names(df)) sum(.data$board, na.rm = TRUE) else NA,
      .groups = "drop"
    );
  
  elections_attr <- elections_attr[, colSums(!is.na(elections_attr)) > 0]
  elections.node.attr <- left_join(elections.node.attr, elections_attr,
                                   by = c("vertices" = "elections"));
  
  ## ========================== 2. cands.node.attr ============================

  if (verbose) cat("\n   candidates, ", sep = "");
  
  candidates_attr <- df %>%
    group_by(.data$candidate) %>%
    summarise(
      candidacy = n_distinct(.data$elections),
      candidacy_ratio = n_distinct(.data$elections) / nrow(elections.node.attr),
      list_pos_avg = if ("list_pos" %in% names(df)) mean(.data$list_pos, na.rm = TRUE) else NA,
      pref_votes_mean = if ("pref_votes" %in% names(df)) mean(.data$pref_votes, na.rm = TRUE) else NA,
      seats = if ("elected" %in% names(df)) sum(.data$elected, na.rm = TRUE) else NA,
      seats_ratio = if ("elected" %in% names(df)) sum(.data$elected, na.rm = TRUE) /
        n_distinct(.data$elections) else NA,
      mayor = if ("mayor" %in% names(df)) sum(.data$mayor, na.rm = TRUE) else NA,
      dep_mayor = if ("dep_mayor" %in% names(df)) sum(.data$dep_mayor, na.rm = TRUE) else NA,
      board = if ("board" %in% names(df)) sum(.data$board, na.rm = TRUE) else NA,
      gov_support = if ("gov_support" %in% names(df)) sum(.data$gov_support, na.rm = TRUE) else NA,
      nom_parties = if ("nom_party" %in% names(df)) summarise_text_share(.data$nom_party) else NA,
      partisanship = if ("pol_affil" %in% names(df)) summarise_text_share(.data$pol_affil) else NA,
      .groups = "drop"
    );
  
  candidates_attr <- candidates_attr[, colSums(!is.na(candidates_attr)) > 0]
  
  # ------------------------------------------------------------------------- #

  cands.node.attr[,c("abbr", "initials")] <- NA;
  
  for (radek in 1:nrow(cands.node.attr)) {
    
    rozlozene.jmeno <- unlist(strsplit(cands.node.attr[radek,"vertices"], split = " "));
    
    zkratka  <- c();
    inicialy <- c();
    
    samohlasky <- c("a", "e", "i", "o", "u", "\u00E1", "\u00E9", "\u011B",
                    "\u00ED", "\u00F3", "\u016F", "\u00FA");
    
    for(i in 1:length(rozlozene.jmeno)) {
      
      if (i == 1 & length(rozlozene.jmeno) == 1) {
        if (nchar(rozlozene.jmeno[i]) <= 6) {
          zkratka  <- rozlozene.jmeno[i];
          inicialy <- if (grepl("[[:digit:]]", rozlozene.jmeno[i])) {
            rozlozene.jmeno[i];
          } else toupper(substr(rozlozene.jmeno[i],1,1));
          
        } else {

          zkratka  <- ifelse(grepl("ch", substr(rozlozene.jmeno[i],5,6)) |
                               grepl("ll", substr(rozlozene.jmeno[i],5,6)) |
                               is.element(substr(rozlozene.jmeno[i],5,5), samohlasky),
                             paste(substr(rozlozene.jmeno[i],1,5), ".", sep = ""),
                             paste(substr(rozlozene.jmeno[i],1,5), ".", sep = ""));
          inicialy <- toupper(substr(rozlozene.jmeno[i],1,1));
        } # konec IF-ELSE pro prvni jmeno z vice jmen
      } else if (i == 1 & length(rozlozene.jmeno) > 1) {
        if (nchar(rozlozene.jmeno[i]) <= 4) {
          zkratka  <- paste0(rozlozene.jmeno[i], ",");
          inicialy <- toupper(substr(rozlozene.jmeno[i],1,1));
        } else {
          zkratka  <- ifelse(grepl("ch", substr(rozlozene.jmeno[i],3,4)) |
                               grepl("ll", substr(rozlozene.jmeno[i],3,4)) |
                               is.element(substr(rozlozene.jmeno[i],3,3), samohlasky),
                             paste(substr(rozlozene.jmeno[i],1,4), ".", sep = ""),
                             paste(substr(rozlozene.jmeno[i],1,3), ".", sep = ""));
          inicialy <- toupper(substr(rozlozene.jmeno[i],1,1));
        } # konec IF-ELSE pro prvni jmeno z vice jmen
      } else if (grepl("[[:digit:]]", rozlozene.jmeno[i])) {
        zkratka  <- c(zkratka, rozlozene.jmeno[i]);
        inicialy <- c(inicialy, rozlozene.jmeno[i]);
      } else {
        zkratka  <- c(zkratka, substr(rozlozene.jmeno[i],1,1));
        inicialy <- c(inicialy, substr(rozlozene.jmeno[i],1,1));
      } # konec IF-ELSE IF-ELSE
    } # konec FOR loopu pro vytvoreni zkratky a inicialu jednotlivych kandidatu
    
    cands.node.attr[radek, "abbr"]     <- paste(zkratka, collapse = "");
    cands.node.attr[radek, "initials"] <- paste(inicialy, collapse = "");
    
  } # konec FOR loopu pro sestaveni zkratek a inicial kandidatU
  
  # ------------------------------------------------------------------------- #
  
  if (skip_groups) {
    cands.node.attr <- left_join(cands.node.attr, candidates_attr,
                                 by = c("vertices" = "candidate"));
  } else {
    cands.node.attr <- cands.node.attr %>%
      left_join(candidates_attr, c("vertices" = "candidate")) %>%
      left_join(skupiny.tab[bp.node.attr$is_candidate,], by = "vertices");
  }; # konec IF-ELSE pro pridani atributu podle detekce skupin
  
  ## ========================== 3. lists.node.attr ============================
  
  if (verbose) cat("lists (& continuity)");
  
  list_attr <- df %>%
    group_by(.data$list_id) %>%
    summarise(
      list_name = unique(.data$list_name),
      elections = unique(.data$elections),
      leader = if ("list_pos" %in% names(df)) paste(.data$candidate[.data$list_pos == 1],
                                                    collapse = "; "),
      cands = n_distinct(.data$candidate),
      seats_n = if ("elected" %in% names(df)) sum(.data$elected, na.rm = TRUE) else NA,
      seats_pct = if ("elected" %in% names(df)) sum(.data$elected, na.rm = TRUE) * 100 /
        elections.node.attr$elected[elections.node.attr$vertices == unique(.data$elections)] else NA,
      votes_n = if ("list_votes" %in% names(df)) sum(.data$list_votes, na.rm = TRUE) else NA,
      votes_pct = if ("list_votes" %in% names(df)) sum(.data$list_votes, na.rm = TRUE) * 100 /
        elections.node.attr$votes[elections.node.attr$vertices == unique(.data$elections)] else NA,
      coef_var = if ("pref_votes" %in% names(df)) ifelse(is.na(sd(.data$pref_votes)),
                                                         0, sd(.data$pref_votes) /
                                                           mean(.data$pref_votes)) else NA,
      success = if ("elected" %in% names(df)) paste0(sum(.data$elected, na.rm = TRUE),
                                                     "/", n_distinct(.data$candidate)) else NA,
      mayor = if ("mayor" %in% names(df)) sum(.data$mayor, na.rm = TRUE) else NA,
      dep_mayors = if ("dep_mayor" %in% names(df)) sum(.data$dep_mayor, na.rm = TRUE) else NA,
      board = if ("board" %in% names(df)) sum(.data$board, na.rm = TRUE) else NA,
      board_pct = if ("board" %in% names(df)) sum(.data$board, na.rm = TRUE) * 100 /
        elections.node.attr$board[elections.node.attr$vertices == unique(.data$elections)] else NA,
      gov_support = if ("gov_support" %in% names(df)) any(.data$gov_support == 1) else NA,
      gov_support_rate = if (all(c("gov_support", "elected") %in% names(df))) {
        if (sum(.data$elected, na.rm = TRUE) > 0) {
          sum(.data$gov_support & .data$elected, na.rm = TRUE) / sum(.data$elected, na.rm = TRUE)
        } else { 0 }
      } else NA,
      
      nom_parties = if ("nom_party" %in% names(df)) summarise_text_share(.data$nom_party) else NA,
      partisanship = if ("pol_affil" %in% names(df)) summarise_text_share(.data$pol_affil) else NA,
      ind_pct = if ("pol_affil" %in% names(df)) {
        sum(.data$pol_affil %in% c("non-partisan", "BEZPP"), na.rm = TRUE) * 100 / n_distinct(.data$candidate)
      } else NA,
      .groups = "drop"
    );
  
  list_attr <- list_attr[, colSums(!is.na(list_attr)) > 0]
  
  # ------------------------------------------------------------------------- #
  
  lists.node.attr$abbr <- NA;
  
  for (radek in 1:nrow(lists.node.attr)) {
    
    rozlozeny.nazev <- unlist(strsplit(lists.node.attr[radek,"vertices"],
                                       split = " "));
    rok.voleb       <- utils::tail(rozlozeny.nazev, 1);
    rozlozeny.nazev <- rozlozeny.nazev[rozlozeny.nazev != rok.voleb];
    
    zkratka <- c();
    
    nezavisly.kandidat <-c("NK,", ",NK", "NK", # ", NK" ma mezeru
                           "IND.", "IND", "IND.,", "IND,", ",IND.", ",IND");
    
    if (any(is.element(rozlozeny.nazev, nezavisly.kandidat))) {
      zkratka <- "IND., ";
      rozlozeny.nazev <- rozlozeny.nazev[!is.element(rozlozeny.nazev,
                                                     nezavisly.kandidat)];
    }; # konec IF pro identifikaci NK
    
    for(slovo in rozlozeny.nazev) {
      
      pismenka <- unlist(strsplit(slovo, split = ""));
      
      for (i in 1:length(pismenka)) {
        if (i == 1) {
          zkratka <- c(zkratka, toupper(pismenka[i]))
        } else if (grepl("[[:digit:]]", pismenka[i]) | 
                   grepl("[[:upper:]]", pismenka[i]) | 
                   pismenka[i] == "-" |
                   pismenka[i] == ",") {
          zkratka <- c(zkratka, pismenka[i]);
        } # konec IF - ostatni pismena se nezapisou
      } # konec FOR loopu prochazejici jednotliva pismenka
    } # konec FOR loopu pro vytvoreni zkratky jednotlivych kand. listin
    
    lists.node.attr[radek, "abbr"] <- paste(zkratka, collapse = "");
    
  } # konec FOR loopu pro sestaveni zkratek kandidatnich listin
  
  # ------------------------------------------------------------------------- #

  if (skip_groups) {
    lists.node.attr <- left_join(lists.node.attr, list_attr,
                                 by = c("vertices" = "list_id"));
  } else {
    lists.node.attr <- lists.node.attr %>%
      left_join(list_attr, by = c("vertices" = "list_id")) %>%
      left_join(skupiny.tab[bp.node.attr$is_list,], by = "vertices");
  }; # konec IF-ELSE pro pridani atributu podle detekce skupin
  
  ## ======================= 4. continuity.node.attr ==========================
  
  # continuity.node.attr <- lists.node.attr # jiz je ve vystupnim objektu zadano
  
  
  ## =========================== 5. bp.node.attr ==============================
  
  if (verbose) {
    if (skip_groups) {
      cat(", and bipartite net");
    } else {
      cat(", bipartite net, ");
    };
  }; # konec IF pro ne/zobrazeni informace o postupu
  
  bp.cands.node.attr <- cands.node.attr[, !names(cands.node.attr) %in%
                                          c("is_isolate", "party", "core")];
  bp.lists.node.attr <- lists.node.attr[, !names(lists.node.attr) %in%
                                          c("is_isolate", "party", "core")];
  
  # ------------------------------------------------------------------------- #
  
  bp.cands.node.attr <- bp.cands.node.attr %>%
    rename_with(.fn = ~paste0("c_", .), .cols = -"vertices")
  
  bp.lists.node.attr <- bp.lists.node.attr %>%
    rename_with(.fn = ~paste0("l_", .), .cols = -"vertices")
  
  # ------------------------------------------------------------------------- #
  
  if (skip_groups) {
    bp.node.attr <- bp.node.attr %>%
      left_join(bp.cands.node.attr, by = "vertices") %>%
      left_join(bp.lists.node.attr, by = "vertices");
  } else {
    bp.node.attr <- bp.node.attr %>%
      left_join(bp.cands.node.attr, by = "vertices") %>%
      left_join(bp.lists.node.attr, by = "vertices") %>%
      left_join(skupiny.tab, by = "vertices");
  }; # konec IF-ELSE pro pridani atributu podle detekce skupin
  
  
  ## ========================== 6. party.node.attr ============================
  
  if (!skip_groups) {
    
    if (verbose) cat("parties");
    
    party_lists_map <- setNames(
      lapply(party.node.attr$lists, function(x) trimws(unlist(strsplit(x, "\\|")))),
      party.node.attr$vertices);
    
    party_compositions_list <- list()
    party_attr_list <- list()
    
    for (party_id in party.node.attr$vertices) {
      
      party_lists <- party_lists_map[[as.character(party_id)]];
      df_party <- df[df$list_id %in% party_lists, ];
  
      if (nrow(df_party) == 0) next
      
      comp_by_lists <- summarise_text_share_elections(df_party$list_name, df_party$elections)
      comp_by_nom_parties <- if ("nom_party" %in% names(df_party)) summarise_text_share_elections(df_party$nom_party, df_party$elections) else NA
      comp_by_pol_affil <- if ("pol_affil" %in% names(df_party)) summarise_text_share_elections(df_party$pol_affil, df_party$elections) else NA
      
      party_compositions_list[[as.character(party_id)]] <- data.frame(
        party = party_id,
        comp_by_lists = comp_by_lists,
        comp_by_nom_parties = comp_by_nom_parties,
        comp_by_pol_affil = comp_by_pol_affil,
        stringsAsFactors = FALSE
      );
      
      attr <- df_party %>%
        summarise(
          party = party_id,
          elections = paste(unique(.data$elections), collapse = ", "),
          leaders = if ("list_pos" %in% names(df_party)) paste(unique(.data$candidate[.data$list_pos == 1]),
                                                               collapse = "; ") else NA,
          cands = n_distinct(.data$candidate),
          seats_n = if ("elected" %in% names(df_party)) sum(.data$elected, na.rm = TRUE) else NA,
          votes_n = if ("list_votes" %in% names(df_party)) sum(.data$list_votes, na.rm = TRUE) else NA,
          mayor = if ("mayor" %in% names(df_party)) sum(.data$mayor, na.rm = TRUE) else NA,
          dep_mayors = if ("dep_mayor" %in% names(df_party)) sum(.data$dep_mayor, na.rm = TRUE) else NA,
          board = if ("board" %in% names(df_party)) sum(.data$board, na.rm = TRUE) else NA,
          partisanship = if ("pol_affil" %in% names(df_party)) summarise_text_share(.data$pol_affil) else NA,
          ind_pct = if ("pol_affil" %in% names(df_party)) {
            mean(.data$pol_affil %in% c("non-partisan", "BEZPP"), na.rm = TRUE) * 100} else NA
        );
      
      if ("pol_affil" %in% names(df_party)) {
        cand_ind_tbl <- aggregate(df_party$pol_affil %in% c("non-partisan", "BEZPP"),
                                  list(candidate = df_party$candidate),
                                  any, na.rm = TRUE)
        attr$ind_pct_unique <- mean(cand_ind_tbl$x) * 100
      } # konec IF pro vypocet procenta unikatnich nezavislych kandidatu
      
      party_attr_list[[as.character(party_id)]] <- attr;
      
    } # konec FOR loopu prochazejici jednotlive strany
    
    party_compositions <- do.call(rbind, party_compositions_list);
    party_attr <- do.call(rbind, party_attr_list);
    party_attr <- party_attr[, colSums(!is.na(party_attr)) > 0];
    
    party_compositions <- generate_group_labels(party_compositions, id_col = "party",
                                                include_pct = TRUE, max_diff = 1,
                                                min_share = 5)

    party.node.attr <- party.node.attr %>%
      left_join(party_compositions, by = c("vertices" = "party")) %>%
      left_join(party_attr, by = c("vertices" = "party"))
    
  }; # konec IF pro pripad zahrnuti nebo vynechani detekce skupin / stran
  
  
  ## ========================== 7. core.node.attr ============================
  
  if (include_cores && !skip_groups) {
    
    if (verbose) cat("\n   and cores", sep = "");

    core_compositions <- data.frame(core = core.node.attr$vertices);
    
    for (core in core_compositions$core) {
      
      core_compositions[core,"comp_by_lists"] <-
        summarise_text_share_elections(df$list_name[df$core == core],
                                       df$elections[df$core == core]);
      
      if ("nom_party" %in% names(df)) {
        core_compositions[core,"comp_by_nom_parties"] <-
          summarise_text_share_elections(df$nom_party[df$core == core],
                                         df$elections[df$core == core])};
      
      if ("pol_affil" %in% names(df)) {
        core_compositions[core,"comp_by_pol_affil"] <-
          summarise_text_share_elections(df$pol_affil[df$core == core],
                                         df$elections[df$core == core])};
    }; # konec FOR loopu pro sestaveni slozeni stran
    
    # ------------------------------------------------------------------------- #
    
    core_compositions <- generate_group_labels(core_compositions, id_col = "core",
                                               include_pct = TRUE, max_diff = 1,
                                               min_share = 5);
    
    # ------------------------------------------------------------------------- #
    
    core_attr <- df %>%
      group_by(.data$core) %>%
      summarise(
        elections = paste(unique(.data$elections), collapse = ", "),
        leaders = if ("list_pos" %in% names(df)) paste(unique(.data$candidate[.data$list_pos == 1]),
                                                       collapse = "; "),
        cands = n_distinct(.data$candidate),
        seats_n = if ("elected" %in% names(df)) sum(.data$elected, na.rm = TRUE) else NA,
        votes_n = if ("list_votes" %in% names(df)) sum(.data$list_votes, na.rm = TRUE) else NA,
        mayor = if ("mayor" %in% names(df)) sum(.data$mayor, na.rm = TRUE) else NA,
        dep_mayors = if ("dep_mayor" %in% names(df)) sum(.data$dep_mayor, na.rm = TRUE) else NA,
        board = if ("board" %in% names(df)) sum(.data$board, na.rm = TRUE) else NA,
        
        partisanship = if ("pol_affil" %in% names(df)) summarise_text_share(.data$pol_affil) else NA,
        ind_pct = if ("pol_affil" %in% names(df)) {
          mean(.data$pol_affil %in% c("non-partisan", "BEZPP"), na.rm = TRUE) * 100
        } else NA,
        .groups = "drop"
      );
    
    core_attr <- core_attr[, colSums(!is.na(core_attr)) > 0];
    
    # ------------------------------------------------------------------------- #
    
    if ("pol_affil" %in% names(df)) {
      cand_ind_tbl <- aggregate(df$pol_affil %in% c("non-partisan", "BEZPP"),
                                list(core      = df$core,
                                     candidate = df$candidate),
                                any, na.rm = TRUE)
      core_pct_tbl <- aggregate(x ~ core, cand_ind_tbl, mean)
      core_pct_tbl$x <- core_pct_tbl$x * 100
      names(core_pct_tbl)[2] <- "ind_pct_unique"
      
      core_attr <- dplyr::left_join(core_attr, core_pct_tbl, by = "core");
    } # konec IF pro vypocet procenta unikatnich nezavislych kandidatu
    
    # ------------------------------------------------------------------------- #
    
    core.node.attr <- core.node.attr %>%
      left_join(core_compositions, by = c("vertices" = "core")) %>%
      left_join(core_attr, by = c("vertices" = "core"))
    
  } # konec IF pro zahrnuti detekce jader
  
  # ------------------------------------------------------------------------- #
  
  if (verbose) {
    if (include_cores && !skip_groups) {
      cat(". Done.\n")
    } else {
      cat(".\n   Done.\n")
    } # konec IF-ELSE pro pripad zahrnuti detekce jader
  }; # konec IF pro ne/zobrazeni informace o postupu
  
  
  # ######################################################################### #
  # ------------------------------------------------------------------------- #
  # ============================ Vystupni objekt ==============================
  # ------------------------------------------------------------------------- #
  # ######################################################################### #
  
  vystup <- list(bipartite  = list(edgelist  = bp.edgelist,
                                   node_attr = bp.node.attr),
                 candidates = list(edgelist  = cands.edgelist,
                                   node_attr = cands.node.attr),
                 lists      = list(edgelist  = lists.edgelist,
                                   node_attr = lists.node.attr),
                 continuity = list(edgelist  = continuity.edgelist,
                                   node_attr = lists.node.attr));
  
  if (!skip_groups) vystup$parties <- list(edgelist  = party.edgelist,
                                           node_attr = party.node.attr);
  
  if (!skip_groups && include_cores) vystup$cores <- list(edgelist  = core.edgelist,
                                                          node_attr = core.node.attr);
  
  vystup$elections <- list(edgelist  = elections.edgelist,
                           node_attr = elections.node.attr);
  
  # ------------------------------------------------------------------------- #
  
  if (verbose) cat("\n",
                   "==========================================================\n",
                   "FINISHED.\n",
                   "==========================================================\n",
                   sep = "");
  
  return(vystup);
  
} # konec funce prepare_network_data()
