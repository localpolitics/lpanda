#' @title Internal function for selecting elections
#'
#' @description
#' Internal helper function that selects a subset of election identifiers
#' from a numeric vector based on a flexible text-based filter. Supports
#' individual values, ranges (e.g., "2002-2006"), open-ended intervals
#' (e.g., "-2002", "2010-"), and combinations separated by commas.
#'
#' @param elections_vector Numeric vector of election identifiers (years or
#'                         year.month).
#' @param filter A character string defining which elections to select. Can
#'               include single values (e.g., "2002", "2002.11"), ranges (e.g.,
#'               "2002-2006"), open-ended intervals (e.g., "-2002", "2014-"),
#'               or combinations (e.g., "-2002, 2003.11, 2010-2014, 2022-").
#'               Also an atomic vector coercible to character (e.g. c(2004,
#'               "2008") can be used.
#'
#' @returns A sorted numeric vector of selected election identifiers.
#'
#' @details If none of the values in the filter match the available elections,
#' the function stops with an error.
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' elections <- c(1994, 1998, 2002, 2006, 2010, 2014, 2018, 2022)
#' select_elections(elections, "2002-")
#' select_elections(elections, "-2002")
#' select_elections(elections, c(2002, 2006))
#' select_elections(elections, "1990-2025")
#' select_elections(elections, "-1998, 2006, 2018-")
#'
select_elections <- function(elections_vector, filter = NULL) {
  
  if (is.null(filter)) return(unique(elections_vector));
  
  if (!is.character(filter)) {
    if (is.atomic(filter)) {
      filter <- paste(filter, collapse = ",");
    } else {
      stop(paste("Invalid filter format for selecting elections.\n",
                 "It must be a character string (e.g. '2014-') or\n",
                 "an atomic vector (e.g. c(2002, '2014-'))."));
    } # konec IF pro prevedeni c() na string
  } # konec IF pro kontrolu zadani formatu filtru
  
  filter_clean <- gsub("\\s+", "", filter);
  filters <- unlist(strsplit(filter_clean, ","));
  to_numeric <- function(x) suppressWarnings(as.numeric(x));
  
  selected <- c()
  
  for (f in filters) {
    if (grepl("^-\\d+(\\.\\d+)?$", f)) {
      max_val <- to_numeric(sub("^-", "", f));
      selected <- c(selected, elections_vector[elections_vector <= max_val]);
    } else if (grepl("^\\d+(\\.\\d+)?-$", f)) {
      min_val <- to_numeric(sub("-$", "", f));
      selected <- c(selected, elections_vector[elections_vector >= min_val]);
    } else if (grepl("^\\d+(\\.\\d+)?-\\d+(\\.\\d+)?$", f)) {
      range_vals <- unlist(strsplit(f, "-"));
      min_val <- to_numeric(range_vals[1]);
      max_val <- to_numeric(range_vals[2]);
      selected <- c(selected, elections_vector[elections_vector >= min_val & elections_vector <= max_val]);
    } else if (grepl("^\\d+(\\.\\d+)?$", f)) {
      val <- to_numeric(f);
      if (val %in% elections_vector) {
        selected <- c(selected, val)
      } else {
        warning(paste0("Election '", f, "' is not in the database."));
      }
    } else {
      warning(paste0("Unrecognized elections in the filter: '", f, "'."));
    }
  } # konec FOR loop pro prochazeni jednotlivych casti filtru
  
  if (length(selected) == 0) {
    stop("None of the elections in the filter are in the database.")
  }
  
  return(sort(unique(selected)));
} # konec funkce select_elections
