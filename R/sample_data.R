#' Simple Sample Dataset
#'
#' Basic fictitious dataset simulating election results.
#'
#' @format A data frame with 18 rows and 5 variables:
#' \describe{
#'   \item{elections}{Election identifier (numeric)}
#'   \item{candidate}{Candidate identifier (character)}
#'   \item{list_name}{Candidate list name (character)}
#'   \item{elected}{Logical; \code{TRUE} if the candidate was elected}
#'   \item{mayor}{Logical; \code{TRUE} if the candidate became mayor}
#' }
#'
#' @source Fictitious data
"sample_data"

# --------------------------------------------------------------------------- #

#' Sample Dataset with Binary Values
#'
#' A variant of \code{\link{sample_data}} containing binary values instead of TRUE/FALSE values.
#'
#' @format A data frame with 18 rows and 5 variables (same structure as \code{\link{sample_data}}).
#' @inherit sample_data source
"sample_binary_values"

# --------------------------------------------------------------------------- #

#' Sample Dataset with Some Different Variable Names
#'
#' A variant of \code{\link{sample_data}} with different names for variables list_name
#' (party) and elected (seat), useful for testing robustness of input handling.
#'
#' @format A data frame with 18 rows and 5 variables (same structure as \code{\link{sample_data}}).
#' @inherit sample_data source
"sample_diff_varnames"

# --------------------------------------------------------------------------- #

#' Sample Dataset Without Candidate Switching
#'
#' A variant of \code{\link{sample_data}} in which candidates may run in multiple
#' elections, but always remain within the same political group (they do not
#' switch between candidate list clusters). This is useful for testing continuity
#' logic and verifying that no cross-group transitions occur.
#'
#' @format A data frame with 15 rows and 5 variables (same structure as \code{\link{sample_data}}).
#' @inherit sample_data source
"sample_no_crossing"

# --------------------------------------------------------------------------- #

#' Sample Dataset Without Continuity Between Elections
#'
#' A variant of \code{\link{sample_data}} in which no candidate appears in more
#' than one election. This breaks the continuity between elections, making it
#' useful for testing whether network-building functions correctly handle datasets
#' with no longitudinal links between candidate lists.
#'
#' @format A data frame with 15 rows and 5 variables (same structure as \code{\link{sample_data}}).
#' @inherit sample_data source
"sample_no_continuity"
