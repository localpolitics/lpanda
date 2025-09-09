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
#' A variant of \code{\link{sample_data}} containing binary values (0/1) instead of
#' TRUE/FALSE values. This is useful for testing functions that must correctly
#' interpret both logical and numeric binary formats.
#'
#' @format A data frame with 18 rows and 5 variables (same structure as \code{\link{sample_data}}).
#' @inherit sample_data source
"sample_binary_values"

# --------------------------------------------------------------------------- #

#' Sample Dataset with Some Different Variable Names
#'
#' A variant of \code{\link{sample_data}} in which some variables have different
#' names (\code{list_name} becomes \code{party} and \code{elected} becomes
#' \code{seat}). This is useful for testing robustness of input handling.
#'
#' @format A data frame with 18 rows and 5 variables (same structure as \code{\link{sample_data}}).
#' @inherit sample_data source
"sample_different_varnames"

# --------------------------------------------------------------------------- #

#' Sample Dataset Without Candidate Switching
#'
#' A variant of \code{\link{sample_data}} in which candidates may run in multiple
#' elections, but always remain within the same political group. In other words,
#' they never switch between candidate list clusters, which makes the dataset
#' useful for testing continuity logic under stable group membership (verifying
#' that no cross-group transitions occur).
#'
#' @format A data frame with 15 rows and 5 variables (same structure as \code{\link{sample_data}}).
#' @inherit sample_data source
"sample_no_switching"

# --------------------------------------------------------------------------- #

#' Sample Dataset Without Continuity Between Elections
#'
#' A variant of \code{\link{sample_data}} in which no candidate appears in more
#' than one election. This breaks the continuity between elections, making the
#' dataset useful for testing whether network-building functions correctly handle
#' cases with no longitudinal links across candidate lists.
#'
#' @format A data frame with 15 rows and 5 variables (same structure as \code{\link{sample_data}}).
#' @inherit sample_data source
"sample_no_continuity"

# --------------------------------------------------------------------------- #

#' Sample Dataset Without Party Pluralism (only one candidate list per election)
#'
#' A variant of \code{\link{sample_data}} in which only one candidate list is
#' running in each election. This removes party pluralism from the party system,
#' making the dataset useful for testing functions under non-competitive conditions.
#'
#' @format A data frame with 9 rows and 5 variables (same structure as \code{\link{sample_data}}).
#' @inherit sample_data source
"sample_no_pluralism"
