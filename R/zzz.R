#' Internal settings for the package
#'
#' This file registers known global variables used in NSE (dplyr).
#' It avoids NOTE messages during R CMD check.
#'
#' @keywords internal
#' @noRd

utils::globalVariables(c(
  ".data"
))
