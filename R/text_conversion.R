#' @title Decide whether to use ASCII for plot texts
#'
#' @description
#' Returns `TRUE` if the current session (non-interactive checks, CI environments,
#' devices that cannot render UTF-8) should fall back to ASCII for any plot
#' text (labels, titles, legend text), otherwise `FALSE`.
#'
#' @param text_encoding Character scalar. One of `"auto"`, `"utf8"`, `"ascii"`.
#'                      In `"ascii"` mode the function always returns `TRUE`.
#'                      In `"utf8"` it always returns `FALSE`. In `"auto"`,
#'                      the function uses a series of pragmatic checks: `TRUE`
#'                      in non-interactive sessions, CI environments, or when
#'                      the current graphics device cannot render a basic UTF-8
#'                      probe. Defaults to `getOption("lpanda.text_encoding", "auto")`.
#'
#' @return Logical scalar.
#'
#' @keywords internal
#' @noRd
#'
should_use_ascii <- function(text_encoding = getOption("lpanda.text_encoding", "auto")) {
  
  mode <- match.arg(text_encoding, c("auto","utf8","ascii"));
  
  if (mode == "ascii") return(TRUE);
  if (mode == "utf8")  return(FALSE);
  if (!interactive()) return(TRUE);
  if (tolower(Sys.getenv("CI")) %in% c("1","true","yes")) return(TRUE);
  if (isTRUE(getOption("lpanda.force_ascii", FALSE))) return(TRUE);
  
  # --- #
  
  op <- try(graphics::par(no.readonly = TRUE), silent = TRUE);
  on.exit(if (!inherits(op, "try-error")) graphics::par(op), add = TRUE);
  try(graphics::par(family = getOption("lpanda.plot_family", "sans")));
  
  ok <- tryCatch({
    invisible(graphics::strwidth("\u0159\u00FC\u00F1\u00E7\u015B", units = "inches"));
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  # --- #
  
  return(!isTRUE(ok))
} # konec fce should_use_ascii()

# --------------------------------------------------------------------------- #

#' @title Convert UTF-8 strings to ASCII safely
#'
#' @description
#' Converts UTF-8 strings to ASCII using `iconv(..., "ASCII//TRANSLIT")`.
#' Any elements that still fail to convert are cleaned by removing non-ASCII
#' characters to avoid plot errors. If, after cleaning, no characters remain
#' for a non-NA input, the result is set to NA.
#'
#' @param x Character vector.
#'
#' @return Character vector of the same length as `x`.
#'
#' @keywords internal
#' @noRd
#'
convert_utf8_to_ascii <- function(x) {
  
  x <- as.character(x);
  y <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "");

  y <- gsub("([~`'\\^\\x{00A8}\"])\\s*([A-Za-z])", "\\2", y, perl = TRUE);
  y <- gsub("([A-Za-z])\\s*([~`'\\^\\x{00A8}\"])", "\\1", y, perl = TRUE);
  
  if (anyNA(y)) {
    idx <- is.na(y);
    y[idx] <- gsub("[^ -~]", "", x[idx]);
  };
  
  y <- gsub("[^ -~]", "", y);
  y[!is.na(y) & !nzchar(y)] <- NA_character_;
  
  return(y)
} # konec fce convert_urf8_to_ascii()
