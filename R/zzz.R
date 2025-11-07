#' Internal settings for the package
#'
#' This file initializes package options and registers known global variables
#' used in NSE (dplyr). It avoids NOTE messages during R CMD check.
#'
#' @keywords internal
#' @noRd

# --------------------------------------------------------------------------- #

utils::globalVariables(c(
  ".data"
));

# --------------------------------------------------------------------------- #

lpanda_default_options <- function() {
  
  list(
    lpanda.text_encoding = "auto",   # "auto" | "utf8" | "ascii"
    lpanda.plot_family   = "sans",   # "sans" | "serif" | "mono"
    lpanda.force_ascii   = FALSE,    # TRUE = transliterate diacritics when needed
    lpanda.silent        = FALSE     # TRUE = suppress startup message
  );
  
}; # konec lpanda_default_options()

# --------------------------------------------------------------------------- #

lpanda_validate_options <- function(x) {
  
  allowed_encoding <- c("auto", "utf8", "ascii");
  
  if (!is.null(x$lpanda.text_encoding)) {
    
    enc <- tolower(as.character(x$lpanda.text_encoding));
    
    if (!enc %in% allowed_encoding) {
      warning("Unknown lpanda.text_encoding '", enc, "'; falling back to 'auto'.");
      x$lpanda.text_encoding <- "auto";
    } else {
      x$lpanda.text_encoding <- enc;
    } # konec IF-ELSE pro overeni, ze zvolene encoding odpovida zadani
  } # konec IF pro encoding
  
  if (!is.null(x$lpanda.plot_family)) {
    x$lpanda.plot_family <- as.character(x$lpanda.plot_family);
  } # konec IF pro lpanda.plot_family
  
  if (!is.null(x$lpanda.force_ascii)) {
    x$lpanda.force_ascii <- isTRUE(x$lpanda.force_ascii);
  } # konec IF pro lpanda.force_ascii
  
  if (!is.null(x$lpanda.silent)) {
    x$lpanda.silent <- isTRUE(x$lpanda.silent);
  } # konec IF pro lpanda.silent
  
  return(x)
} # konec funkce lpanda_validate_options

# --------------------------------------------------------------------------- #

.onLoad <- function(libname, pkgname) {
  
  defs <- lpanda_default_options();
  toset <- setdiff(names(defs), names(options()));
  
  if (length(toset)) {
    opt <- defs[toset];
    opt <- lpanda_validate_options(opt);
    options(opt);
  }; # konec IF pro nastaveni options
  
  invisible();
  
} # konec .onLoad

# --------------------------------------------------------------------------- #

.onAttach <- function(libname, pkgname) {
  
  if (isTRUE(getOption("lpanda.silent", FALSE))) return(invisible());
  
  msg <- paste0("lpanda (", utils::packageVersion(pkgname), ") ",
                "successfully loaded. Type ?lpanda for help.");
  packageStartupMessage(msg);
  
  invisible();
  
} # konec .onAttach

# --------------------------------------------------------------------------- #

# Unload hook (just in case it comes in handy in the future)
.onUnload <- function(libpath) {
  
  invisible();
  
} # konec .onUnload
