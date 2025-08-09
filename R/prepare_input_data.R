#' @title Prepare Input Data for LPANDA
#'
#' @description
#' Checks the suitability of input data for the diachronic analysis of local
#' political actor network (LPANDA) and prepares it for use in other functions.
#'
#' @param df A \link[base]{data.frame} containing data from elections, with one
#'           row per candidate. See \code{\link{prepare_network_data}} for more
#'           details.
#'
#' @param input_variable_map A \link[base]{list} mapping variable names in `df`
#'                           that differ from the expected ones (for more detail
#'                           see \code{\link{prepare_network_data}}).
#'
#' @returns A cleaned and validated \link[base]{data.frame} prepared for use in
#'          LPANDA package functions.
#'
#' @importFrom utils head
#' @importFrom stats setNames
#'
#' @keywords internal
#' @noRd
#'
#' @examples
#' \donttest{
#' data(sample_diff_varnames, package = "lpanda")
#' df <- sample_diff_varnames
#' str(df)
#'
#' # checks data usability, changes variable names for analysis purposes,
#' # and adds new variables: unique 'list_id' and 'const_size' (calculated
#' # from 'elected') which should appear only once per election and constituency.
#'
#' input_variable_map <- list(list_name = "party", elected = "seat")
#' df_prepared <- prepare_input_data(df, input_variable_map)
#' str(df_prepared)
#'
#' # Dataset structure of an actual election results:
#' data(Jilove_DC_cz, package = "lpanda")
#' df2 <- Jilove_DC_cz
#' str(df2)
#'
#' # checks the usability of the data for analysis purposes, adds the unique
#' # variables 'list_id', 'list_votes' (calculated from 'pref_votes') and
#' # 'const_size' (calculated from 'elected'), and sets the correct variable
#' # types.
#'
#' df2_prepared <- prepare_input_data(df2);
#' str(df2_prepared);
#' }
#'
prepare_input_data <- function(df, input_variable_map = list()) {
  
  # ------------------------------------------------------------------------- #
  # ======================== Primarni uprava vstupu ===========================
  # ------------------------------------------------------------------------- #
  
  default_variables <- as.list(setNames(variables_meta$var_name,
                                        variables_meta$var_name));
  
  variables_class <- as.list(setNames(variables_meta$class,
                                      variables_meta$var_name));
  
  required_variables = variables_meta$var_name[variables_meta$required == TRUE
                                               & variables_meta$role == "input"];
  
  # ------------------------------------------------------------------------- #
  # ========= Kontrola, ze vstupni datova tabulka ('df') je dataframe =========
  # ------------------------------------------------------------------------- #
  
  if (!inherits(df, "data.frame")) {
    
    if (is.matrix(df)) {
      
      message("Input is a matrix. Attempting conversion to dataframe.")
      df <- as.data.frame(df);
      
      if (is.null(colnames(df)) || any(colnames(df) == "")) {
        
        stop("Matrix input had no valid column names. Cannot\nproceed with unnamed columns.");
      }
      
    } else {
      
      stop(paste("Input must be a dataframe or matrix. Unsupported\ninput type:", class(df)));
      
    } # konec IF-ELSE pro konverzi matice na datovou tabulku
    
  } else {
    
    if (is.null(colnames(df)) || any(colnames(df) == "")) {
      
      stop("The input 'df' has unnamed columns. Cannot proceed.");
    }
  } # konec IF pro kontrolu, ze vstupni tabulka je data.frame
  
  
  # ------------------------------------------------------------------------- #
  # ========= Kontrola, ze vstupni datova tabulka ('df') neni prazdna =========
  # ------------------------------------------------------------------------- #
  
  if (nrow(df) == 0) stop("Input 'df' does not contain any data.");
  
  
  # ------------------------------------------------------------------------- #
  # ===== Kontroly nazvu promennych a jejich uprava dle defaultnich nazvu =====
  # ------------------------------------------------------------------------- #
  
  invalid_tags <- setdiff(names(input_variable_map), names(default_variables));
  
  if (length(invalid_tags) > 0) {
    
    stop(paste("Invalid tags in 'input_variable_map' list:",
               paste(invalid_tags, collapse = ", ")))
  }; # konec IF kontrolujicich spravnost tagu
  
  # ------------------------------------------------------------------------- #
  
  merged_variables <- default_variables;
  
  for (tag in names(default_variables)) {
    
    user_val    <- input_variable_map[[tag]];
    default_val <- default_variables[[tag]];
    
    if (!is.null(user_val)) {
      
      if (user_val %in% names(df)) {
        
        merged_variables[[tag]] <- user_val;
        
      } else if (default_val %in% names(df)) {
        
        merged_variables[[tag]] <- default_val;
        warning(paste0("Column '", user_val, "' for variable '", tag,
                       "' was not found in the data. However, a column with ",
                       "the default name (", default_val, ") was found in ",
                       "the data and will be used."));
        
      } else {
        
        warning(paste0("Column '", user_val, "' for variable '", tag,
                       "' was not found, and default column '", default_val,
                       "' is also missing. The variable will be skipped."))
        
      } # konec IF-ELSE IF-ELSE pro vyhodnoceni zadaneho nazvu promenne
    } # konec IF pro pripad, ze by v zadani bylo neco v input_variable_map
  } # konec FOR loopu pro slouceni zadanych a defaultnich nazvu promennych
  
  # ------------------------------------------------------------------------- #
  
  missing_required <-
    required_variables[!merged_variables[required_variables] %in% names(df)];
  
  if (length(missing_required) > 0) {
    stop(paste("Missing required variables in the data:",
               paste(missing_required, collapse = ", ")))
  }; # konec IF pro kontrolu existence povinnych promennych
  
  for (sloupec in unlist(merged_variables[required_variables])) {
    
    missing_rows <- which(is.na(df[[sloupec]]) | df[[sloupec]] == "")
    
    if (length(missing_rows) > 0) {
      stop(paste0("Column '", sloupec,
                  "' contains empty or missing values in rows: ",
                  paste(utils::head(missing_rows, 5), collapse = ", "),
                  if (length(missing_rows) > 5) " ..." else ""))
    }; # konec IF pro stopku
  }; # konec FOR loopu pro kontrolu chybejicich dat v povinnych promennych
  
  
  # ------------------------------------------------------------------------- #
  
  real_variable_map <- merged_variables[sapply(merged_variables,
                                               function(x) x %in% names(df))];
  
  ignored_cols <- setdiff(names(df), unlist(real_variable_map))
  
  if (length(ignored_cols) > 0) {
    message("Note: The following columns from the original data were ignored: ",
            paste(ignored_cols, collapse = ", "))
  } # konec IF pro upozorneni, ze nektere sloupce byly vynechany
  
  df <- df[, unlist(real_variable_map), drop = FALSE];
  colnames(df) <- names(real_variable_map);
  
  
  # ------------------------------------------------------------------------- #
  # ======================= Kontrola pouzitelnosti dat =======================
  # ------------------------------------------------------------------------- #
  
  for (sloupec in names(df)) {
    
    if (variables_class[[sloupec]] == "character") {
      
      df[[sloupec]] <- trimws(df[[sloupec]]);
      df[[sloupec]] <- gsub("\u00A0", " ", as.character(df[[sloupec]]), fixed = TRUE);
      df[[sloupec]] <- as.character(df[[sloupec]]);
      
      if (sloupec == 'candidate') {
        
        election_candidate <- paste(df$elections, df$candidate, sep = "_");
        duplicitni <- duplicated(election_candidate) | duplicated(election_candidate,
                                                                  fromLast = TRUE)
        
        duplicitni_jmena <- df[duplicitni,];
        duplicitni_jmena <- unique(paste(duplicitni_jmena$elections,
                                         duplicitni_jmena$candidate, sep = ": "));
        
        if (length(duplicitni_jmena) > 0) {
          stop(paste0("Duplicate candidate names found in the same elections:\n",
                      paste("-", duplicitni_jmena, collapse = "\n")));
        } # konec IF pro stopku v pripade nalezeni duplicitnich jmen
      } # konec IF pro kontrolu duplicitnich jmen ve stejnych volbach
    }; # konec IF pro character
    
    
    if (variables_class[[sloupec]] == "numeric") {
      
      df[[sloupec]] <- trimws(df[[sloupec]]);
      df[[sloupec]] <- gsub("\u00A0", "", as.character(df[[sloupec]]), fixed = TRUE);
      
      if (sloupec == "elections") {
        
        suppressWarnings(election_num_values <- as.numeric(df$elections));
        invalid_values <- unique(df$elections[is.na(election_num_values) &
                                                !is.na(df$elections)]);
        
        if (length(invalid_values) > 0) {
          stop(paste0(
            "The 'elections' format must be numeric (e.g., 2024 or 2024.11).\n",
            "The following values could not be converted: ",
            paste(invalid_values, collapse = ", ")));
        } # konec IF pro stopku
      } # konec IF pro kontrolu pripadne chyby v oznaceni voleb
      
      df[[sloupec]] <- as.numeric(df[[sloupec]])
      
    }; # konec IF pro numeric
    
    
    if (variables_class[[sloupec]] == "logical") {
      
      puv.hodnoty <- df[[sloupec]];
      puv.hodnoty <- toupper(trimws(puv.hodnoty));
      puv.hodnoty <- gsub("\u00A0", "", as.character(puv.hodnoty), fixed = TRUE);
      
      true_vals <- c("TRUE", "T", "1");
      false_vals <- c("FALSE", "F", "0");
      
      upr.hodnoty <- rep(NA, length(puv.hodnoty));
      upr.hodnoty[puv.hodnoty %in% true_vals] <- TRUE;
      upr.hodnoty[puv.hodnoty %in% false_vals] <- FALSE;
      
      nerozpoznane <- is.na(upr.hodnoty) & !is.na(df[[sloupec]]);
      
      if (any(nerozpoznane)) {
        warning("Some values in the column '", sloupec,
                "' could not be recognized as logical (TRUE/FALSE/1/0), e.g.: ",
                paste(utils::head(unique(df[[sloupec]][nerozpoznane]), n = 5),
                      collapse = ", "));
      } # konec IF pro oznameni nerozpoznanych dat v logical sloupci
      
      df[[sloupec]] <- as.logical(upr.hodnoty);
      
    }; # konec IF pro logical
  }; # konec FOR loopu pro odstraneni pevnych mezer a nastaveni jednotnych class
  
  
  # ------------------------------------------------------------------------- #
  # ============= Serazeni promennych a pridani 'list_id' do 'df' =============
  # ------------------------------------------------------------------------- #
  
  included_variables <- names(df);
  
  list_id <- paste0(df$list_name, " (", df$elections, ")");
  
  if (!'list_id' %in% included_variables) {
    included_variables <- append(included_variables, "list_id",
                                 match("list_name", included_variables));
    df$list_id <- list_id;
    
  } else {
    
    if (!all(df$list_id == list_id)) df$list_id <- list_id;
    
  } # konec IF-ELSE pro pridani list_id mezi promenne (pokud jiz neexistuje)
  
  df <- df[included_variables];
  
  # ------------------------------------------------------------------------- #
  # ================== Kontrola hlasu pro kandidatni listiny ==================
  # ------------------------------------------------------------------------- #
  
  if (!'list_id' %in% names(df)) df$list_id <- paste0(df$list_name, " (",
                                                      df$elections, ")");
  
  listiny <- unique(df$list_id);
  
  if ((!"list_votes" %in% names(df)) | suppressWarnings(all(is.na(df$list_votes)))) {
    
    if ("pref_votes" %in% names(df) & suppressWarnings(all(!is.na(df$pref_votes)))) {
      
      if (!"list_votes" %in% names(df)) df$list_votes <- NA_integer_;
      
      for (i in seq_len(length(listiny))) {
        filtr <- df$list_id == listiny[i]
        df$list_votes[which(filtr)[1]] <- sum(df$pref_votes[filtr])
      } # konec FOR loopu pro secteni pref_votes a vlozeni do list_votes
      
      included_variables <- append(included_variables, "list_votes",
                                   match("pref_votes", included_variables));
      df <- df[included_variables];
      
      message("Missing 'list_votes' values completed using 'pref_votes'.")
      
    } else if ("list_votes" %in% names(df)) {
      
      warning("Column 'list_votes' is entirely NA and will be removed.")
      df$list_votes <- NULL;
      
    } # konec IF-ELSE pro preklopeni pref_votes do list_votes, pokud byly
    # prazdne, nebo vymazani prazdneho list_votes, pokud chybi pref_votes
    
  } else {
    
    has_votes <- tapply(df$list_votes, df$list_id, function(x) any(!is.na(x)))
    without_votes <- names(has_votes)[!has_votes];
    
    # pokud hlasy chybi...
    if (length(without_votes) > 0) {
      
      warning("Some 'list_votes' values are missing, column will not be used.")
      df$list_votes <- NULL
      
    } # konec IF pro odstraneni nekompletniho sloupce list_votes
  } # konec IF-ELSE pro kontrolu a doplneni hlasu pro kandidatni listiny
  
  
  # ------------------------------------------------------------------------- #
  # ========== Kontrola elig_voters, ballot_cast a const_size ==========
  # ------------------------------------------------------------------------- #
  
  roky_voleb <- unique(df$elections);
  
  for (sloupec in c("elig_voters", "ballots_cast")) {
    if (sloupec %in% names(df)) {
      chybi_hodnota <- c()
      
      for (rok in roky_voleb) {
        filtr <- df$elections == rok
        if (all(is.na(df[filtr, sloupec]))) {
          chybi_hodnota <- c(chybi_hodnota, rok)
        } # konec IF
      } # konec FOR loopu pro hledani chybejicich hodnot
      
      if (length(chybi_hodnota) > 0) {
        warning(paste0("Column '", sloupec,
                       "' is incomplete and will be removed. Missing value for elections: ",
                       paste(chybi_hodnota, collapse = ", ")), ".")
        df[[sloupec]] <- NULL
      } # konec IF pro odstraneni sloupce, pokud chybi nejaka hodnota
    } # konec IF pro existujici sloupec
  } # konec FOR loopu prochazejici zadane sloupce
  
  # --- #
  
  if ("elected" %in% names(df) && any(!is.na(df$elected))) {
    
    if (!"const_size" %in% names(df)) {
      df$const_size <- NA_integer_
      message("Column 'const_size' was missing and will be created based on 'elected'.")
    } # konec IF
    
    roky_voleb <- unique(df$elections);
    doplnene_roky <- c();
    
    for (rok in roky_voleb) {
      
      if (all(is.na(df$const_size[df$elections == rok]))) {
        
        filtr <- df$elections == rok;
        total_elected <- sum(as.numeric(df$elected)[filtr], na.rm = TRUE);
        
        if (total_elected > 0) {
          
          df$const_size[which(filtr)[1]] <- total_elected;
          doplnene_roky <- c(doplnene_roky, rok);
          
        } # konec IF
      } # konec IF pro doplneni chybejici hodnoty const_size
    } # konec FOR loopu pro hledani voleb, kde chybi velikost obvodu
    
    if (length(doplnene_roky) > 0) {
      message(paste0("const_size was calculated from 'elected' for elections: ",
                     paste(doplnene_roky, collapse = ", "), "."));
    } # konec IF
  } # konec IF zjistujici, jestli existuje 'elected' sloupec
  
  # ------------------------------------------------------------------------- #
  # ================================= OUTPUT ==================================
  # ------------------------------------------------------------------------- #
  
  return(df);
  
} # konec funkce prepare_input_data()
