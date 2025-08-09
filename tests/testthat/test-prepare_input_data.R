test_that("matrix is transformed to data.frame", {
  
  df <- lpanda::sample_data;
  df <- as.matrix(df);
  expect_true(is.matrix(df));
  
  df_fixed <- suppressMessages(suppressWarnings(prepare_input_data(df)));
  expect_true(is.data.frame(df_fixed));
  
})

test_that("you get error with empty data.frame", {
  
  df <- lpanda::sample_data;
  df <- df[NULL,];
  
  expect_error(prepare_input_data(df));
  
})

test_that("it will stop with mistakes in tags", {
  
  df <- lpanda::sample_diff_varnames;
  
  # list_names instead of list_name
  expect_error(suppressMessages(suppressWarnings(prepare_input_data(df,
                                                                    list(list_names = "party", elected = "seat")))));
  
  # here it should be correct
  expect_no_error(suppressMessages(suppressWarnings(prepare_input_data(df,
                                                                       list(list_name = "party", elected = "seat")))));
  
})

test_that("it will stop if any required variable is missing", {
  
  df <- lpanda::sample_data;
  
  df_without_elections <- df[,!(names(df) %in% "elections")];
  expect_error(prepare_input_data(df_without_elections));
  
  df_without_candidates <- df[,!(names(df) %in% "candidate")];
  expect_error(prepare_input_data(df_without_candidates));
  
  df_without_list_name <- df[,!(names(df) %in% "list_name")];
  expect_error(prepare_input_data(df_without_list_name));
})


test_that("it will stop if any required variable contains NA or unexpected type", {
  
  df_with_na <- function(df, column_name) {
    df[sample(nrow(df), 1), column_name] <- NA
    return(df)
  }
  
  df <- lpanda::sample_data;
  
  expect_error(suppressMessages(suppressWarnings(prepare_input_data(df_with_na(df, "elections")))));
  expect_error(suppressMessages(suppressWarnings(prepare_input_data(df_with_na(df, "candidate")))));
  expect_error(suppressMessages(suppressWarnings(prepare_input_data(df_with_na(df, "list_name")))));
  expect_no_error(suppressMessages(suppressWarnings(prepare_input_data(df_with_na(df, "elected")))));
  
})

test_that("it will stop if 'elections' variable contains unexpected type", {
  
  df <- lpanda::sample_data;
  
  # error:
  df$elections[3] <- "14-Nov";
  expect_error(suppressMessages(suppressWarnings(prepare_input_data(df))));
  
  # no error:
  df$elections[3] <- "14.11";
  expect_no_error(suppressMessages(suppressWarnings(prepare_input_data(df))));
  
})

test_that("no extra or non-breaking spaces in character columns", {
  
  df <- lpanda::sample_data;
  
  df$elections[sample(nrow(df),1)] <- "  1787  ";
  df$candidate[sample(nrow(df),1)] <- "  Capo di\u00A0Tutti Capi   ";
  df$list_name[sample(nrow(df),1)] <- "  Capi\u00A0Locale ";
  df$elected[sample(nrow(df),1)]   <- "  TRUE ";
  df$mayor[sample(nrow(df),1)]     <- "  1 ";
  
  df_clean <- suppressMessages(suppressWarnings(prepare_input_data(df)));
  col_text <- unlist(as.vector(df_clean));
  expect_false(any(grepl("\u00A0", col_text)))
  expect_false(any(grepl("  +", col_text)))
  expect_true(all(col_text == trimws(col_text), na.rm = TRUE))
})


test_that("There are no duplicate candidate names found in the same elections", {
  
  df <- lpanda::sample_data;
  
  selected_candidate <- sample(unique(
    df$candidate[duplicated(df$candidate)
                 | duplicated(df$candidate, fromLast = TRUE)]), 1);
  
  df$elections[df$candidate == selected_candidate][1] <-
    df$elections[df$candidate == selected_candidate][2];
  
  expect_error(prepare_input_data(df));
  
})

test_that("it changes variables to correct data class", {
  
  df <- Jilove_DC_cz;
  df_prepared <- suppressMessages(suppressWarnings(prepare_input_data(df)));
  
  for (sloupec in names(df_prepared)) {
    expect_equal(class(df_prepared[,sloupec]),
                 variables_meta$class[variables_meta$var_name == sloupec])
  }
})

test_that("it changes variability names, add 'list_id' and 'const_size'", {
  
  df <- lpanda::sample_diff_varnames;
  df_fixed <- suppressMessages(suppressWarnings(prepare_input_data(df,
                                                                   list(list_name = "party",
                                                                        elected = "seat"))));
  
  expect_equal(names(df_fixed),
               c("elections", "candidate", "list_name", "list_id",
                 "elected", "mayor", "const_size"))
  
  expect_equal(df_fixed$list_id,
               paste0(df$party, " (", df$elections, ")"));
  
  expect_equal(sum(df_fixed$const_size, na.rm = TRUE),
               sum(df_fixed$elected))
  
})

test_that("if a 'list_votes' exist, it appears only once per list", {
  
  df <- suppressMessages(suppressWarnings(prepare_input_data(Jilove_DC_cz)));
  
  if ('list_votes' %in% names(df)) {
    
    n_unique <- tapply(df$list_votes, df$list_id,
                       function(x) length(unique(na.omit(x))));
    expect_true(all(n_unique == 1))
    
  } # konec IF zjistujici, ze existuje 'list_votes'
  
})

test_that("if 'elig_voters' and 'ballot_cast' exist, they appears only once
          per elections", {
            
            df <- suppressMessages(suppressWarnings(prepare_input_data(Jilove_DC_cz)));
            
            for (variable in c('elig_voters', 'ballot_cast')) {
              
              if (variable %in% names(df)) {
                
                n_unique <- tapply(df[variable], df$elections,
                                   function(x) length(unique(na.omit(x))));
                expect_true(all(n_unique == 1))
                
              } # konec IF existuje dana promenna
            } # konec FOR loopu
})
