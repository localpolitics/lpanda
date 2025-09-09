# Script for generating multiple fictitious datasets used for demo/testing purposes

# 1. Basic sample data
sample_data <- tibble::tribble(

  ~elections, ~candidate, ~list_name, ~elected, ~mayor,
      14,        "c01",      "A",      TRUE,    TRUE,
      14,        "c02",      "A",      TRUE,    FALSE,
      14,        "c03",      "A",      TRUE,    FALSE,
      18,        "c01",      "A",      TRUE,    FALSE,
      18,        "c02",      "A",      FALSE,   FALSE,
      18,        "c03",      "B",      TRUE,    TRUE,
      18,        "c04",      "A",      FALSE,   FALSE,
      18,        "c05",      "B",      FALSE,   FALSE,
      18,        "c06",      "B",      FALSE,   FALSE,
      18,        "c07",      "C",      TRUE,    FALSE,
      18,        "c08",      "C",      FALSE,   FALSE,
      18,        "c09",      "C",      FALSE,   FALSE,
      22,        "c01",      "A",      FALSE,   FALSE,
      22,        "c03",      "D",      TRUE,    TRUE,
      22,        "c04",      "A",      TRUE,    FALSE,
      22,        "c05",      "D",      FALSE,   FALSE,
      22,        "c07",      "D",      TRUE,    FALSE,
      22,        "c10",      "A",      FALSE,   FALSE

);

# --------------------------------------------------------------------------- #

# 2. Same data, binary logical values
sample_binary_values <- sample_data;
sample_binary_values[,c("elected", "mayor")] <- ifelse(
  sample_binary_values[,c("elected", "mayor")] == TRUE, 1, 0);

# --------------------------------------------------------------------------- #

# 3. Same data, different column names
sample_different_varnames <- sample_data;
names(sample_different_varnames) <- c("elections", "candidate", "party", "seat", "mayor");

# --------------------------------------------------------------------------- #

# 4. Sample with no party switching
sample_no_switching <- tibble::tribble(

  ~elections, ~candidate, ~list_name, ~elected, ~mayor,
  14,        "c01",        "A",      TRUE,    TRUE,
  14,        "c02",        "A",      TRUE,    FALSE,
  14,        "c03",        "A",      TRUE,    FALSE,
  18,        "c01",        "A",      TRUE,    FALSE,
  18,        "c02",        "A",      FALSE,   FALSE,
  18,        "c04",        "A",      FALSE,   FALSE,
  18,        "c05",        "B",      TRUE,    TRUE,
  18,        "c06",        "B",      FALSE,   FALSE,
  18,        "c07",        "B",      FALSE,   FALSE,
  18,        "c08",        "C",      TRUE,    FALSE,
  18,        "c09",        "C",      FALSE,   FALSE,
  18,        "c10",        "C",      FALSE,   FALSE,
  22,        "c05",        "B",      TRUE,    TRUE,
  22,        "c06",        "B",      TRUE,    FALSE,
  22,        "c11",        "B",      TRUE,    FALSE,

);

# --------------------------------------------------------------------------- #

# 5. Sample with no continuity
sample_no_continuity <- tibble::tribble(

  ~elections, ~candidate, ~list_name, ~elected, ~mayor,
  14,        "c01",        "A",      TRUE,    TRUE,
  14,        "c02",        "A",      TRUE,    FALSE,
  14,        "c03",        "A",      TRUE,    FALSE,
  18,        "c04",        "A",      TRUE,    FALSE,
  18,        "c05",        "A",      FALSE,   FALSE,
  18,        "c06",        "A",      FALSE,   FALSE,
  18,        "c07",        "B",      TRUE,    TRUE,
  18,        "c08",        "B",      FALSE,   FALSE,
  18,        "c09",        "B",      FALSE,   FALSE,
  18,        "c10",        "C",      TRUE,    FALSE,
  18,        "c11",        "C",      FALSE,   FALSE,
  18,        "c12",        "C",      FALSE,   FALSE,
  22,        "c13",        "B",      TRUE,    TRUE,
  22,        "c14",        "B",      TRUE,    FALSE,
  22,        "c15",        "B",      TRUE,    FALSE

);

# --------------------------------------------------------------------------- #

# 6. Sample with no pluralism
sample_no_pluralism <- tibble::tribble(
  
  ~elections, ~candidate, ~list_name, ~elected, ~mayor,
  14,        "c01",        "A",      TRUE,    TRUE,
  14,        "c02",        "A",      TRUE,    FALSE,
  14,        "c03",        "A",      TRUE,    FALSE,
  18,        "c01",        "B",      TRUE,    FALSE,
  18,        "c03",        "B",      TRUE,    TRUE,
  18,        "c04",        "B",      TRUE,    FALSE,
  22,        "c03",        "C",      TRUE,    TRUE,
  22,        "c04",        "C",      TRUE,    FALSE,
  22,        "c05",        "C",      TRUE,    FALSE
  
);

# --------------------------------------------------------------------------- #

# Save all datasets into package /data/ directory
usethis::use_data(
  sample_data,
  sample_binary_values,
  sample_different_varnames,
  sample_no_switching,
  sample_no_continuity,
  sample_no_pluralism,
  overwrite = TRUE)
