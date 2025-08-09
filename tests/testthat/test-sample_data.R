# Test for basic sample_data =====

test_that("sample_data has expected structure and content", {

  expect_s3_class(sample_data, "data.frame")
  expect_named(sample_data, c("elections", "candidate", "list_name", "elected",
                              "mayor"))
  expect_equal(ncol(sample_data), 5)
  expect_equal(nrow(sample_data), 18)
  expect_true(all(sample_data$elected %in% c(TRUE, FALSE)))
  expect_true(is.logical(sample_data$mayor))

})

# --------------------------------------------------------------------------- #

# Test for sample_binary_values =====

test_that("sample_binary_values contains binary values in elected and mayor", {

  expect_true(all(sample_binary_values$elected %in% c(0,1)));
  expect_true(all(sample_binary_values$mayor %in% c(0,1)));

})

# --------------------------------------------------------------------------- #

# Test for sample_diff_varnames =====

test_that("sample_diff_varnames contains same columns but with different names", {

  expect_false(all(names(sample_diff_varnames) == names(sample_data)));

  # Same number of rows
  expect_equal(nrow(sample_diff_varnames), nrow(sample_data));

  # same values
  for (i in 1:ncol(sample_diff_varnames)) {
    expect_equal(sample_diff_varnames[[i]], sample_data[[i]]);
  }
})

# --------------------------------------------------------------------------- #

# Test for sample_no_crossing =====

test_that("sample_no_crossing doesn't have a candidate that changed group", {

  df <- sample_no_crossing;
  df$list_id <- paste0(df$list_name, " (", df$elections, ")");

  elections <- sort(unique(df$elections));

  for (i in 2:length(elections)) {

    # vytvoreni incidence matice
    tab <- table(df[df$elections %in% elections[c(i-1,i)],
                    c("candidate", "list_id")]);

    # projekce a dichotomizace
    inci <- t(tab) %*% tab; # list_id one-mode projekce
    inci[lower.tri(inci)] <- 0; # vymaze se spodni cast
    diag(inci) <- 0; # vymaze se diagonala
    inci[inci >0] <- 1; # dichotomizace

    # IN/OUT degree by melo byt maximalne 1 po dichotomizaci
    expect_true(all(c(colSums(inci) <= 1, rowSums(inci) <= 1)))
  } # konec FOR loopu pro dve po sobe jdouci volby
})

# --------------------------------------------------------------------------- #

# Test for sample_no_continuity =====

test_that("No candidate in sample_no_continuity runs in more than one election", {

  df <- sample_no_continuity;

  candidate_counts <- table(df$candidate)
  repeated_candidacy <- names(candidate_counts)[candidate_counts > 1]

  expect_length(repeated_candidacy, 0)
})

# --------------------------------------------------------------------------- #

# Tests for all sample files =====

test_that("sample datasets are consistent in data class", {

  for (df in list(sample_data, sample_binary_values, sample_diff_varnames,
                  sample_no_crossing, sample_no_continuity)) {
    expect_s3_class(df,  "data.frame")
    expect_type(df[[1]], "double") # type = "double"; class = "numeric"
    expect_type(df[[2]], "character")
    expect_type(df[[3]], "character")
    expect_type(as.logical(df[[4]]), "logical")
    expect_type(as.logical(df[[5]]), "logical")
  }
})
