# --------------------------------------------------------------------------- #
# Testy pro should_use_ascii() a convert_utf8_to_ascii (interni funkce)
# --------------------------------------------------------------------------- #

test_that("should_use_ascii respects explicit modes", {
  expect_false(should_use_ascii("utf8"));
  expect_true(should_use_ascii("ascii"));
})

test_that("should_use_ascii(auto) flips in CI env", {
  withr::with_envvar(c(CI = "true"), {
    expect_true(should_use_ascii("auto"));
  });
})

test_that("convert_utf8_to_ascii produces printable ASCII and keeps vector length", {
  
  x <- c("\u010C\u0160\u0158\u017D\u00DD\u00C1\u00C9\u011A\u016E\u00DC",
         "\u010CSSD",
         "\u00D1\u00E7\u015B",
         "plain",
         "",
         NA,
         NA_integer_);
  
  y <- convert_utf8_to_ascii(x);
  
  expect_length(y, length(x));
  expect_type(y, "character")
  expect_true(all(grepl("^[ -~]*$", y[!is.na(y)])));
  expect_identical(y[4], "plain");
  expect_true(is.na(y[5]));
  expect_true(is.na(y[6]));
  expect_true(is.na(y[7]));
})
