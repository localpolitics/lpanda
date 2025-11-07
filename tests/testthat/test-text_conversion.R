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

test_that("convert_utf8_to_ascii transliterates and preserves length", {
  
  x <- c("\u010C\u0160\u0158\u017D\u00DD\u00C1\u00C9\u011A\u016E\u00DC",
         "\u010CSSD",
         "\u00D1\u00E7\u015B",
         "plain",
         NA);
  
  y <- convert_utf8_to_ascii(x);
  expect_length(y, length(x));
  expect_true(all(grepl("^[ -~]*$", y[!is.na(y)])));
  
  clean <- function(z) tolower(gsub("[^A-Za-z0-9]+", "", z));
  
  expect_identical(clean(y[1]), "csrzyaeeuu");
  expect_identical(clean(y[2]), "cssd");
  expect_identical(clean(y[3]), "ncs");
  expect_identical(y[4], "plain");
  expect_true(is.na(y[5]));
})
