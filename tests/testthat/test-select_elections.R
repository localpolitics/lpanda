# --------------------------------------------------------------------------- #
# Testy pro select_elections() (interni funkce)
# --------------------------------------------------------------------------- #

test_that("select_elections works with range filters", {
  x <- c(1994.1030, 1998.11, 2002.11, 2006, 2010.10, 2014, 2018.10, 2022)
  
  expect_equal(
    select_elections(x, "2002-"),
    x[x >= 2002]
  )
  
  expect_equal(
    select_elections(x, "-1998"),
    x[x <= 1998]
  )
  
  expect_equal(
    select_elections(x, "2002-2014"),
    x[x >= 2002 & x <= 2014]
  )
})

test_that("it works with atomic vectors and elections in the same year", {
  x <- c(1994.10, 1998.11, 2002.02, 2002.11, 2006, 2010.10, 2014, 2018.10, 2022)
  expect_equal(
    select_elections(x, c(2002.02, 2002.11, 2006)),
    x[x %in% c(2002.02, 2002.11, 2006)]
  )
})

test_that("it returns all if filter is NULL", {
  x <- c(1994.1030, 1998.11, 2002.11)
  expect_equal(select_elections(x, NULL), x)
})

test_that("it throws error on invalid string format", {
  x <- c(1994, 1998, 2002)
  expect_error(suppressWarnings(select_elections(x, "1994/2002")))
  expect_error(suppressWarnings(select_elections(x, "komunalni volby 2010")))
})
