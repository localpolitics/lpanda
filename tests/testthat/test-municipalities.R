# Test municipality data: Jilove_DC_cz

test_that("Jilove_DC_cz data is a valid election dataset", {

  expect_s3_class(Jilove_DC_cz, "data.frame")

  expected_cols <- c("elections", "candidate", "list_name", "list_pos",
                     "pref_votes", "elected", "nom_party", "pol_affil",
                     "mayor", "dep_mayor", "board", "gov_support",
                     "elig_voters", "ballots_cast")

  expect_named(Jilove_DC_cz, expected_cols, ignore.order = TRUE)
  expect_true(nrow(Jilove_DC_cz) > 0)

  expect_type(Jilove_DC_cz$elections,    "integer")
  expect_type(Jilove_DC_cz$candidate,    "character")
  expect_type(Jilove_DC_cz$list_name,    "character")
  expect_type(Jilove_DC_cz$list_pos,     "integer")
  expect_type(Jilove_DC_cz$pref_votes,   "integer")
  expect_type(Jilove_DC_cz$elected,      "integer")
  expect_type(Jilove_DC_cz$nom_party,    "character")
  expect_type(Jilove_DC_cz$pol_affil,    "character")
  expect_type(Jilove_DC_cz$mayor,        "integer")
  expect_type(Jilove_DC_cz$dep_mayor,    "integer")
  expect_type(Jilove_DC_cz$board,        "integer")
  expect_type(Jilove_DC_cz$gov_support,  "integer")
  expect_type(Jilove_DC_cz$elig_voters,  "integer")
  expect_type(Jilove_DC_cz$ballots_cast, "integer")

}) # konce testu pro Jilove_DC_cz
