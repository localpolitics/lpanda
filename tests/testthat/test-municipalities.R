# Municipality datasets tests

test_that("municipality datasets are valid election datasets", {
  
  municipalities_list <- list(Bublava_SO_cz = Bublava_SO_cz,
                              Cernosice_PZ_cz = Cernosice_PZ_cz,
                              Dasnice_SO_cz = Dasnice_SO_cz,
                              Doubice_DC_cz = Doubice_DC_cz,
                              Horomerice_PZ_cz = Horomerice_PZ_cz,
                              Hradce_CB_cz  = Hradce_CB_cz,
                              Jilove_DC_cz  = Jilove_DC_cz,
                              Kamenna_CB_cz = Kamenna_CB_cz,
                              Nebanice_CH_cz = Nebanice_CH_cz,
                              Potucky_KV_cz = Potucky_KV_cz,
                              Prameny_CH_cz = Prameny_CH_cz,
                              Roztoky_PZ_cz = Roztoky_PZ_cz,
                              Ustek_LT_cz   = Ustek_LT_cz);
  
  expected_cols <- c("elections", "candidate", "list_name", "list_pos",
                     "pref_votes", "elected", "nom_party", "pol_affil",
                     "mayor", "dep_mayor", "board", "gov_support",
                     "elig_voters", "ballots_cast")
  
  for (municipality in municipalities_list) {
    
    expect_s3_class(municipality, "data.frame");
    
    expect_named(municipality, expected_cols, ignore.order = TRUE)
    expect_true(nrow(municipality) > 0)
    
    expect_true(is.numeric(municipality$elections)) # type integer nefunguje kvuli napr. 2022.11 (double) a naopak
    expect_type(municipality$candidate,    "character")
    expect_type(municipality$list_name,    "character")
    expect_type(municipality$list_pos,     "integer")
    expect_type(municipality$pref_votes,   "integer")
    expect_type(municipality$elected,      "integer")
    expect_type(municipality$nom_party,    "character")
    expect_type(municipality$pol_affil,    "character")
    expect_type(municipality$mayor,        "integer")
    expect_type(municipality$dep_mayor,    "integer")
    expect_type(municipality$board,        "integer")
    expect_type(municipality$gov_support,  "integer")
    expect_type(municipality$elig_voters,  "integer")
    expect_type(municipality$ballots_cast, "integer")
    
  } # konec FOR loopu
  
}) # konce testu pro municipalities
