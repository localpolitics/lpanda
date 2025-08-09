# --------------------------------------------------------------------------- #
# Tabulka s internimi promennymi
# --------------------------------------------------------------------------- #

variables_meta <- tibble::tribble(

  ~var_name,   ~required, ~role,   ~class,      ~bp.E.attr,
  "elections",    TRUE,   "input",  "numeric",   TRUE,
  "candidate",    TRUE,   "input",  "character", FALSE,
  "list_name",    TRUE,   "input",  "character", FALSE,
  "list_id",      TRUE,   "output", "character", FALSE,
  "list_pos",     FALSE,  "input",  "numeric",   TRUE,
  "pref_votes",   FALSE,  "input",  "numeric",   TRUE,
  "list_votes",   FALSE,  "input",  "numeric",   FALSE,
  "elected",      FALSE,  "input",  "logical",   TRUE,
  "nom_party",    FALSE,  "input",  "character", TRUE,
  "pol_affil",    FALSE,  "input",  "character", TRUE,
  "mayor",        FALSE,  "input",  "logical",   TRUE,
  "dep_mayor",    FALSE,  "input",  "logical",   TRUE,
  "board",        FALSE,  "input",  "logical",   TRUE,
  "gov_support",  FALSE,  "input",  "logical",   TRUE,
  "elig_voters",  FALSE,  "input",   "numeric",  FALSE,
  "ballots_cast", FALSE,  "input",   "numeric",  FALSE,
  "const_size",   FALSE,  "input",   "numeric",  FALSE

  );


# --------------------------------------------------------------------------- #
# Ulozeni internich datovych objektu
# --------------------------------------------------------------------------- #

usethis::use_data(variables_meta, internal = TRUE, overwrite = TRUE)
