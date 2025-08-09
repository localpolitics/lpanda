# Script for generating and saving municipal datasets from CSV files

# List of municipalities (without .csv extension)
municipalities <- c("Doubice_DC_cz",
                    "Jilove_DC_cz");

for (m in municipalities) {
  adresa.csv <- file.path("data-raw", paste0(m, ".csv"));
  adresa.rda <- file.path("data", paste0(m, ".rda"));

  if (file.exists(adresa.csv)) {
    message("Loading data for: ", m)

    df <- read.csv(adresa.csv, header = TRUE, sep = ";");

    # je treba pouzit assign(), jelikoz jinak by se to ulozilo jako df.rda
    # a ne pod jmenem dane obce
    assign(m, df)

    # usethis::use_data() pro tenhle FOR loop nefunguje
    # list je argument typu 'character' a ne list() (!!!!!!)
    save(list = m, file = adresa.rda);
    message("Saved to: ", adresa.rda);

  } else {

    warning("File not found: ", adresa.csv)

  } # konec IF-ELSE pro zjisteni existence CSV souboru
} # konec FOR loopu pro vytvoreni datasetu obci
