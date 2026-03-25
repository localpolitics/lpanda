# Script for generating and saving municipal datasets from CSV files

# List of municipalities (without .csv extension)
municipalities <- c("Bublava_SO_cz",
                    "Cernosice_PZ_cz",
                    "Dasnice_SO_cz",
                    "Doubice_DC_cz",
                    "Horomerice_PZ_cz",
                    "Hradce_CB_cz",
                    "Jilove_DC_cz",
                    "Kamenna_CB_cz",
                    "Nebanice_CH_cz",
                    "Potucky_KV_cz",
                    "Prameny_CH_cz",
                    "Roztoky_PZ_cz",
                    "Ustek_LT_cz");

for (m in municipalities) {
  adresa.csv <- file.path("data-raw", paste0(m, ".csv"));
  adresa.rda <- file.path("data", paste0(m, ".rda"));

  if (file.exists(adresa.csv)) {
    
    if (file.exists(adresa.rda)) {
      
      csv.time <- file.mtime(adresa.csv);
      rda.time <- file.mtime(adresa.rda);
      
      if (rda.time >= csv.time) {
        message("Up-to-date, skipping: ", m);
        next
      }
    }; # konec IF pro preskoceni datasetu, ktery jiz existuje a je aktualni
    
    message("Loading data for: ", m);

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
