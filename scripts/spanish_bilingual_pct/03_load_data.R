# Load data 
#
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "perception", "00_libs.R"))

# -----------------------------------------------------------------------------

pct_fr = read.csv(here("scripts", "spanish_bilingual_pct", 
                        "data", "tidy",
                        "pct_tidy_french_sp.csv"))

pct_ger = read.csv(here("scripts", "spanish_bilingual_pct", 
                        "data", "tidy",
                        "pct_tidy_german_sp.csv"))


all_pct = read.csv(here("scripts", "spanish_bilingual_pct", 
                 "data", "tidy", "all_pct.tidy_sp.csv"))
  