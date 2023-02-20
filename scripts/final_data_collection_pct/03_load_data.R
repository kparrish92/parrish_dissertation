# Load data 
#
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "perception", "00_libs.R"))

# -----------------------------------------------------------------------------

pct_fr = read.csv(here("scripts", "final_data_collection_pct", 
                        "data", "tidy",
                        "pct_tidy_french.csv"))

pct_ger = read.csv(here("scripts", "final_data_collection_pct", 
                        "data", "tidy",
                        "pct_tidy_german.csv"))

resp_fr = read.csv(here("scripts", "final_data_collection_pct", 
                        "data", "tidy",
                        "resp_df.fren.csv"))

resp_ger = read.csv(here("scripts", "final_data_collection_pct", 
                         "data", "tidy",
                         "resp_df.german.csv"))


