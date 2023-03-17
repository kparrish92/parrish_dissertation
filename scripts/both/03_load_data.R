source(here::here("scripts", "both", "00_libs.R"))
source(here::here("scripts", "both", "01_helpers.R"))

# Tidy combined df 
comb_df = read.csv(here("data", "both", "combined.csv"))

# centroid dfs for plots 
centroid_df_eng = read.csv(here("data", "both", "centroid_df_eng.csv")) 
centroid_df_span = read.csv(here("data", "both", "centroid_df_span.csv"))


did_both = comb_df %>% 
  group_by(prolific_id, L1, mode) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = mode, values_from = n) %>% 
  filter(!is.na(perception)) %>% 
  filter(!is.na(production))

## Groups who did both
nrow(did_both) 
nrow(did_both %>% filter(L1 == "English"))
nrow(did_both %>% filter(L1 == "Spanish"))

bio_both = read.csv(here("data", "tidy", "full_data_wbio.csv")) %>% 
  filter(prolific_id %in% did_both$prolific_id)

comb_df_filt = comb_df %>% 
  filter(prolific_id %in% did_both$prolific_id)





