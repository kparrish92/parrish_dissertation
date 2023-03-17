source(here::here("scripts", "both", "00_libs.R"))
source(here::here("scripts", "both", "01_helpers.R"))
source(here::here("scripts", "both", "03_load_data.R"))


model_span <- brm(formula = language_chosen ~ phoneme*mode + (1 | prolific_id),  
                  data=comb_df_filt %>% filter(L1 == "Spanish"), 
                  family = bernoulli(link = "logit"),
                  file = here("data", "both", "models", "span_both.rds"))

model_eng <- brm(formula = language_chosen ~ phoneme*mode + (1 | prolific_id),  
                 data=comb_df_filt %>% filter(L1 == "English"), 
                 family = bernoulli(link = "logit"),
                 file = here("data", "both", "models", "span_eng.rds"))
