# Load data 
#
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "final_data_collection_pct", "00_libs.R"))
source(here::here("scripts", "final_data_collection_pct", "03_load_data.R"))

# -----------------------------------------------------------------------------

pct_fr = pct_fr %>% 
  rename(rating = slider_2.response) %>% 
  rename(stim_language = language)

pct_ger = pct_ger %>% 
  rename(rating = slider.response)

all_pct = rbind(pct_fr, pct_ger) %>% 
  separate(resp, into = c("word", "language")) %>% 
  mutate(language_num = case_when(
    language == "eng" ~ 1,
    language == "span" ~ 0
  )) 

length(unique(all_pct$participant))

all_pct %>% 
  write.csv(here("scripts", "final_data_collection_pct", 
                 "data", "tidy", "all_pct.tidy,csv"))



fixef(mod)

library(lme4)
library(lmerTest)
mod = brm(language_num ~ stim_language*phoneme*frame +
             (1 | participant), 
          data = all_pct, family = "binomial")
summary(mod)

mod %>% 
  write_rds(here("scripts", "final_data_collection_pct", "models", 
                 "bayesian_log.RDS"))

df_bayesian = data.frame(fixef(mod))

# the intercept is the probability of choosing an English word
# when the played phoneme is French /i/ (across both frames)
# 
# intercept + phonemeo = 
# the prob of choosing an english word when the stim was fr and o


# intercept + phonemeo:germ = 
# the prob of choosing an english word when the stim was German and o
