

# get full list of sound files 
all_sound_files = read.csv(here("data", "production", "tidy", "all_soundfiles.csv"))

prolific_ids = read.csv(here("data", "production", "tidy", "prolific_ids.csv"))

per = read.csv(here("data", "perception", 
                                "tidy", "pct_tidy.csv"))



part = "5fa4c21a04bc5912180291bc"

per_sub = per %>% 
  filter(participant == part) %>% 
  select(participant, phoneme, stim_language, language_chosen) %>%
  rename(prolific_id = participant,
        language = stim_language) %>% 
  mutate(mode = "perception") %>% 
  filter(language == "French" | language == "German")

pro_sub = production_ids %>% 
  filter(prolific_id == part) %>% 
  mutate(language_chosen = rbinom(96, 1, .7)) %>% 
  mutate(language_chosen = case_when(
    language_chosen == 1 ~ "eng",
    language_chosen == 0 ~ "span"
  )) %>% 
  mutate(mode = "production") %>% 
  select(prolific_id, phoneme, language, language_chosen, mode) %>% 
  filter(language == "French" | language == "German")

comb_df = rbind(per_sub, pro_sub)


model <- brm(formula = language_chosen ~ mode*phoneme,  
                          data=comb_df, 
                          family = bernoulli(link = "logit"))

conditional_effects(model)


