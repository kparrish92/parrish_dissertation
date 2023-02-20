

# get full list of sound files 
all_sound_files = read.csv(here("data", "production", "tidy", "all_soundfiles.csv"))

prolific_ids = read.csv(here("data", "production", "tidy", "prolific_ids.csv"))


part = "5fa4c21a04bc5912180291bc"

pro_sub = production_ids %>% 
  filter(prolific_id == part) %>% 
  mutate(language_chosen = rbinom(95, 1, .7)) %>% 
  mutate(language_chosen = case_when(
    language_chosen == 1 ~ "eng",
    language_chosen == 0 ~ "span"
  )) %>% 
  mutate(mode = "production") %>% 
  select(prolific_id, phoneme, language, language_chosen, mode) %>% 
  filter(language == "French" | language == "German")

 