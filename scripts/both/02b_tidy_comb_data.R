source(here::here("scripts", "both", "00_libs.R"))
source(here::here("scripts", "both", "01_helpers.R"))


# get full list of sound files 
#all_sound_files = read.csv(here("data", "production", "tidy", "all_soundfiles.csv"))

#prolific_ids = read.csv(here("data", "production", "tidy", "prolific_ids.csv"))

per = read.csv(here("data", "perception", 
                                "tidy", "pct_tidy.csv")) %>% 
  mutate(phoneme = case_when(
    phoneme == "i" ~ "i",
    phoneme == "schwa" ~ "schwa_a",
    phoneme == "o" ~ "o",
    phoneme == "y" ~ "u")) %>%  
  mutate(stim_language = case_when(
    stim_language == "french" ~ "French",
    stim_language == "German" ~ "German",
    stim_language == "english" ~ "English",
    stim_language == "spanish" ~ "Spanish"
    )) %>% 
  select(participant, phoneme, stim_language, language_chosen, L1) %>%
  rename(prolific_id = participant,
        language = stim_language) %>% 
  mutate(mode = "perception") %>% 
  filter(language == "French" | language == "German") 
  
  

prod = read.csv(here("data", "both", "production_data_tidy.csv")) %>%  
  mutate(language_chosen = case_when(
  language_chosen == 1 ~ "span",
  language_chosen == 0 ~ "eng")) %>% 
  mutate(mode = "production") %>% 
  select(prolific_id, phoneme, language, language_chosen, mode, group) %>% 
  rename(L1 = group) %>% 
  mutate(L1 = case_when(
    L1 == "L1 Spanish bilingual" ~ "Spanish",
    L1 == "L1 English bilingual" ~ "English"
  ))


comb_df = rbind(per, prod) 

comb_df %>% 
  write.csv(here("data", "both", "combined.csv"))



unique(comb_df$L1)

unique(comb_df$phoneme)

desc = comb_df %>% 
  group_by(mode, phoneme, language_chosen, language) %>% 
  summarize(n = n())


comb_df %>% 
  filter(L1 == "English") %>% 
  ggplot(aes(x = language_chosen, fill = language)) + 
  geom_bar(color = "black", position = position_dodge()) + 
  facet_grid(mode~phoneme)

comb_df %>% 
  filter(L1 == "Spanish") %>% 
  ggplot(aes(x = language_chosen, fill = language_chosen)) + 
  geom_bar(color = "black", position = position_dodge()) + 
  facet_grid(mode~phoneme)

comb_df %>% 
  filter(L1 == "English") %>% 
  ggplot(aes(x = language_chosen, fill = language_chosen)) + 
  geom_bar(color = "black", position = position_dodge()) + 
  facet_grid(mode~phoneme)

### The model predicts the probability of picking a Spanish category



##### Two afc choice, playing each audio from the participants 
# German or French - does it sound more like English or Spanish?
# Random pull of X tokens each 
model <- brm(formula = language_chosen ~ phoneme*mode + (1 | prolific_id),  
                          data=comb_df, 
                          family = bernoulli(link = "logit"))

conditional_effects(model)

comb_df %>% 
  group_by(L1, mode) %>% 
  summarize(n = n())


 
model_span <- brm(formula = language_chosen ~ phoneme*mode + (1 | prolific_id),  
             data=comb_df %>% filter(L1 == "Spanish"), 
             family = bernoulli(link = "logit"))

model_eng <- brm(formula = language_chosen ~ phoneme*mode + (1 | prolific_id),  
                  data=comb_df %>% filter(L1 == "English"), 
                  family = bernoulli(link = "logit"))


conditional_effects(model_span)

conditional_effects(model_eng)

model_span_a <- brm(formula = language_chosen ~ language*mode + (1 | prolific_id),  
                  data=comb_df %>% filter(L1 == "Spanish"), 
                  family = bernoulli(link = "logit"))

conditional_effects(model_span_a)
