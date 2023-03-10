

# get full list of sound files 
all_sound_files = read.csv(here("data", "production", "tidy", "all_soundfiles.csv"))

prolific_ids = read.csv(here("data", "production", "tidy", "prolific_ids.csv"))

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
    )) 

# Load production tidy data
production_ids = read.csv(here("data", "tidy", "full_data.csv")) %>% 
  mutate(language = case_when(
    language == "french" ~ "French",
    language == "German" ~ "German",
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish"
  )) 

part = "5fa4c21a04bc5912180291bc"

per_sub = per %>% 
  select(participant, phoneme, stim_language, language_chosen, L1) %>%
  rename(prolific_id = participant,
        language = stim_language) %>% 
  mutate(mode = "perception") %>% 
  filter(language == "French" | language == "German") 
  
  

pro_sub = full_data_g_f %>%  
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


comb_df = rbind(per_sub, pro_sub) 


unique(pro_sub$phoneme)
unique(per_sub$phoneme)


unique(comb_df$L1)

unique(comb_df$phoneme)

desc = comb_df %>% 
  group_by(mode, phoneme, language_chosen, language) %>% 
  summarize(n = n())

desc %>% 
  ggplot(aes(x = language_chosen, y = n, fill = phoneme)) +
  geom_bar()

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
