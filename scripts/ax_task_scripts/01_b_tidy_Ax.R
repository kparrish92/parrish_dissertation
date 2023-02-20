source(here::here("scripts", "ax_task_scripts", "00_libs.R"))

# Adjusted ratios
d_prime = function(n_hit, n_fa, n_miss, n_cr)
{
  hit_rate_adjusted <- (n_hit + 0.5)/(n_hit + n_miss + 1)
  fa_rate_adjusted <- (n_fa + 0.5)/(n_fa + n_cr + 1)
  
  dprime <- qnorm(hit_rate_adjusted) - qnorm(fa_rate_adjusted)
  return(dprime)
}

eng_mono = read.csv(here("data", "raw", "ax_task_data", "eng_mono_ax.csv")) 



# load and tidy data 
eng_l1_blp = read.csv(here("data", "raw", "blp", 
                           "BLP_English-Spanish.csv")) %>% 
  rename(prolific_id = 
           What.is.your.prolific.ID.) %>% 
  rename(l2_ao = At.what.age.did.you.start.learning.SPANISH.) %>% 
  rename(l2_aoa 
         = 
           At.what.age.did.you.start.to.feel.comfortable.using.SPANISH.) %>% 
  rename(l2_prof_production = How.well.do.you.speak.SPANISH.) %>% 
  rename(l2_prof_perception = How.well.do.you.understand.SPANISH.) %>% 
  select(prolific_id, l2_ao, l2_aoa, l2_prof_production, l2_prof_perception)


span_l1_blp = read.csv(here("data", "raw", "blp", 
                            "BLP_Spanish-English.csv")) %>%
  rename(l2_ao = 
           X.A.qué.edad.empezó.a.aprender.INGLÉS.) %>% 
  rename(prolific_id = 
           Prolific.ID) %>% 
  rename(l2_aoa 
         = 
           X.A.qué.edad.empezó.a.sentirse.cómodo.usando.INGLÉS.) %>% 
  rename(l2_prof_production = X.Cómo.habla..en.INGLÉS.) %>% 
  rename(l2_prof_perception = X.Cómo.entiende.en.INGLÉS.) %>% 
  select(prolific_id, l2_ao, l2_aoa, l2_prof_production, l2_prof_perception)

### German data 
ax_german <- dir_ls(here("data", "raw", "ax_task_data"),
                      regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_2afc_trial.keys)) %>% 
  filter(!is.na(id)) %>% 
  mutate(L1 = case_when(
    id %in% eng_l1_blp$prolific_id ~ "English",
    id %in% span_l1_blp$prolific_id ~ "Spanish",
    id %in% eng_mono$participant_id ~ "English monolingual"
  )) %>% 
  select(id, L1, the_col, pake, key_resp_2afc_trial.keys, 
         key_resp_2afc_trial.corr, correct_response) %>% 
  mutate(steps = str_remove(pake, "conditions/german/pake_")) %>% 
  mutate(steps = str_remove(steps, ".wav")) %>% 
  select(-pake) %>% 
  mutate("type" = case_when(
    key_resp_2afc_trial.keys == 0 & correct_response == 0 ~ "miss",
    key_resp_2afc_trial.keys == 1 & correct_response == 1 ~ "hit",
    key_resp_2afc_trial.keys == 1 & correct_response == 0 ~ "fa",
    key_resp_2afc_trial.keys == 0 & correct_response == 1 ~ "correct rejection"
  )) %>% 
  filter(!is.na(L1))



ax_french <- dir_ls(here("data", "raw", "ax_task_data"),
                    regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_2afc_trial_2.keys)) %>% 
  filter(!is.na(id)) %>% 
  mutate(L1 = case_when(
    id %in% eng_l1_blp$prolific_id ~ "English",
    id %in% span_l1_blp$prolific_id ~ "Spanish",
    id %in% eng_mono$participant_id ~ "English monolingual"
  )) %>% 
  select(id, L1, the_col_fr, pic, key_resp_2afc_trial_2.keys, 
         key_resp_2afc_trial_2.corr, correct_response) %>% 
  mutate(steps = str_remove(pic, "conditions/french/pic_")) %>% 
  mutate(steps = str_remove(steps, ".wav")) %>% 
  select(-pic) %>% 
  mutate("type" = case_when(
    key_resp_2afc_trial_2.keys == 0 & correct_response == 0 ~ "miss",
    key_resp_2afc_trial_2.keys == 1 & correct_response == 1 ~ "hit",
    key_resp_2afc_trial_2.keys == 1 & correct_response == 0 ~ "fa",
    key_resp_2afc_trial_2.keys == 0 & correct_response == 1 ~ "correct rejection"
  )) %>% 
  filter(!is.na(L1))

ax_french %>% 
  group_by(L1) %>% 
  summarize(pct = sum(as.numeric(correct_response)), n_total = n()) %>% 
  mutate(percent = pct/n_total)

ax_german %>% 
  group_by(L1) %>% 
  summarize(pct = sum(as.numeric(correct_response)), n_total = n()) %>% 
  mutate(percent = pct/n_total)


sum(as.numeric(ax_french$correct_response))
sum(as.numeric(ax_german$correct_response))


#how_many = ax_raw %>% 
 # group_by(id) %>% 
  #summarise(n = n())

fren_all = ax_french %>% 
  group_by(type, L1, the_col_fr) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = type, values_from = n) %>% 
  mutate(d_prime = d_prime(n_hit = hit, n_fa = fa, n_miss = miss, 
                         n_cr = `correct rejection`))

german_all = ax_german %>% 
  group_by(type, L1, the_col) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = type, values_from = n) %>% 
  mutate(d_prime = d_prime(n_hit = hit, n_fa = fa, n_miss = miss, 
                           n_cr = `correct rejection`))


all_part_fr = ax_french %>% 
  group_by(type, id, the_col_fr) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = type, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(d_prime = d_prime(n_hit = hit, n_fa = fa, n_miss = miss, 
                           n_cr = `correct rejection`)) %>% 
  mutate(language = "French")


all_part_gr = ax_german %>% 
  group_by(type, id, the_col) %>% 
  summarize(n = n()) %>% 
  pivot_wider(names_from = type, values_from = n) %>% 
  replace(is.na(.), 0) %>% 
  mutate(d_prime = d_prime(n_hit = hit, n_fa = fa, n_miss = miss, 
                           n_cr = `correct rejection`)) %>% 
  rename(the_col_fr = the_col) %>% 
  mutate(language = "German")


all_df = rbind(all_part_gr, all_part_fr) %>% 
  mutate(group = case_when(
    id %in% eng_l1_blp$prolific_id ~ "English",
    id %in% span_l1_blp$prolific_id ~ "Spanish",
    id %in% eng_mono$participant_id ~ "English monolingual"
  ))


all_df %>% 
  write.csv(here("data", "tidy", "all_ax.csv"))
