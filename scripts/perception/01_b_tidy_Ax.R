
source(here::here("scripts", "perception", "00_libs.R"))
source(here::here("scripts", "perception", "03_load_data.R"))

# Adjusted ratios
d_prime = function(n_hit, n_fa, n_miss, n_cr)
{
  hit_rate_adjusted <- (n_hit + 0.5)/(n_hit + n_miss + 1)
  fa_rate_adjusted <- (n_fa + 0.5)/(n_fa + n_cr + 1)
  
  dprime <- qnorm(hit_rate_adjusted) - qnorm(fa_rate_adjusted)
  return(dprime)
}

eng_mono = read.csv(here("data", 'raw', "eng_mono_ax.csv")) 
span_blp_ax = read.csv(here("data", "tidy", "span_l1_blp_ax.csv"))

eng_blp_ax = read.csv(here("data", "tidy", "eng_l1_blp_ax.csv"))
### German data 
ax_german <- dir_ls(here("data", "raw", "ax_task_data"),
                    regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_2afc_trial.keys)) %>% 
  filter(!is.na(id)) %>% 
  mutate(L1 = case_when(
    id %in% eng_blp_ax$prolific_id ~ "English",
    id %in% span_blp_ax$prolific_id ~ "Spanish",
    id %in% eng_mono$participant_id ~ "English monolingual"
  )) %>% 
  select(id, L1, the_col, pake, key_resp_2afc_trial.keys, 
         key_resp_2afc_trial.corr, correct_response) %>% 
  mutate(steps = str_remove(pake, "conditions/german/pake_")) %>% 
  mutate(steps = str_remove(steps, ".wav")) %>% 
  select(-pake) %>% 
  mutate("type" = case_when(
    key_resp_2afc_trial.keys == 0 & correct_response == 0 ~ "correct rejection",
    key_resp_2afc_trial.keys == 1 & correct_response == 1 ~ "hit",
    key_resp_2afc_trial.keys == 1 & correct_response == 0 ~ "fa",
    key_resp_2afc_trial.keys == 0 & correct_response == 1 ~ "miss"
  ))



ax_french <- dir_ls(here("data", "raw", "ax_task_data"),
                    regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_2afc_trial_2.keys)) %>% 
  filter(!is.na(id)) %>% 
  mutate(L1 = case_when(
    id %in% eng_blp_ax$prolific_id ~ "English",
    id %in% span_blp_ax$prolific_id ~ "Spanish",
    id %in% eng_mono$participant_id ~ "English monolingual"
  )) %>% 
  select(id, L1, the_col_fr, pic, key_resp_2afc_trial_2.keys, 
         key_resp_2afc_trial_2.corr, correct_response) %>% 
  mutate(steps = str_remove(pic, "conditions/french/pic_")) %>% 
  mutate(steps = str_remove(steps, ".wav")) %>% 
  select(-pic) %>% 
  mutate("type" = case_when(
    key_resp_2afc_trial_2.keys == 0 & correct_response == 0 ~ "correct rejection",
    key_resp_2afc_trial_2.keys == 1 & correct_response == 1 ~ "hit",
    key_resp_2afc_trial_2.keys == 1 & correct_response == 0 ~ "fa",
    key_resp_2afc_trial_2.keys == 0 & correct_response == 1 ~ "miss"
  ))



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

sp_1_prof = span_blp_ax %>% 
  select(prolific_id, l2_prof_perception)
en_1_prof = eng_blp_ax  %>% 
  select(prolific_id, l2_prof_perception)


prof_df = rbind(sp_1_prof, en_1_prof) %>% 
  rename(id = prolific_id)

all_df = rbind(all_part_gr, all_part_fr) %>% 
  mutate(group = case_when(
    id %in% eng_blp_ax$prolific_id ~ "English",
    id %in% span_blp_ax$prolific_id ~ "Spanish",
    id %in% eng_mono$participant_id ~ "English monolingual"
  )) %>% 
  left_join(prof_df, by = "id") %>% 
  mutate("cont_type" = case_when(
    the_col_fr == "fisch" ~ "i",
    the_col_fr == "pake" ~ "a",
    the_col_fr == "pike" ~ "i",
    the_col_fr == "pisch" ~ "i",
    the_col_fr == "fat" ~ "a",
    the_col_fr == "fit" ~ "i",
    the_col_fr == "pat" ~ "a",
    the_col_fr == "pic" ~ "i"
  ))


all_df %>% 
  filter(!is.na(group)) %>% 
  write.csv(here("data", "tidy", "all_ax.csv"))

## Recreate error type 
ax_german %>% 
  filter(!is.na(L1)) %>% 
  ggplot(aes(x = type, fill = type)) + geom_bar(color = "black") +
  theme_minimal() + facet_wrap(~the_col, nrow = 2) +
  ggsave(here("sections", "figs", "error_type_ger.png"))

## Recreate error type 
ax_french %>% 
  filter(!is.na(L1)) %>% 
  ggplot(aes(x = type, fill = type)) + geom_bar(color = "black") +
  theme_minimal() + facet_wrap(~the_col_fr, nrow = 2) +
  ggsave(here("sections", "figs", "error_type_fr.png"))






