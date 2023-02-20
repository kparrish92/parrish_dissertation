
source(here::here("scripts", "perception", "00_libs.R"))
source(here::here("scripts", "perception", "03_load_data.R"))

eng_mono = read.csv(here("data", 'raw', "eng_mono_ax.csv")) 

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
  ))



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
  ))

ax_french %>% 
  group_by(L1) %>% 
  summarize(pct_right = sum(as.numeric(correct_response))/nrow(correct_response))

ax_french %>% 
  group_by(L1) %>% 
  summarize(pct = sum(as.numeric(correct_response)), n_total = n()) %>% 
  mutate(percent = pct/n_total)


ax_german %>% 
  group_by(L1) %>% 
  summarize(pct = sum(as.numeric(correct_response)), n_total = n()) %>% 
  mutate(percent = pct/n_total)



summarize(pct_right = sum(as.numeric(correct_response))/nrow(correct_response))


sum(as.numeric(ax_french$correct_response))


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
  ggplot(aes(x = d_prime, y = the_col_fr, color = language)) + geom_point()


all_df %>% 
  ggplot(aes(x = d_prime, y = the_col_fr, color = language)) + geom_boxplot() +
  facet_grid(~group)

all_df %>% 
  filter(!is.na(group)) %>% 
  ggplot(aes(x = d_prime, y = the_col_fr, color = group)) + geom_boxplot() 

all_df %>% 
  ggplot(aes(x = d_prime, y = language, color = language)) + geom_boxplot()


all_df %>% 
  group_by(group) %>% 
  summarize(n = n())

g = all_df %>% 
  filter(group == "English monolingual")


# Adjusted ratios
d_prime = function(n_hit, n_fa, n_miss, n_cr)
{
  hit_rate_adjusted <- (n_hit + 0.5)/(n_hit + n_miss + 1)
  fa_rate_adjusted <- (n_fa + 0.5)/(n_fa + n_cr + 1)
  
  dprime <- qnorm(hit_rate_adjusted) - qnorm(fa_rate_adjusted)
  return(dprime)
}

d_prime(n_hit, n_fa, n_miss, n_cr)
c = dprime(n_hit = 147,
           n_fa = 555,
           n_miss = 537,
           n_cr = 35)

dprime(n_hit = 272,
       n_fa = 1370,
       n_miss = 1110,
       n_cr = 141)

ax_german %>% 
  group_by(type, L1) %>% 
  summarize(n = n())

library(psycho)






