# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# This script tidies the bilingual data for later reporting and plotting

# -------------------------------------------------------
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------

### English L1 tidy 
pct_data_g <- dir_ls(here("data", "perception", "raw", "english_pct"),
                   regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_2.keys)) %>% 
  filter(!is.na(stim)) %>% 
  filter(!is.na(participant)) %>% 
  filter(expName == "task") %>% 
  select(key_resp_2.keys, slider.response, stim, participant) %>% 
  rename(resp = key_resp_2.keys) %>% 
  mutate(phoneme = case_when(
    stim == "stim/f_schwa_g.wav" ~ "schwa",
    stim == "stim/p_schwa_g.wav" ~ "schwa",
    stim == "stim/fof_g.wav" ~ "o",
    stim == "stim/pof_g.wav" ~ "o",
    stim == "stim/fief_g.wav" ~ "i",
    stim == "stim/pief_g.wav" ~ "i",
    stim == "stim/peuf_g.wav" ~ "y",
    stim == "stim/feuf_g.wav" ~ "y")) %>% 
  mutate(frame = case_when(
    stim == "stim/f_schwa_g.wav" ~ "fricative",
    stim == "stim/p_schwa_g.wav" ~ "bilabial",
    stim == "stim/fof_g.wav" ~ "fricative",
    stim == "stim/pof_g.wav" ~ "bilabial",
    stim == "stim/feuf_g.wav" ~ "fricative",
    stim == "stim/peuf_g.wav" ~ "bilabial",
    stim == "stim/fief_g.wav" ~ "fricative",
    stim == "stim/pief_g.wav" ~ "bilabial")) %>% 
  mutate(stim_language = "German") %>% 
  mutate(resp = case_when(
    resp == "1" ~ "fun_eng",
    resp == "2" ~ "fought_eng",
    resp == "3" ~ "feel_eng",
    resp == "4" ~ "fool_eng",
    resp == "5" ~ "son_span",
    resp == "6" ~ "su_span",
    resp == "7" ~ "fin_span")) %>% 
  mutate(L1 = "English")

# Get completed participants in German 
comp_german = pct_data_g %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n > 39)

# filter initial tidy df for only those who finished the task.
pct_data_g_finish = pct_data_g %>% 
  filter(participant %in% comp_german$participant)

# Repeat the process for the French trials
pct_data_fr <- dir_ls(here("data", "raw", "english_pct"),
                   regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp.keys)) %>% 
  filter(!is.na(stim)) %>% 
  filter(!is.na(participant)) %>% 
  filter(expName == "task") %>% 
  select(key_resp.keys, slider_2.response, stim, participant) %>% 
  rename(resp = key_resp.keys) %>% 
  mutate(resp = case_when(
    resp == "1" ~ "fun_eng",
    resp == "2" ~ "fought_eng",
    resp == "3" ~ "feel_eng",
    resp == "4" ~ "fool_eng",
    resp == "5" ~ "son_span",
    resp == "6" ~ "su_span",
    resp == "7" ~ "fin_span")) %>% 
  mutate(frame = case_when(
    stim == "stim/f_schwa_f.wav" ~ "fricative",
    stim == "stim/p_schwa_f.wav" ~ "bilabial",
    stim == "stim/fof_f.wav" ~ "fricative",
    stim == "stim/pof_f.wav" ~ "bilabial",
    stim == "stim/feuf_f.wav" ~ "fricative",
    stim == "stim/peuf_f.wav" ~ "bilabial",
    stim == "stim/fif_f.wav" ~ "fricative",
    stim == "stim/pif_f.wav" ~ "bilabial")) %>% 
  mutate(stim_language = "french") %>%
  mutate(phoneme = case_when(
    stim == "stim/f_schwa_f.wav" ~ "schwa",
    stim == "stim/p_schwa_f.wav" ~ "schwa",
    stim == "stim/fof_f.wav" ~ "o",
    stim == "stim/pof_f.wav" ~ "o",
    stim == "stim/fif_f.wav" ~ "i",
    stim == "stim/pif_f.wav" ~ "i",
    stim == "stim/peuf_f.wav" ~ "y",
    stim == "stim/feuf_f.wav" ~ "y")) %>% 
  rename(slider.response = slider_2.response) %>% 
  mutate(L1 = "English")


comp_french = pct_data_fr %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n > 39)

# filter initial tidy df for only those who finished the task.
pct_data_f_finish = pct_data_fr %>% 
  filter(participant %in% comp_french$participant)


# Spanish L1 data 

pct_data_g_sl1 <- dir_ls(here("data", "raw", "spanish_vct"),
                     regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_2.keys)) %>% 
  filter(!is.na(stim)) %>% 
  filter(!is.na(participant)) %>% 
  select(key_resp_2.keys, slider.response, stim, participant) %>% 
  rename(resp = key_resp_2.keys) %>% 
  mutate(phoneme = case_when(
    stim == "stim/f_schwa_g.wav" ~ "schwa",
    stim == "stim/p_schwa_g.wav" ~ "schwa",
    stim == "stim/fof_g.wav" ~ "o",
    stim == "stim/pof_g.wav" ~ "o",
    stim == "stim/fief_g.wav" ~ "i",
    stim == "stim/pief_g.wav" ~ "i",
    stim == "stim/peuf_g.wav" ~ "y",
    stim == "stim/feuf_g.wav" ~ "y")) %>% 
  mutate(frame = case_when(
    stim == "stim/f_schwa_g.wav" ~ "fricative",
    stim == "stim/p_schwa_g.wav" ~ "bilabial",
    stim == "stim/fof_g.wav" ~ "fricative",
    stim == "stim/pof_g.wav" ~ "bilabial",
    stim == "stim/feuf_g.wav" ~ "fricative",
    stim == "stim/peuf_g.wav" ~ "bilabial",
    stim == "stim/fief_g.wav" ~ "fricative",
    stim == "stim/pief_g.wav" ~ "bilabial")) %>% 
  mutate(stim_language = "German") %>% 
  mutate(resp = case_when(
    resp == "1" ~ "fun_eng",
    resp == "2" ~ "fought_eng",
    resp == "3" ~ "feel_eng",
    resp == "4" ~ "fool_eng",
    resp == "5" ~ "son_span",
    resp == "6" ~ "su_span",
    resp == "7" ~ "fin_span")) %>% 
  mutate(L1 = "Spanish")

comp_german_sl1 = pct_data_g_sl1 %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n > 39)

pct_data_g_sl1_finish = pct_data_g_sl1 %>% 
  filter(participant %in% comp_german_sl1$participant)


####

# Repeat the process for the French trials
pct_data_fr_sl1 <- dir_ls(here("data", "raw", "spanish_vct"),
                      regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp.keys)) %>% 
  filter(!is.na(stim)) %>% 
  filter(!is.na(participant)) %>% 
  select(key_resp.keys, slider_2.response, stim, participant) %>% 
  rename(resp = key_resp.keys) %>% 
  mutate(resp = case_when(
    resp == "1" ~ "fun_eng",
    resp == "2" ~ "fought_eng",
    resp == "3" ~ "feel_eng",
    resp == "4" ~ "fool_eng",
    resp == "5" ~ "son_span",
    resp == "6" ~ "su_span",
    resp == "7" ~ "fin_span")) %>% 
  mutate(frame = case_when(
    stim == "stim/f_schwa_f.wav" ~ "fricative",
    stim == "stim/p_schwa_f.wav" ~ "bilabial",
    stim == "stim/fof_f.wav" ~ "fricative",
    stim == "stim/pof_f.wav" ~ "bilabial",
    stim == "stim/feuf_f.wav" ~ "fricative",
    stim == "stim/peuf_f.wav" ~ "bilabial",
    stim == "stim/fif_f.wav" ~ "fricative",
    stim == "stim/pif_f.wav" ~ "bilabial")) %>% 
  mutate(stim_language = "french") %>%
  mutate(phoneme = case_when(
    stim == "stim/f_schwa_f.wav" ~ "schwa",
    stim == "stim/p_schwa_f.wav" ~ "schwa",
    stim == "stim/fof_f.wav" ~ "o",
    stim == "stim/pof_f.wav" ~ "o",
    stim == "stim/fif_f.wav" ~ "i",
    stim == "stim/pif_f.wav" ~ "i",
    stim == "stim/peuf_f.wav" ~ "y",
    stim == "stim/feuf_f.wav" ~ "y")) %>% 
  rename(slider.response = slider_2.response) %>% 
  mutate(L1 = "Spanish")

comp_french_sl1 = pct_data_fr_sl1 %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n > 39)

# filter initial tidy df for only those who finished the task.
pct_data_fr_sl1_finish = pct_data_fr_sl1 %>% 
  filter(participant %in% comp_french_sl1$participant)

#####
# Bind all dfs and save 

pct_data_final = rbind(pct_data_f_finish,
                       pct_data_fr_sl1_finish,
                           pct_data_g_finish,
                       pct_data_g_sl1_finish) %>% 
  separate(resp, into = c("choice", "language_chosen")) %>% 
  mutate(language_choice_num = case_when(
    language_chosen == "eng" ~ 1,
    language_chosen == "span" ~ 0)) %>%
  mutate(choice_num = case_when(
    choice == "fin" ~ 1,
    choice == "fool" ~ 2,
    choice == "fun" ~ 3,
    choice == "fought" ~ 4,
    choice == "son" ~ 5,
    choice == "su" ~ 6,
    choice == "feel" ~ 7))
  

#####
pct_data_final %>% 
  write.csv(here("data", "tidy", "pct_tidy.csv"))

comp_french %>% 
  select(participant) %>% 
  write.csv(here("data", "tidy", "participant_list_enl1.csv"), row.names = FALSE) 

comp_french_sl1 %>% 
  select(participant) %>% 
  write.csv(here("data", "tidy", "participant_list_sl1.csv"), row.names = FALSE) 

            
            
            