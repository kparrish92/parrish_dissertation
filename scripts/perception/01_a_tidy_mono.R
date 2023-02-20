# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# This script tidies the monolingual data for later reporting and plotting 
# -------------------------------------------------------

# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_libs.R"))

# -----------------------------------------------------------------------------


### Spanish monolingual German  
mono_span_g <- dir_ls(here("data", "raw", "mono_pct_span"),
                     regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_2.keys)) %>% 
  filter(!is.na(stim)) %>% 
  filter(!is.na(participant)) %>% 
  select(check.keys, key_resp_2.keys, slider.response, stim, participant) %>% 
  rename(resp = key_resp_2.keys) %>% 
  rename(rating = slider.response) %>% 
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
  mutate(L1 = "Spanish_mono")

comp_mono_span_g = mono_span_g %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n == 24)

mono_span = mono_span_g %>% 
  filter(check.keys == 2) %>% 
  select(participant)

mono_span_g_finish = mono_span_g %>% 
  filter(participant %in% comp_mono_span_g$participant) %>% 
  filter(participant %in% mono_span$participant)
  
### Span monolingual French
mono_span_f <- dir_ls(here("data", "raw", "mono_pct_span"),
                     regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp.keys)) %>% 
  filter(!is.na(stim)) %>% 
  filter(!is.na(participant)) %>% 
  select(check.keys, key_resp.keys, slider_2.response, stim, participant) %>% 
  rename(resp = key_resp.keys) %>% 
  rename(rating = slider_2.response) %>% 
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
  mutate(L1 = "Spanish_mono")


comp_mono_span_f = mono_span_f %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n == 24)

mono_span_f_finish = mono_span_f %>% 
  filter(participant %in% comp_mono_span_g$participant) %>% 
  filter(participant %in% mono_span$participant)

all_mono_span_tidy = rbind(mono_span_f_finish, mono_span_g_finish)


### English monolingual German  
mono_eng_g <- dir_ls(here("data", "raw", "mono_pct_eng"),
                     regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp_2.keys)) %>% 
  filter(!is.na(stim)) %>% 
  filter(!is.na(participant)) %>%
  select(check.keys, key_resp_2.keys, slider.response, stim, participant) %>% 
  rename(resp = key_resp_2.keys) %>%
  rename(rating = slider.response) %>% 
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
  mutate(L1 = "English_mono")

comp_mono_eng_g = mono_eng_g %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n == 8)

mono_eng = mono_eng_g %>% 
  filter(check.keys == 2) %>% 
  select(participant)

mono_eng_g_finish = mono_eng_g %>% 
  filter(participant %in% comp_mono_eng_g$participant) %>% 
  filter(participant %in% mono_eng$participant)


### English monolingual French
mono_eng_f <- dir_ls(here("data", "raw", "mono_pct_eng"),
                     regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>% 
  filter(!is.na(key_resp.keys)) %>% 
  filter(!is.na(stim)) %>% 
  filter(!is.na(participant)) %>% 
  select(check.keys, key_resp.keys, slider_2.response, stim, participant) %>% 
  rename(resp = key_resp.keys) %>% 
  rename(rating = slider_2.response) %>% 
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
  mutate(L1 = "English_mono")

comp_mono_eng_f = mono_eng_f %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n == 8)

mono_eng_f_finish = mono_eng_f %>% 
  filter(participant %in% comp_mono_eng_g$participant)

tidy_mono_groups = rbind(mono_eng_f_finish, mono_eng_g_finish,
                         mono_span_f_finish, mono_span_g_finish) %>% 
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


tidy_mono_groups %>% 
  write.csv(here("data", "tidy", "tidy_mono_groups.csv"))


