# Tidy phoneme categorization task in German ----------------------
#
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "perception", "00_libs.R"))

# -----------------------------------------------------------------------------


stim_list = c(
  "stim/pof_g.wav",
  "stim/pief_g.wav",
  "stim/peuf_g.wav",
  "stim/p_schwa_g.wav",
  "stim/fof_g.wav",
  "stim/fief_g.wav",
  "stim/feuf_g.wav",
  "stim/f_schwa_g.wav")


pct_data <- dir_ls(here("scripts", "spanish_bilingual_pct", "data", "raw"), regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>%
  filter(stim %in% stim_list) %>% 
  filter(!is.na(key_resp_2.keys)) %>%
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
  select(participant, stim, resp, stim_language, slider.response,
         phoneme, frame)

pct_data %>% 
  write.csv(here("scripts", "spanish_bilingual_pct", 
                 "data", "tidy",
                 "pct_tidy_german_sp.csv"))



