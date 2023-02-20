# Tidy phoneme categorization task in French ----------------------
#
# Source libs -----------------------------------------------------------------

source(here::here("scripts", "perception", "00_libs.R"))

# -----------------------------------------------------------------------------



stim_list = c(
  "stim/pof_f.wav",
  "stim/pif_f.wav",
  "stim/peuf_f.wav",
  "stim/p_schwa_f.wav",
  "stim/fof_f.wav",
  "stim/fif_f.wav",
  "stim/feuf_f.wav",
  "stim/f_schwa_f.wav")

pct_data <- dir_ls(here("scripts", "spanish_bilingual_pct", "data", "raw"), regexp = "\\.csv$") %>%
  map_dfr(read_csv, .id = "source", 
          col_types = cols(.default = "c")) %>%
  filter(stim %in% stim_list) %>% 
  filter(!is.na(key_resp.keys)) %>%
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
  mutate(language = "french") %>%
  mutate(phoneme = case_when(
    stim == "stim/f_schwa_f.wav" ~ "schwa",
    stim == "stim/p_schwa_f.wav" ~ "schwa",
    stim == "stim/fof_f.wav" ~ "o",
    stim == "stim/pof_f.wav" ~ "o",
    stim == "stim/fif_f.wav" ~ "i",
    stim == "stim/pif_f.wav" ~ "i",
    stim == "stim/peuf_f.wav" ~ "y",
    stim == "stim/feuf_f.wav" ~ "y")) %>% 
  select(participant, stim, resp, language, slider_2.response,
         phoneme, frame) 



pct_data %>% 
  write.csv(here("scripts", "spanish_bilingual_pct", 
                 "data", "tidy",
                 "pct_tidy_french_sp.csv"))

