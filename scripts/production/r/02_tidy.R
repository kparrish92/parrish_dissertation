source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))


prod_eng = read.csv(here("data", "production", "raw", "data_prod_eng.csv"), header=T, na.strings=c(""," ","NA")) %>%
  janitor::clean_names() %>% 
  select(word_french, word_french_recording, word_german, german_recording, english_recording,
         word, rec_session_id) %>% 
  rename(english_word = "word",
         german_word = "word_german",
         french_word = "word_french",
         french_file = "word_french_recording",
         german_file = "german_recording",
         english_file = "english_recording") %>% 
  unite(col='word', c(german_word, french_word, english_word), na.rm = TRUE) %>% 
  unite(col = 'file', c(german_file, french_file, english_file), na.rm = TRUE) %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(file)) %>% 
  mutate(language = substring(file, 30)) %>% 
  mutate(language = str_remove(language, "_recording.wav")) %>% 
  mutate(language = str_remove(language, "_"))

prod = read.csv(here("data", "production", "raw", "data_prod.csv"), header=T, na.strings=c(""," ","NA")) %>%
  janitor::clean_names() %>% 
  select(word_french, word_french_recording, word_german, german_recording, english_recording,
         word, word_d7c64, rec_session_id, spanish_recording) %>% 
  rename(english_word = "word",
         spanish_word = "word_d7c64",
         german_word = "word_german",
         french_word = "word_french",
         spanish_file = "spanish_recording",
         french_file = "word_french_recording",
         german_file = "german_recording",
         english_file = "english_recording") %>% 
  unite(col='word', c(german_word, french_word, english_word, spanish_word), na.rm = TRUE) %>% 
  unite(col = 'file', c(german_file, french_file, english_file, spanish_file), na.rm = TRUE) %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(file)) %>% 
  mutate(language = substring(file, 30)) %>% 
  mutate(language = str_remove(language, "_recording.wav")) %>% 
  mutate(language = str_remove(language, "_"))


prod_span = read.csv(here("data", "production", "raw", "data_prod_span.csv"), header=T, na.strings=c(""," ","NA")) %>%
  janitor::clean_names() %>% 
  select(word_french, word_french_recording, word_german, german_recording, 
         word_db1f2, rec_session_id, spanish_recording) %>% 
  rename(spanish_word = "word_db1f2",
         german_word = "word_german",
         french_word = "word_french",
         spanish_file = "spanish_recording",
         french_file = "word_french_recording",
         german_file = "german_recording") %>% 
  unite(col='word', c(german_word, french_word, spanish_word), na.rm = TRUE) %>% 
  unite(col = 'file', c(german_file, french_file, spanish_file), na.rm = TRUE) %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(file)) %>% 
  mutate(language = substring(file, 30)) %>% 
  mutate(language = str_remove(language, "_recording.wav")) %>% 
  mutate(language = str_remove(language, "_"))

prod_ids = read.csv(here("data", "production", "raw", "data_prod.csv"), header=T, na.strings=c(""," ","NA")) %>%
  janitor::clean_names() %>% 
  filter(!is.na(prolific_id)) %>% 
  filter(!is.na(end_time))

participants_done = prod %>% 
  group_by(rec_session_id) %>% 
  summarize(n = n()) %>% 
  filter(n == 96)

participants_done_eng = prod_eng %>% 
  group_by(rec_session_id) %>% 
  summarize(n = n()) %>% 
  filter(n == 72)

participants_done_span = prod_span %>% 
  group_by(rec_session_id) %>% 
  summarize(n = n()) %>% 
  filter(n == 72)

prod_tidy = prod %>% 
  filter(rec_session_id %in% participants_done$rec_session_id) %>% 
  mutate("file_name_append" = paste0(rec_session_id,"_",file)) %>% 
  mutate(file_name_append = str_remove(file_name_append, ".wav")) %>% 
  mutate("text" = case_when(
    word == "f_i" & language == "french" ~ "fif",
    word == "p_i" & language == "french" ~ "pif",
    word == "f_schwa" & language == "french" ~ "faf",
    word == "p_schwa" & language == "french" ~ "paf",
    word == "f_u" & language == "french" ~ "fuf",
    word == "p_u" & language == "french" ~ "puf",
    word == "f_o" & language == "french" ~ "fof",
    word == "p_o" & language == "french" ~ "pof",
    word == "f_i" & language == "english" ~ "feef",
    word == "p_i" & language == "english" ~ "peef",
    word == "f_schwa" & language == "english" ~ "fuff",
    word == "p_schwa" & language == "english" ~ "puff",
    word == "f_u" & language == "english" ~ "foof",
    word == "p_u" & language == "english" ~ "poof",
    word == "f_o" & language == "english" ~ "foff",
    word == "p_o" & language == "english" ~ "poff",
    word == "f_i" & language == "German" ~ "fif",
    word == "p_i" & language == "German" ~ "pif",
    word == "f_schwa" & language == "German" ~ "faf",
    word == "p_schwa" & language == "German" ~ "paf",
    word == "f_u" & language == "German" ~ "fuf",
    word == "p_u" & language == "German" ~ "puf",
    word == "f_o" & language == "German" ~ "fof",
    word == "p_o" & language == "German" ~ "pof",
    word == "f_i" & language == "spanish" ~ "fif",
    word == "p_i" & language == "spanish" ~ "pif",
    word == "f_a" & language == "spanish" ~ "faf",
    word == "p_a" & language == "spanish" ~ "paf",
    word == "f_u" & language == "spanish" ~ "fuf",
    word == "p_u" & language == "spanish" ~ "puf",
    word == "f_o" & language == "spanish" ~ "fof",
    word == "p_o" & language == "spanish" ~ "pof"
  ))


prod_tidy_eng = prod_eng %>% 
  filter(rec_session_id %in% participants_done_eng$rec_session_id) %>% 
  mutate("file_name_append" = paste0(rec_session_id,"_",file)) %>% 
  mutate(file_name_append = str_remove(file_name_append, ".wav")) %>% 
  mutate("text" = case_when(
    word == "f_i" & language == "french" ~ "fif",
    word == "p_i" & language == "french" ~ "pif",
    word == "f_schwa" & language == "french" ~ "faf",
    word == "p_schwa" & language == "french" ~ "paf",
    word == "f_u" & language == "french" ~ "fuf",
    word == "p_u" & language == "french" ~ "puf",
    word == "f_o" & language == "french" ~ "fof",
    word == "p_o" & language == "french" ~ "pof",
    word == "f_i" & language == "english" ~ "feef",
    word == "p_i" & language == "english" ~ "peef",
    word == "f_schwa" & language == "english" ~ "fuff",
    word == "p_schwa" & language == "english" ~ "puff",
    word == "f_u" & language == "english" ~ "foof",
    word == "p_u" & language == "english" ~ "poof",
    word == "f_o" & language == "english" ~ "foff",
    word == "p_o" & language == "english" ~ "poff",
    word == "f_i" & language == "German" ~ "fif",
    word == "p_i" & language == "German" ~ "pif",
    word == "f_schwa" & language == "German" ~ "faf",
    word == "p_schwa" & language == "German" ~ "paf",
    word == "f_u" & language == "German" ~ "fuf",
    word == "p_u" & language == "German" ~ "puf",
    word == "f_o" & language == "German" ~ "fof",
    word == "p_o" & language == "German" ~ "pof"
  ))

prod_tidy_span = prod_span %>% 
  filter(rec_session_id %in% participants_done_span$rec_session_id) %>% 
  mutate("file_name_append" = paste0(rec_session_id,"_",file)) %>% 
  mutate(file_name_append = str_remove(file_name_append, ".wav")) %>%
  mutate("text" = case_when(
    word == "f_i" & language == "french" ~ "fif",
    word == "p_i" & language == "french" ~ "pif",
    word == "f_schwa" & language == "french" ~ "faf",
    word == "p_schwa" & language == "french" ~ "paf",
    word == "f_u" & language == "french" ~ "fuf",
    word == "p_u" & language == "french" ~ "puf",
    word == "f_o" & language == "french" ~ "fof",
    word == "p_o" & language == "french" ~ "pof",
    word == "f_i" & language == "spanish" ~ "fif",
    word == "p_i" & language == "spanish" ~ "pif",
    word == "f_a" & language == "spanish" ~ "faf",
    word == "p_a" & language == "spanish" ~ "paf",
    word == "f_u" & language == "spanish" ~ "fuf",
    word == "p_u" & language == "spanish" ~ "puf",
    word == "f_o" & language == "spanish" ~ "fof",
    word == "p_o" & language == "spanish" ~ "pof",
    word == "f_i" & language == "German" ~ "fif",
    word == "p_i" & language == "German" ~ "pif",
    word == "f_schwa" & language == "German" ~ "faf",
    word == "p_schwa" & language == "German" ~ "paf",
    word == "f_u" & language == "German" ~ "fuf",
    word == "p_u" & language == "German" ~ "puf",
    word == "f_o" & language == "German" ~ "fof",
    word == "p_o" & language == "German" ~ "pof"))



# Full datasets 
file_names_e_full = match_files("sound_files_eng_mono")
file_names_s_full = match_files("sound_files_span_mono")
file_names_b_full = match_files("sound_files_bilingual")

# Subsets 
file_names_e_sub = match_files("sound_files_eng_mono")
#file_names_s_sub = match_files("sound_files_span_mono")
file_names_b_sub = match_files("sound_files_bilingual")


txt_files_words_e = left_join(prod_tidy, file_names_e_full, by = "file_name_append") %>% 
  select(file_name_append, text, language, word)

txt_files_words_b = left_join(prod_tidy, file_names_b_full, by = "file_name_append") %>% 
  select(file_name_append, text, language, word)

txt_files_words_b %>% 
  group_by(rec_session_id, language) %>% 
  summarize(n = n())


vot_txt_files_words_e = left_join(prod_tidy, file_names_e_full, by = "file_name_append") %>% 
  select(file_name_append, text, language, word) %>% 
  filter(str_detect(word, "p"))

vot_txt_files_words_b = left_join(prod_tidy, file_names_b_full, by = "file_name_append") %>% 
  select(file_name_append, text, language, word) %>% 
  filter(str_detect(word, "p"))

z = vot_txt_files_words_b %>% 
  mutate(file_copy = file_name_append)

z$file_copy <- substr(z$file_copy, 0, 6)

z %>% 
  write.csv(here("data", "production", "tidy", "stop_files.csv"))

## get files for participants who did both experiments 

all_e_files = left_join(prod_tidy, file_names_e_full, by = "file_name_append") %>% 
  select(file_name_append, text, language, word)

all_s_files = left_join(prod_tidy, file_names_b_full, by = "file_name_append") 

all_s_files %>% 
  group_by(rec_session_id, language) %>% 
  summarize(n = n())

all_s_files %>% 
  write.csv(here("data", "production", "tidy", "all_soundfiles.csv"))




txt_files_words_e_mono = left_join(prod_tidy_eng, file_names_e_full, by = "file_name_append") %>% 
  select(file_name_append, text, language, word)


txt_files_words_s_mono = left_join(prod_tidy_span, file_names_s_full, by = "file_name_append") %>% 
  select(file_name_append, text, language, word)

### Subsets 

sub_bil = read.csv(here("data", "production", "tidy","sub_bil.csv")) %>% 
  filter(f1e != "--undefined--") %>% 
  filter(!f1e == "f1e") %>% 
  rename("file_name_append" = prefix) %>% 
  left_join(., txt_files_words_b, by = "file_name_append") %>% 
  select(f1e, f2e, language, text, file_name_append, word) %>% 
  separate(word, into = c("consonant", "phoneme"), sep = "_") %>% 
  mutate(correct = "original")

sub_bil_corr = read.csv(here("data","production", "tidy","sub_bil_corrected.csv")) %>% 
  filter(f1e != "--undefined--") %>% 
  rename("file_name_append" = prefix) %>% 
  left_join(., txt_files_words_b, by = "file_name_append") %>% 
  select(f1e, f2e, language, text, file_name_append, word) %>% 
  separate(word, into = c("consonant", "phoneme"), sep = "_") %>% 
  mutate(correct = "corrected")

df_p = rbind(sub_bil, sub_bil_corr) %>% 
  write.csv(here("data", "production", "tidy", "bilingual_subset.csv"))


### Tidy full dataset 

## Bilingual speakers 
eng_bil = read.csv(here("data","production", "tidy","eng_bil_data.csv")) %>% 
  mutate(group = "L1 English bilingual") %>% 
  select(f1e, f2e, language, participant, text, 
         group, consonant, phoneme) 

span_bil = read.csv(here("data","production", "tidy","span_bil_data.csv")) %>% 
  mutate(group = "L1 Spanish bilingual")  %>% 
  select(f1e, f2e, language, participant, text, 
         group, consonant, phoneme) 

eng_mono = read.csv(here("data","production", "tidy","eng_data copy.csv")) %>% 
  mutate(group = "L1 English monolingual") %>% 
  rename("file_name_append" = prefix) %>% 
  left_join(., txt_files_words_e_mono, by = "file_name_append") %>% 
  separate(word, into = c("consonant", "phoneme"), sep = "_") %>%
  separate(file_name_append, into = c("participant", "trash"), sep = "_") %>%
  filter(!is.na(trash)) %>% 
  select(f1e, f2e, language, participant, text, 
         group, consonant, phoneme) 

span_mono = read.csv(here("data","production", "tidy","span_mono.csv")) %>% 
  mutate(group = "L1 Spanish monolingual") %>% 
  rename("file_name_append" = prefix) %>% 
  left_join(., txt_files_words_s_mono, by = "file_name_append") %>% 
  separate(word, into = c("consonant", "phoneme"), sep = "_") %>%
  separate(file_name_append, into = c("participant", "trash"), sep = "_") %>%
  filter(!is.na(trash)) %>% 
  select(f1e, f2e, language, participant, text, 
         group, consonant, phoneme) 

 
## And biographical data 
full_df = rbind(eng_mono, span_bil, eng_bil, span_mono) %>% 
  write.csv(here("data","production", "tidy","full_data_tidy.csv"))
