library(here)
library(tidyverse)

# Load participants who completed the perception experiments
enl1 = read.csv(here("data", "tidy", "participant_list_enl1.csv")) 
spl1 = read.csv(here("data", "tidy", "participant_list_sl1.csv"))

# Filter the data to match prolific and labvanced ids
ids = read.csv(here("data", "raw", "data_prod.csv"), header=T, na.strings=c(""," ","NA")) %>% 
  filter(!is.na(prolific_id)) %>% 
  filter(!is.na(end_time))

library(here)
library(tidyverse)
library(stringr)

# create text files for Webmaus autosegmentation based on file names

file_names  <- list.files(path= here("production_files", "sound_files_bilingual"),
                          recursive=T,
                          pattern=".wav"
                          ,full.names=T) %>% 
  as.data.frame() %>% 
  rename(file_name_append = ".") %>% 
  mutate(file_name_append = str_remove(file_name_append, 
                                       "/Users/kyleparrish/Documents/GitHub/production_diss/sound_files/"),
         file_name_append = str_remove(file_name_append, ".wav")) 


# load prod_tidy 


# Load the trial order of each participanant and tidy 
prod = read.csv(here("data", "raw", "data_prod.csv"), header=T, na.strings=c(""," ","NA")) %>%
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


prod_ids = read.csv(here("data", "raw", "data_prod.csv"), header=T, na.strings=c(""," ","NA")) %>%
  janitor::clean_names() %>% 
  filter(!is.na(prolific_id)) %>% 
  filter(!is.na(end_time))

participants_done = prod %>% 
  group_by(rec_session_id) %>% 
  summarize(n = n()) %>% 
  filter(n == 96)

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
    word == "f_i" & language == "spanish" ~ "fif",
    word == "p_i" & language == "spanish" ~ "pif",
    word == "f_a" & language == "spanish" ~ "faf",
    word == "p_a" & language == "spanish" ~ "paf",
    word == "f_u" & language == "spanish" ~ "fuf",
    word == "p_u" & language == "spanish" ~ "puf",
    word == "f_o" & language == "spanish" ~ "fof",
    word == "p_o" & language == "spanish" ~ "pof",
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
  ))


txt_files_words = left_join(prod_tidy, file_names, by = "file_name_append") %>% 
  select(file_name_append, text, language, word)

# Checking to make sure all languages have same number of tokens 

prod_tidy %>% 
  group_by(language) %>% 
  summarize(n = n())

prod_tidy %>% 
  filter(language == "english") %>% 
  group_by(word) %>% 
  summarize(n = n())


enl1_prod = enl1 %>% 
  filter(participant %in% prod$prolific_id)

spl1_prod = spl1 %>% 
  filter(participant %in% prod$prolific_id)

eng_prol = read.csv(here("data", "raw", "prol_data.csv")) %>% 
  filter(status == "AWAITING REVIEW")

eng_prol$participant_id

file_names  <- list.files(path= here("sound_files"),
                          recursive=T,
                          pattern=".wav"
                          ,full.names=T) %>% 
  as.data.frame() %>% 
  rename(file_name_append = ".") %>% 
  mutate(file_name_append = str_remove(file_name_append, 
                                       "/Users/kyleparrish/Documents/GitHub/production_diss/sound_files/"),
         file_name_append = str_remove(file_name_append, ".wav")) 


# load prod_tidy 

txt_files_words = left_join(prod_tidy, file_names, by = "file_name_append") %>% 
  select(file_name_append, text, language, word)


###

ex_formant_data = read.csv(here("data", "tidy", "bil_data.csv")) %>% 
  filter(!f1e == "--undefined--") %>%
  rename("file_name_append" = prefix) %>% 
  filter(f1e != "f1e") %>%
  left_join(., txt_files_words, by = "file_name_append") %>% 
  select(f1e, f2e, language, text, file_name_append, word) %>% 
  separate(word, into = c("consonant", "phoneme"), sep = "_")

ex_formant_data$f1e = as.numeric(ex_formant_data$f1e)
ex_formant_data$f2e = as.numeric(ex_formant_data$f2e)

glimpse(ex_formant_data)

ex_formant_data %>% 
  ggplot(aes(x = f1e, y = f2e, color = language)) + geom_point() + 
  scale_x_reverse() + scale_y_reverse() + 
  theme_classic() + facet_grid(~phoneme)



ex_formant_data %>% 
  ggplot(aes(x = f2e, y = f1e, color = phoneme)) + geom_point(alpha = .01) + 
  stat_ellipse(level = 0.67) +
  scale_x_reverse() + scale_y_reverse() 


ex_formant_data %>% 
  filter(phoneme == 'i') %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + geom_point(alpha = .01) + 
  stat_ellipse(level = 0.67) +
  scale_x_reverse() + scale_y_reverse() 


ex_formant_data %>% 
  filter(phoneme == 'o') %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + geom_point(alpha = .01) + 
  stat_ellipse(level = 0.67) +
  scale_x_reverse() + scale_y_reverse() 

ex_formant_data %>% 
  filter(phoneme == 'a' | phoneme == "schwa") %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + geom_point(alpha = .01) + 
  stat_ellipse(level = 0.67) +
  scale_x_reverse() + scale_y_reverse() 

ex_formant_data %>% 
  filter(phoneme == 'u') %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + geom_point(alpha = .01) + 
  stat_ellipse(level = 0.67) +
  scale_x_reverse() + scale_y_reverse() 

ex_formant_data %>% 
  ggplot(aes(x = f2e, y = f1e, color = language, shape = phoneme)) + geom_point(alpha = .5) + 
  stat_ellipse(level = 0.67) +
  scale_x_reverse() + scale_y_reverse() 


ex = ex_formant_data %>% 
  separate(file_name_append, into = c("participant", "file_r", sep = "_"))

unique(ex$participant)

### Production ids 

eng_part_2 = read.csv(here("data", "raw", "eng_2.csv")) %>% 
  filter(status == "APPROVED")

span_part_2 = read.csv(here("data", "raw", "span_2.csv")) %>% 
  filter(status == "APPROVED")

eng_1 = prod_ids %>% 
  filter(prolific_id %in% eng_part_2$participant_id | prolific_id %in% enl1$participant) %>% 
  select(prolific_id, rec_session_id) %>% 
  mutate(l1 = "English")

span_1 = prod_ids %>% 
  filter(prolific_id %in% span_part_2$participant_id | prolific_id %in% spl1$participant) %>% 
  select(prolific_id, rec_session_id) %>% 
  mutate(l1 = "Spanish")

groups = rbind(eng_1, span_1) %>% 
  rename("participant" = rec_session_id)

groups$participant = as.character(groups$participant)

#### Filter join id data to ex df to see how many participants are segmented and also the groups 

df_plot = ex %>% left_join(groups, by = "participant")

df_plot %>% 
  group_by(l1) %>% 
  summarize(n = n())

df_plot_s = df_plot %>% filter(l1 == "Spanish")
length(unique(df_plot_s$participant))

df_plot_s %>% 
  write.csv(here("data", "tidy", "span_bil_data.csv"))


df_plot_e = df_plot %>% filter(l1 == "English")
length(unique(df_plot_e$participant))


df_plot_e %>% 
  write.csv(here("data", "tidy", "eng_bil_data.csv"))

df_plot %>% 
  filter(phoneme == 'i') %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + geom_point(alpha = .01) + 
  stat_ellipse(level = 0.95) +
  scale_x_reverse() + scale_y_reverse() + facet_grid(~l1)

df_plot %>% 
  filter(phoneme == 'o') %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + geom_point(alpha = .01) + 
  stat_ellipse(level = 0.95) +
  scale_x_reverse() + scale_y_reverse() + facet_grid(~l1)

df_plot %>% 
  filter(phoneme == 'a' | phoneme == "schwa") %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + geom_point(alpha = .01) + 
  stat_ellipse(level = 0.95) +
  scale_x_reverse() + scale_y_reverse() + facet_grid(~l1) 

df_plot %>% 
  filter(phoneme == 'u') %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + geom_point(alpha = .01) + 
  stat_ellipse(level = 0.95) +
  scale_x_reverse() + scale_y_reverse() + facet_grid(~l1)


df_plot_mean = df_plot %>% 
  group_by(phoneme, language) %>%
  summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
            meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
  mutate(phoneme_l = paste0(phoneme, "_", language))

df_plot = df_plot %>% 
  mutate(phoneme_l = paste0(phoneme, "_", language))

library(scales)
library(ggdark)

ggplot(data = df_plot_mean, aes(x = meanF2, y = meanF1, color = phoneme_l, 
                                label = phoneme_l)) + 
  geom_label(size = 2, fill = "#333333") + # Font size for vowels
  scale_y_reverse(position = "right", 
                  labels = unit_format(unit = "Hz", sep = ""),
                  breaks = seq(0, 1500, 250)) + 
  scale_x_reverse(position = "top", 
                  labels = unit_format(unit = "Hz", sep = ""),
                  breaks = seq(0, 3000, 500)) + labs(x = "F2\n",
       y = "F1\n",
       title = "Final plot (C)") + 
  stat_ellipse(data = df_plot, aes(x = f2e, y = f1e), type = "norm") +
  coord_cartesian(xlim = c(3000, 200), 
                  ylim = c(1000, 100)) +
  dark_theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 13),
        panel.background = element_rect(fill = "#333333"),
        plot.background = element_rect(fill = "#333333", color = "#333333"),
        panel.grid.major = element_line(color = "#5b5b5b"),
        panel.grid.minor = element_line(color = "#5b5b5b"))





boxplot.stats(df_plot$f2e)$out

out <- boxplot.stats(df_plot$f1e)$out
out_ind <- which(df_plot$f1e %in% c(out))


new_df = df_plot[-out_ind, ]

new_df_mean = new_df %>% 
  group_by(phoneme, language) %>%
  summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
            meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
  mutate(phoneme_l = paste0(phoneme, "_", language))


ggplot(data = new_df_mean, aes(x = meanF2, y = meanF1, color = phoneme_l, 
                                     label = phoneme_l)) + 
  geom_label(size = 2, fill = "#333333") + # Font size for vowels
  scale_y_reverse(position = "right", 
                  labels = unit_format(unit = "Hz", sep = ""),
                  breaks = seq(0, 1500, 250)) + 
  scale_x_reverse(position = "top", 
                  labels = unit_format(unit = "Hz", sep = ""),
                  breaks = seq(0, 3000, 500)) + labs(x = "F2\n",
                                                     y = "F1\n",
                                                     title = "Final plot (C)") + 
  stat_ellipse(data = new_df, aes(x = f2e, y = f1e), type = "norm") +
  coord_cartesian(xlim = c(3000, 200), 
                  ylim = c(1000, 100)) +
  dark_theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 13),
        panel.background = element_rect(fill = "#333333"),
        plot.background = element_rect(fill = "#333333", color = "#333333"),
        panel.grid.major = element_line(color = "#5b5b5b"),
        panel.grid.minor = element_line(color = "#5b5b5b"))



