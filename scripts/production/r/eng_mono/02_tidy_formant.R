
source(here::here("scripts", "r", "eng_mono", "0_libs.R"))


ex_formant_data = read.csv(here("data", "tidy", "eng_data.csv")) %>% 
  filter(!f1e == "--undefined--") %>% 
  filter(!f1e == "f1e") %>% 
  rename("file_name_append" = prefix) %>% 
  left_join(., txt_files_words, by = "file_name_append") %>% 
  select(f1e, f2e, language, text, file_name_append, word) %>% 
  separate(word, into = c("consonant", "phoneme"), sep = "_")

ex_formant_data$f1e = as.numeric(ex_formant_data$f1e)
ex_formant_data$f2e = as.numeric(ex_formant_data$f2e)




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



df_plot_mean = ex_formant_data %>% 
  group_by(phoneme, language) %>%
  summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
            meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
  mutate(phoneme_l = paste0(phoneme, "_", language))

df_plot = ex_formant_data %>% 
  mutate(phoneme_l = paste0(phoneme, "_", language))


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
  stat_ellipse(data = df_plot, aes(x = f2e, y = f1e), type = "norm", 
               alpha = .1, linetype = "dashed") +
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


ex_formant_data %>% 
  write.csv(here("data", "tidy", "eng_mono_tidy.csv"))

