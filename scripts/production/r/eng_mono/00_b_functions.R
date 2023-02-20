

plot_vowels = function(df)
{
df_plot_mean = df %>% 
  group_by(phoneme, language) %>%
  summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
            meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
  mutate(phoneme_l = paste0(phoneme, "_", language))

df = df %>% 
  mutate(phoneme_l = paste0(phoneme, "_", language))


plot = ggplot(data = df_plot_mean, aes(x = meanF2, y = meanF1, color = phoneme, 
                                label = phoneme_l)) + 
  geom_point() +
  geom_text_repel(size = 3) +
  scale_y_reverse(position = "right", 
                  labels = unit_format(unit = "Hz", sep = ""),
                  breaks = seq(0, 1500, 250)) + 
  scale_x_reverse(position = "top", 
                  labels = unit_format(unit = "Hz", sep = ""),
                  breaks = seq(0, 3000, 500)) + labs(x = "F2\n",
                                                     y = "F1\n",
                                                     title = "Final plot (C)") + 
  stat_ellipse(data = df, aes(x = f2e, y = f1e), type = "norm", alpha = .4, 
               linetype = "dashed") +
  coord_cartesian(xlim = c(3500, 0), 
                  ylim = c(1250, 0)) +
  dark_theme_gray() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5), 
        text = element_text(size = 13),
        panel.background = element_rect(fill = "#333333"),
        plot.background = element_rect(fill = "#333333", color = "#333333"),
        panel.grid.major = element_line(color = "#5b5b5b"),
        panel.grid.minor = element_line(color = "#5b5b5b"))

return(plot)
}

e_b = plot_vowels(eng_bil_df)


e_m = plot_vowels(eng_mono_df)


s_b = plot_vowels(span_bil_df)

ggarrange(e_b, e_m)

df_1 = eng_bil_df
df_2 = eng_mono_df
vowel = "i"

compare_vowels = function(df_1, df_2, vowel)
{
  df_plot_mean_1 = df_1 %>% 
    group_by(phoneme, language) %>%
    summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
              meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    mutate(group = deparse(substitute(df_1))) 
  
  
  df_plot_mean_2 = df_2 %>% 
    group_by(phoneme, language) %>%
    summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
              meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    mutate(group = deparse(substitute(df_2)))
  
  df_1 = df_1 %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    select(f1e, f2e, language, phoneme) %>% 
    mutate(group = deparse(substitute(df_1)))
  
  df_2 = df_2 %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    select(f1e, f2e, language, phoneme) %>% 
    mutate(group = deparse(substitute(df_2)))
  
    
  df_plot = rbind(df_1, df_2)  %>% 
  mutate(phoneme_l = paste0(phoneme, "_", language)) 
  df_plot_mean = rbind(df_plot_mean_1, df_plot_mean_2)
  
  plot = df_plot_mean %>% 
    filter(phoneme == vowel) %>% 
    ggplot(aes(x = meanF2, y = meanF1, color = group, 
                                         label = phoneme_l)) + 
    geom_point(size = 1) +
    geom_text_repel(size = 2) +
    scale_y_reverse(position = "right", 
                    labels = unit_format(unit = "Hz", sep = ""),
                    breaks = seq(0, 1500, 250)) + 
    scale_x_reverse(position = "top", 
                    labels = unit_format(unit = "Hz", sep = ""),
                    breaks = seq(0, 3000, 500)) + labs(x = "F2\n",
                                                       y = "F1\n",
                                                       title = "Final plot (C)") + 
    stat_ellipse(data = df_plot, aes(x = f2e, y = f1e), type = "norm", alpha = .4, 
                 linetype = "dashed") +
    coord_cartesian(xlim = c(3500, 0), 
                    ylim = c(1250, 0)) +
    dark_theme_gray() +
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 13),
          panel.background = element_rect(fill = "#333333"),
          plot.background = element_rect(fill = "#333333", color = "#333333"),
          panel.grid.major = element_line(color = "#5b5b5b"),
          panel.grid.minor = element_line(color = "#5b5b5b"))
  
  return(plot)
}




compare_vowels_all = function(df_1, df_2)
{
  df_plot_mean_1 = df_1 %>% 
    group_by(phoneme, language) %>%
    summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
              meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    mutate(group = deparse(substitute(df_1))) 
  
  
  df_plot_mean_2 = df_2 %>% 
    group_by(phoneme, language) %>%
    summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
              meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    mutate(group = deparse(substitute(df_2)))
  
  df_1 = df_1 %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    select(f1e, f2e, language, phoneme) %>% 
    mutate(group = deparse(substitute(df_1)))
  
  df_2 = df_2 %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    select(f1e, f2e, language, phoneme) %>% 
    mutate(group = deparse(substitute(df_2)))
  
  
  df_plot = rbind(df_1, df_2)  %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) 
  df_plot_mean = rbind(df_plot_mean_1, df_plot_mean_2)
  
  plot = df_plot_mean %>% 
    ggplot(aes(x = meanF2, y = meanF1, color = phoneme, 
               label = phoneme_l, group = phoneme, shape = group)) + 
    geom_point(size = 1) +
    geom_text_repel(size = 2) +
    scale_y_reverse(position = "right", 
                    labels = unit_format(unit = "Hz", sep = ""),
                    breaks = seq(0, 1500, 250)) + 
    scale_x_reverse(position = "top", 
                    labels = unit_format(unit = "Hz", sep = ""),
                    breaks = seq(0, 3000, 500)) + labs(x = "F2\n",
                                                       y = "F1\n",
                                                       title = "Final plot (C)") + 
    stat_ellipse(data = df_plot, aes(x = f2e, y = f1e), type = "norm", alpha = .4, 
                 linetype = "dashed") +
    coord_cartesian(xlim = c(3500, 0), 
                    ylim = c(1250, 0)) +
    dark_theme_gray() +
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 13),
          panel.background = element_rect(fill = "#333333"),
          plot.background = element_rect(fill = "#333333", color = "#333333"),
          panel.grid.major = element_line(color = "#5b5b5b"),
          panel.grid.minor = element_line(color = "#5b5b5b"))
  
  return(plot)
}




compare_vowels_all_3 = function(df_1, df_2, df_3)
{
  df_plot_mean_1 = df_1 %>% 
    group_by(phoneme, language) %>%
    summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
              meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    mutate(group = deparse(substitute(df_1))) 
  
  
  df_plot_mean_2 = df_2 %>% 
    group_by(phoneme, language) %>%
    summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
              meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    mutate(group = deparse(substitute(df_2)))
  
  df_plot_mean_3 = df_3 %>% 
    group_by(phoneme, language) %>%
    summarize(meanF2 = mean(f2e), sdf2 = sd(f2e), 
              meanF1 = mean(f1e), sdf1 = sd(f1e)) %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    mutate(group = deparse(substitute(df_3)))
  
  df_1 = df_1 %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    select(f1e, f2e, language, phoneme) %>% 
    mutate(group = deparse(substitute(df_1)))
  
  df_2 = df_2 %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    select(f1e, f2e, language, phoneme) %>% 
    mutate(group = deparse(substitute(df_2)))
  
  df_3 = df_3 %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) %>% 
    select(f1e, f2e, language, phoneme) %>% 
    mutate(group = deparse(substitute(df_3)))
  
  
  df_plot = rbind(df_1, df_2, df_3)  %>% 
    mutate(phoneme_l = paste0(phoneme, "_", language)) 
  df_plot_mean = rbind(df_plot_mean_1, df_plot_mean_2, df_plot_mean_3)
  
  plot = df_plot_mean %>% 
    ggplot(aes(x = meanF2, y = meanF1, color = phoneme_l, 
               label = phoneme_l, group = phoneme_l, shape = group)) + 
    geom_point(size = 1) +
    geom_text_repel(size = 2) +
    scale_y_reverse(position = "right", 
                    labels = unit_format(unit = "Hz", sep = ""),
                    breaks = seq(0, 1500, 250)) + 
    scale_x_reverse(position = "top", 
                    labels = unit_format(unit = "Hz", sep = ""),
                    breaks = seq(0, 3000, 500)) + labs(x = "F2\n",
                                                       y = "F1\n",
                                                       title = "Final plot (C)") + 
    stat_ellipse(data = df_plot, aes(x = f2e, y = f1e), type = "norm", alpha = .4, 
                 linetype = "dashed", level = .67) +
    coord_cartesian(xlim = c(3500, 0), 
                    ylim = c(1250, 0)) +
    dark_theme_gray() +
    theme(plot.title = element_text(hjust = 0.5), 
          text = element_text(size = 13),
          panel.background = element_rect(fill = "#333333"),
          plot.background = element_rect(fill = "#333333", color = "#333333"),
          panel.grid.major = element_line(color = "#5b5b5b"),
          panel.grid.minor = element_line(color = "#5b5b5b"))
  
  return(plot)
}