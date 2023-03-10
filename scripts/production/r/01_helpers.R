#### Need to tidy each full dataset and the subsets 


#### Make this a function 

match_files = function(folder)
{
path = paste0("/Users/kyleparrish/Documents/GitHub/ru_dissertation_parrish/production_files/",
                folder,"/")  
file_names  <- list.files(path= here("production_files", 
                                     paste0(folder)),
                          recursive=T,
                          pattern=".wav"
                          ,full.names=T) %>% 
  as.data.frame() %>% 
  rename(file_name_append = ".") %>% 
  mutate(file_name_append = str_remove(file_name_append, path)) 
return(file_names)                                      
}




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



make_matrix = function(mod)
{  
  mod_ex = mod %>% 
    as.data.frame()
  
  rr = abs(rope_range(mod)[1])
  
  cond_effects = mod_ex %>% 
    mutate("eng_l1_fre" = b_Intercept + b_languageFrench,
           "eng_l1_eng" = b_Intercept,
           "eng_l1_ger" = b_Intercept + b_languageGerman,
           "eng_l1_spa" = b_Intercept + b_languageSpanish,
           "span_l1_fre" = b_Intercept + b_languageFrench + 
             b_groupL1Spanishbilingual +
             `b_languageFrench:groupL1Spanishbilingual`,
           "span_l1_eng" = b_Intercept + b_groupL1Spanishbilingual,
           "span_l1_ger" = b_Intercept + b_languageGerman + 
             b_groupL1Spanishbilingual +
             `b_languageGerman:groupL1Spanishbilingual`,
           "span_l1_spa" = b_Intercept + b_languageSpanish +
             b_groupL1Spanishbilingual + 
             `b_languageSpanish:groupL1Spanishbilingual`) %>% 
    select(eng_l1_fre:span_l1_spa)
  
  #df_est = conditional_effects(mod)["language:group"] %>%
  #  as.data.frame()
  
  df = conditional_effects(mod)["language:group"] %>%
    as.data.frame() %>% 
    mutate(lang_type = case_when(
      language.group.effect1__ == "English" & 
        language.group.effect2__ == "L1 English bilingual" ~ "eng_l1_eng",
      language.group.effect1__ == "Spanish" & 
        language.group.effect2__ == "L1 English bilingual" ~ "eng_l1_spa",
      language.group.effect1__ == "German" & 
        language.group.effect2__ == "L1 English bilingual" ~ "eng_l1_ger",
      language.group.effect1__ == "French" & 
        language.group.effect2__ == "L1 English bilingual" ~ "eng_l1_fre",
      language.group.effect1__ == "English" & 
        language.group.effect2__ == "L1 Spanish bilingual" ~ "span_l1_eng",
      language.group.effect1__ == "Spanish" & 
        language.group.effect2__ == "L1 Spanish bilingual" ~ "span_l1_spa",
      language.group.effect1__ == "German" & 
        language.group.effect2__ == "L1 Spanish bilingual" ~ "span_l1_ger",
      language.group.effect1__ == "French" & 
        language.group.effect2__ == "L1 Spanish bilingual" ~ "span_l1_fre"
    )) %>% 
    select(lang_type, language.group.estimate__) %>% 
    mutate(upper = language.group.estimate__ + rr, 
           lower = language.group.estimate__ - rr)
  
  
  #df_f <- character()
  #for(thisRun in 1:nrow(list_of_files))
  #{
  #  df <- read_textgrid(list_of_files$.[thisRun]) %>%
  #    add_words()
  #  df_f <- rbind(df_f, df)  
  # }
  # Example working loop
  
  df_p <- character()
  for(i in 1:nrow(df))
  {
    df$language.group.estimate__[i] + rr
    df$language.group.estimate__[i] - rr
    
    mdf1 = data.frame(n = nrow(cond_effects %>% filter(eng_l1_eng > df$language.group.estimate__[i] - rr 
                                                       & eng_l1_eng < df$language.group.estimate__[i] + rr)),
                      comp = "eng_l1_eng",
                      comp2 = df$lang_type[i])
    
    mdf2 = data.frame(n = nrow(cond_effects %>% filter(eng_l1_fre > df$language.group.estimate__[i] - rr 
                                                       & eng_l1_fre < df$language.group.estimate__[i] + rr)),
                      comp = "eng_l1_fre",
                      comp2 = df$lang_type[i])
    
    mdf3 = data.frame(n = nrow(cond_effects %>% filter(eng_l1_ger > df$language.group.estimate__[i] - rr 
                                                       & eng_l1_ger < df$language.group.estimate__[i] + rr)),
                      comp = "eng_l1_ger",
                      comp2 = df$lang_type[i])
    
    mdf4 = data.frame(n = nrow(cond_effects %>% filter(eng_l1_spa > df$language.group.estimate__[i] - rr 
                                                       & eng_l1_spa < df$language.group.estimate__[i] + rr)),
                      comp = "eng_l1_spa",
                      comp2 = df$lang_type[i])
    
    mdf5 = data.frame(n = nrow(cond_effects %>% filter(span_l1_spa > df$language.group.estimate__[i] - rr 
                                                       & span_l1_spa < df$language.group.estimate__[i] + rr)),
                      comp = "span_l1_spa",
                      comp2 = df$lang_type[i])
    
    mdf6 = data.frame(n = nrow(cond_effects %>% filter(span_l1_eng > df$language.group.estimate__[i] - rr 
                                                       & span_l1_eng < df$language.group.estimate__[i] + rr)),
                      comp = "span_l1_eng",
                      comp2 = df$lang_type[i])
    
    mdf7 = data.frame(n = nrow(cond_effects %>% filter(span_l1_ger > df$language.group.estimate__[i] - rr 
                                                       & span_l1_ger < df$language.group.estimate__[i] + rr)),
                      comp = "span_l1_ger",
                      comp2 = df$lang_type[i])
    
    mdf8 = data.frame(n = nrow(cond_effects %>% filter(span_l1_fre > df$language.group.estimate__[i] - rr 
                                                       & span_l1_fre < df$language.group.estimate__[i] + rr)),
                      comp = "span_l1_fre",
                      comp2 = df$lang_type[i])
    
    df_f = rbind(mdf1, mdf2, mdf3, mdf4, mdf5, mdf6, mdf7, mdf8)
    
    df_p <- rbind(df_p, df_f)  
  }
  
  df_pivot = df_p %>% 
    mutate(n = n/4000) %>% 
    pivot_wider(names_from = comp, values_from = n) 
  
  plot = ggplot(df_p, aes(comp, comp2)) +
    geom_tile(aes(fill = n)) + 
    geom_text(aes(label = round(n, 1))) +
    scale_fill_gradient(low = "white", high = "deepskyblue3")
  return(plot)
  
}


# function to add a single word given xmin and xmax of textgrid dfs 
add_word = function(x_min, x_max, df)
{
  library(dplyr)
  # subset
  df <- df %>% filter(x_min <= xmin & x_max >= xmax)
  # mutate
  df$word <- df$text[1]
  # bind
  return(df)  
}  

thisRun = 1

# using 'add_word' function, apply it to all words in df 

add_words <- function(df)
{
  new_df <- character()
  # subset words 
  word_df <-  df %>% 
    dplyr::filter(tier_name == "ORT-MAU")
  # setup loop 
  for(thisRun in 1:nrow(word_df))
  {
    df_n <- add_word(word_df$xmin[thisRun], word_df$xmax[thisRun], df) %>% 
      mutate(duration = word_df$xmax[thisRun] - word_df$xmin[thisRun])
    new_df <- rbind(new_df, df_n)  
  }
  return(new_df)
}


run_ind = function(participant)
{
  # load all  all Textgrid files under "participant_uploads"
  list_of_files <- list.files(path = here("production_files", 
                                          "participant_uploads_bilingual",
                                          participant), recursive = TRUE,
                              pattern = "\\.TextGrid$", 
                              full.names = TRUE) %>% 
    as.data.frame()
  
  library(readtextgrid)
  
  df2 <- character()
  
  for (iteration in 1:nrow(list_of_files)) {
    df <- read_textgrid(list_of_files$.[iteration]) %>% 
      add_words() %>% 
      mutate(file = list_of_files$.[iteration])
    df2 <- rbind(df, df2)
  }
  
  
  vot_df = df2 %>% 
    filter(text == "p_vot") %>% 
    mutate(file = str_remove(file, 
                             "Users/kyleparrish/Documents/GitHub/ru_dissertation_parrish/production_files/participant_uploads_bilingual/")) %>%
    mutate(file = str_remove(file, 
                             "_blockNr_1_taskNr_")) %>%
    mutate(file = str_remove(file, 
                             "_recording.TextGrid")) %>% 
    mutate(file = str_remove(file, 
                             "/")) %>% 
    mutate(file = str_remove(file, 
                             "data_out")) %>% 
    separate(file, into = c("participant", "trail", "other", "f", "language", sep = "_"))
  
  
  vot_df %>% 
    group_by(language) %>% 
    summarize(n = n())
  
  vot_df %>% 
    ggplot(aes(x = duration, y = text, fill = language)) + geom_boxplot()
  return(vot_df)
}


run_ind_plot = function(participant)
{
  # load all  all Textgrid files under "participant_uploads"
  list_of_files <- list.files(path = here("production_files", 
                                          "participant_uploads_bilingual",
                                          participant), recursive = TRUE,
                              pattern = "\\.TextGrid$", 
                              full.names = TRUE) %>% 
    as.data.frame()
  
  library(readtextgrid)
  
  df2 <- character()
  
  for (iteration in 1:nrow(list_of_files)) {
    df <- read_textgrid(list_of_files$.[iteration]) %>% 
      add_words() %>% 
      mutate(file = list_of_files$.[iteration])
    df2 <- rbind(df, df2)
  }
  
  
  vot_df = df2 %>% 
    filter(text == "p_vot") %>% 
    mutate(file = str_remove(file, 
                             "Users/kyleparrish/Documents/GitHub/ru_dissertation_parrish/production_files/participant_uploads_bilingual/")) %>%
    mutate(file = str_remove(file, 
                             "_blockNr_1_taskNr_")) %>%
    mutate(file = str_remove(file, 
                             "_recording.TextGrid")) %>% 
    mutate(file = str_remove(file, 
                             "/")) %>% 
    mutate(file = str_remove(file, 
                             "data_out")) %>% 
    separate(file, into = c("participant", "trail", "other", "f", "language", sep = "_")) 
  
  
  vot_df %>% 
    group_by(language) %>% 
    summarize(n = n())
  
  plot = vot_df %>% 
    ggplot(aes(x = duration, y = text, fill = language)) + geom_boxplot()
  return(plot)
}


plot_ind_eng = function(model, participant) 
{
  vot_dg_e = vot_data_t_e %>%
    filter(participant == participant) %>% 
    data_grid(language) %>%
    add_fitted_draws(model, dpar = TRUE, category = "vot",
                     re_formula = NA) %>% 
    mutate(language = case_when(
      language == "english" ~ "English",
      language == "spanish" ~ "Spanish",
      language == "German" ~ "German",
      language == "french" ~ "French"
    ))
  
  
  table = conditional_effects(model)[["language"]] %>% 
    rename("estimate" = "estimate__") %>% 
    rename("lower" = "lower__") %>% 
    rename("upper" = "upper__") %>% 
    select(language, estimate, lower, upper) %>% 
    mutate(language = case_when(
      language == "english" ~ "English",
      language == "spanish" ~ "Spanish",
      language == "German" ~ "German",
      language == "french" ~ "French"
    ))
  
  table$estimate = round(table$estimate)
  
  table$lower = round(table$lower)
  table$upper = round(table$upper)
  
  plot = vot_dg_e %>% 
    ggplot(aes(x = .value, fill = language)) +
    stat_halfeye(slab_colour = "black", slab_alpha =  .8) +
    scale_size_continuous(guide = "none") +
    theme_minimal() +
    xlab("VOT") + ylab("") + 
    theme(legend.position = "bottom") +
    theme(legend.key = element_rect(fill = "white", colour = "black")) +
    theme(axis.text.y = element_blank(), 
          axis.ticks = element_blank()) +
    guides(fill = guide_legend("Language", 
                               override.aes = list(linetype = 0, 
                                                   shape = NA))) +
    annotate(geom = "table",
             x = Inf,
             y = 2,
             label = list(table))
  
  return(plot)
}  


