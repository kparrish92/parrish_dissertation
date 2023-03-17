source(here::here("scripts", "both", "00_libs.R"))
source(here::here("scripts", "both", "01_helpers.R"))


full_data = read.csv(here("data", "tidy", "full_data_clean.csv")) %>% 
  mutate(language = case_when(
    language == "french" ~ "French",
    language == "German" ~ "German",
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish"
  ))

# Tidy combined df 
comb_df = read.csv(here("data", "both", "combined.csv"))


## Calculate centroid per lang per group per segment

full_data_b = full_data %>%
  mutate(phoneme = case_when(
    phoneme == "i" ~ "i",
    phoneme == "schwa" ~ "schwa_a",
    phoneme == "o" ~ "o",
    phoneme == "u" ~ "u",
    phoneme == "a" ~ "schwa_a"
  )) %>% 
  filter(group != "L1 English monolingual") %>% 
  filter(group != "L1 Spanish monolingual") 

centroid_df_eng = full_data_b %>%
  group_by(language, group, phoneme) %>% 
  summarize(mean_f1_e = mean(f1e),
            mean_f2_e = mean(f2e)) %>% 
  filter(language == "English") %>% 
  filter(group != "L1 English monolingual") %>% 
  filter(group != "L1 Spanish monolingual") %>% 
  select(group, phoneme, mean_f1_e, mean_f2_e)

centroid_df_eng %>% 
  write.csv(here("data", "both", "centroid_df_eng.csv"))


centroid_df_span = full_data_b %>%
  group_by(language, group, phoneme) %>% 
  summarize(mean_f1_s = mean(f1e),
            mean_f2_s = mean(f2e)) %>% 
  filter(language == "Spanish") %>% 
  filter(group != "L1 English monolingual") %>% 
  filter(group != "L1 Spanish monolingual") %>% 
  select(-language)

centroid_df_span %>% 
  write.csv(here("data", "both", "centroid_df_span.csv"))


full_data_g_f = full_data_b %>% 
  filter(language == "German" | language == "French") %>% 
  left_join(centroid_df_eng, by = c("group", "phoneme")) %>%
  left_join(centroid_df_span, by = c("group", "phoneme")) %>% 
  select(language.x, f1e, f2e, prolific_id, text, group, phoneme,
         mean_f1_e, mean_f2_e, mean_f1_s, mean_f2_s, group) %>% 
  mutate(dist_from_e = e_dist(f1e, mean_f1_e,f2e, mean_f2_e)) %>% 
  mutate(dist_from_s = e_dist(f1e, mean_f1_s,f2e, mean_f2_s)) %>% 
  mutate(sim_prob_e = calc_prob(dist_from_e, dist_from_s)) %>% 
  mutate(language_chosen = rbinom(3513, 1, sim_prob_e)) %>% 
  rename("language" = "language.x") %>% 
  write.csv(here("data", "both", "production_data_tidy.csv"))



full_data_g_f %>% 
  ggplot(aes(y = mean_f1_e, x = mean_f2_e, label = phoneme, 
             color = "English centroid")) + 
  scale_x_reverse() + scale_y_reverse() +
  geom_point(aes(y = f1e, x = f2e, color = phoneme, alpha = .001)) +
  geom_label() +
  geom_label(aes(y = mean_f1_s, x = mean_f2_s, label = phoneme, 
                 color = "Spanish centroid")) +
  theme_minimal() + guides(fill = guide_legend(
      title = "Legend Title",
      override.aes = aes(label = ""))) +
  xlab("F1") + ylab("F2") +
  facet_grid(language~group) + ggsave(here("sections",
                                             "figs",
                                             "centroid.png"))



full_data_g_f %>% 
  ggplot(aes(y = mean_f1_e, x = mean_f2_e, label = phoneme, 
             color = "English centroid")) + 
  scale_x_reverse() + scale_y_reverse() +
  geom_label() +
  geom_label(aes(y = mean_f1_s, x = mean_f2_s, label = phoneme, 
                 color = "Spanish centroid")) +
  labs(color = NULL) +
  theme_minimal() + guides(fill = guide_legend(
    title = "Legend Title",
    override.aes = aes(label = "")))

full_data_g_f %>%
  ggplot(aes(y = mean_f1_e, x = mean_f2_e, color = phoneme))

full_data_g_f = full_data_g_f %>% 
  filter(prolific_id %in% did_both$prolific_id)

e_cent = full_data_g_f %>%
  group_by(phoneme, group) %>% 
  summarize(f1 = mean(mean_f1_e),
            f2 = mean(mean_f2_e)) %>% 
  mutate(Language = "English")
  
s_cent = full_data_g_f %>%
  group_by(phoneme, group) %>% 
  summarize(f1 = mean(mean_f1_s),
            f2 = mean(mean_f2_s)) %>% 
  mutate(Language = "Spanish")

cent_df = rbind(e_cent, s_cent)

cent_df %>%   
  ggplot(aes(y = f1, x = f2, label = phoneme)) +
  geom_point(data = full_data_g_f, aes(y = f1e, x = f2e,
                                       color = phoneme), alpha = .2, 
             show.legend = F) +
  scale_x_reverse() + scale_y_reverse() +
  theme_minimal() + 
  geom_label(aes(color = Language), size = 3, show.legend = F) + 
  facet_grid(phoneme~group) +
  ggsave(here("sections",
                "figs",
                "centroid.png"))


  
