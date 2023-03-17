#### the use Euclidian Distance from centroid 
#### of Spanish or English to
#### simulate judgements 

full_data = read.csv(here("data", "tidy", "full_data_clean.csv")) %>% 
  mutate(language = case_when(
    language == "french" ~ "French",
    language == "German" ~ "German",
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish"
  ))

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

centroid_df_span = full_data_b %>%
  group_by(language, group, phoneme) %>% 
  summarize(mean_f1_s = mean(f1e),
            mean_f2_s = mean(f2e)) %>% 
  filter(language == "Spanish") %>% 
  filter(group != "L1 English monolingual") %>% 
  filter(group != "L1 Spanish monolingual") %>% 
  select(-language)



centroid_df_eng %>% 
  ggplot(aes(y = mean_f1_e, x = mean_f2_e, label = phoneme, 
             color = language)) +
  geom_text() + scale_x_reverse() + scale_y_reverse() +
  facet_grid(~group) +
  geom_point(aes(y = f1e, x = f2e, data = full_data_b))

# full_data_g_f %>% 
#  ggplot(aes(y = mean_f1_e, x = mean_f2_e, label = phoneme, 
#             color = "English centroid")) + 
#  scale_x_reverse() + scale_y_reverse() +
#  geom_point(aes(y = f1e, x = f2e, color = phoneme, alpha = .001)) +
#  geom_label() +
#  geom_label(aes(y = mean_f1_s, x = mean_f2_s, label = phoneme, 
#                 color = "Spanish centroid")) +
#  facet_grid(language.x~group) + ggsave(here("sections",
#                                             "figs",
#                                             "centroid.png"))



## Join centroid df to full data 

# Every French and German data point's distance from the Spanish
# or English centroid 

# join by group, phoneme


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
  rename("language" = "language.x")



### rbinom to sim Eng or Span - comb and run model

full_data_g_f %>% 
  filter(phoneme == "schwa_a") %>% 
  ggplot(aes(y = mean_f1_e, x = mean_f2_e, label = phoneme, 
             color = "English centroid")) + 
  scale_x_reverse() + scale_y_reverse() +
  geom_label() +
  geom_point(aes(y = f1e, x = f2e, color = language, alpha = .001)) +
  geom_label(aes(y = mean_f1_s, x = mean_f2_s, label = phoneme, 
                 color = "Spanish centroid")) +
  facet_grid(~group) + ggsave(here("sections",
                                             "figs",
                                             ""))

