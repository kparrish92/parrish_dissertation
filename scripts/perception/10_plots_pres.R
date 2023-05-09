

glimpse(ef)

desc_total = ef %>% 
  group_by(phoneme, L1, stim_language, choice) %>% 
  summarize(n = n(), mean_rating = mean(slider.response))

total_possible = ef %>% 
  group_by(phoneme, L1, stim_language) %>% 
  summarize(total_n = n())

f_df = left_join(desc_total, total_possible, by = c("phoneme", "L1", "stim_language")) %>% 
  mutate(pct = n/total_n) %>% 
  mutate(stim_language = case_when(
    stim_language == "English" ~ "English",
    stim_language == "Spanish" ~ "Spanish",
    stim_language == "french" ~ "French",
    stim_language == "German" ~ "German",
  ))

f_df %>% 
  filter(phoneme == "o") %>% 
  ggplot(aes(x = choice, y = pct, fill = mean_rating)) +  
  geom_col(color = "black") +
  scale_x_discrete(limits=c("fin", "su", "son",
                            "fool", "fought", "fun",
                            "feel")) + 
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  facet_grid(stim_language~L1) +  
  ylim(0,1) +
  geom_hline(yintercept = 1/7, linetype = "dashed") +
  ggtitle("Categorizations of /o/") + 
  ggsave(here("slides", "img", "desc_o.png")) 

unique(f_df$phoneme)

f_df %>% 
  filter(phoneme == "i") %>% 
  ggplot(aes(x = choice, y = pct, fill = mean_rating)) +  
  geom_col(color = "black") +
  scale_x_discrete(limits=c("fin", "su", "son",
                            "fool", "fought", "fun",
                            "feel")) + 
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  facet_grid(stim_language~L1) +  
  ylim(0,1) +
  geom_hline(yintercept = 1/7, linetype = "dashed") +
  ggtitle("Categorizations of /i/") + 
  ggsave(here("slides", "img", "desc_i.png")) 



f_df %>% 
  filter(phoneme == "y") %>% 
  ggplot(aes(x = choice, y = pct, fill = mean_rating)) +  
  geom_col(color = "black") +
  scale_x_discrete(limits=c("fin", "su", "son",
                            "fool", "fought", "fun",
                            "feel")) + 
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  facet_grid(stim_language~L1) +  
  ylim(0,1) +
  geom_hline(yintercept = 1/7, linetype = "dashed") +
  ggtitle("Categorizations of /y/") + 
  ggsave(here("slides", "img", "desc_y.png")) 



f_df %>% 
  filter(phoneme == "schwa") %>% 
  ggplot(aes(x = choice, y = pct, fill = mean_rating)) +  
  geom_col(color = "black") +
  scale_x_discrete(limits=c("fin", "su", "son",
                            "fool", "fought", "fun",
                            "feel")) + 
  theme(panel.background = element_rect(fill = "white"),
        legend.position = "bottom") +
  facet_grid(stim_language~L1) +  
  ylim(0,1) +
  geom_hline(yintercept = 1/7, linetype = "dashed") +
  ggtitle("Categorizations of /^/") + 
  ggsave(here("slides", "img", "desc_schwa.png")) 
