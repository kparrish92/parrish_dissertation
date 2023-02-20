source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))
source(here::here("scripts", "production", "r", "03_load_data.R"))


bil_sub %>% 
  ggplot(aes(x = f2e, y = f1e, color = correct)) + 
  geom_point(alpha = .25) + scale_x_reverse() +
  xlab("F2") + ylab("F1") +
  theme_minimal() + 
  scale_y_reverse() + stat_ellipse(linetype = "dashed") + 
  facet_grid(~phoneme) +
  ggtitle("Force-aligned and hand-corrected vowels") + 
  theme(strip.text.x = element_text(size = 6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        axis.text.y = element_text(size = 6)) +
  ggsave(here("includes", "figures", "hand_correct.png"))

# How many participants of group

## L1 English bil
nrow(unique(full_data %>% filter(group == "L1 English bilingual") %>% 
         select(prolific_id)))
## L1 English mono
nrow(unique(full_data %>% filter(group == "L1 English monolingual") %>% 
              select(prolific_id)))
## L1 Spanish bil
nrow(unique(full_data %>% filter(group == "L1 Spanish bilingual") %>% 
              select(prolific_id)))
## L1 Spanish mono
nrow(unique(full_data %>% filter(group == "L1 Spanish monolingual") %>% 
              select(prolific_id)))


## Ao,aoa, proficiency 


## Vowels 


plot_vowels(full_data %>% filter(group == "L1 Spanish bilingual"))


stim_data = data.frame(phoneme = c("i", "o", "^", "y"),
                       f1e = c(508, 598.39, 601.57, 444.18), 
                       f2e = c(1966.73, 1267.28, 1755.68, 1776.17), 
                       language = "French reference")

stim_data_german = data.frame(phoneme = c("o", "i", "y", "^"),
                              f1e = c(417, 322, 411, 736), 
                              f2e = c(849, 2533, 2053, 1304), 
                              language = "German reference")

choices = rbind(stim_data, stim_data_german)


full_data %>% 
  filter(phoneme == "u") %>%
  ggplot(aes(x = f2e, y = f1e, color = language)) + 
  geom_point(alpha = .1) + scale_x_reverse() +
  xlab("F2") + ylab("F1") +
  theme_minimal() + 
  scale_y_reverse() + stat_ellipse(linetype = "dashed") + 
  ggtitle("Productions of /u/ and /y/") + 
  geom_point(aes(y=444.18, x=1776.17), colour="#7cae00") +
  geom_point(aes(y=411, x=2053), colour="#00BFC4") +
  facet_grid(~group)

full_data %>% 
  filter(phoneme == "o") %>%
  ggplot(aes(x = f2e, y = f1e, color = language)) + 
  geom_point(alpha = .1) + scale_x_reverse() +
  xlab("F2") + ylab("F1") +
  theme_minimal() + 
  scale_y_reverse() + stat_ellipse(linetype = "dashed") + 
  ggtitle("Productions of /o/") + 
  geom_point(aes(y=598, x=1267), colour="#7cae00") +
  geom_point(aes(y=417, x=849), colour="#00BFC4") +
  facet_grid(~group)

full_data %>% 
  filter(phoneme == "i") %>%
  ggplot(aes(x = f2e, y = f1e, color = language)) + 
  geom_point(alpha = .1) + scale_x_reverse() +
  xlab("F2") + ylab("F1") +
  theme_minimal() + 
  scale_y_reverse() + stat_ellipse(linetype = "dashed") + 
  ggtitle("Productions of /i/") + 
  geom_point(aes(y=508, x=1966), colour="#7cae00") +
  geom_point(aes(y=322, x=2533), colour="#00BFC4") +
  facet_grid(~group)

full_data %>% 
  filter(phoneme == "schwa" | phoneme == "a") %>%
  ggplot(aes(x = f2e, y = f1e, color = language)) + 
  geom_point(alpha = .1) + scale_x_reverse() +
  xlab("F2") + ylab("F1") +
  theme_minimal() + 
  scale_y_reverse() + stat_ellipse(linetype = "dashed") + 
  ggtitle("Title") +
  geom_point(aes(y=601, x=1755), colour="#7cae00") +
  geom_point(aes(y=736, x=1304), colour="#00BFC4") +
  facet_grid(~group)

bil_sub %>% 
  filter(correct == "corrected") %>% 
  group_by(phoneme, language) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y = n, x = language, fill = phoneme)) + 
  geom_col(position = position_dodge(width = -.5), color = "black") +
  geom_hline(yintercept = 23.75, linetype = "dashed", color = "black")
