full_data = read.csv(here("data", "tidy", "full_data_clean.csv")) %>% 
  mutate(language = case_when(
    language == "french" ~ "French",
    language == "German" ~ "German",
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish"
  ))


span_blp_ax = read.csv(here("data", "tidy", "span_l1_blp_ax.csv"))
eng_blp_ax = read.csv(here("data", "tidy", "eng_l1_blp_ax.csv"))
span_blp_pct = read.csv(here("data", "perception", "tidy", "span_l1_blp.csv"))
eng_blp_pct = read.csv(here("data", "perception", "tidy", "eng_l1_blp.csv"))
span_blp_prod = read.csv(here("data", "tidy", "span_l1_blp.csv"))
eng_blp_prod = read.csv(here("data", "tidy", "eng_l1_blp.csv"))


all_df_ax # all who did ax 
ef # all who did percep assim task 

span_blp_ax$L1_group <- "Spanish"
span_blp_pct$L1_group <- "Spanish"
span_blp_prod$L1_group <- "Spanish"

eng_blp_ax$L1_group <- "English"
eng_blp_pct$L1_group <- "English"
eng_blp_prod$L1_group <- "English"


plot_blp = rbind(span_blp_ax, eng_blp_ax, span_blp_pct,
                 eng_blp_pct, span_blp_prod, eng_blp_prod) %>% 
  filter(!is.na(l2_ao)) %>%
  mutate(l2_ao = str_replace(l2_ao, "Since birth", "0"),
         l2_ao = str_replace(l2_ao, "Desde el nacimiento", 
                             "0"),
         l2_aoa = str_replace(l2_aoa, 
                              "Tan pronto como recuerdo.", 
                              "0")) %>% 
  filter(l2_aoa != "Not yet comfortable.") %>% 
  filter(l2_aoa != "Aún no me siento cómodo.") %>% 
  filter(l2_aoa != "--") %>% 
  filter(l2_ao != "--") %>% 
  filter(!is.na(l2_aoa))


plot_blp$l2_ao <- factor(plot_blp$l2_ao, 
                         levels = 
                           c("0",
                             "1", 
                             "2", 
                             "3",
                             "4",
                             "5",
                             "6",
                             "7",
                             "8",
                             "9",
                             "10",
                             "11",
                             "12",
                             "13",
                             "14",
                             "15",
                             "16",
                             "17",
                             "18",
                             "19",
                             "20+"))

ao = plot_blp %>% 
  ggplot(aes(y = l2_ao, fill = L1_group)) + 
  geom_bar(color = "black") + 
  facet_wrap(~L1_group) +
  scale_fill_manual(values = c("seagreen", "tan1")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ylab("Age of Onset") + xlab("Count") 



# Age of acquisition

plot_blp$l2_aoa <- factor(plot_blp$l2_aoa, 
                          levels = 
                            c("0",
                              "1", 
                              "2", 
                              "3",
                              "4",
                              "5",
                              "6",
                              "7",
                              "8",
                              "9",
                              "10",
                              "11",
                              "12",
                              "13",
                              "14",
                              "15",
                              "16",
                              "17",
                              "18",
                              "19",
                              "20+"))

aoa = plot_blp %>% 
  ggplot(aes(y = l2_aoa, fill = L1_group)) + 
  geom_bar(color = "black") +
  facet_wrap(~L1_group) +
  scale_fill_manual(values = c("seagreen", "tan1")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ylab("Age of Acquisition") + xlab("Count") 


ggarrange(ao, aoa, labels = c('A', 'B')) +
  ggsave(here("slides", "img", "ao_aoa_up.png"))

# Self-rated Production proficiency
spoken = plot_blp %>% 
  ggplot(aes(x = l2_prof_production, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,7) + facet_wrap(~L1_group) +
  ylab("Count") + xlab("Self-rating") +
  scale_fill_manual(values = c("seagreen", "tan1")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ggtitle("Spoken production") 


mean(eng_blp$l2_prof_production)
sd(eng_blp$l2_prof_production)

mean(span_blp$l2_prof_production) 
sd(eng_blp$l2_prof_production)


# Self-rated perception proficiency
perception = plot_blp %>% 
  ggplot(aes(x = l2_prof_perception, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,7) + facet_wrap(~L1_group) +
  ylab("Count") + xlab("Self-rating") +
  scale_fill_manual(values = c("seagreen", "tan1")) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ggtitle("Spoken reception") 

ggarrange(perception, spoken, labels = c('A', 'B')) +
  ggsave(here("slides", "img", "prof_up.png"))


# production data - vowels 
full_data %>% 
  group_by(group, prolific_id) %>% 
  summarize(n = n()) %>% 
  group_by(group) %>% 
  summarize(n = n())

# pct data 

pct_tidy %>% 
  group_by(L1, participant) %>% 
  summarize(n = n()) %>% 
  group_by(L1) %>% 
  summarize(n = n())

pct_tidy_mono %>% 
  group_by(L1, participant) %>% 
  summarize(n = n()) %>% 
  group_by(L1) %>% 
  summarize(n = n())

# ax task
all_df_ax %>% 
  group_by(id, group) %>% 
  summarize(n = n()) %>% 
  group_by(group) %>% 
  summarize(n = n())


prod_ids = full_data %>% 
  select(group, prolific_id) %>% 
  rename("participant" = prolific_id) %>% 
  rename("L1" = group) 

pct_ids = pct_tidy %>% 
  select(L1, participant) %>% 
  mutate(L1 = case_when(
    L1 == "English" ~ "L1 English bilingual",
    L1 == "Spanish" ~ "L1 Spanish bilingual"
  ))


pct_mo_ids = pct_tidy_mono %>% 
  select(L1, participant) %>% 
  mutate(L1 = case_when(
    L1 == "English_mono" ~ "L1 English monolingual",
    L1 == "Spanish_mono" ~ "L1 Spanish monolingual"
  ))

ax_ids = all_df_ax %>% 
  select(id, group) %>% 
  rename("participant" = id) %>% 
  rename("L1" = group) %>% 
  mutate(L1 = case_when(
    L1 == "English" ~ "L1 English bilingual",
    L1 == "Spanish" ~ "L1 Spanish bilingual",
    L1 == "English monolingual" ~ "L1 English bilingual"
  ))

final_count = rbind(prod_ids, pct_ids, pct_mo_ids, ax_ids) %>% 
  group_by(L1, participant) %>% 
  summarize(n = n()) %>% 
  group_by(L1) %>% 
  summarize(n = n())

final_count %>% 
  write.csv(here("data", "perception", "tidy", "count.csv"))
