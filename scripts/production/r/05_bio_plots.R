source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))
source(here::here("scripts", "production", "r", "03_load_data.R"))


eng_blp$L1_group <- "English"
span_blp$L1_group <- "Spanish"

plot_blp = rbind(eng_blp, span_blp) %>% 
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
  ggsave(here("includes", "figures", "ao_aoa_combined_production.png"),
         dpi = 1200)

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
  ggsave(here("includes", "figures", "proficiency_combined_production.png"),
         dpi = 1200)


plot_blp %>%
  write.csv(here("data", "tidy", "plot_blp.csv"))

