

# load data 
source(here::here("scripts", "perception", "00_libs.R"))
source(here::here("scripts", "perception", "03_load_data.R"))


eng_blp_ax$L1_group <- "English"
span_blp_ax$L1_group <- "Spanish"

plot_blp = rbind(eng_blp_ax, span_blp_ax) %>% 
  mutate(l2_ao = str_replace(l2_ao, "Since birth", "0"),
         l2_ao = str_replace(l2_ao, "Desde el nacimiento", 
                             "0"),
         l2_aoa = str_replace(l2_aoa, 
                              "Tan pronto como recuerdo.", 
                              "0"))


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

ao_ax = plot_blp %>% 
  ggplot(aes(y = l2_ao, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,25) +
  facet_wrap(~L1_group) +
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

aoa_ax = plot_blp %>% 
  ggplot(aes(y = l2_aoa, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,25) +
  facet_wrap(~L1_group) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ylab("Age of Acquisition") + xlab("Count") 


ggarrange(ao_ax, aoa_ax, labels = c('A', 'B')) +
  ggsave(here("sections", "figs", "ao_aoa_combined_ax.png"),
         dpi = 1200)

# Self-rated Production proficiency
spoken = plot_blp %>% 
  ggplot(aes(x = l2_prof_production, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,7) + facet_wrap(~L1_group) +
  ylab("Count") + xlab("Self-rating") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ggtitle("Spoken proficiency") 


mean(eng_blp_ax$l2_prof_production)
sd(eng_blp_ax$l2_prof_production)

mean(span_blp_ax$l2_prof_production) 
sd(span_blp_ax$l2_prof_production)


# Self-rated perception proficiency
perception = plot_blp %>% 
  ggplot(aes(x = l2_prof_perception, fill = L1_group)) + 
  geom_bar(color = "black") + 
  xlim(0,7) + facet_wrap(~L1_group) +
  ylab("Count") + xlab("Self-rating") +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue"),
        legend.position = "none") +
  ggtitle("Perceptual Ability") 

ggarrange(perception, spoken, labels = c('A', 'B')) +
  ggsave(here("sections", "figs", "proficiency_combined_ax.png"),
         dpi = 1200)

