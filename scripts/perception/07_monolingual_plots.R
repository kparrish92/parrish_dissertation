# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# This script creates the monolingual data plots reported in the 
# manuscript
# -------------------------------------------------------
# Source libs ------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "perception", "03_load_data.R"))

# Load monolingual models 
span_mono_mod = read_rds(here("data", "models", "ns_span_mono.rds"))
eng_mono_mod = read_rds(here("data", "models", "ns_eng_mono.rds"))

# Load conditional probability df of bilingual 
pct_tidy_mono %>% 
  filter(L1 == "English_mono") %>% 
  group_by(phoneme, stim_language) %>% 
  summarize(n = n()) 

desc_df_span = pct_tidy_mono %>% 
  filter(L1 == "Spanish_mono") %>% 
  group_by(phoneme, stim_language, choice) %>%
  summarize(n = n(), mean_rating = mean(rating)) %>% 
  mutate(percentage = n/174) 

desc_df_eng_f = pct_tidy_mono %>% 
  filter(L1 == "English_mono") %>% 
  filter(stim_language == "french") %>% 
  group_by(phoneme, stim_language, choice) %>% 
  summarize(n = n(), mean_rating = mean(rating)) %>% 
  mutate(percentage = n/112) 

desc_df_eng_g = pct_tidy_mono %>% 
  filter(L1 == "English_mono") %>% 
  filter(stim_language == "German") %>% 
  group_by(phoneme, stim_language, choice) %>% 
  summarize(n = n(), mean_rating = mean(rating)) %>% 
  mutate(percentage = n/100) 

desc_df_eng = rbind(desc_df_eng_g, desc_df_eng_f)

desc_df_eng %>% 
  write.csv(here("data", "perception", "tidy", "eng_desc_df_mono.csv"))

## Plot 
desc_df_span %>%
  ggplot(aes(x = choice, y = percentage, 
             fill = mean_rating)) +
  geom_col(color = "black") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.text.x = element_text(color="black",
                                   size=7, angle=25)) +
  facet_grid(phoneme~stim_language)

desc_df_eng %>%
  ggplot(aes(x = choice, y = percentage, 
             fill = mean_rating)) +
  geom_col(color = "black") +
  theme(panel.background = element_rect(fill = "white")) +
  theme(axis.text.x = element_text(color="black",
                                   size=7, angle=25)) +
  facet_grid(phoneme~stim_language)

## Table 

desc_df_span %>% 
  write.csv(here("data", "perception", "tidy", "desc_df_span_mono.csv"))

desc_df_span %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()

desc_df_eng %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) %>%
  xtable()

### Plot models 

effects_sp = make_conditions(span_mono_mod, vars = c("stim_language", 
                                                     "phoneme"))
effects_eng = make_conditions(eng_mono_mod, vars = c("stim_language", 
                                                     "phoneme"))

df_sp = conditional_effects(span_mono_mod, categorical = TRUE, 
                            conditions = effects_sp) 

df_eng = conditional_effects(eng_mono_mod, categorical = TRUE, 
                            conditions = effects_eng)

df_sp_plot = df_sp[["phoneme:cats__"]]

df_eng_plot = df_eng[["phoneme:cats__"]]

df_sp_plot %>% 
ggplot(aes(y = estimate__, x = phoneme, fill = cats__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  ylim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue")) + facet_wrap(~stim_language) +
  xlab("Probability")

####



## Plots by phoneme 

cond_df = read.csv(here("data", "perception", "tidy", "cond_prob_df.csv")) %>% 
  dplyr::select(conditional_prob, phoneme, cats__, 
         conditional_prob_lower, conditional_prob_upper, 
         stim_language, L1, language) %>% 
  rename(estimate__ = conditional_prob,
         upper__ = conditional_prob_upper,
         lower__ = conditional_prob_lower) 

cond_df %>% 
  filter(language == "Spanish") %>% 
  filter(phoneme == "i") %>% 
  ggplot(aes(y = estimate__, x = phoneme, fill = cats__)) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  ylim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue")) + facet_grid(L1~stim_language) +
  xlab("Probability") +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, 
                                    size = 1), 
        strip.background = element_rect(color = "black", size = 1))

cond_df %>% 
  filter(language == "Spanish") %>% 
  filter(phoneme == "i") %>% 
  ggplot(aes(x = estimate__, y = phoneme, fill = cats__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue")) + facet_grid(L1~stim_language) +
  xlab("Stimulus Language") +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, 
                                    size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  ylab("L1 Group") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.title=element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=25)) +
  theme(axis.text.y = element_text(face="bold", color="black",
                                   size=14, angle=0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=8),
        legend.position = "bottom")


cond_df %>% 
  filter(language == "English") %>% 
  filter(phoneme == "o") %>% 
  ggplot(aes(x = estimate__, y = phoneme, fill = cats__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue")) + facet_grid(L1~stim_language) +
  xlab("Stimulus Language") +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, 
                                    size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  ylab("L1 Group") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.title=element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=25)) +
  theme(axis.text.y = element_text(face="bold", color="black",
                                   size=14, angle=0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=8),
        legend.position = "bottom")



df_eng_plot %>% 
  filter(phoneme == "o") %>% 
  ggplot(aes(x = estimate__, y = phoneme, fill = cats__)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) + 
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue")) + facet_grid(~stim_language) +
  xlab("Stimulus Language") +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, 
                                    size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  ylab("L1 Group") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.title=element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=25)) +
  theme(axis.text.y = element_text(face="bold", color="black",
                                   size=14, angle=0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=8),
        legend.position = "bottom")


sp_mono = df_sp_plot %>% 
  dplyr::select(estimate__, phoneme, cats__, 
                upper__, lower__, stim_language) %>% 
  mutate(group = "monolingual") 

eng_mono = df_eng_plot %>% 
  dplyr::select(estimate__, phoneme, cats__, 
                upper__, lower__, stim_language) %>% 
  mutate(group = "monolingual") 

eng_mono$stim_language = gsub("french", "French", eng_mono$stim_language)
sp_mono$stim_language = gsub("french", "French", sp_mono$stim_language)


eng_bil = cond_df %>% 
  filter(L1 == "English") %>% 
  filter(language == "English") %>% 
  dplyr::select(estimate__, phoneme, cats__, 
                upper__, lower__, stim_language) %>% 
  mutate(group = "bilingual")

sp_bil = cond_df %>% 
  filter(L1 == "Spanish") %>% 
  filter(language == "Spanish") %>% 
  dplyr::select(estimate__, phoneme, cats__, 
                upper__, lower__, stim_language) %>% 
  mutate(group = "bilingual")


  
rbind(eng_mono, eng_bil) %>% 
  mutate(place = "place") %>% 
  ggplot(aes(x = estimate__, y = place, color = cats__, 
             shape = group)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  position = position_dodge(width = 1)) + 
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue")) + facet_grid(phoneme~stim_language) +
  xlab("Probability") +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, 
                                    size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  ylab("L1 Group") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.title=element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=25)) +
  theme(axis.text.y = element_text(face="bold", color="black",
                                   size=14, angle=0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=8),
        legend.position = "bottom") + 
  ggsave(here("MDPI_template", "figs", "cond_prob_eng.png"), dpi = 300)


rbind(sp_mono, sp_bil) %>%  
  mutate(place = "place") %>% 
  ggplot(aes(x = estimate__, y = place, color = cats__, 
             shape = group)) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  position = position_dodge(width = 1)) + 
  xlim(0,1) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(
          size = 0.05, 
          linetype = 'solid',
          colour = "blue")) + facet_grid(phoneme~stim_language) +
  xlab("Probability") +
  theme(panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = "black", fill = NA, 
                                    size = 1), 
        strip.background = element_rect(color = "black", size = 1)) +
  ylab("L1 Group") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.title=element_blank()) + 
  guides(colour = guide_legend(nrow = 1)) +
  theme(axis.text.x = element_text(color="black",
                                   size=8, angle=25)) +
  theme(axis.text.y = element_text(face="bold", color="black",
                                   size=14, angle=0)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.text=element_text(size=8),
        legend.position = "bottom") +
  ggsave(here("MDPI_template", "figs", "cond_prob_sp.png"), dpi = 300)




eng_cond_df_comb = rbind(eng_mono, eng_bil) %>% 
  write.csv(here("data", "tidy", "eng_cond_df_comb.csv"))

span_cond_df_comb = rbind(sp_mono, sp_bil) %>% 
  write.csv(here("data", "tidy", "span_cond_df_comb.csv"))


