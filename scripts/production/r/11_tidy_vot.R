
source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))
source(here::here("scripts", "production", "r", "03_load_data.R"))


### Disqualified participants based on audio quality


dq_vot_qual = c(493453, 499532, 499567)

### Remove participants who had low quality audio/a lot of background noise

vot_data_t = vot_data %>% 
  filter(!participant %in% dq_vot_qual)

vot_data_t_e = vot_data_t %>% 
  filter(group == "English L1")

vot_data_t_s = vot_data_t %>% 
  filter(group == "Spanish L1")

vot_data_t_e %>% 
  mutate(language = case_when(
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish",
    language == "German" ~ "German",
    language == "french" ~ "French"
  )) %>%
  ggplot(aes(x = vot, y = language, fill = language)) + 
  geom_boxplot(outlier.size = 0) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_grid(~participant) +
  ggsave(here("sections", "figs", "eng_vot.png"))

vot_data_t_s %>% 
  mutate(language = case_when(
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish",
    language == "German" ~ "German",
    language == "french" ~ "French"
  )) %>% 
  ggplot(aes(x = vot, y = language, fill = language)) + 
  geom_boxplot(outlier.size = 0) + 
  theme_minimal() +
  theme(legend.position = "none") +
  ggsave(here("sections", "figs", "span_vot.png"))




## Spanish model 
mod_vot = brm(vot ~ language + (language | participant) + (1 | word), 
    data = vot_data_t_s, file = here("models", "production", "sp_vot_slope.rds"))

## English models 
mod_vot_e1 = brm(vot ~ language + (1 | word), 
              data = vot_data_t_e %>% filter(participant == 
                                               "492828"),
              file = here("models", "production", "mod_e_492828.rds"))

mod_492828 = readRDS(here("models", "production", "mod_e_492828.rds"))
round(fixef(mod_492828), digits = 2)

mod_vot_e2 = brm(vot ~ language + (1 | word), 
                 data = vot_data_t_e %>% filter(participant == 
                                                  "493072"),
                 file = here("models", "production", "mod_e_493072.rds"))

mod_493072 = readRDS(here("models", "production", "mod_e_493072.rds"))


mod_vot_e3 = brm(vot ~ language + (1 | word), 
                 data = vot_data_t_e %>% filter(participant == 
                                                  "500265"),
                 file = here("models", "production", "mod_e_500265.rds"))


plot_ind_eng(mod_vot_e1, "492828") +
  ggsave(here("sections", "figs", "e_492828.png"))


plot_ind_eng(mod_vot_e2, "493072") +
  ggsave(here("sections", "figs", "e_493072.png"))


plot_ind_eng(mod_vot_e3, "500265") +
  ggsave(here("sections", "figs", "e_500265.png"))



level_order <- c('English', 'Spanish', 'German', 'French') 


vot_dg = vot_data_t_s %>%
  data_grid(language) %>%
  add_fitted_draws(mod_vot, dpar = TRUE, category = "vot",
                   re_formula = NA) %>% 
  mutate(language = case_when(
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish",
    language == "German" ~ "German",
    language == "french" ~ "French"
  ))


table = conditional_effects(mod_vot)[["language"]] %>% 
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


vot_dg %>% 
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
           label = list(table)) +
  ggsave(here("sections", "figs", "vot_mod.png"))


#### ES plots 
library(bayestestR)
equivalence_test(mod_vot_e1)

equivalence_test(mod_vot_e2)

equivalence_test(mod_vot_e3)



part = "492828"

mod = mod_vot_e1

data_grid_m = vot_data_t_e %>% 
  filter(participant == part) %>%
  data_grid(language) %>%
  add_fitted_draws(mod_vot_e1, dpar = TRUE, category = "vot",
                   re_formula = NA) %>%
  ungroup() %>%
  select(language, .value, .draw) %>%
  pivot_wider(names_from = language, values_from = .value) %>% 
  mutate(spanish_french = spanish - french) %>% 
  mutate(english_german = english - German)


plot_df_m_long = data_grid_m %>% 
  select(spanish_french, english_german) %>% 
  pivot_longer(c(spanish_french, english_german), names_to = "effect", 
               values_to = "VOT_eff")

plot_df_m_long %>% 
  ggplot(aes(x = VOT_eff, y = effect, fill = after_stat(x < 0))) + 
  stat_halfeye() +
  theme_minimal() 



vot_data_t_s %>% 
  mutate(language = case_when(
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish",
    language == "German" ~ "German",
    language == "french" ~ "French"
  )) %>% 
  ggplot(aes(x = vot, y = language, fill = language)) + 
  geom_boxplot(outlier.size = 0) + 
  theme_minimal() +
  theme(legend.position = "none") + 
  facet_wrap(~participant)