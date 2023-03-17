source(here::here("scripts", "both", "00_libs.R"))
source(here::here("scripts", "both", "01_helpers.R"))
source(here::here("scripts", "both", "03_load_data.R"))

span_mod = readRDS(here("data", "both", "models", "span_both.rds"))
eng_mod = readRDS(here("data", "both", "models", "span_eng.rds"))

## Spanish descriptive plot
comb_df_filt %>% 
  mutate(language_chosen = case_when(
    language_chosen == "eng" ~ "English",
    language_chosen == "span" ~ "Spanish",
  )) %>% 
  filter(L1 == "Spanish") %>% 
  ggplot(aes(x = language_chosen, fill = language_chosen)) + 
  geom_bar(color = "black", position = position_dodge()) + 
  theme_minimal() +
  xlab("Language Chosen") + 
  scale_fill_discrete(name = "Language chosen") +
  theme(legend.position = "bottom") +
  facet_grid(mode~phoneme) +
  ggsave(here("sections", "figs", "both_span_desc.png"))

## English descriptive plot
comb_df_filt %>% 
  mutate(language_chosen = case_when(
    language_chosen == "eng" ~ "English",
    language_chosen == "span" ~ "Spanish",
  )) %>% 
  filter(L1 == "English") %>% 
  ggplot(aes(x = language_chosen, fill = language_chosen)) + 
  geom_bar(color = "black", position = position_dodge()) + 
  theme_minimal() +
  xlab("Language Chosen") + 
  scale_fill_discrete(name = "Language chosen") +
  theme(legend.position = "bottom") +
  facet_grid(mode~phoneme) +
  ggsave(here("sections", "figs", "both_eng_desc.png"))



comb_dg_span = comb_df_filt %>% 
  data_grid(phoneme, mode) %>% 
  add_fitted_draws(span_mod, dpar = TRUE, category = "language_chosen",
                   re_formula = NA)

table = conditional_effects(span_mod)[["phoneme:mode"]] %>% 
  rename(".value" = "estimate__") %>% 
  rename("lower" = "lower__") %>% 
  rename("upper" = "upper__") 


comb_dg_span %>% 
  ggplot(aes(x = .value, fill = mode)) +
  stat_halfeye(slab_colour = "black", slab_alpha =  .8) +
  scale_size_continuous(guide = "none") +
  theme_minimal() +
  xlab("Probability of choosing a Spanish category") + ylab("") + 
  theme(legend.position = "bottom") +
  theme(legend.key = element_rect(fill = "white", colour = "black")) +
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank()) +
  guides(fill = guide_legend("Language", 
                             override.aes = list(linetype = 0, 
                                                 shape = NA))) +
  geom_text(data = mutate_if(table, is.numeric, round, 2),
            aes(y = 1.5, label = paste0(`.value`, " [", `lower`, " - ", `upper`, "]")), 
            hjust = .5, vjust = 2, size = 2.5, family = "sans") +
  facet_wrap(~phoneme, ncol = 1) +
  ggsave(here("sections", "figs", "both_span_mod.png"))


comb_dg_eng = comb_df_filt %>% 
  data_grid(phoneme, mode) %>% 
  add_fitted_draws(eng_mod, dpar = TRUE, category = "language_chosen",
                   re_formula = NA)

table_eng = conditional_effects(eng_mod)[["phoneme:mode"]] %>% 
  rename(".value" = "estimate__") %>% 
  rename("lower" = "lower__") %>% 
  rename("upper" = "upper__") 


comb_dg_eng %>% 
  ggplot(aes(x = .value, fill = mode)) +
  stat_halfeye(slab_colour = "black", slab_alpha =  .8) +
  scale_size_continuous(guide = "none") +
  theme_minimal() +
  xlab("Probability of choosing a Spanish category") + ylab("") + 
  theme(legend.position = "bottom") +
  theme(legend.key = element_rect(fill = "white", colour = "black")) +
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank()) +
  guides(fill = guide_legend("Language", 
                             override.aes = list(linetype = 0, 
                                                 shape = NA))) +
  geom_text(data = mutate_if(table_eng, is.numeric, round, 2),
            aes(y = 1.5, label = paste0(`.value`, " [", `lower`, " - ", `upper`, "]")), 
            hjust = .5, vjust = 2, size = 2.5, family = "sans") +
  facet_wrap(~phoneme, ncol = 1) +
  ggsave(here("sections", "figs", "both_eng_mod.png"))

