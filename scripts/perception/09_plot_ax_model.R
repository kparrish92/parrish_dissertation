library(tidybayes)
library(modelr)
library(peRspective)
library(bayestestR)
# load data
source(here::here("scripts", "perception", "00_libs.R"))

all_df_ax = read.csv(here("data", "tidy", "all_ax.csv"))
ax_mod = readRDS(here("data", "perception", "models", "all_df_ax.rds"))

graph_df = all_df_ax %>% 
  filter(group != "English monolingual") %>% 
  data_grid(group,language, cont_type, l2_prof_perception) %>%
  add_fitted_draws(ax_mod, dpar = TRUE, category = "d_prime",
                   re_formula = NA)

## Plot posterior distritbution after math
graph_df %>%
  ggplot(aes(y = group, x = .value, fill = language)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  theme_minimal() +
  xlab("D Prime") + ylab("Condition") + 
  theme(legend.position = "bottom") +
  ggsave(here("sections", "figs", "ax-post.png"))




graph_df %>% 
  group_by(group, language) %>% 
  summarize(mean_p = mean(.value),
            hdi_lo = hdi(.value)[1],
            hdi_hi = hdi(.value)[2]) %>% 
  write.csv(here("data", "tidy", "ax_mod_report_df.csv"))
  

## Create model table 

describe_posterior(ax_mod, rope_range = c(-0.1, 0.1)) %>% 
  as_tibble() %>% 
  select(-c("CI", "ROPE_CI", "ROPE_low", "ROPE_high")) %>% 
  mutate(Parameter = case_when(
    Parameter == "b_Intercept" ~ "Intercept",                                                              
    Parameter == "b_groupSpanish" ~ "L1 Spanish",                                                            
    Parameter == "b_languageGerman" ~ "German",                                                          
    Parameter == "b_cont_typei" ~ "i",
    Parameter == "b_l2_prof_perception" ~ "Proficiency",
    Parameter == "b_groupSpanish:languageGerman" ~ "L1 Spanish:German",
    Parameter == "b_groupSpanish:cont_typei" ~ "L1 Spanish:i",
    Parameter == "b_languageGerman:cont_typei" ~ "German:o",
    Parameter == "b_groupSpanish:languageGerman:cont_typei" ~ "L1 Spanish:German:i"
  )) %>% 
  mutate(across(-c("Parameter", "ESS"), specify_decimal, k = 2)) %>% 
  mutate(ESS = round(ESS)) %>% 
  mutate(HDI = glue::glue("[{CI_low}, {CI_high}]")) %>% 
  select(Parameter, Median, HDI, `% in ROPE` = ROPE_Percentage, MPE = pd, Rhat, ESS) %>% 
  write.csv(here("data", "tidy", "ax_mod_report.csv"))



## ES plot 
graph_df %>%
  ggplot(aes(y = "", x = .value, fill = group)) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  theme_minimal() +
  xlab("D Prime") + ylab("Condition") + 
  theme(legend.position = "bottom") 


e = graph_df %>% filter(group == "English")
s = graph_df %>% filter(group == "Spanish")

es = data.frame(es = e$.value - s$.value, lab = "df") %>% 
  mutate(is_positve = case_when(
    es > 0 ~ 1,
    es < 0 ~ 0
))

## ES plot 
es %>%
  ggplot(aes(y = "", x = es, fill = after_stat(x < 0))) +
  stat_halfeye(alpha = .5) +
  scale_size_continuous(guide = "none") +
  theme_minimal() +
  xlab("D Prime") + ylab("Condition") + 
  theme(legend.position = "bottom")) 


