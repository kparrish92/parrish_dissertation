# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22 - Updated 4/18/23
# This script runs 4 Bayesian Multinomial models  
# 2 per bilingual group (1 in French and one in German)
# A single model for AX discrimination is also run
# -------------------------------------------------------


# load data 
source(here::here("scripts", "perception", "00_libs.R"))
source(here::here("scripts", "perception", "03_load_data.R"))


prior = c(prior(normal(0, 8), class = Intercept, dpar = mufin),
          prior(normal(0, 8), class = Intercept, dpar = mufool),
          prior(normal(0, 8), class = Intercept, dpar = mufought),
          prior(normal(0, 8), class = Intercept, dpar = mufun),
          prior(normal(0, 8), class = Intercept, dpar = muson),
          prior(normal(0, 8), class = Intercept, dpar = musu))



# English bilingual model French
mod_eng <- 
  brm(formula = choice ~ phoneme + l2_prof_perception +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
   #   control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = english_l1_pct,
      file = here("data", "perception", "models", "ns_eng_prof.rds"))


# English bilingual model German
mod_eng_g <- 
  brm(formula = choice ~ phoneme + l2_prof_perception +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
  #    cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = english_l1_pct_g,
      file = here("data", "perception", "models", "ns_eng_g_prof.rds"))


mod_span <- 
  brm(formula = choice ~ phoneme + l2_prof_perception +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
  #    cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = spanish_l1_pct,
      file = here("data", "perception", "models", "ns_span_prof.rds"))

mod_span_g <- 
  brm(formula = choice ~ phoneme + l2_prof_perception +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
 #     cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = spanish_l1_pct_g,
      file = here("data", "perception", "models", "ns_span_g_prof.rds"))


### Ax models 

mod_ax <- 
  brm(formula = d_prime ~ group*language*cont_type + l2_prof_perception +
        (1 | id),
      warmup = 1000, iter = 4000,
      data = all_df_ax %>% filter(group != "English monolingual"),
      file = here("data", "perception", "models", "all_df_ax"))




