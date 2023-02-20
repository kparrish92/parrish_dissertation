# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# This script runs 4 Bayesian Multinomial models  
# 2 per bilingual group (1 in French and one in German)
# -------------------------------------------------------


# load data 
source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "03_load_data.R"))


prior = c(prior(normal(0, 8), class = Intercept, dpar = mufin),
          prior(normal(0, 8), class = Intercept, dpar = mufool),
          prior(normal(0, 8), class = Intercept, dpar = mufought),
          prior(normal(0, 8), class = Intercept, dpar = mufun),
          prior(normal(0, 8), class = Intercept, dpar = muson),
          prior(normal(0, 8), class = Intercept, dpar = musu))



# English bilingual model French
mod_eng <- 
  brm(formula = choice ~ phoneme +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = english_l1_pct,
      file = here("data", "models", "ns_eng.rds"))


# English bilingual model German
mod_eng_g <- 
  brm(formula = choice ~ phoneme +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = english_l1_pct_g,
      file = here("data", "models", "ns_eng_g.rds"))


mod_span <- 
  brm(formula = choice ~ phoneme +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = spanish_l1_pct,
      file = here("data", "models", "ns_span.rds"))

mod_span_g <- 
  brm(formula = choice ~ phoneme +
        (1 | participant),
      prior = prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = spanish_l1_pct_g,
      file = here("data", "models", "ns_span_g.rds"))

