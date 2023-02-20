# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# This script runs the monolingual Bayesian models, just one each since 
# an interaction phoneme*stim_language is included 
# -------------------------------------------------------

# Source libs ------------------------------------------

source(here::here("scripts", "00_libs.R"))
source(here::here("scripts", "03_load_data.R"))


# English L1 model 

eng_prior = c(
          prior(normal(0, 8), class = Intercept, dpar = mufool),
          prior(normal(0, 8), class = Intercept, dpar = mufought),
          prior(normal(0, 8), class = Intercept, dpar = mufun))


pct_tidy_mono %>% filter(L1 == "English_mono") %>% 
  group_by(phoneme, choice, stim_language) %>% 
  summarize(n = n())

mod_eng_mono <- 
  brm(formula = choice ~ phoneme*stim_language +
        (1 | participant),
      prior = eng_prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy_mono %>% filter(L1 == "English_mono"),
      file = here("data", "models", "ns_eng_mono.rds"))

span_prior = c(prior(normal(0, 8), class = Intercept, dpar = muson),
          prior(normal(0, 8), class = Intercept, dpar = musu))


mod_span_mono <- 
  brm(formula = choice ~ phoneme*stim_language +
        (1 | participant),
      prior = span_prior, 
      warmup = 1000, iter = 2000, chains = 4, 
      family = categorical(link = "logit"), 
      cores = parallel::detectCores(), 
      control = list(adapt_delta = 0.99, max_treedepth = 15), 
      data = pct_tidy_mono %>% filter(L1 == "Spanish_mono"),
      file = here("data", "models", "ns_span_mono.rds"))

