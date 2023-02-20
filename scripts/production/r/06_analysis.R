source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))
source(here::here("scripts", "production", "r", "03_load_data.R"))

full_data_adj_bil = full_data %>% 
  mutate(phoneme=ifelse(phoneme=="a",gsub("a","schwa",phoneme),phoneme)) %>% 
  filter(group == "L1 English bilingual" | group == "L1 Spanish bilingual")

  
# English bilingual model French
imod_f1_bil <- 
  brm(formula = f1e ~ language*group +
        (1 | participant),
      warmup = 1000, iter = 2000, chains = 4, 
      cores = parallel::detectCores(), 
      data = full_data_adj_bil %>% filter(phoneme == "i"),
      file = here("data", "production", "models", "i_mod_bil_f1.rds"))


omod_f1_bil <- 
  brm(formula = f1e ~ language*group +
        (1 | participant),
      warmup = 1000, iter = 2000, chains = 4, 
      cores = parallel::detectCores(), 
      data = full_data_adj_bil %>% filter(phoneme == "o"),
      file = here("data", "production", "models", "o_mod_bil_f1.rds"))


wedge_mod_f1_bil <- 
  brm(formula = f1e ~ language*group +
        (1 | participant),
      warmup = 1000, iter = 2000, chains = 4, 
      cores = parallel::detectCores(), 
      data = full_data_adj_bil %>% filter(phoneme == "schwa"),
      file = here("data", "production", "models", "schwa_mod_bil_f1.rds"))


u_mod_f1_bil <- 
  brm(formula = f1e ~ language*group +
        (1 | participant),
      warmup = 1000, iter = 2000, chains = 4, 
      cores = parallel::detectCores(), 
      data = full_data_adj_bil %>% filter(phoneme == "u"),
      file = here("data", "production", "models", "u_mod_bil_f1.rds"))

#######

imod_f2_bil <- 
  brm(formula = f2e ~ language*group +
        (1 | participant),
      warmup = 1000, iter = 2000, chains = 4, 
      cores = parallel::detectCores(), 
      data = full_data_adj_bil %>% filter(phoneme == "i"),
      file = here("data", "production", "models", "i_mod_bil_f2.rds"))


omod_f2_bil <- 
  brm(formula = f2e ~ language*group +
        (1 | participant),
      warmup = 1000, iter = 2000, chains = 4, 
      cores = parallel::detectCores(), 
      data = full_data_adj_bil %>% filter(phoneme == "o"),
      file = here("data", "production", "models", "o_mod_bil_f2.rds"))


wedge_mod_f2_bil <- 
  brm(formula = f2e ~ language*group +
        (1 | participant),
      warmup = 1000, iter = 2000, chains = 4, 
      cores = parallel::detectCores(), 
      data = full_data_adj_bil %>% filter(phoneme == "schwa"),
      file = here("data", "production", "models", "schwa_mod_bil_f2.rds"))


u_mod_f2_bil <- 
  brm(formula = f2e ~ language*group +
        (1 | participant),
      warmup = 1000, iter = 2000, chains = 4, 
      cores = parallel::detectCores(), 
      data = full_data_adj_bil %>% filter(phoneme == "u"),
      file = here("data", "production", "models", "u_mod_bil_f2.rds"))



