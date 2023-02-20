# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# This script tidies the BLP/background data  
# -------------------------------------------------------

source(here::here("scripts", "perception", "00_libs.R"))
source(here::here("scripts", "perception", "03_load_data.R"))

# data 

english_l1_pct = pct_tidy %>% 
  filter(L1 == "English") %>% 
  filter(stim_language == "french")

spanish_l1_pct = pct_tidy %>% 
  filter(L1 == "Spanish") %>% 
  filter(stim_language == "french")


#------------ average age of onset of L2: Eng L1 ---------

# load and tidy data 
eng_l1_blp = read.csv(here("data", "raw", "blp", 
                           "BLP_English-Spanish.csv")) %>% 
  filter(What.is.your.prolific.ID. %in% english_l1_pct$participant) %>% 
  rename(prolific_id = 
           What.is.your.prolific.ID.) %>% 
  rename(l2_ao = At.what.age.did.you.start.learning.SPANISH.) %>% 
  rename(l2_aoa 
         = 
           At.what.age.did.you.start.to.feel.comfortable.using.SPANISH.) %>% 
  rename(l2_prof_production = How.well.do.you.speak.SPANISH.) %>% 
  rename(l2_prof_perception = How.well.do.you.understand.SPANISH.) %>% 
  select(prolific_id, l2_ao, l2_aoa, l2_prof_production, l2_prof_perception)


eng_l1_blp$l2_aoa

# removed eng, not comfortable
eng_nc = nrow(
  eng_l1_blp %>% filter(l2_aoa == "Not yet comfortable."))

eng_l1_blp %>% 
  filter(l2_aoa != "Not yet comfortable.") %>% 
  write.csv(here("data", "tidy", "eng_l1_blp.csv"))

#------------ average age of onset of L2: Span L1 ---------

## 

span_l1_blp = read.csv(here("data", "raw", "blp", 
                            "BLP_Spanish-English.csv")) %>% 
  filter(Prolific.ID %in% spanish_l1_pct$participant) %>%
  rename(l2_ao = 
           X.A.qué.edad.empezó.a.aprender.INGLÉS.) %>% 
  rename(prolific_id = 
           Prolific.ID) %>% 
  rename(l2_aoa 
         = 
           X.A.qué.edad.empezó.a.sentirse.cómodo.usando.INGLÉS.) %>% 
  rename(l2_prof_production = X.Cómo.habla..en.INGLÉS.) %>% 
  rename(l2_prof_perception = X.Cómo.entiende.en.INGLÉS.) %>% 
  select(prolific_id, l2_ao, l2_aoa, l2_prof_production, l2_prof_perception)

span_nc = nrow(
  span_l1_blp %>% filter(l2_aoa == "Aún no me siento cómodo."))

span_l1_blp %>% 
  filter(l2_aoa != "Aún no me siento cómodo.") %>% 
  write.csv(here("data", "tidy", "span_l1_blp.csv"))

removed = cbind(span_nc, eng_nc) %>% 
  as.data.frame() %>% 
  write.csv(here("data", "tidy", "removed.csv"))


#### Tidy BLP for ax 

eng_l1_blp_ax = read.csv(here("data", "raw", "blp", 
                           "BLP_English-Spanish.csv")) %>% 
  filter(What.is.your.prolific.ID. %in% all_df_ax$id) %>% 
  rename(prolific_id = 
           What.is.your.prolific.ID.) %>% 
  rename(l2_ao = At.what.age.did.you.start.learning.SPANISH.) %>% 
  rename(l2_aoa 
         = 
           At.what.age.did.you.start.to.feel.comfortable.using.SPANISH.) %>% 
  rename(l2_prof_production = How.well.do.you.speak.SPANISH.) %>% 
  rename(l2_prof_perception = How.well.do.you.understand.SPANISH.) %>% 
  select(prolific_id, l2_ao, l2_aoa, l2_prof_production, l2_prof_perception)

eng_nc_ax = nrow(
  eng_l1_blp_ax %>% filter(l2_aoa == "Not yet comfortable."))

eng_l1_blp_ax %>% 
  filter(l2_aoa != "Not yet comfortable.") %>% 
  write.csv(here("data", "tidy", "eng_l1_blp_ax.csv"))

span_l1_blp_ax = read.csv(here("data", "raw", "blp", 
                            "BLP_Spanish-English.csv")) %>% 
  filter(Prolific.ID %in% all_df_ax$id) %>%
  rename(l2_ao = 
           X.A.qué.edad.empezó.a.aprender.INGLÉS.) %>% 
  rename(prolific_id = 
           Prolific.ID) %>% 
  rename(l2_aoa 
         = 
           X.A.qué.edad.empezó.a.sentirse.cómodo.usando.INGLÉS.) %>% 
  rename(l2_prof_production = X.Cómo.habla..en.INGLÉS.) %>% 
  rename(l2_prof_perception = X.Cómo.entiende.en.INGLÉS.) %>% 
  select(prolific_id, l2_ao, l2_aoa, l2_prof_production, l2_prof_perception)

span_nc_ax = nrow(
  span_l1_blp %>% filter(l2_aoa == "Aún no me siento cómodo."))

span_l1_blp %>% 
  filter(l2_aoa != "Aún no me siento cómodo.") %>% 
  write.csv(here("data", "tidy", "span_l1_blp_ax.csv"))

cbind(span_nc_ax, eng_nc_ax) %>% 
  as.data.frame() %>% 
  write.csv(here("data", "tidy", "removed_ax.csv"))

