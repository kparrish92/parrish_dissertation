source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))

# Get data 
full_data = read.csv(here("data","production", "tidy","full_data_tidy.csv"))

# Get bio data 

bil_bio = read.csv(here("data","production", "raw","data_prod.csv")) %>% 
  filter(!prolific_id == "") %>% 
  select(rec_session_id, prolific_id)
eng_bio = read.csv(here("data","production", "raw","data_prod_eng.csv")) %>% 
  filter(!prolific_id == "") %>% 
  select(rec_session_id, prolific_id)
span_bio = read.csv(here("data","production", "raw","data_prod_span.csv")) %>% 
  filter(!prolific_id == "") %>% 
  select(rec_session_id, prolific_id)

prol_ids = rbind(bil_bio, eng_bio, span_bio) %>% 
  rename("participant" = rec_session_id) %>% 
  write.csv(here("data", "production", "tidy", "prolific_ids.csv"))


full_data_p = full_data %>% 
  left_join(prol_ids, by = "participant")

# load and tidy data 
eng_l1_blp = read.csv(here("data", "perception", "raw", "blp", 
                           "BLP_English-Spanish.csv")) %>% 
  filter(What.is.your.prolific.ID. %in% full_data_p$prolific_id) %>% 
  rename(prolific_id = 
           What.is.your.prolific.ID.) %>% 
  rename(l2_ao = At.what.age.did.you.start.learning.SPANISH.) %>% 
  rename(l2_aoa 
         = 
           At.what.age.did.you.start.to.feel.comfortable.using.SPANISH.) %>% 
  rename(l2_prof_production = How.well.do.you.speak.SPANISH.) %>% 
  rename(l2_prof_perception = How.well.do.you.understand.SPANISH.) %>% 
  select(prolific_id, l2_ao, l2_aoa, l2_prof_production, l2_prof_perception)


span_l1_blp = read.csv(here("data","perception", "raw", "blp", 
                            "BLP_Spanish-English.csv")) %>% 
  filter(Prolific.ID %in% full_data_p$prolific_id) %>%
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

bio_df = rbind(span_l1_blp, eng_l1_blp)

span_l1_blp %>% 
  write.csv(here("data", "tidy", "span_l1_blp.csv"))

eng_l1_blp %>% 
  write.csv(here("data", "tidy", "eng_l1_blp.csv"))


full_data_p_f = full_data %>% 
  left_join(prol_ids, by = "participant") %>% 
  left_join(bio_df, by = "prolific_id") 

# Load bioplots 

plot_blp = read.csv(here("data", "tidy", "plot_blp.csv"))

# How many participants 
full_data_bil = full_data_p_f %>% 
  filter(prolific_id %in% plot_blp$prolific_id) 
  
eng_mono = full_data_p_f %>% 
  filter(group == "L1 English monolingual")

span_mono = full_data_p_f %>% 
  filter(group == "L1 Spanish monolingual")

full_data_all = rbind(full_data_bil, eng_mono, span_mono)

full_data_p_f %>% 
  write.csv(here("data", "tidy", "full_data_wbio.csv"))


full_data_all %>% 
  write.csv(here("data", "tidy", "full_data.csv"))

