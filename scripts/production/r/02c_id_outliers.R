source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))
source(here::here("scripts", "production", "r", "03_load_data.R"))

### A script to identify the outliers for hand-correction and removal 


outliers = bil_sub %>% 
  group_by(phoneme, language) %>% 
  summarize(mean_f1 = mean(f1e), sd_f1 = sd(f1e),
            mean_f2 = mean(f2e), sd_f2 = sd(f2e),
            n = n()) %>% 
  mutate(out_f1_upper = (mean_f1 + sd_f1*2)) %>% 
  mutate(out_f2_upper = (mean_f2 + sd_f2*2)) %>% 
  mutate(out_f1_lower = (mean_f1 - sd_f1*2)) %>% 
  mutate(out_f2_lower = (mean_f2 - sd_f2*2))

df = full_data_all 

df_f <- character()
for(thisRun in 1:nrow(outliers))
{
  df_out = df %>% 
    filter(phoneme == outliers$phoneme[thisRun] & 
             language == outliers$language[thisRun]) %>% 
    filter(f1e > outliers$out_f1_upper[thisRun] | 
             f1e < outliers$out_f1_lower[thisRun] 
           | f2e > outliers$out_f2_upper[thisRun] | 
             f2e < outliers$out_f2_lower[thisRun])
  df_f <- rbind(df_f, df_out)
}

removed_data = df %>%
  filter(X.1 %in% df_f$X.1)

removed_data %>% 
  group_by(language, phoneme) %>% 
  summarize(n = n())

df %>% 
  group_by(group) %>% 
  summarize(n = n())

df %>%
  filter(!X.1 %in% df_f$X.1) %>% 
  write.csv(here("data", "tidy", "full_data_clean.csv"))