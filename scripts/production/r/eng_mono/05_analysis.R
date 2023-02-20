source(here::here("scripts", "r", "eng_mono", "0_libs.R"))
source(here::here("scripts", "r", "eng_mono", "03_load_data.R"))


df1 = eng_bil_df %>% 
  select(f1e, f2e, language, phoneme) %>% 
  mutate(group = "English bilingual")

df2 =eng_mono_df %>% 
  select(f1e, f2e, language, phoneme) %>% 
  mutate(group = "English monolingual")

df3 = span_bil_df %>% 
  select(f1e, f2e, language, phoneme) %>% 
  mutate(group = "Spanish bilingual")

all_df = rbind(df1,df2,df3)

conditional_effects(mod_b)


mod_b = brm(f1e ~ group*phoneme*language, data = all_df)

mod_b_f2 = brm(f1e ~ group*phoneme*language, data = all_df)
