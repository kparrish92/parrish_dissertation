

# load data 
source(here::here("scripts", "perception", "00_libs.R"))
source(here::here("scripts", "perception", "03_load_data.R"))

# English bilingual French categorizations

df = pct_eng_mono %>% 
  filter(stim_language == "french")
 
make_table = function(df)
{
no_total = df %>% 
  group_by(phoneme, stim_language) %>% 
  summarize(n = n())

desc_df = df %>% 
  group_by(phoneme, stim_language, choice) %>% 
  summarize(n = n()) %>% 
  mutate(percentage = n/no_total$n[1]) 

table = desc_df %>% 
  select(choice, phoneme, percentage) %>% 
  pivot_wider(values_from = percentage, names_from = phoneme) 
return(table)
}

pct_eng_mono_f = pct_eng_mono %>% 
  filter(stim_language == "french")
pct_eng_mono_g = pct_eng_mono %>% 
  filter(stim_language == "German")

pct_span_mono_f = pct_span_mono %>% 
  filter(stim_language == "french")
pct_span_mono_g = pct_span_mono %>% 
  filter(stim_language == "German")


make_table(english_l1_pct_f)

make_table(english_l1_pct_g)

make_table(spanish_l1_pct)

make_table(spanish_l1_pct_g)

make_table(pct_eng_mono_f)

make_table(pct_eng_mono_g)

make_table(pct_span_mono_f)

make_table(pct_span_mono_g)


### 
glimpse(cond_df_prob)

#### Example of probability tables 2
cond_df_prob %>% 
  filter(L1 == "Spanish") %>% 
  filter(phoneme == "i") %>% 
  filter(conditional_prob > .2)