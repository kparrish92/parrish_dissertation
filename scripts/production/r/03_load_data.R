source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))


bil_sub = read.csv(here("data", "production", "tidy", 
                        "bilingual_subset.csv")) %>% 
  mutate(language = case_when(
                          language == "french" ~ "French",
                          language == "German" ~ "German",
                          language == "english" ~ "English",
                          language == "spanish" ~ "Spanish"
                        ))

full_data = read.csv(here("data", "tidy", "full_data_clean.csv")) %>% 
  mutate(language = case_when(
    language == "french" ~ "French",
    language == "German" ~ "German",
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish"
  ))
full_data_all = read.csv(here("data", "tidy", "full_data.csv")) 

span_blp = read.csv(here("data", "tidy", "span_l1_blp.csv"))

eng_blp = read.csv(here("data", "tidy", "eng_l1_blp.csv"))


removed_data = full_data_all %>%
  filter(!X.1 %in% full_data$X.1)

removed_data %>% 
  group_by(group) %>% 
  summarize(n = n())

final_data = full_data_all %>%
  filter(X.1 %in% full_data$X.1)

bil_sub_or = bil_sub %>% filter(correct == "original") 
bil_sub_cor = bil_sub %>% filter(correct == "corrected") 


loop_df = data.frame(Langauge = rep(unique(bil_sub_or$language), 5),
                     phoneme = rep(unique(bil_sub_or$phoneme), 4))
thisRun = 1
# f1 loop
bil_sub_or$f1e
bil_sub_cor$f1e



F1_rmd <- matrix(ncol = 4, nrow = 20)

for(thisRun in 1:nrow(loop_df))
{
  bil_sub_or_filt = bil_sub_or %>% filter(
    language == loop_df$Langauge[thisRun] & 
      phoneme == loop_df$phoneme[thisRun])
  bil_sub_cor_filt = bil_sub_cor %>% filter(
    language == loop_df$Langauge[thisRun] &
      phoneme == loop_df$phoneme[thisRun])
  
  F1_rmd[thisRun ,1] = round(sqrt(mean((bil_sub_or_filt$f1e - bil_sub_cor_filt$f1e)^2)))
  F1_rmd[thisRun ,2] = loop_df$Langauge[thisRun]
  F1_rmd[thisRun ,3] = "F1"
  F1_rmd[thisRun ,4] = loop_df$phoneme[thisRun]
}


F2_rmd <- matrix(ncol = 4, nrow = 20)

for(thisRun in 1:nrow(loop_df))
{
  bil_sub_or_filt = bil_sub_or %>% filter(
    language == loop_df$Langauge[thisRun] & 
      phoneme == loop_df$phoneme[thisRun])
  bil_sub_cor_filt = bil_sub_cor %>% filter(
    language == loop_df$Langauge[thisRun] &
      phoneme == loop_df$phoneme[thisRun])
  
  F2_rmd[thisRun ,1] = round(sqrt(mean((bil_sub_or_filt$f2e - bil_sub_cor_filt$f2e)^2)))
  F2_rmd[thisRun ,2] = loop_df$Langauge[thisRun]
  F2_rmd[thisRun ,3] = "F2"
  F2_rmd[thisRun ,4] = loop_df$phoneme[thisRun]
}


f1_df = as.data.frame(F1_rmd)
f2_df = as.data.frame(F2_rmd)

f1f2_df = rbind(f1_df, f2_df) %>% 
  rename("rmd" = V1,
         "Language" = V2,
         "Formant" = V3,
         "Phoneme" = V4) %>% 
  filter(!rmd == "NaN")

### F1 vs F2 rmd 


removed_df = removed_data %>% 
  group_by(language, phoneme) %>% 
  summarize(n = n()) %>% 
  mutate(language = case_when(
    language == "french" ~ "French",
    language == "German" ~ "German",
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish"
  )) %>% 
  unite(col='match', c(language, phoneme), na.rm = TRUE)


f1_f2_m = f1f2_df %>% 
  unite(col='match', c(Language, Phoneme), na.rm = TRUE) %>% 
  left_join(removed_df) %>% 
  separate(match, into = c("Language", "Phoneme"))




f1_f2_m %>% 
  ggplot(aes(x = as.numeric(rmd), y = as.numeric(n)), 
         color = Language, label = Phoneme) + 
  geom_label() + geom_smooth(method = "lm") + facet_wrap(~Formant)


f1_f2_m %>% 
  ggplot(aes(x = as.numeric(rmd), y = as.numeric(n)), 
         color = Language, label = Phoneme) + geom_point()

f1_f2_m %>% 
  ggplot(aes(x = as.numeric(rmd), y = as.numeric(n)), color = Phoneme, 
         label = rmd) + 
  geom_label(size = 2, position = position_dodge(width = -.5)) +
  facet_wrap(~Formant) + theme_minimal() 

f1_f2_m %>% 
  ggplot(aes(x = as.numeric(rmd), y = n, color = Language, 
             label = Phoneme)) + 
  geom_label(size = 2, position = position_dodge(width = -.5)) +
  facet_wrap(~Formant) + theme_minimal() 



### RMD plot 
f1f2_df %>% 
  ggplot(aes(x = as.numeric(rmd), y = Language, color = Phoneme, 
             label = rmd)) + 
  geom_label(size = 2, position = position_dodge(width = -.5)) +
  facet_wrap(~Formant) + theme_minimal() 

## RMD vs removed datapoints 

five_palette = c("#C383BF",
                 "#303036",
                 "#DAB444",
                 "#FFFAFF",
                 "#30BCED")

f1_f2_m %>% 
  ggplot(aes(x = as.numeric(rmd), y = n, color = Language, 
             label = Phoneme)) + 
  geom_label(size = 2, position = position_dodge(width = -.5)) +
  facet_wrap(~Formant) + theme_minimal() 

## Remaining data 

text_df = full_data %>% 
  group_by(phoneme, language) %>% 
  summarize(n = n())

full_data %>% 
  group_by(phoneme, language) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x = n, y = language, fill = phoneme)) +
  xlim(0,1200) +
  theme_minimal() +
  xlab("Number of tokens") +
  ylab("Language") +
  scale_fill_manual(values=five_palette) +
  theme(strip.text.x = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.position="bottom", 
        legend.text=element_text(size=8),
        legend.title=element_text(size=8),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=8,face="bold")) +
  geom_col(position = position_dodge(width = -.75), color = "black") + 
  geom_text(data = text_df, aes(label = paste0("n = ", n), x = Inf,
                                hjust = "inward"), size = 2.5, position = position_dodge(width = -.75))



l1_e = nrow(unique(full_data %>% filter(group == "L1 English bilingual") %>% select(prolific_id)))
l1_s = nrow(unique(full_data %>% filter(group == "L1 Spanish bilingual") %>% select(prolific_id)))
e_m = nrow(unique(full_data %>% filter(group == "L1 English monolingual") %>% select(prolific_id)))
s_m = nrow(unique(full_data %>% filter(group == "L1 Spanish monolingual") %>% select(prolific_id)))


stops = full_data %>%
  filter(consonant == "p")
  
## List for finding stops 

l1_e_list_match = unique(full_data %>% filter(group == "L1 English bilingual") %>% 
                           select(prolific_id, participant)) %>% 
  write.csv(here("data", "production", "tidy", "el1_list.csv"))


l1_s_list_match = unique(full_data %>% filter(group == "L1 Spanish bilingual") %>% 
                           select(prolific_id, participant)) %>% 
  write.csv(here("data", "production", "tidy", "sl1_list.csv"))

