mod = imod_f1_bil

make_matrix = function(mod)
{  
mod_ex = mod %>% 
  as.data.frame()

rr = abs(rope_range(mod)[1])

cond_effects = mod_ex %>% 
  mutate("eng_l1_fre" = b_Intercept + b_languageFrench,
         "eng_l1_eng" = b_Intercept,
         "eng_l1_ger" = b_Intercept + b_languageGerman,
         "eng_l1_spa" = b_Intercept + b_languageSpanish,
         "span_l1_fre" = b_Intercept + b_languageFrench + 
           b_groupL1Spanishbilingual +
           `b_languageFrench:groupL1Spanishbilingual`,
         "span_l1_eng" = b_Intercept + b_groupL1Spanishbilingual,
         "span_l1_ger" = b_Intercept + b_languageGerman + 
           b_groupL1Spanishbilingual +
           `b_languageGerman:groupL1Spanishbilingual`,
         "span_l1_spa" = b_Intercept + b_languageSpanish +
           b_groupL1Spanishbilingual + 
           `b_languageSpanish:groupL1Spanishbilingual`) %>% 
  select(eng_l1_fre:span_l1_spa)

#df_est = conditional_effects(mod)["language:group"] %>%
#  as.data.frame()

df = conditional_effects(mod)["language:group"] %>%
  as.data.frame() %>% 
  mutate(lang_type = case_when(
    language.group.effect1__ == "English" & 
      language.group.effect2__ == "L1 English bilingual" ~ "eng_l1_eng",
    language.group.effect1__ == "Spanish" & 
      language.group.effect2__ == "L1 English bilingual" ~ "eng_l1_spa",
    language.group.effect1__ == "German" & 
      language.group.effect2__ == "L1 English bilingual" ~ "eng_l1_ger",
    language.group.effect1__ == "French" & 
      language.group.effect2__ == "L1 English bilingual" ~ "eng_l1_fre",
    language.group.effect1__ == "English" & 
      language.group.effect2__ == "L1 Spanish bilingual" ~ "span_l1_eng",
    language.group.effect1__ == "Spanish" & 
      language.group.effect2__ == "L1 Spanish bilingual" ~ "span_l1_spa",
    language.group.effect1__ == "German" & 
      language.group.effect2__ == "L1 Spanish bilingual" ~ "span_l1_ger",
    language.group.effect1__ == "French" & 
      language.group.effect2__ == "L1 Spanish bilingual" ~ "span_l1_fre"
  )) %>% 
  select(lang_type, language.group.estimate__) %>% 
  mutate(upper = language.group.estimate__ + rr, 
         lower = language.group.estimate__ - rr)


#df_f <- character()
#for(thisRun in 1:nrow(list_of_files))
#{
#  df <- read_textgrid(list_of_files$.[thisRun]) %>%
#    add_words()
#  df_f <- rbind(df_f, df)  
# }
# Example working loop

df_p <- character()
for(i in 1:nrow(df))
{
df$language.group.estimate__[i] + rr
df$language.group.estimate__[i] - rr

mdf1 = data.frame(n = nrow(cond_effects %>% filter(eng_l1_eng > df$language.group.estimate__[i] - rr 
                                                   & eng_l1_eng < df$language.group.estimate__[i] + rr)),
                  comp = "eng_l1_eng",
                  comp2 = df$lang_type[i])

mdf2 = data.frame(n = nrow(cond_effects %>% filter(eng_l1_fre > df$language.group.estimate__[i] - rr 
                                                   & eng_l1_fre < df$language.group.estimate__[i] + rr)),
                  comp = "eng_l1_fre",
                  comp2 = df$lang_type[i])

mdf3 = data.frame(n = nrow(cond_effects %>% filter(eng_l1_ger > df$language.group.estimate__[i] - rr 
                                                   & eng_l1_ger < df$language.group.estimate__[i] + rr)),
                  comp = "eng_l1_ger",
                  comp2 = df$lang_type[i])

mdf4 = data.frame(n = nrow(cond_effects %>% filter(eng_l1_spa > df$language.group.estimate__[i] - rr 
                                                   & eng_l1_spa < df$language.group.estimate__[i] + rr)),
                  comp = "eng_l1_spa",
                  comp2 = df$lang_type[i])

mdf5 = data.frame(n = nrow(cond_effects %>% filter(span_l1_spa > df$language.group.estimate__[i] - rr 
                                                   & span_l1_spa < df$language.group.estimate__[i] + rr)),
                  comp = "span_l1_spa",
                  comp2 = df$lang_type[i])

mdf6 = data.frame(n = nrow(cond_effects %>% filter(span_l1_eng > df$language.group.estimate__[i] - rr 
                                                   & span_l1_eng < df$language.group.estimate__[i] + rr)),
                  comp = "span_l1_eng",
                  comp2 = df$lang_type[i])

mdf7 = data.frame(n = nrow(cond_effects %>% filter(span_l1_ger > df$language.group.estimate__[i] - rr 
                                                   & span_l1_ger < df$language.group.estimate__[i] + rr)),
                  comp = "span_l1_ger",
                  comp2 = df$lang_type[i])

mdf8 = data.frame(n = nrow(cond_effects %>% filter(span_l1_fre > df$language.group.estimate__[i] - rr 
                                                   & span_l1_fre < df$language.group.estimate__[i] + rr)),
                  comp = "span_l1_fre",
                  comp2 = df$lang_type[i])

df_f = rbind(mdf1, mdf2, mdf3, mdf4, mdf5, mdf6, mdf7, mdf8)

df_p <- rbind(df_p, df_f)  
}

df_pivot = df_p %>% 
  mutate(n = n/4000) %>% 
  pivot_wider(names_from = comp, values_from = n) 

plot = ggplot(df_p, aes(comp, comp2)) +
  geom_tile(aes(fill = n)) + 
  geom_text(aes(label = round(n, 1))) +
  scale_fill_gradient(low = "white", high = "deepskyblue3")
return(plot)

}

make_matrix(imod_f1_bil)


## plot data

