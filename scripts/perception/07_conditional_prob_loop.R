# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# A script to calculate and plot conditional probabilities 
# in order to examine whether monolingual and bilingual groups 
# categorize sounds differently in a given languages
# -------------------------------------------------------


library(here)
library(tidyverse)

cond_df = read.csv(here("data", "tidy", "cond_df.csv"))

x = data.frame(L1 = c("Spanish"),
               effect1__ = c("i", "i", "o", "o", "y", "y", "schwa", "schwa",
                             "i", "i", "o", "o", "y", "y", "schwa", "schwa"),
               Language = c("English", "English", "English", "English", 
                            "English", "English", "English", "English",
                            "Spanish", "Spanish", "Spanish", "Spanish", 
                            "Spanish", "Spanish", "Spanish", "Spanish"),
               stim_language = c("German", "French", "German", "French",
                                 "German", "French", "German", "French",
                                 "German", "French", "German", "French",
                                 "German", "French", "German", "French"))

y = data.frame(L1 = c("English"),
               effect1__ = c("i", "i", "o", "o", "y", "y", "schwa", "schwa",
                             "i", "i", "o", "o", "y", "y", "schwa", "schwa"),
               Language = c("English", "English", "English", "English", 
                            "English", "English", "English", "English",
                            "Spanish", "Spanish", "Spanish", "Spanish", 
                            "Spanish", "Spanish", "Spanish", "Spanish"),
               stim_language = c("German", "French", "German", "French",
                                 "German", "French", "German", "French",
                                 "German", "French", "German", "French",
                                 "German", "French", "German", "French"))

loop_df = rbind(x,y)

## calculate condition probability 

total_df = cond_df[0, ] %>% 
  mutate(total_prob = "n/a",
         total_prob_upper = "n/a",
         total_prob_lower = "n/a")

total_df = character()
for (thisRun in 1:nrow(loop_df)) {
filter_df = cond_df %>% 
  filter(L1 == loop_df$L1[thisRun]) %>% 
  filter(effect1__ == loop_df$effect1__[thisRun]) %>% 
  filter(language == loop_df$Language[thisRun]) %>% 
  filter(stim_language == loop_df$stim_language[thisRun]) %>% 
  mutate(total_prob = sum(estimate__),
         total_prob_upper = sum(upper__),
         total_prob_lower = sum(lower__))

total_df = rbind(total_df, filter_df)
}


total_df_tidy = total_df %>%
  mutate(conditional_prob = estimate__/total_prob,
         conditional_prob_upper = upper__/total_prob_upper,
         conditional_prob_lower = lower__/total_prob_lower) %>% 
  write.csv(here("data", "tidy", "cond_prob_df.csv"))
  
  
  

