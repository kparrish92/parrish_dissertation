# 01 helpers

source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))

# Run auto VOT
# errors 492821, 492835 
# 493072 eng 07
# 493151 
# 493272

eng_l1_bil = full_data %>% 
  filter(group == "L1 English bilingual")

span_l1_bil = full_data %>% 
  filter(group == "L1 Spanish bilingual")








participants_done = c(492813, 492814, 492817, 492818, 492821,
492824, 492825, 492828, 492834, 492835, 492845,
492854, 492856, 492864, 492872, 492880, 492882,
492909, 493072, 493097, 493151, 493215, 493232,
493265, 493272, 493275, 493278, 493297, 493310,
493341, 493414, 493445, 493447, 493453, 493454,
493488, 493499, 493522, 493817, 493828, 493860,
494162, 494205)

df_f = character()
for (i in 1:length(participants_done)) {
df = run_ind(participants_done[i]) 
df_f = rbind(df, df_f)
}

vot_tidy = df_f %>% 
  mutate(group = case_when(
    participant %in% eng_l1_bil$participant ~ "eng_l1",
    participant %in% span_l1_bil$participant ~ "span_l1",
  )) %>% 
  mutate(word_duration = xmax - xmin)

vot_tidy %>% 
  group_by(group) %>% 
  summarize(n = n())

nrow(unique(vot_tidy %>% filter(group == "eng_l1") %>% 
  select(participant)))

nrow(unique(vot_tidy %>% filter(group == "span_l1") %>% 
              select(participant)))


  vot_tidy %>% 
  filter(!is.na(group)) %>% 
  ggplot(aes(x = word_duration, y = text, fill = language)) + 
  geom_boxplot(outlier.shape = NA) +
  xlim(0,.2) +
  facet_grid(~group)
  
  
  vot_tidy %>% 
    filter(!is.na(group)) %>% 
    ggplot(aes(x = word_duration, y = text, fill = group)) + 
    geom_boxplot(outlier.shape = NA) +
    xlim(0,.2) +
    facet_grid(~language)