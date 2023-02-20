### Which participants need to do the BLP? 

# Load participants who completed the perception experiments & have BLP 
enl1 = read.csv(here("data", "tidy", "participant_list_enl1.csv")) 
spl1 = read.csv(here("data", "tidy", "participant_list_sl1.csv"))


### English 
need_info_eng = data.frame("prolific_id" = unique(df_plot_e$prolific_id)) %>% 
  filter(!prolific_id %in% enl1$participant)

need_info_span = data.frame("prolific_id" = unique(df_plot_s$prolific_id)) %>% 
  filter(!prolific_id %in% spl1$participant)

need_info_eng$prolific_id
need_info_span$prolific_id

nrow(new_df %>% 
  filter(consonant == "p"))