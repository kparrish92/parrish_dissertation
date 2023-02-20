### Find the prolific ids of the participants 
## who did BOTH the perception and production exp

# Load perception tidy data
perception_ids = read.csv(here::here("data", 
                                     "perception", 
                                     "tidy", "pct_tidy.csv"))

# Load production tidy data
production_ids = read.csv(here("data", "tidy", "full_data.csv")) %>% 
  mutate(language = case_when(
    language == "french" ~ "French",
    language == "German" ~ "German",
    language == "english" ~ "English",
    language == "spanish" ~ "Spanish"
  )) 


## participants per group perception
perception_ids %>%
  group_by(participant, L1) %>% 
  summarize(n = n()) %>% 
  group_by(L1) %>% 
  summarize(n = n())

## participants per group production
production_ids %>%
  group_by(prolific_id, group) %>% 
  summarize(n = n()) %>% 
  group_by(group) %>% 
  summarize(n = n())


# Filter and find how many participants did both
both = perception_ids %>% 
  filter(participant %in% production_ids$prolific_id)

# Filter and find how many participants did both
both_p = production_ids %>% 
  filter(prolific_id %in% perception_ids$participant)

# Return length of unique prolific ids in the df 
length(unique(both$participant))
length(unique(both_p$prolific_id))

# have self-rated proficiency - no need for lextale for now
# this is the quantity of participants per group to compare their
# perception and production of vowels 
both %>%
  group_by(participant, L1) %>% 
  summarize(n = n()) %>% 
  group_by(L1) %>% 
  summarize(n = n())

both_p %>%
  group_by(prolific_id, language) %>% 
  summarize(n = n())


# Filter perception data for only those who did both 
perception_data_filtered = perception_ids %>% 
  filter(participant %in% both$participant)

# Filter production data for only those who did both 
production_data_filtered = production_ids %>% 
  filter(prolific_id %in% both$participant) %>% 
  filter(language == "French" | language == "German")

# load lextale data 
lextale_spanish_raw = dir_ls(here("data/production/raw", "lextale"), regexp = "\\.csv$") %>% 
  map_dfr(read_csv, .id = "source", col_types = cols(.default = "c"))

lex_done = unique(lextale_spanish_raw$id)

# get list of participants who did lextale in this subset
d = both %>% 
  filter(participant %in% lex_done)


list = unique(d$participant)

# Get list of ids that need to do production lextale Spanish L1
need_lex_sp = production_ids %>% 
  filter(group == "L1 Spanish bilingual") %>% 
  filter(!participant %in% lex_done)
unique(need_lex_sp$prolific_id) # they all do 

# Get list of ids that need to do production lextale English L1
need_lex_eng = production_ids %>% 
  filter(group == "L1 English bilingual") %>% 
  filter(!participant %in% lex_done)
unique(need_lex_eng$prolific_id)  

# Get list of ids that need to do perception lextale Spanish L1
need_lex_sp_per = perception_ids %>% 
  filter(L1 == "Spanish") %>% 
  filter(!participant %in% lex_done)

unique(need_lex_sp_per$participant) # they all do 

# Get list of ids that need to do perception lextale English L1
need_lex_eng_per = perception_ids %>% 
  filter(L1 == "English") %>% 
  filter(!participant %in% lex_done)
unique(need_lex_eng_per$participant)  


# Sp list
unique(c(unique(need_lex_sp$prolific_id),unique(need_lex_sp_per$participant)))  
# Eng list 
unique(c(unique(need_lex_eng$prolific_id), unique(need_lex_eng_per$participant)))
  

