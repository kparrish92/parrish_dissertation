

source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))

el1 = read.csv(here("data", "production", 
                    "tidy", "el1_list.csv"))

sl1 = read.csv(here("data", "production", 
                    "tidy", "sl1_list.csv"))

vot_disq = read.csv(here("data",
              "raw", "disq_vot.csv"))

vot_disq %>% 
  group_by(key_resp.keys) %>%
  summarise(n = n())
  

vot_disq$file_word <- substr(vot_disq$file_word, 0, 11)

vot_disq_tidy = vot_disq %>% 
  mutate(file_word = str_remove(file_word, "stim/")) %>% 
  mutate(choice = case_when(
    key_resp.keys == "q" ~ "English",
    key_resp.keys == "p" ~ "Spanish",
  )) %>% 
  select(file_word, choice) %>% 
  mutate(group = 
           case_when(
             file_word %in% el1$participant ~ "English L1",
             file_word %in% sl1$participant ~ "Spanish L1")) %>% 
  mutate(is_correct = case_when(
    group == "English L1" & choice == "Spanish" ~ 1,
    group == "Spanish L1" & choice  == "English" ~ 1,
    group == "English L1" & choice == "English" ~ 0,
    group == "Spanish L1" & choice == "Spanish" ~ 0
  ))

is_correct_participant = vot_disq_tidy %>% 
  group_by(file_word) %>%
  summarize(n = sum(is_correct)) %>% 
  filter(n > 4) %>% 
  mutate(group = 
           case_when(
             file_word %in% el1$participant ~ "English L1",
             file_word %in% sl1$participant ~ "Spanish L1"))


