
# run find_ids to get both df 

# get full list of sound files 
prolific_ids = read.csv(here("data", "production", "tidy", "prolific_ids.csv")) %>% 
  rename("rec_session_id" = "participant")

all_sound_files = read.csv(here("data", "production", "tidy", "all_soundfiles.csv")) %>% 
  left_join(prolific_ids, by = "rec_session_id")

ids = unique(both$participant)

slim = all_sound_files %>% 
  filter(prolific_id %in% ids) %>% 
  filter(language == "french" | language == "German")

slim %>% 
  group_by(prolific_id, language) %>% 
  summarize(n = n())

tidy = slim %>%
  mutate(file = paste0("/Users/kyleparrish/Documents/GitHub/ru_dissertation_parrish/production_files/sound_files_bilingual/",
                       file_name_append, ".wav"))



library(here)



from.dir <- here("production_files", "sound_files_bilingual")
to.dir   <- here("production_files", "both_files")
files_1    <- list.files(path = from.dir, 
                         full.names = TRUE, recursive = TRUE) %>% 
  as.data.frame() %>% 
  rename("fn" = ".") %>% 
  filter(fn %in% tidy$file)

files = files_1$fn
for (f in files) file.copy(from = f, to = to.dir)

f = files_1 %>% 
  mutate(fn = str_remove(fn, "/Users/kyleparrish/Documents/GitHub/ru_dissertation_parrish/production_files/sound_files_bilingual/")) %>% 
  mutate(file_word = paste0("stim/", fn)) %>% 
  select(file_word) %>% 
  write.csv(here("data", "tidy", "conditions_both.csv"))


