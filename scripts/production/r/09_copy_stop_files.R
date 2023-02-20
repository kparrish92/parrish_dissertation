# A script to extract and copy all stop consonants only 

source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))
#source(here::here("scripts", "production", "r", "03_load_data.R"))

stop_files = read.csv(here("data", "production", "tidy", "stop_files.csv"))

el1 = read.csv(here("data", "production", 
                    "tidy", "el1_list.csv"))

sl1 = read.csv(here("data", "production", 
                "tidy", "sl1_list.csv"))

tidy = stop_files %>%
  mutate(group = 
           case_when(
             file_copy %in% el1$participant ~ "English L1",
             file_copy %in% sl1$participant ~ "Spanish L1")) %>% 
  filter(!is.na(group)) %>% 
  filter(language == "english" & group == "Spanish L1" | 
           language == "spanish" & group == "English L1") %>% 
  filter(text != "puff") %>%
  filter(text != "pof") %>%
  mutate(file = paste0("/Users/kyleparrish/Documents/GitHub/ru_dissertation_parrish/production_files/sound_files_bilingual/",
                       file_name_append, ".wav"))

### There are 66 Span L1 and 20 Eng L1
# total of 48*86 tokens = 4128 total tokens 
list_participants_tokens = tidy %>% 
  group_by(group, file_copy) %>% 
  summarize(n = n())


# Copy all files to make psychopy experiment 
# Commenting this code after running to not re-copy files each time
# it takes a while 


library(here)

dat.files  <- list.files(path= here("sound_files_eng_mono"),
                         recursive=T,
                         pattern=".wav"
                         ,full.names=T)

from.dir <- here("production_files", "sound_files_bilingual")
to.dir   <- here("production_files", "stop_files_bilingual")
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
  write.csv(here("data", "tidy", "conditions.csv"))
  

