
source(here::here("scripts", "r", "eng_mono", "0_libs.R"))


# create text files for Webmaus autosegmentation based on file names

# this part of the script was used to add the participant id to the audio file name 
# commented because it should only should be run once 

loop_list <- list.files(here("production_files", "participant_uploads_span_mono")) %>% 
  as.data.frame()

for (iteration in 1:nrow(loop_list)) {
  
  id <- loop_list$.[iteration]
  
 my_path <- here("production_files", "participant_uploads_span_mono", paste(id)) # Define working directory
  
file_names_old <- list.files(my_path) # get directory names/participant numbers 
  
file_names_new <- matrix(nrow = length(file_names_old))
  
for (thisRun in 1:length(file_names_old)) {
   file_names_new[thisRun] <- paste(id, file_names_old[thisRun], sep = "_")
  }
  
  file.rename(paste(my_path, file_names_old, sep = "/"),       # Rename files
              paste(my_path, file_names_new, sep = "/"))
  
}


# ----- Copy files ---- # This section copies the sound files to a single directory

dat.files  <- list.files(path= here("production_files", "participant_uploads_span_mono"),
                         recursive=T,
                         pattern=".wav"
                         ,full.names=T)



from.dir <- here("production_files", "participant_uploads_span_mono")
to.dir   <- here("production_files", "sound_files_span_mono")
files    <- list.files(path = from.dir, full.names = TRUE, recursive = TRUE)
for (f in files) file.copy(from = f, to = to.dir)



file_names  <- list.files(path= here("production_files", 
                                     "sound_files_span_mono"),
                          recursive=T,
                          pattern=".wav"
                          ,full.names=T) %>% 
  as.data.frame() %>% 
  rename(file_name_append = ".") %>% 
  mutate(file_name_append = str_remove(file_name_append, 
                                       "/Users/kyleparrish/Documents/GitHub/ru_dissertation_parrish/production_files/sound_files_span_mono/"),
         file_name_append = str_remove(file_name_append, ".wav")) 

prod = read.csv(here("data", "production", "raw", "data_prod_span.csv"), header=T, na.strings=c(""," ","NA")) %>%
  janitor::clean_names() %>% 
  select(word_french, word_french_recording, word_german, german_recording, 
         word, rec_session_id, spanish_recording) %>% 
  rename(spanish_word = "word",
         german_word = "word_german",
         french_word = "word_french",
         spanish_file = "spanish_recording",
         french_file = "word_french_recording",
         german_file = "german_recording") %>% 
  unite(col='word', c(german_word, french_word, spanish_word), na.rm = TRUE) %>% 
  unite(col = 'file', c(german_file, french_file, spanish_file), na.rm = TRUE) %>% 
  mutate_all(na_if,"") %>% 
  filter(!is.na(file)) %>% 
  mutate(language = substring(file, 30)) %>% 
  mutate(language = str_remove(language, "_recording.wav")) %>% 
  mutate(language = str_remove(language, "_"))


prod_ids = read.csv(here("data", "production", "raw", "data_prod.csv"), header=T, na.strings=c(""," ","NA")) %>%
  janitor::clean_names() %>% 
  filter(!is.na(prolific_id)) %>% 
  filter(!is.na(end_time))

participants_done = prod %>% 
  group_by(rec_session_id) %>% 
  summarize(n = n()) %>% 
  filter(n == 72)


prod_tidy = prod %>% 
  filter(rec_session_id %in% participants_done$rec_session_id) %>% 
  mutate("file_name_append" = paste0(rec_session_id,"_",file)) %>% 
  mutate(file_name_append = str_remove(file_name_append, ".wav")) %>% 
  mutate("text" = case_when(
    word == "f_i" & language == "french" ~ "fif",
    word == "p_i" & language == "french" ~ "pif",
    word == "f_schwa" & language == "french" ~ "faf",
    word == "p_schwa" & language == "french" ~ "paf",
    word == "f_u" & language == "french" ~ "fuf",
    word == "p_u" & language == "french" ~ "puf",
    word == "f_o" & language == "french" ~ "fof",
    word == "p_o" & language == "french" ~ "pof",
    word == "f_i" & language == "english" ~ "feef",
    word == "p_i" & language == "english" ~ "peef",
    word == "f_schwa" & language == "english" ~ "fuff",
    word == "p_schwa" & language == "english" ~ "puff",
    word == "f_u" & language == "english" ~ "foof",
    word == "p_u" & language == "english" ~ "poof",
    word == "f_o" & language == "english" ~ "foff",
    word == "p_o" & language == "english" ~ "poff",
    word == "f_i" & language == "German" ~ "fif",
    word == "p_i" & language == "German" ~ "pif",
    word == "f_schwa" & language == "German" ~ "faf",
    word == "p_schwa" & language == "German" ~ "paf",
    word == "f_u" & language == "German" ~ "fuf",
    word == "p_u" & language == "German" ~ "puf",
    word == "f_o" & language == "German" ~ "fof",
    word == "p_o" & language == "German" ~ "pof",
  ))

txt_files_words = left_join(prod_tidy, file_names, by = "file_name_append") %>% 
  select(file_name_append, text, language, word)

### Loops to create txt files for each language 
for(thisRun in 1:nrow(txt_files_words))
{
  filename <- txt_files_words$file_name_append[thisRun]  
  direc <- paste("production_files/","sound_files_span_mono", "/", sep = "")
  end <- ".txt"
  path <- paste0(direc,filename,end)
  fileConn<-file(paste0(path))
  writeLines(txt_files_words$text[thisRun], fileConn)
  close(fileConn)
}

file_names_t  <- list.files(path= here("production_files", 
                                     "sound_files_span_mono"),
                          recursive=T,
                          pattern=".TextGrid"
                          ,full.names=T) %>% 
  as.data.frame() %>% 
  rename(file_name_append = ".") %>% 
  mutate(file_name_append = str_remove(file_name_append, 
                                       "/Users/kyleparrish/Documents/GitHub/ru_dissertation_parrish/production_files/sound_files_span_mono/"),
         file_name_append = str_remove(file_name_append, ".TextGrid")) 


wavs_to_go = file_names %>%
  filter(!file_name_append %in% file_names_t$file_name_append)

for(i in 1:nrow(wavs_to_go))
{
  filename <- paste0(wavs_to_go$file_name_append[i],".wav")  
  direc <- paste("production_files/","sound_files_span_mono", "/", sep = "")
  path <- paste0(direc,filename)
  file.remove(path)
}


