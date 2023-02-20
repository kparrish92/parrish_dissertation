library(here)
library(tidyverse)
library(stringr)

# create text files for Webmaus autosegmentation based on file names

file_names  <- list.files(path= here("production_files", "participant_uploads_bilingual"),
                          recursive=T,
                          pattern=".wav"
                          ,full.names=T) %>% 
  as.data.frame() %>% 
  rename(file_name_append = ".") %>% 
  mutate(file_name_append = str_remove(file_name_append, 
                           "/Users/kyleparrish/Documents/GitHub/ru_dissertation_parrish/production_files/participant_uploads_bilingual/"),
         file_name_append = str_remove(file_name_append, ".wav")) 


# load prod_tidy 

txt_files_words = left_join(prod_tidy, file_names, by = "file_name_append") %>% 
  select(file_name_append, text, language, word) %>% 
  mutate(sub_folder = substr(file_name_append, 1, 6))


for(thisRun in 1:nrow(txt_files_words))
{
  filename <- txt_files_words$file_name_append[thisRun]  
  direc <- paste("production_files", "/", "participant_uploads_bilingual", "/", 
                 txt_files_words$sub_folder[thisRun], "/", sep = "")
  end <- ".txt"
  path <- paste0(direc,filename,end)
  fileConn<-file(paste0(path))
  writeLines(txt_files_words$text[thisRun], fileConn)
  close(fileConn)
}
