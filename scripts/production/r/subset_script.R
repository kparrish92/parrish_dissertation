# Create a script to pull a random file list for a praat script 

library(tidyverse)
library(here)

dat.files  <- list.files(path= here("sound_files"),
                         recursive=T,
                         pattern=".wav"
                         ,full.names=T) %>% 
  as.data.frame()

three_pct = nrow(dat.files)*.03

set.seed(1)

sample = dat.files[sample(nrow(dat.files), 381), ] %>% 
  as.data.frame() 

sample$wav = sample$.
sample$textgrid = sample$.

sample_tidy = sample %>% 
  dplyr::select(wav, textgrid) %>% 
  mutate(textgrid = str_remove(textgrid, ".wav")) %>% 
  mutate(textgrid = paste0(textgrid, ".TextGrid")) %>% 
  pivot_longer(cols = 1:2, values_to = "file", names_to = "wavor") %>% 
  dplyr::select(file)


dat.files  <- sample_tidy



from.dir <- here("sound_files")
to.dir   <- here("subset")
files    <- dat.files
for (f in files) file.copy(from = f, to = to.dir)


