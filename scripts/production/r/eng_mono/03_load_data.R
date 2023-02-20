
source(here::here("scripts", "r", "eng_mono", "0_libs.R"))


eng_mono_df = read.csv(here("data","tidy","eng_mono_tidy.csv"))

eng_bil_df = read.csv(here("data","tidy","eng_bil_data.csv"))

span_bil_df = read.csv(here("data","tidy","span_bil_data.csv"))

### Subsets 

sub_bil_corr = read.csv(here("data","tidy","sub_bil_corrected.csv")) %>% 
  filter(f1e != "--undefined--")

sub_bil = read.csv(here("data","tidy","sub_bil.csv")) %>% 
  filter(f1e != "--undefined--")