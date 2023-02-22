source(here::here("scripts", "perception", "00_libs.R"))
source(here::here("scripts", "perception", "03_load_data.R")

all_df_ax %>% 
  filter(language == "German") %>% 
  ggplot(aes(x = d_prime, y = the_col_fr, fill = group)) + 
  geom_boxplot(outlier.shape = NULL) +
  theme_minimal() + ylab("Carrier word") +
  ggsave(here("sections", "figs", "ax_german.png"))

all_df_ax %>% 
  filter(language == "French") %>% 
  ggplot(aes(x = d_prime, y = the_col_fr, fill = group)) + 
  geom_boxplot(outlier.shape = NULL) +
  theme_minimal() + ylab("Carrier word") +
  ggsave(here("sections", "figs", "ax_french.png"))



all_df_ax %>% 
  ggplot(aes(x = d_prime, y = the_col_fr, color = language)) + geom_boxplot() +
  facet_grid(~group)

all_df_ax %>% 
  filter(!is.na(group)) %>% 
  ggplot(aes(x = d_prime, y = the_col_fr, color = group)) + geom_boxplot() 

all_df_ax %>% 
  ggplot(aes(x = d_prime, y = language, color = language)) + geom_boxplot()


all_df_ax %>% 
  group_by(group) %>% 
  summarize(n = n())
