source(here::here("scripts", "perception", "00_libs.R"))
source(here::here("scripts", "perception", "03_load_data.R"))


all_df_ax %>% 
  group_by(the_col_fr, group, language) %>% 
  summarize(Mean = mean(d_prime),
            SD = sd(d_prime)) %>% 
  rename("Token" = the_col_fr) %>%
  rename("Group" = group)


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


