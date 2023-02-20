
# Load data 
#
# Source libs --------------------------------------------

source(here::here("scripts", "spanish_bilingual_pct", 
                  "00_libs.R"))
source(here::here("scripts", "spanish_bilingual_pct", 
                  "03_load_data.R"))

# ------------------------------

report_df = read.csv(here("scripts", "spanish_bilingual_pct", 
               "data", "tidy",
               "report_bilabial_span_l1.csv"))

# some code to check number of participants 
# (that the rest of the code is working)

length(unique(all_pct$participant))

unique(all_pct$stim)

desc = all_pct %>% 
  group_by(participant) %>% 
  summarize(n = n()) %>% 
  filter(n > 79)

report_df %>% 
  ggplot(aes(x = estimate, y = phoneme, color = language,
             xmin=hdi_hi, xmax=hdi_lo)) + 
  geom_pointrange(position = position_dodge(width = .5), 
                  shape = 21, fill = "white") + 
  xlim(.45,.75) + 
  geom_vline(xintercept = .5, linetype = "dashed") +
  scale_color_manual(values=c("#1F9646", "darkgoldenrod3"))  + 
  xlab("Probability") + ylab("Phoneme of stimulus") +
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"),
        legend.position = "bottom") + 
  theme(legend.position = "bottom") + 
  ggtitle("Probability of choosing an English word") +
ggsave(here("scripts", "spanish_bilingual_pct", "span_l1_prob.png"))

# check how many phonemes total were presented
all_pct %>% 
  group_by(phoneme) %>% 
  summarize(n = n())

graph_df = all_pct %>% 
  group_by(phoneme, word, stim_language, language) %>% 
  summarise("response" = n(), rating_mean = mean(rating),
            rating_sd = sd(rating)) %>% 
  filter(response > 11)

graph_df %>% 
  filter(phoneme == "i") %>% 
  ggplot(aes(x = word, y = response, fill = language)) + 
  geom_bar(stat = "identity") +
  ggtitle("responses to /i/") + facet_grid(~stim_language) +
  scale_fill_manual(values=c("#1F9646", "darkgoldenrod3")) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"))

graph_df %>% 
  filter(phoneme == "o") %>% 
  ggplot(aes(x = word, y = response, fill = language)) + 
  geom_bar(stat = "identity") +
  ggtitle("responses to /o/") + facet_grid(~stim_language) +
  scale_fill_manual(values=c("#1F9646", "darkgoldenrod3")) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"))

graph_df %>% 
  filter(phoneme == "o") %>% 
  ggplot(aes(x = language, y = response, fill = language)) + 
  geom_bar(stat = "identity") +
  ggtitle("responses to o") + facet_grid(~stim_language) +
  scale_fill_manual(values=c("#1F9646", "darkgoldenrod3")) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"))

graph_df %>% 
  filter(phoneme == "schwa") %>% 
  ggplot(aes(x = word, y = response, fill = language)) + 
  geom_bar(stat = "identity") +
  ggtitle("responses to schwa") + facet_grid(~stim_language) +
  scale_fill_manual(values=c("#1F9646", "darkgoldenrod3")) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"))

graph_df %>% 
  filter(phoneme == "schwa") %>% 
  ggplot(aes(x = language, y = response, fill = language)) + 
  geom_bar(stat = "identity") +
  ggtitle("responses to schwa") + facet_grid(~stim_language) +
  scale_fill_manual(values=c("#1F9646", "darkgoldenrod3")) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"))

graph_df %>% 
  filter(phoneme == "y") %>% 
  ggplot(aes(x = word, y = response, fill = language)) + 
  geom_bar(stat = "identity") +
  ggtitle("responses to y") + facet_grid(~stim_language) +
  scale_fill_manual(values=c("#1F9646", "darkgoldenrod3")) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79"))


graph_df %>% 
  ggplot(aes(x = language, y = response, fill = language)) + 
  geom_bar(stat = "identity") +
  ggtitle("Overall Responses") + facet_grid(~stim_language) +
  scale_fill_manual(values=c("#1F9646", "darkgoldenrod3")) + 
  theme_bw() +
  theme(panel.background = element_rect(fill = "grey79")) 
