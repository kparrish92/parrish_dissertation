source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))
source(here::here("scripts", "production", "r", "03_load_data.R"))

# a plot for pres 

full_data %>% 
  filter(phoneme == "o") %>%
  filter(group == "L1 Spanish bilingual" | group == "L1 English bilingual") %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + 
  geom_point(alpha = .1) + scale_x_reverse() +
  xlab("F2") + ylab("F1") +
  theme_minimal() + 
  scale_y_reverse() + stat_ellipse(linetype = "dashed") + 
  ggtitle("Productions of /o/") + 
  geom_point(aes(y=598, x=1267), colour="#7cae00") +
  geom_point(aes(y=417, x=849), colour="#00BFC4") +
  theme(strip.text.x = element_text(size = 6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        axis.text.y = element_text(size = 6),
        legend.position="bottom", 
        legend.text=element_text(size=6),
        legend.title=element_text(size=6),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=12,face="bold")) +
  facet_grid(~group) + ggsave(here("slides", "img", "04_o_prod.png"))

full_data %>% 
  filter(phoneme == "i") %>%
  filter(group == "L1 Spanish bilingual" | group == "L1 English bilingual") %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + 
  geom_point(alpha = .1) + scale_x_reverse() +
  xlab("F2") + ylab("F1") +
  theme_minimal() + 
  scale_y_reverse() + stat_ellipse(linetype = "dashed") + 
  ggtitle("Productions of /i/") + 
  geom_point(aes(y=508, x=1966), colour="#7cae00") +
  geom_point(aes(y=322, x=2533), colour="#00BFC4") +
  theme(strip.text.x = element_text(size = 6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        axis.text.y = element_text(size = 6),
        legend.position="bottom", 
        legend.text=element_text(size=6),
        legend.title=element_text(size=6),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=12,face="bold")) +
  facet_grid(~group) +
  ggsave(here("slides", "img", "04_i_prod.png"))

full_data %>% 
  filter(phoneme == "u") %>%
  filter(group == "L1 Spanish bilingual" | group == "L1 English bilingual") %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + 
  geom_point(alpha = .1) + scale_x_reverse() +
  xlab("F2") + ylab("F1") +
  theme_minimal() + 
  scale_y_reverse() + stat_ellipse(linetype = "dashed") + 
  ggtitle("Productions of /u/ and /y/") + 
  geom_point(aes(y=444.18, x=1776.17), colour="#7cae00") +
  geom_point(aes(y=411, x=2053), colour="#00BFC4") +
  theme(strip.text.x = element_text(size = 6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        axis.text.y = element_text(size = 6),
        legend.position="bottom", 
        legend.text=element_text(size=6),
        legend.title=element_text(size=6),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=12,face="bold")) +
  facet_grid(~group) +
  ggsave(here("slides", "img", "04_u_prod.png"))


full_data %>% 
  filter(phoneme == "schwa" | phoneme == "a") %>%
  filter(group == "L1 Spanish bilingual" | group == "L1 English bilingual") %>% 
  ggplot(aes(x = f2e, y = f1e, color = language)) + 
  geom_point(alpha = .1) + scale_x_reverse() +
  xlab("F2") + ylab("F1") +
  theme_minimal() + 
  scale_y_reverse() + stat_ellipse(linetype = "dashed") + 
  ggtitle("Productions of /a/ and the wedge") +
  geom_point(aes(y=601, x=1755), colour="#7cae00") +
  geom_point(aes(y=736, x=1304), colour="#00BFC4") +
  theme(strip.text.x = element_text(size = 6),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6),
        axis.text.y = element_text(size = 6),
        legend.position="bottom", 
        legend.text=element_text(size=6),
        legend.title=element_text(size=6),
        axis.text=element_text(size=8),
        axis.title=element_text(size=8,face="bold"),
        plot.title = element_text(size=12,face="bold")) +
  facet_grid(~group) +
  ggsave(here("slides", "img", "04_schwa_prod.png"))

