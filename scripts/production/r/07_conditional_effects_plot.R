source(here::here("scripts", "production", "r", "00_libs.R"))
source(here::here("scripts", "production", "r", "01_helpers.R"))
source(here::here("scripts", "production", "r", "03_load_data.R"))

make_cond_df = function(file)
{
  path = conditional_effects(
    readRDS(here("data", "production", "models", 
                 paste0(file))))[["language:group"]] %>% 
    mutate(phoneme = file)
  return(path)
}

f1_eng_bil = make_cond_df("i_mod_bil_f1.rds")
f1_eng_mono = make_cond_df("o_mod_bil_f1.rds")
f1_span_bil = make_cond_df("schwa_mod_bil_f1.rds")
f1_span_mono = make_cond_df("u_mod_bil_f1.rds")

f2_eng_bil = make_cond_df("i_mod_bil_f2.rds")
f2_eng_mono = make_cond_df("o_mod_bil_f2.rds")
f2_span_bil = make_cond_df("schwa_mod_bil_f2.rds")
f2_span_mono = make_cond_df("u_mod_bil_f2.rds")


f1_models = rbind(f1_eng_bil, f1_eng_mono, f1_span_bil, f1_span_mono) %>% 
  mutate(phoneme = case_when(
    phoneme == "i_mod_bil_f1.rds" ~ "i",
    phoneme == "o_mod_bil_f1.rds" ~ "o",
    phoneme == "schwa_mod_bil_f1.rds" ~ "schwa",
    phoneme == "u_mod_bil_f1.rds" ~ "u"
  ))

f2_models = rbind(f2_eng_bil, f2_eng_mono, f2_span_bil, f2_span_mono) %>% 
  mutate(phoneme = case_when(
    phoneme == "i_mod_bil_f2.rds" ~ "i",
    phoneme == "o_mod_bil_f2.rds" ~ "o",
    phoneme == "schwa_mod_bil_f2.rds" ~ "schwa",
    phoneme == "u_mod_bil_f2.rds" ~ "u"
  ))

f1mod = "i_mod_bil_f1.rds"
f2mod = "i_mod_bil_f2.rds"
segment = "i"  

plot_f1_f2 = function(segment, f1mod, f2mod)
{
library(bayestestR)
f1_mod = readRDS(here("data", "production", "models", paste0(f1mod)))
f2_mod = readRDS(here("data", "production", "models", paste0(f2mod)))

rr_f1_mod = abs(rope_range(f1_mod)[1])
rr_f2_mod = abs(rope_range(f2_mod)[1])

f1_plot = f1_models %>% 
  filter(phoneme == segment) %>%
  ggplot(aes(y = estimate__, x = language, fill = group)) + 
  geom_point(position = position_dodge(width = .5), size = .5) +
  geom_pointrange(aes(ymin = lower__, ymax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) +
  geom_pointrange(aes(ymin = estimate__ - rr_f1_mod, ymax = estimate__ + rr_f1_mod), 
                  shape = 21, 
                  position = position_dodge(width = .5), color = "grey") +
  scale_y_reverse() + 
  ylab("Estimate") + 
  xlab("Language") + 
  theme_minimal() +
  theme(legend.position = "bottom") 

f2_plot = f2_models %>% 
  filter(phoneme == segment) %>%
  ggplot(aes(x = estimate__, y = language, fill = group)) + 
  geom_point(position = position_dodge(width = .5), size = .5) +
  geom_pointrange(aes(xmin = lower__, xmax = upper__), 
                  shape = 21, 
                  position = position_dodge(width = .5)) +
  geom_pointrange(aes(xmin = estimate__ - rr_f2_mod, xmax = estimate__ + rr_f2_mod), 
                  shape = 21, 
                  position = position_dodge(width = .5), color = "grey") +
  scale_x_reverse() + 
  ylab("Language") + 
  xlab("Estimate") +
  theme_minimal() +
  theme(legend.position = "bottom")


figure = ggarrange(f1_plot, f2_plot, common.legend = TRUE, 
                   legend = "bottom", nrow = 2, labels = c('a', 'b'),
                   font.label = list(size = 8))

f = annotate_figure(figure,
               bottom = text_grob(paste0("F1 ROPE = +/- ", round(rr_f1_mod), "\n F2 ROPE = +/- "
                                                             , round(rr_f2_mod)),
                                                      hjust = 1.2, x = 1, face = "italic", size = 6))
                
return(f)
}
 

plot_f1_f2("i", "i_mod_bil_f1.rds", "i_mod_bil_f2.rds") +
  ggsave(here("sections", "figs", "i_plots.png"))

plot_f1_f2("o", "o_mod_bil_f1.rds", "o_mod_bil_f2.rds") +
  ggsave(here("sections", "figs", "o_plots.png"))

plot_f1_f2("schwa", "schwa_mod_bil_f1.rds", "schwa_mod_bil_f2.rds") +
  ggsave(here("sections", "figs", "schwa_plots.png"))

plot_f1_f2("u", "u_mod_bil_f1.rds", "u_mod_bil_f2.rds") +
  ggsave(here("sections", "figs", "u_plots.png"))


