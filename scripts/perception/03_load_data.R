# ------------------------------------------------------
# Author: Kyle Parrish
# Date 5/2/22
# A script to load data - it is typically run by other scripts 
#
# -------------------------------------------------------


span_blp_ax = read.csv(here("data", "tidy", "span_l1_blp_ax.csv"))

eng_blp_ax = read.csv(here("data", "tidy", "eng_l1_blp_ax.csv"))
span_blp = read.csv(here("data", "perception", "tidy", "span_l1_blp.csv"))
eng_blp = read.csv(here("data", "perception", "tidy", "eng_l1_blp.csv"))

removed = read.csv(here("data", "perception", "tidy", "removed.csv"))

e_prof = span_blp %>% 
  select(prolific_id, l2_prof_perception)
s_prof = eng_blp %>% 
  select(prolific_id, l2_prof_perception)

sp_1_prof = span_blp_ax %>% 
  select(prolific_id, l2_prof_perception)
en_1_prof = eng_blp_ax  %>% 
  select(prolific_id, l2_prof_perception)

prof = rbind(s_prof, e_prof) %>% 
  rename(participant = prolific_id)

prof_df = rbind(sp_1_prof, en_1_prof) %>% 
  rename(id = prolific_id)

pct_tidy = read.csv(here::here("data", "perception", "tidy", "pct_tidy.csv")) %>% 
  left_join(prof, by = "participant")

pct_tidy_mono = read.csv(here("data", "perception", "tidy", "tidy_mono_groups.csv"))

pct_eng_mono = pct_tidy_mono %>% 
  filter(L1 == "English_mono")

pct_span_mono = pct_tidy_mono %>% 
  filter(L1 == "Spanish_mono")

count_df = read.csv(here("data", "perception", "tidy", "count.csv"))


english_l1_pct = pct_tidy %>% 
  filter(L1 == "English") %>% 
  filter(participant %in% eng_blp$prolific_id)

english_l1_pct_g = pct_tidy %>% 
  filter(L1 == "English") %>% 
  filter(stim_language == "German") %>% 
  filter(participant %in% eng_blp$prolific_id)

spanish_l1_pct = pct_tidy %>% 
  filter(L1 == "Spanish") %>% 
  filter(stim_language == "french") %>% 
  filter(participant %in% span_blp$prolific_id)

spanish_l1_pct_g = pct_tidy %>% 
  filter(L1 == "Spanish") %>% 
  filter(stim_language == "German") %>% 
  filter(participant %in% span_blp$prolific_id)


cond_df_prob = read.csv(here("data", "perception", "tidy", "cond_prob_df.csv"))

eng_cond_df_comb = read.csv(here("data", "perception", "tidy", "eng_cond_df_comb.csv"))

sp_cond_df_comb = read.csv(here("data", "perception", "tidy", "span_cond_df_comb.csv"))


desc_df_eng = read.csv(here("data", "perception", "tidy", "desc_df_span.csv"))

desc_df_span = read.csv(here("data", "perception", "tidy", "desc_df_span.csv"))


### AX small data 

all_df_ax = read.csv(here("data", "tidy", "all_ax.csv"))


eng_mono_ax = all_df_ax %>% filter(group == "English monolingual")


#### who was removed for not self-reporting being comfortable

removed_ax = read.csv(here("data", "tidy", "removed_ax.csv"))

# Find participants who did the the AX and PCT

## AX participants, reporting 

ax_ids = unique(all_df_ax$id)

report_ax_table = read.csv(here("data", "tidy", "ax_mod_report.csv"))
report_ax = read.csv(here("data", "tidy", "ax_mod_report_df.csv"))
report_ax$mean_p = round(report_ax$mean_p, digits = 2)
report_ax$hdi_lo = round(report_ax$hdi_lo, digits = 2)
report_ax$hdi_hi = round(report_ax$hdi_hi, digits = 2)

all_df_ax = read.csv(here("data", "tidy", "all_ax.csv"))
ax_mod = readRDS(here("data", "perception", "models", "all_df_ax.rds"))

## PCT participants 

pct_ids = c(unique(eng_blp$prolific_id),
            unique(span_blp$prolific_id),
            unique(pct_eng_mono$participant),
            unique(pct_span_mono$participant))


ax_did_pct = sum(ax_ids %in% pct_ids)

e_ax = nrow(
  unique(all_df_ax %>% filter(group == "English") %>% select(id)))

e_mon_ax = nrow(
  unique(all_df_ax %>% filter(group == "English monolingual") %>% select(id)))

s_ax = nrow(
  unique(all_df_ax %>% filter(group == "Spanish") %>% select(id)))



