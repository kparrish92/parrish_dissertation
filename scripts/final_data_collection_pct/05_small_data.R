# A script to create objects for reporting

mod = read_rds(here("scripts", "final_data_collection_pct", "models",
                    "bayesian_log.RDS"))

df_bayesian = data.frame(fixef(mod))


# inputs - model 

df_bayesian = data.frame(fixef(mod))

# French /i/ (baseline)
fr_i_b = data.frame("estimate" = plogis(df_bayesian$Estimate[1]),
"hdi_hi" = plogis(df_bayesian$Q97.5[1]),
"hdi_lo" = plogis(df_bayesian$Q2.5[1])) %>% 
  mutate(language = "french", phoneme = "i", frame = "b")

fr_o_b = data.frame("estimate" = plogis(df_bayesian$Estimate[1] + 
                                           df_bayesian$Estimate[3]),
                     "hdi_hi" = plogis(df_bayesian$Q97.5[1] + 
                                         df_bayesian$Estimate[3]),
                     "hdi_lo" = plogis(df_bayesian$Q2.5[1] + 
                                         df_bayesian$Estimate[3])) %>% 
  mutate(language = "french", phoneme = "o", frame = "b")


fr_schwa_b = data.frame("estimate" = plogis(df_bayesian$Estimate[1] + 
                                              df_bayesian$Estimate[4]),
                        "hdi_hi" = plogis(df_bayesian$Q97.5[1] + 
                                            df_bayesian$Estimate[4]),
                        "hdi_lo" = plogis(df_bayesian$Q2.5[1] + 
                                            df_bayesian$Estimate[4]))  %>% 
  mutate(language = "french", phoneme = "schwa", frame = "b")


fr_y_b = data.frame("estimate" = plogis(df_bayesian$Estimate[1] + 
                                          df_bayesian$Estimate[5]),
                    "hdi_hi" = plogis(df_bayesian$Q97.5[1] + 
                                        df_bayesian$Estimate[5]),
                    "hdi_lo" = plogis(df_bayesian$Q2.5[1] + 
                                        df_bayesian$Estimate[5]))  %>% 
  mutate(language = "french", phoneme = "y", frame = "b")



# German /i/ 
ger_i_b = data.frame("estimate" = plogis(df_bayesian$Estimate[1] + 
                                          df_bayesian$Estimate[2]),
                    "hdi_hi" = plogis(df_bayesian$Q97.5[1] + 
                                        df_bayesian$Estimate[2]),
                    "hdi_lo" = plogis(df_bayesian$Q2.5[1] + 
                                        df_bayesian$Estimate[2])) %>% 
  mutate(language = "german", phoneme = "i", frame = "b")


ger_o_b = data.frame("estimate" = plogis(df_bayesian$Estimate[1] + 
                                           df_bayesian$Estimate[2] +
                                           df_bayesian$Estimate[7]),
                     "hdi_hi" = plogis(df_bayesian$Q97.5[1] + 
                                         df_bayesian$Estimate[2] +
                                       df_bayesian$Estimate[7]),
                     "hdi_lo" = plogis(df_bayesian$Q2.5[1] + 
                                         df_bayesian$Estimate[2] + 
                                       df_bayesian$Estimate[7]))  %>% 
  mutate(language = "german", phoneme = "o", frame = "b")

ger_schwa_b = data.frame("estimate" = plogis(df_bayesian$Estimate[1] + 
                                               df_bayesian$Estimate[2] +
                                               df_bayesian$Estimate[8]),
                         "hdi_hi" = plogis(df_bayesian$Q97.5[1] + 
                                             df_bayesian$Estimate[2] +
                                             df_bayesian$Estimate[8]),
                         "hdi_lo" = plogis(df_bayesian$Q2.5[1] + 
                                             df_bayesian$Estimate[2] + 
                                             df_bayesian$Estimate[8]))  %>% 
  mutate(language = "german", phoneme = "schwa", frame = "b")


ger_y_b = data.frame("estimate" = plogis(df_bayesian$Estimate[1] + 
                                           df_bayesian$Estimate[2] +
                                           df_bayesian$Estimate[9]),
                     "hdi_hi" = plogis(df_bayesian$Q97.5[1] + 
                                         df_bayesian$Estimate[2] +
                                         df_bayesian$Estimate[9]),
                     "hdi_lo" = plogis(df_bayesian$Q2.5[1] + 
                                         df_bayesian$Estimate[2] + 
                                         df_bayesian$Estimate[9]))  %>% 
  mutate(language = "german", phoneme = "y", frame = "b")

report_df <- rbind(fr_i_b,fr_o_b,
      fr_schwa_b,fr_y_b,
      ger_i_b,ger_o_b,
      ger_schwa_b,ger_y_b)

report_df %>% 
  write.csv(here("scripts", "final_data_collection_pct", 
               "data", "tidy",
               "report_bilabial_eng_l1.csv"))