# appendix table by outcome ----
appendix_table_fct <- function(outcome_var, tab.cap) {
  coefs_mse <- fread(here("out", "tables", "raw_data", paste0(outcome_var, "_baseline_bandwidth_mse.csv")))
  coefs_1517 <- fread(here("out", "tables", "raw_data", paste0(outcome_var, "_baseline_bandwidth_1.csv")))
  
  coefs_all <- bind_rows(coefs_mse,
                         coefs_1517)
  
  # drop first columns with poly.order, donut, controls and mean
  coefs_all <- coefs_all %>% 
    filter(!row_number() %in% c(1, 7, 9:12))
  coefs_all <- bind_rows(coefs_all[c(1:11,13)],
                         coefs_all[c(12, 14:16)])
  # export raw data
  fwrite(coefs_all, here("out", "tables", "raw_data", paste0(outcome_var, "_baseline_all.csv")))
  # export table
  export_table_appendix(input = coefs_all,
                        caption = tab.cap,
                        label = paste0(outcome_var, "_baseline_bandwidth_all"),
                        file_name = paste0(outcome_var, "_baseline_all"))
}
# ----