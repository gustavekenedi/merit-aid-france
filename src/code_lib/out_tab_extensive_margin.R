out_tab_extensive_margin <- function(coef_ci = coef_ci_ext_margin_clean) {
  
  coef_table_clean <- coef_ci %>%
    filter(poly == 1 & donut == T & controls == F) %>% 
    mutate(baseline = mean_y,
           bandwidth = ifelse(h_bandwidth != 1, "mse-optimal", "(15, 17)"),
           coef = as.character(ifelse(bandwidth == "mse-optimal",
                                      paste0(round(coef, 3), "$^{", robust_sig, "}$"),
                                      paste0(round(coef, 3), "$^{", sig, "}$"))),
           ci = ifelse(bandwidth == "mse-optimal",
                       paste0("[", round(robust_ci_lower, 2), ", ", round(robust_ci_upper, 2), "]"),
                       paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")),
           n_obs = formatC(n_cont + n_treat, format = "d", big.mark = ",")) %>% 
    select(outcome_lab, baseline, bandwidth, coef, ci, n_obs) %>%
    pivot_wider(names_from = bandwidth,
                values_from = c(coef, ci, n_obs),
                names_glue = "{bandwidth}_{.value}") %>%
    select(outcome_lab, baseline,
           `mse-optimal_coef`, `mse-optimal_ci`, `mse-optimal_n_obs`,
           `(15, 17)_coef`, `(15, 17)_ci`, `(15, 17)_n_obs`)
}