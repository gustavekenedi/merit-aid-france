out_tab_intensive_margin <- function(out.vars.vec,
                                         out.lab.vec) {
  
  1:length(out.vars.vec) %>% 
    map_dfr(function(x) {
      
      coef_table_mse <- out_coef_ci(read_rds(here("out", "regressions", paste0(out.vars.vec[x], "_bandwidth_mse.rds")))) %>% 
        mutate(bandwidth = "mse-optimal")
      coef_table_1517 <- out_coef_ci(read_rds(here("out", "regressions", paste0(out.vars.vec[x], "_bandwidth_1.rds")))) %>% 
        mutate(bandwidth = "(15, 17)")
      coef_all <- bind_rows(coef_table_mse,
                            coef_table_1517) |> 
        mutate(outcome = out.vars.vec[x],
               outcome_lab = out.lab.vec[x])
      
      mean_y_outcomes <- fread(here("out/data/mean_y_outcomes.csv"))
      coef_all <- coef_all |> 
        left_join(mean_y_outcomes)
      
      coef_all %>%
        filter(poly == 1 & donut == T & controls == F) %>% 
        mutate(baseline = mean_y,
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
    })
}