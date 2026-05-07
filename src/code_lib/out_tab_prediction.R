# function to generate discontinuity in outcome prediction table ----
out_tab_prediction <- function(data = coef_ci) {
  
  results = data %>% 
    filter(donut %in% c(F, "[15.7, 16.05]")) %>% 
    mutate(coef = ifelse(bandwidth == "MSE optimal",
                         paste0(round(coef, 3), "$^{", robust_sig, "}$"),
                         paste0(round(coef, 3), "$^{", sig, "}$")),
           ci = ifelse(bandwidth == "MSE optimal",
                       paste0("[", round(robust_ci_lower, 2), ", ", round(robust_ci_upper, 2), "]"),
                       paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")))
  
  results_clean <- data.frame()
  
  vars = results %>% pull(outcomes) %>% unique()
  z = 1
  
  for (i in 1:length(vars)) {
    print(i)
    # variable
    results_clean[z,1] <- results %>%
      filter(outcomes == vars[i]) %>% 
      filter(donut == F & bandwidth == "MSE optimal") %>%
      pull(outcomes_lab_clean)
    
    # coefficient
    results_clean[z,2] <- results %>%
      filter(outcomes == vars[i]) %>% 
      filter(donut == F & bandwidth == "MSE optimal") %>%
      pull(coef)
    results_clean[z,3] <- results %>%
      filter(outcomes == vars[i]) %>% 
      filter(donut == F & bandwidth == "[15, 17]") %>% 
      pull(coef)
    results_clean[z,4] <- results %>%
      filter(outcomes == vars[i]) %>% 
      filter(donut == "[15.7, 16.05]" & bandwidth == "MSE optimal") %>% 
      pull(coef)
    results_clean[z,5] <- results %>%
      filter(outcomes == vars[i]) %>% 
      filter(donut == "[15.7, 16.05]" & bandwidth == "[15, 17]") %>% 
      pull(coef)
    
    z = z + 1
    
    # row name
    results_clean[z,1] <- ""
    
    # robust confidence interval
    results_clean[z,2] <- results %>%
      filter(outcomes == vars[i]) %>% 
      filter(donut == F & bandwidth == "MSE optimal") %>%
      pull(ci)
    results_clean[z,3] <- results %>%
      filter(outcomes == vars[i]) %>% 
      filter(donut == F & bandwidth == "[15, 17]") %>% 
      pull(ci)
    results_clean[z,4] <- results %>%
      filter(outcomes == vars[i]) %>% 
      filter(donut == "[15.7, 16.05]" & bandwidth == "MSE optimal") %>% 
      pull(ci)
    results_clean[z,5] <- results %>%
      filter(outcomes == vars[i]) %>% 
      filter(donut == "[15.7, 16.05]" & bandwidth == "[15, 17]") %>% 
      pull(ci)
    
    z = z + 1
  }
  
  # number of observations (total = control + treatment, single row) ----
  n_15_17_no_donut <- results |>
    filter(h_bandwidth == 1 & donut == F & outcomes == "enrol_year_bac_all_0914") |>
    distinct(n_cont, n_treat) |>
    mutate(total = n_cont + n_treat) |>
    pull(total)

  n_15_17_donut <- results |>
    filter(h_bandwidth == 1 & donut == "[15.7, 16.05]" & outcomes == "enrol_year_bac_all_0914") |>
    distinct(n_cont, n_treat) |>
    mutate(total = n_cont + n_treat) |>
    pull(total)

  n_mse_no_donut <- results |>
    filter(h_bandwidth != 1 & donut == F) |>
    summarise(total = round(mean(n_cont + n_treat), 0)) |>
    pull(total)

  n_mse_donut <- results |>
    filter(h_bandwidth != 1 & donut == "[15.7, 16.05]") |>
    summarise(total = round(mean(n_cont + n_treat), 0)) |>
    pull(total)

  n_obs_dt <- tibble::tribble(~a, ~b, ~c, ~d, ~e,
                              "Observations", n_mse_no_donut, n_15_17_no_donut, n_mse_donut, n_15_17_donut)
  n_obs_dt <- as.data.frame(n_obs_dt) |>
    mutate(across(where(is.numeric), ~ format(.x, big.mark = ",")))
  names(n_obs_dt) <- names(results_clean)
  # ----
  
  # bind estimates with number of observables
  results_clean <- results_clean |> 
    bind_rows(n_obs_dt)
  
  colnames(results_clean) <- c("", rep(c("MSE-Optimal", "[15, 17]"), 2))
  
  return(results_clean)
}
# ----