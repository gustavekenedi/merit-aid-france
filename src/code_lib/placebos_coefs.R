placebos_coefs <- function(out_vars) {
  
  coef_placebos <- 1:length(out_vars) %>% 
    map_dfc(function(x) {
      
      placebo_15 <- readRDS(here("out", "regressions", paste0(out_vars[x], "_bandwidth_", band, "_placebo15.rds")))
      placebo_14 <- readRDS(here("out", "regressions", paste0(out_vars[x], "_bandwidth_", band, "_placebo14.rds")))
      placebo_notbcs <- readRDS(here("out", "regressions", paste0(out_vars[x], "_bandwidth_", band, "_notbcs.rds")))
      
      coefs <- bind_rows(out_coef_ci(placebo_15[[1]],
                                     donut_vec = c(F, T), controls_vec = c(F, F)) %>% 
                           mutate(outcome = out_vars[x],
                                  placebo = "15",
                                  mean_y = placebo_15[[2]]),
                         out_coef_ci(placebo_14[[1]],
                                     donut_vec = c(F, T), controls_vec = c(F, F)) %>% 
                           mutate(outcome = out_vars[x],
                                  placebo = "14",
                                  mean_y = placebo_14[[2]]),
                         out_coef_ci(placebo_notbcs[[1]],
                                     donut_vec = c(F, T), controls_vec = c(F, F)) %>% 
                           mutate(outcome = out_vars[x],
                                  placebo = "notbcs",
                                  mean_y = placebo_notbcs[[2]])) %>%
        mutate(coef = case_when(band == "mse" ~ paste0(round(coef, 3), "$^{", robust_sig, "}$"),
                                band != "mse" ~ paste0(round(coef, 3), "$^{", sig, "}$")),
               ci = case_when(band == "mse" ~ paste0("[", round(robust_ci_lower, 2), ", ", round(robust_ci_upper, 2), "]"),
                              band != "mse" ~ paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]")),
               # total number of observations (control + treatment) on each
               # side of the threshold, formatted with thousands separator
               n_obs = format(n_cont + n_treat, big.mark = ",")) %>%
        select(outcome, placebo, mean_y, coef, ci, n_obs, donut) %>%
        mutate(mean_y = as.character(round(mean_y, 2))) |>
        pivot_longer(cols = c(mean_y, coef, ci, n_obs),
                     names_to = "stat") %>%
        select(placebo, donut, stat, !!out_vars[x] := value)
    })

  coef_placebos <- coef_placebos %>%
    select(donut = donut...2, stat = stat...3, all_of(outcomes)) %>%
    mutate(donut = case_when(stat == "mean_y" ~ "Mean [15.5, 15.7)",
                             donut == F & stat == "coef" ~ "No donut",
                             donut == F & stat == "ci" ~ "",
                             donut == F & stat == "n_obs" ~ "Observations",
                             donut == T & stat == "coef" ~ "Donut [15.7, 16.05]",
                             donut == T & stat == "ci" ~ "",
                             donut == T & stat == "n_obs" ~ "Observations")) %>%
    select(donut, all_of(outcomes))
}
