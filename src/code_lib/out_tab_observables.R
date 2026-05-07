# Prepare data for the latex regression table for observables methodology
# validity test. Returns a data frame; the kable formatting is done in the
# calling notebook (5_paper_figures_tables.Rmd). ----
out_tab_observables <- function(data = coef_observables,
                                ci_not_robust_when_not_mse = TRUE,
                                ci_digits = 2) {

  results = data %>%
    # filter(donut %in% c(F, "[15.7, 16.05]")) %>%
    mutate(coef = ifelse(variable_lab == "Parent Income", round(coef), round(coef, 3)),
           conventional_ci = paste0("[", ifelse(variable_lab == "Parent Income", round(ci_lower), round(ci_lower, ci_digits)), ", ", ifelse(variable_lab == "Parent Income", round(ci_upper), round(ci_upper, ci_digits)), "]"),
           robust_ci = paste0("[", ifelse(variable_lab == "Parent Income", round(robust_ci_lower), round(robust_ci_lower, ci_digits)), ", ", ifelse(variable_lab == "Parent Income", round(robust_ci_upper), round(robust_ci_upper, ci_digits)), "]"),
           variable_lab = as.character(variable_lab),
           mean_y_clean = case_when(variable_lab == "Age" ~ as.character(round(mean_y, 2)),
                                    mean_y > 1 ~ formatC(mean_y, format = "d", big.mark = ","),
                                    TRUE ~ as.character(round(mean_y, 2))))

  if (ci_not_robust_when_not_mse == T) {
    results <- results %>%
      mutate(coef = case_when(bandwidth == "MSE-Optimal" ~ paste0(coef, "$^{", robust_sig, "}$"),
                              bandwidth != "MSE-Optimal" ~ paste0(coef, "$^{", sig, "}$")))

  } else {
    results <- results %>%
      mutate(coef = paste0(coef, "$^{", robust_sig, "}$"))
  }

  results_clean <- data.frame()

  vars = results %>% pull(variable) %>% unique()
  z = 1

  for (i in 1:length(vars)) {
    print(i)
    # variable
    results_clean[z,1] <- results %>%
      filter(variable == vars[i]) %>%
      filter(donut == "No Donut" & bandwidth == "MSE-Optimal") %>%
      pull(variable_lab)

    # outcome average
    results_clean[z,2] <- results %>%
      filter(variable == vars[i]) %>%
      filter(donut == "No Donut" & bandwidth == "MSE-Optimal") %>%
      pull(mean_y_clean)

    # coefficients
    results_clean[z,3] <- results %>%
      filter(variable == vars[i]) %>%
      filter(donut == "No Donut" & bandwidth == "MSE-Optimal") %>%
      pull(coef)
    results_clean[z,4] <- results %>%
      filter(variable == vars[i]) %>%
      filter(donut == "No Donut" & bandwidth == "(15, 17)") %>%
      pull(coef)
    results_clean[z,5] <- results %>%
      filter(variable == vars[i]) %>%
      filter(donut == "Donut [15.7, 16.05]" & bandwidth == "MSE-Optimal") %>%
      pull(coef)
    results_clean[z,6] <- results %>%
      filter(variable == vars[i]) %>%
      filter(donut == "Donut [15.7, 16.05]" & bandwidth == "(15, 17)") %>%
      pull(coef)

    z = z + 1

    # row name
    results_clean[z,c(1,2)] <- ""

    # robust confidence interval
    results_clean[z,3] <- results %>%
      filter(variable == vars[i]) %>%
      filter(donut == "No Donut" & bandwidth == "MSE-Optimal") %>%
      pull(robust_ci)
    if(ci_not_robust_when_not_mse == T) {
      results_clean[z,4] <- results %>%
        filter(variable == vars[i]) %>%
        filter(donut == "No Donut" & bandwidth == "(15, 17)") %>%
        pull(conventional_ci)
    } else {
      results_clean[z,4] <- results %>%
        filter(variable == vars[i]) %>%
        filter(donut == "No Donut" & bandwidth == "(15, 17)") %>%
        pull(robust_ci)
    }
    results_clean[z,5] <- results %>%
      filter(variable == vars[i]) %>%
      filter(donut == "Donut [15.7, 16.05]" & bandwidth == "MSE-Optimal") %>%
      pull(robust_ci)
    if (ci_not_robust_when_not_mse == T) {
      results_clean[z,6] <- results %>%
        filter(variable == vars[i]) %>%
        filter(donut == "Donut [15.7, 16.05]" & bandwidth == "(15, 17)") %>%
        pull(conventional_ci)
    } else {
      results_clean[z,6] <- results %>%
        filter(variable == vars[i]) %>%
        filter(donut == "Donut [15.7, 16.05]" & bandwidth == "(15, 17)") %>%
        pull(robust_ci)
    }

    z = z + 1
  }

  colnames(results_clean) <- c("", rep(c("MSE-Optimal", "+/- 1"), 2))

  # number of observations (total = control + treatment, single row) ----
  n_15_17_no_donut <- results |>
    filter(h_bandwidth == 1 & donut == "No Donut" & variable == "female") |>
    distinct(n_cont, n_treat) |>
    mutate(total = n_cont + n_treat) |>
    pull(total)

  n_15_17_donut <- results |>
    filter(h_bandwidth == 1 & donut == "Donut [15.7, 16.05]" & variable == "female") |>
    distinct(n_cont, n_treat) |>
    mutate(total = n_cont + n_treat) |>
    pull(total)

  n_mse_no_donut <- results |>
    filter(h_bandwidth != 1 & donut == "No Donut") |>
    summarise(total = round(mean(n_cont + n_treat), 0)) |>
    pull(total)

  n_mse_donut <- results |>
    filter(h_bandwidth != 1 & donut == "Donut [15.7, 16.05]") |>
    summarise(total = round(mean(n_cont + n_treat), 0)) |>
    pull(total)

  n_obs_dt <- tibble::tribble(~a, ~b, ~c, ~d, ~e, ~f,
                              "Observations", "", n_mse_no_donut, n_15_17_no_donut, n_mse_donut, n_15_17_donut)
  n_obs_dt <- as.data.frame(n_obs_dt) |>
    mutate(across(where(is.numeric), ~ format(.x, big.mark = ",")))
  names(n_obs_dt) <- names(results_clean)
  # ----

  # joint significance test (F-stat and p-value rendered in italic) ----
  joint_sig_test <- tibble::tribble(~a, ~b, ~c, ~d, ~e, ~f,
                                    "\\textit{F}-stat", "",  round(joint_test_nodonut_mse$F[2], 2),         round(joint_test_nodonut_15_17$F[2], 2),         round(joint_test_donut_mse$F[2], 2),         round(joint_test_donut_15_17$F[2], 2),
                                    "\\textit{p}-value", "", round(joint_test_nodonut_mse$`Pr(>F)`[2], 3),  round(joint_test_nodonut_15_17$`Pr(>F)`[2], 3),  round(joint_test_donut_mse$`Pr(>F)`[2], 3),  round(joint_test_donut_15_17$`Pr(>F)`[2], 3))
  joint_sig_test <- as.data.frame(joint_sig_test) |>
    mutate(across(everything(), ~ as.character(.x)))
  names(joint_sig_test) <- names(results_clean)
  # ----

  # bind estimates with joint sig test and number of observations row
  results_clean <- results_clean |>
    bind_rows(joint_sig_test) |>
    bind_rows(n_obs_dt)

  return(results_clean)
}
# ----
