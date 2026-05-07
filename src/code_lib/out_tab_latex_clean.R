# clean export latex table ----
out_tab_latex_clean <- function(reg_list,
                                robust_ci_95 = T,
                                donut_vec = c(F, T, T, F, T, T),
                                controls_vec = c(F, F, T, F, F, T),
                                indep_var_name = "Eligibility",
                                mean_baseline = round(mean(rdd[[outcome]][round(rdd$moy1,2) >= 15.5 & round(rdd$moy1,2) < 15.8]), 3),
                                digits = 3){
  
  results <- out_coef_ci(reg_list, donut_vec = donut_vec, controls_vec = controls_vec)
  
  results_clean <- results %>% 
    mutate(coef = case_when(robust_ci_95 == T ~ paste0(round(coef, digits), "$^{", robust_sig, "}$"),
                            robust_ci_95 == F ~ paste0(round(coef, digits), "$^{", sig, "}$")),
           se = paste0("(", round(se, digits), ")"),
           conventional_95_ci = paste0("[", round(ci_lower, 2), ", ", round(ci_upper, 2), "]"),
           robust_se = paste0("(", round(robust_se, digits), ")"),
           robust_95_ci = paste0("[", round(robust_ci_lower, 2), ", ", round(robust_ci_upper, 2), "]"),
           p_value = round(p_value, digits),
           robust_p_value = round(robust_p_value, digits),
           n_cont = formatC(n_cont, format = "d", big.mark = ","),
           n_treat = formatC(n_treat, format = "d", big.mark = ","),
           bandwidth = paste0("(", round(threshold - h_bandwidth, 2), ", ", round(threshold + h_bandwidth, 2), ")"),
           donut = ifelse(donut == T, "\\checkmark", ""),
           controls = ifelse(controls == T, "\\checkmark", ""),
           baseline = mean_baseline)
  
  if (robust_ci_95 == T) {
    results_clean <- results_clean %>% 
      select(coef, robust_95_ci, robust_p_value, n_cont, n_treat, poly, bandwidth, donut, controls, baseline)
  } else {
    results_clean <- results_clean %>% 
      select(coef, conventional_95_ci, p_value, n_cont, n_treat, poly, bandwidth, donut, controls, baseline)
  }
  
  results_clean = t(results_clean)
  rownames(results_clean) <- NULL
  
  results_clean <- cbind(data.frame(x = c(indep_var_name,"","","\\# obs. left","\\# obs. right","Poly. order", "Bandwidth","Donut","Controls", paste0("Mean [", results$threshold[1] - 0.5, ", ", results$threshold[1] - 0.3, ")"))),
                         results_clean)
  
  if (robust_ci_95 == T) {
    results_clean[2,1] <- "Robust 95\\% CI"
    results_clean[3,1] <- "Robust p-value"
  } else {
    results_clean[2,1] <- "Conventional 95\\% CI"
    results_clean[3,1] <- "Conventional p-value"
  }
  
  return(results_clean)
}
# ----