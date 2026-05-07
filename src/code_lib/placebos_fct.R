placebos_fct <- function(outcome,
                         cohort_dt,
                         rdd_dt = rdd,
                         band = 1,
                         tab.cap = tab.cap) {
  
  rdd_dt <- rdd_dt |> 
    filter(!is.na(get(outcome)))
  
  Y = rdd_dt[[outcome]]
  X = rdd_dt$moy1
  donut = rdd_dt$donut == TRUE
  donut15 = rdd_dt$donut15 == TRUE
  donut14 = rdd_dt$donut14 == TRUE
  cutoff_16 = 16
  
  ### Placebo 1: 15 ----
  sub = list(NULL, donut15)
  
  system.time({
    grappe <- makeCluster(length(sub))
    registerDoParallel(grappe)
    reg <- foreach(i=1:length(sub), .packages=c("rdrobust")) %dopar% {
      rdrobust(Y, X, c = 15, all = T, p = 1, h = band, subset = sub[[i]])}
    stopCluster(grappe)
  })
  
  baseline = round(mean(Y[X >= 14.5 & X < 14.7]), 3)
  
  saveRDS(list(reg, baseline), here("out", "regressions", paste0(outcome, "_bandwidth_", ifelse(is.null(band), "mse", band), "_placebo15.rds")))
  
  # coef_table <- out_tab_latex_clean(reg, mean_baseline = baseline,
  #                                   indep_var_name = "Effect at 15",
  #                                   donut_vec = c(F, T), controls_vec = c(F, F))
  # export_table(coef_table,
  #              poly_header = F,
  #              caption = paste0(tab.cap, " - Placebo 15"),
  #              label = paste0(outcome, "_placebo15"),
  #              file_name = paste0(outcome, "_bandwidth_", ifelse(is.null(band), "mse", band), "_placebo15.tex"))
  # -----
  
  ### Placebo 2: 14 ----
  sub = list(NULL, donut14)
  
  system.time({
    grappe <- makeCluster(length(sub))
    registerDoParallel(grappe)
    reg <- foreach(i=1:length(sub), .packages=c("rdrobust")) %dopar% {
      rdrobust(Y, X, c = 14, all = T, p = 1, h = band, subset = sub[[i]])}
    stopCluster(grappe)
  })
  
  baseline = round(mean(Y[X >= 13.5 & X < 13.7]), 3)
  
  saveRDS(list(reg, baseline), here("out", "regressions", paste0(outcome, "_bandwidth_", ifelse(is.null(band), "mse", band), "_placebo14.rds")))
  
  # coef_table <- out_tab_latex_clean(reg, mean_baseline = baseline,
  #                                   indep_var_name = "Effect at 14",
  #                                   donut_vec = c(F, T), controls_vec = c(F, F))
  # export_table(coef_table,
  #              poly_header = F,
  #              caption = paste0(tab.cap, " - Placebo 14"),
  #              label = paste0(outcome, "_placebo14"),
  #              file_name = paste0(outcome, "_bandwidth_", ifelse(is.null(band), "mse", band), "_placebo14.tex"))
  # -----
  
  ### Placebo 3: non-BCS ----
  setDT(cohort_dt)
  rdd_nonbcs <- cohort_dt[boursier_esr_all == F]
  
  # Variables
  Y_nonbcs = rdd_nonbcs[[outcome]]
  X_nonbcs = rdd_nonbcs$moy1
  donut_nonbcs = rdd_nonbcs$donut == TRUE
  
  sub_nonbcs = list(NULL, donut_nonbcs)
  
  system.time({
    grappe <- makeCluster(length(sub_nonbcs))
    registerDoParallel(grappe)
    reg <- foreach(i=1:length(sub_nonbcs), .packages=c("rdrobust")) %dopar% {
      rdrobust(Y_nonbcs, X_nonbcs, c = 16, all = T, p = 1, h = band, subset = sub_nonbcs[[i]])}
    stopCluster(grappe)
  })
  
  baseline = round(mean(Y_nonbcs[X_nonbcs >= 15.5 & X_nonbcs < 15.7]), 3)
  
  saveRDS(list(reg, baseline), here("out", "regressions", paste0(outcome, "_bandwidth_", ifelse(is.null(band), "mse", band), "_notbcs.rds")))
  
  # coef_table <- out_tab_latex_clean(reg, mean_baseline = baseline,
  #                                   indep_var_name = "Effect at 16 - Non-BCS",
  #                                   donut_vec = c(F, T), controls_vec = c(F, F))
  # export_table(coef_table,
  #              poly_header = F,
  #              caption = paste0(tab.cap, " - Placebo Non-BCS"),
  #              label = paste0(outcome, "_placebo_notbcs"),
  #              file_name = paste0(outcome, "_bandwidth_", ifelse(is.null(band), "mse", band), "_notbcs.tex"))
  # -----
}
