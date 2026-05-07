# export results by outcome and bandwidth ----
export_fct <- function(outcome, tab.cap, bandwidth = 1) {
  print(outcome)
  
  # setup
  rdd_reg <- rdd |> filter(!is.na(get(outcome)))
  Y = rdd_reg[[outcome]]
  X = rdd_reg$moy1
  donut = rdd_reg$donut == TRUE
  covs = cbind(model.matrix(~rdd_reg$gender+0),
               model.matrix(~rdd_reg$age+0),
               # model.matrix(~rdd_reg$natio+0),
               model.matrix(~rdd_reg$pcs_depp+0),
               model.matrix(~rdd_reg$parent_inc+0),
               model.matrix(~rdd_reg$echelon+0),
               model.matrix(~rdd_reg$paris_academie+0),
               model.matrix(~rdd_reg$top5_academie+0),
               model.matrix(~rdd_reg$idf_born+0),
               model.matrix(~rdd_reg$exagt_name+0),
               model.matrix(~rdd_reg$private_hs+0),
               model.matrix(~rdd_reg$year_bac+0))
  poly = c(1,1,1,2,2,2)
  controls = list(no_covs = NULL, no_covs = NULL, controls = covs)
  controls_par = rep(names(controls), 2)
  sub = list(no_donut = NULL, with_donut = donut, with_donut = donut)
  sub_par = rep(names(sub), 2)
  cutoff_16 = 16
  
  # estimation
  system.time({
    grappe <- makeCluster(n_cores)
    registerDoParallel(grappe)
    reg <- foreach(i = 1:length(poly), .packages = c("rdrobust")) %dopar% {
      rdrobust(y = Y, x = X, c = cutoff_16, all = T,
               p = poly[i],
               covs = controls[[controls_par[i]]],
               subset = sub[[sub_par[i]]],
               h = bandwidth)}
    stopCluster(grappe)
  })
  
  if(is.null(bandwidth)) {
    bandwidth = "mse"
  }
  saveRDS(reg, here("out", "regressions", paste0(outcome, "_bandwidth_", bandwidth, ".rds")))
  
  # generate regression table
  mean_y = round(mean(Y[X >= 15.5 & X < 15.7], na.rm = T), 3)
  if(bandwidth == "mse") {
    coef_table <- out_tab_latex_clean(reg, mean_baseline = mean_y, robust_ci_95 = T)
  } else {
    coef_table <- out_tab_latex_clean(reg, mean_baseline = mean_y, robust_ci_95 = F)
  }
  
  # export raw data for table
  fwrite(coef_table, here("out", "tables", "raw_data", paste0(outcome, "_baseline_bandwidth_", bandwidth, ".csv")))
  
  # export table
  export_table(coef_table,
               poly_header = TRUE,
               caption = tab.cap,
               label = paste0(outcome, "_bandwidth_", bandwidth),
               file_name = paste0(outcome, "_baseline_bandwidth_", bandwidth, ".tex"))
}
# ----