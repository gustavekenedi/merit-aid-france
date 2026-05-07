# estimate RD by variable group ----
rdd_reg_bygroup <- function(dt = rdd,
                            out = outcome,
                            cutoff = 16,
                            poly_max = 1,
                            group_var = NULL,
                            bandwidth = NULL,
                            with_controls = F,
                            without_donut = F) {
  
  # number of groups
  n_group <- dt[[group_var]] %>% unique()
  n_group <- n_group[!is.na(n_group) & !str_detect(n_group, "NA")]
  
  # vector of polynomials
  if (with_controls == F && without_donut == F) {
    poly <- rep(1:poly_max)
  } else if ((with_controls == T && without_donut == F) | (with_controls == F & without_donut == T)) {
    poly <- rep(1:poly_max, each = 2)
  } else if (with_controls == T & without_donut == T) {
    poly <- rep(1:poly_max, each = 4)
  }
  
  n_group_par <- rep(n_group, each = length(poly)*poly_max)
  poly_par = rep(poly, length(n_group_par)/(length(poly)*poly_max))
  
  system.time({
    grappe <- makeCluster(n_cores)
    registerDoParallel(grappe)
    
    coef_ci <- foreach(i = 1:length(poly_par),
                       .packages=c("rdrobust", "data.table", "tidyverse", "here"),
                       .combine = rbind) %dopar% {
      
      setDT(dt)
      rdd_gp <- dt[get(group_var) %in% n_group_par[[i]]]
      
      # variables
      Y = rdd_gp[[out]]
      X = rdd_gp$moy1
      if (with_controls == T) {
        if (group_var == "gender") {
          covs = cbind(model.matrix(~rdd_gp$natio+0),
                       model.matrix(~rdd_gp$age+0),
                       model.matrix(~rdd_gp$pcs_depp+0),
                       model.matrix(~rdd_gp$parent_inc+0),
                       model.matrix(~rdd_gp$acam+0),
                       model.matrix(~rdd_gp$exagt_name+0),
                       model.matrix(~rdd_gp$private_hs+0),
                       model.matrix(~rdd_gp$year_bac+0))
        }
        if (group_var == "pcs_depp") {
          covs = cbind(model.matrix(~rdd_gp$gender+0),
                       model.matrix(~rdd_gp$age+0),
                       model.matrix(~rdd_gp$natio+0),
                       model.matrix(~rdd_gp$parent_inc+0),
                       model.matrix(~rdd_gp$acam+0),
                       model.matrix(~rdd_gp$exagt_name+0),
                       model.matrix(~rdd_gp$private_hs+0),
                       model.matrix(~rdd_gp$year_bac+0))
        }
        if (group_var == "exagt_name") {
          covs = cbind(model.matrix(~rdd_gp$gender+0),
                       model.matrix(~rdd_gp$natio+0),
                       model.matrix(~rdd_gp$pcs_depp+0),
                       model.matrix(~rdd_gp$parent_inc+0),
                       model.matrix(~rdd_gp$acam+0),
                       model.matrix(~rdd_gp$private_hs+0),
                       model.matrix(~rdd_gp$year_bac+0))
        }
        if (!group_var %in% c("gender", "pcs_depp", "exagt_name")) {
          covs = cbind(model.matrix(~rdd_gp$gender+0),
                       model.matrix(~rdd_gp$age+0),
                       # model.matrix(~rdd_gp$natio+0),
                       model.matrix(~rdd_gp$pcs_depp+0),
                       # model.matrix(~rdd_gp$parent_inc+0),
                       # model.matrix(~rdd_gp$acam+0),
                       model.matrix(~rdd_gp$exagt_name+0),
                       # model.matrix(~rdd_gp$private_hs+0),
                       model.matrix(~rdd_gp$year_bac+0)
          )
        }
      }
      
      donut = rdd_gp$donut == TRUE
      
      if (with_controls == T && without_donut == T) {
        controls = rep(list(NULL, covs, NULL, covs), length(n_group_par)/(length(poly)*poly_max))
        sub = rep(list(NULL, donut), each = length(poly)/(2*poly_max), length(n_group)*poly_max)
      } else if (with_controls == T && without_donut == F) {
        controls = rep(list(NULL, covs), length(n_group_par)/(length(poly)*poly_max))
        sub = rep(list(donut, donut), each = length(poly)/(2*poly_max), length(n_group)*poly_max)
      } else if(with_controls == F && without_donut == T) {
        controls = rep(list(NULL, NULL), length(n_group_par)/(length(poly)*poly_max))
        sub = rep(list(NULL, donut), each = length(poly)/(2*poly_max), length(n_group)*poly_max)
      } else if (with_controls == F && without_donut == F) {
        controls = rep(list(NULL), length(n_group_par)/(length(poly)*poly_max))
        sub = rep(list(donut), each = length(poly)/(poly_max), length(n_group)*poly_max)
      }
      
      # if (with_controls == T) { 
      #   controls = rep(list(NULL, covs, NULL, covs), length(n_group_par)/(length(poly)*poly_max))
      # }
      # sub = rep(list(NULL, donut), each = length(poly)/(2*poly_max), length(n_group)*poly_max)
      
      if (is.null(bandwidth)) {
        # MSE-optimal bandwidth
        reg <- rdrobust(Y, X, c = cutoff, all = T,
                        p = poly_par[[i]], covs = if(with_controls == T) controls[[i]] else NULL, subset = sub[[i]])
        
      } else {
        # specified bandwidth
        reg <- rdrobust(Y, X, c = cutoff, all = T, h = bandwidth,
                        p = poly_par[[i]], covs = if(with_controls == T) controls[[i]] else NULL, subset = sub[[i]])
      }
      
      # save coefficients for graph
      source(here("src", "code_lib", "out_coef_ci.R"))
      data <- out_coef_ci(list(reg),
                          donut_vec = ifelse(is.null(sub[[i]]), F, T),
                          controls_vec = ifelse(with_controls == T, ifelse(is.null(controls[[i]]), F, T), F)) %>% 
        mutate({{ group_var }} := n_group_par[[i]],
               mean_y = mean(Y[X >= 15.5 & X < 15.7], na.rm = T))
    }
    
    stopCluster(grappe)
  })
  
  return(coef_ci)
}
# ----