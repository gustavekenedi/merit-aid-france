# packages
library(tidyverse)
library(data.table)

# load data
cohort_09_14 <- fread(here("out/data/cohort_09_14.csv"))

# only need-based grant eligible students
rdd <- cohort_09_14[boursier_esr_all == T][!(mention != "Très Bien" & aidemerite_new == T)]

# replace by NA degree completion and min years to degree for students in BTS and other BPBAC because I cannot observe their degree in the graduation data
rdd <- rdd |>
  mutate(across(c(grad_anytime_all, min_years_since_bac), ~ ifelse(source_sise == "bpbac" & ins_princip_lab != "cpge", NA, .x)))

# create variables for main enrollment in HS graduation year
rdd[, enrol_year_bac_all_0914_licence := (ins_princip_lab == "licence/master")]
rdd[, enrol_year_bac_all_0914_medecine := (ins_princip_lab == "medecine")]
rdd[, enrol_year_bac_all_0914_bts := (ins_princip_lab == "bts")]
rdd[, enrol_year_bac_all_0914_dut := (ins_princip_lab == "dut")]
rdd[, enrol_year_bac_all_0914_cpge := (ins_princip_lab == "cpge")]
rdd[, enrol_year_bac_all_0914_other := (ins_princip_lab %in% c("iep/dauphine", "inge", "priv", "mana"))]

out.vars <- c("enrol_year_bac_all_0914",
              "enrol_anytime_all_0914",
              "enrol_year_bac_all_0914_licence",
              "enrol_year_bac_all_0914_medecine",
              "enrol_year_bac_all_0914_bts",
              "enrol_year_bac_all_0914_dut",
              "enrol_year_bac_all_0914_cpge",
              "enrol_year_bac_all_0914_other",
              "enrol_2_ontime_all_0914",
              "enrol_2_anytime_all_0914",
              "enrol_3_ontime_all_0914",
              "enrol_3_anytime_all_0914",
              "quality",
              "quality_bacyear_plus1",
              "quality_bacyear_plus2",
              "tot_enrol_yrs",
              "max_enrol_degetu",
              "grad_5_all",
              "grad_7_all",
              "grad_9_all",
              "master_anytime_all",
              "max_ects_credits",
              "master_select_anytime_all",
              "quality_first_grad_degree")


# obtain mean Y
mean_y_outcomes <- 1:length(out.vars) %>% 
  map(function(x) {
    print(out.vars[x])
    rdd <- rdd |> filter(!is.na(get(out.vars[x])))
    Y = rdd[[out.vars[x]]]
    X = rdd$moy1
    mean_y = round(mean(Y[X >= 15.5 & X < 15.7], na.rm = T), 2)
    
    mean_y_dt <- data.frame(outcome = out.vars[x],
                            mean_y = mean_y)
    
    return(mean_y_dt)
    
  }) |> 
  rbindlist()
fwrite(mean_y_outcomes, here("out/tables/raw_data/mean_y_outcomes.csv"))