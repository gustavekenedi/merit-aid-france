# outcomes
outcomes <- c("enrol_year_bac_all_0914",
              "enrol_anytime_all_0914",
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
              "max_ects_credits",
              "master_anytime_all",
              "master_select_anytime_all",
              "quality_first_grad_degree")

# outcome labels
outcomes_lab <- c("Enrollment in the High School Graduation Year",
                  "Enrollment at Least Once",
                  "Enrollment in 2nd Year in High School Graduation Year + 1",
                  "Enrollment in 2nd Year at Least Once",
                  "Enrollment in 3rd Year in High School Graduation Year + 2",
                  "Enrollment in 3rd Year at Least Once",
                  "Degree Quality in High School Graduation Year",
                  "Degree Quality in High School Graduation Year + 1",
                  "Degree Quality in High School Graduation Year + 2",
                  "Number of Years Enrolled in Higher Education",
                  "Highest Level of Study Attained (in Years)",
                  "Degree Completion within 5 Years",
                  "Degree Completion within 7 Years",
                  "Degree Completion within 9 Years",
                  "Total Number of Credits Obtained",
                  "Enrolling in a Masters Degree",
                  "Enrolling in a Selective Masters Degree",
                  "First Graduate Degree Quality")

# outcome labels short
outcomes_lab_short <- c("Enrollment in the HS Grad. Year",
                        "Enrollment at Least Once",
                        "Enrollment in 2nd Year in HS Grad, Year + 1",
                        "Enrollment in 2nd Year at Least Once",
                        "Enrollment in 3rd Year in HS Grad. Year + 2",
                        "Enrollment in 3rd Year at Least Once",
                        "Degree Quality in HS Grad. Year",
                        "Degree Quality in HS Grad. Year + 1",
                        "Degree Quality in HS Grad. Year + 2",
                        "Number of Years Enrolled in HS",
                        "Highest Level of Study Attained",
                        "Degree Completion within 5 Years",
                        "Degree Completion within 7 Years",
                        "Degree Completion within 9 Years",
                        "Total Number of Credits Obtained",
                        "Enrolling in a Masters Degree",
                        "Enrolling in a Selective Masters Degree",
                        "First Graduate Degree Quality")

out_vars_labs <- data.frame(outcome = outcomes,
                            outcome_lab = outcomes_lab,
                            outcome_lab_short = outcomes_lab_short,
                            category = c(rep("Discrete Outcomes", 6), rep("Continuous Outcomes", 5), rep("Discrete Outcomes", 3), "Credits", rep("Discrete Outcomes", 2), rep("Continuous Outcomes", 1)),
                            order = 1:length(outcomes)) |> 
  mutate(category = fct_relevel(category, "Discrete Outcomes", "Continuous Outcomes", "Credits"))


fwrite(out_vars_labs, here("out/data/out_vars_labs.csv"))
