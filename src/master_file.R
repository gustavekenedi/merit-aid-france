# MASTER file for cleaning and analysis scripts
####################################################
## Project: Beyond the Enrollment Gap: Financial Barriers and High-Achieving, Low Income Students' Persistence in Higher Education
## Journal: Journal of Human Resources
## Date created: 24.9.2025
## Author: Gustave Kenedi
####################################################

# 0. Packages --------------------------------------------------------------
library(knitr)
library(here)


# DATA PREPARATION --------------------------------------------------------
# NOTE: the code files in 01_data_preparation are to prepare the raw data for
# analysis. They were originally run in a separate R project, so some of the
# relative file paths inside them may need to be updated to work from this
# project's root before they can be re-run.

# 01A. Higher education institution names ---------------------------------
source(here("src/01_data_preparation/01a_hei_names.R"))

# 01B. Merge sector and discipline classifications ------------------------
source(here("src/01_data_preparation/01b_merge_sectdis_discipli.R"))

# 02. Data merging and anonymization --------------------------------------
source(purl(here("src/01_data_preparation/02_data_merging_anonym.Rmd"), output = tempfile()))


# DATA CLEANING -----------------------------------------------------------

# 1A. Bac (OCEAN) data -----------------------------------------------------
# last run: 26.11.2025
source(purl(here("src/02_data_cleaning/1a_ocean_cleaning_09_14.Rmd"), output = tempfile()))

# 1B. Financial aid application (AGLAE) data -------------------------------
# last run: 26.11.2025
source(purl(here("src/02_data_cleaning/1b_aglae_cleaning_09_14.Rmd"), output = tempfile()))

# 1C. HE enrollment (SISE + BPBAC) data ---------------------------------------
# last run: 26.11.2025
source(purl(here("src/02_data_cleaning/1c_all_enrol_cleaning_09_14.Rmd"), output = tempfile()))

# 1D. HE graduation (SISE + BPBAC) data ---------------------------------------
# last run: 26.11.2025
source(purl(here("src/02_data_cleaning/1d_all_result_cleaning_09_14.Rmd"), output = tempfile()))

# 2A. Sample construction --------------------------------------------------
# last run: 26.11.2025
source(purl(here("src/02_data_cleaning/2a_cohort_09_14.Rmd"), output = tempfile()))

# 2B. Variable creation ---------------------------------------------------
# last run: 26.11.2025
source(purl(here("src/02_data_cleaning/2b_variable_creation_09_14.Rmd"), output = tempfile()))


# DATA ANALYSIS -----------------------------------------------------------

# 0A. Descriptive statistics ---------------------------------------------------
source(purl(here("src/03_analysis/0a_descriptive_stats_09_14_final.Rmd"), output = tempfile()))

# 0B. Stats in paper ---------------------------------------------------
source(purl(here("src/03_analysis/0b_various_stats_in_paper_09_14_final.Rmd"), output = tempfile()))

# 0C. OVE student survey: financial difficulties (merit aid v. high-income) ----
source(purl(here("src/03_analysis/0c_student_survey_financial_difficulties.qmd"), output = tempfile()))

# 1A. Method robustness ---------------------------------------------------
source(purl(here("src/03_analysis/1a_method_robustness_09_14_final.Rmd"), output = tempfile()))

# 2A. Extensive margin results --------------------------------------------------------
source(purl(here("src/03_analysis/2a_extensive_margin_analysis_09_14_final.Rmd"), output = tempfile()))

# 2B. Intensive margin results ------------------------------------------
source(purl(here("src/03_analysis/2b_intensive_margin_analysis_09_14_final.Rmd"), output = tempfile()))

# 2C. Robustness -----------------------------------------------
source(purl(here("src/03_analysis/2c_robustness_09_14_final.Rmd"), output = tempfile()))

# 3A. OVE student survey: expensive urban areas ---------------------------
source(purl(here("src/03_analysis/3a_ove_expensive_urban_areas.qmd"), output = tempfile()))

# 3B. Heterogeneity --------------------------------------------------------
source(purl(here("src/03_analysis/3b_heterogeneity_09_14_final.Rmd"), output = tempfile()))

# 4. OVE student survey: impact of paid work on studies -------------------
source(purl(here("src/03_analysis/4_student_survey_impact_work_studies.qmd"), output = tempfile()))

# 5. Figure and Tables -----------------------------------------------------------
source(purl(here("src/03_analysis/5_paper_figures_tables.Rmd"), output = tempfile()))
