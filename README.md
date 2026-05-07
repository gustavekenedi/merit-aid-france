# Replication files for *Beyond the Enrollment Gap: Financial Barriers and High-Achieving, Low-Income Students' Persistence in Higher Education*

*Author:* Gustave Kenedi

*Journal:* Journal of Human Resources

**Should you notice any error(s) in my code or issues with my analysis, please reach out at `gustavekenedi@gmail.com`, I'll be happy to discuss it with you and make any corrections if necessary.**

## General notes

This repository contains all the code files necessary to replicate the paper's results, figures, and tables. **(Almost) all the figures and tables in the paper can be reproduced directly from this repository, even without access to the underlying microdata**: the figure- and table-level CSV/RDS files shipped under `out/figures/raw_data/` and `out/tables/raw_data/` are sufficient, and `src/03_analysis/5_paper_figures_tables.Rmd` rebuilds every figure and table from those files. Re-running the full pipeline end-to-end (cleaning → estimation → outputs) requires access to the raw microdata; see the [Data](#data) section below.

#### Software requirements

You can download `R` [here](https://cloud.r-project.org/) and `RStudio` [here](https://posit.co/downloads/). Once all set up, the necessary packages will be installed on first run by the notebooks themselves.

#### Folders

The repository is organised as follows:

```
replication_files/
├── src/
│   ├── master_file.R                          # orchestrates the full pipeline
│   ├── 01_data_preparation/                   # raw → usable preprocessing
│   ├── 02_data_cleaning/                      # source-specific cleaning + sample construction
│   ├── 03_analysis/                           # descriptives, RDD, robustness, heterogeneity
│   └── code_lib/                              # shared functions used across cleaning + analysis
├── out/
│   ├── data/                                  # outcome variable labels
│   ├── figures/                               # paper figures (.pdf) + raw_data/ (CSV/RDS)
│   ├── regressions/                           # regression output tables
│   └── tables/                                # LaTeX tables + raw_data/
└── replication_files.Rproj
```

- `src/master_file.R`: orchestrates the full pipeline.
- `src/01_data_preparation/`: prepares the raw administrative data for analysis. These scripts were originally run in a separate R project, so some relative file paths inside them may need to be updated to work from this project's root before they can be re-run.
- `src/02_data_cleaning/`: cleans each administrative source (OCEAN, AGLAE, SISE, BPBAC) and builds the analysis sample.
- `src/03_analysis/`: descriptive statistics, the main RDD estimation, method-robustness checks, heterogeneity analyses, OVE-survey-based descriptive analyses, and the script that assembles the paper's figures and tables.
- `src/code_lib/`: shared functions used across the cleaning and analysis files.
- `out/`: paper figures (.pdf), LaTeX tables, regression outputs, and the figure-/table-level raw-data CSVs/RDS that allow regenerating the outputs without re-running the pipeline.

## Data

The project relies on two data sources. None of the underlying microdata can be shared in this repository — both must be obtained from their respective providers.

### 1. Administrative education data — DEPP, restricted access

The bulk of the analysis uses linked French administrative data accessed through the **DEPP** (*Direction de l'évaluation, de la prospective et de la performance*), the statistical office of the French Ministry of Education. Instructions for requesting access are documented in the DEPP data catalogue FAQ: <https://catalogue.depp.education.fr/index.php/faq>. **I'm happy to help with the access process.** The datasets used are:

- **High school exit exam (Bac)** — _Organisation des Concours et des Examens Académiques et Nationaux (OCEAN)_, 2006-2023
- **Financial aid** — _Application pour la Gestion du Logement et de l'Aide à l'Étudiant (AGLAE)_, 2008-2018
- **Higher education enrollment and graduation** — _Système d'Information sur le Suivi de l'Étudiant (SISE)_ and _Base Post-Bac (BPBAC)_, 2009-2023

Students are linked across these sources via an anonymized identifier.

### 2. OVE *Conditions de vie des étudiants* student survey — 2010 and 2013 waves

The paper also uses two waves of the *Observatoire de la vie étudiante*'s *Conditions de vie des étudiants* student survey to provide descriptive evidence on student-reported financial difficulties, paid work alongside studies, and study costs in high-rent urban areas. Neither wave can be redistributed; both must be requested directly from Progedo:

- 2010 wave: <https://data.progedo.fr/studies/doi/10.13144/lil-0645>
- 2013 wave: <https://data.progedo.fr/studies/doi/10.13144/lil-1086>

## Code

The cleaning and analysis files are numbered in the order in which they should be run, and the master script `src/master_file.R` runs them all in sequence.

### Data preparation — `src/01_data_preparation/`

- `01a_hei_names.R`: maps higher-education institution names to identifiers.
- `01b_merge_sectdis_discipli.R`: merges sector and discipline classifications.
- `02_data_merging_anonym.Rmd`: anonymises identifiers and pre-merges raw files.

### Data cleaning — `src/02_data_cleaning/`

- `1a_ocean_cleaning_09_14.Rmd`: cleans OCEAN Bac records (2009–2014); builds gender, track, mention, and grade variables.
- `1b_aglae_cleaning_09_14.Rmd`: cleans AGLAE financial-aid applications and merges in Bac characteristics.
- `1c_all_enrol_cleaning_09_14.Rmd`: harmonises and merges SISE and BPBAC enrollment data, 2008–2023.
- `1d_all_result_cleaning_09_14.Rmd`: cleans degree/results data from SISE and BPBAC, 2009–2023.
- `2a_cohort_09_14.Rmd`: constructs the analysis cohort by joining OCEAN and AGLAE; assigns DEPP-style SES categories.
- `2b_variable_creation_09_14.Rmd`: builds outcome variables (enrollment, degree quality, completion, ECTS credits, etc.).

### Analysis — `src/03_analysis/`

- `0a_descriptive_stats_09_14_final.Rmd`: descriptive statistics for the analysis sample.
- `0b_various_stats_in_paper_09_14_final.Rmd`: assorted figures and shares cited in the paper text.
- `0c_student_survey_financial_difficulties.qmd`: OVE 2013 comparison of self-reported financial outcomes between merit-aid-eligible students and high-income high-achievers.
- `1a_method_robustness_09_14_final.Rmd`: design-validity checks: covariate balance, placebo tests, donut RDD, predicted-outcome discontinuity.
- `2a_extensive_margin_analysis_09_14_final.Rmd`: main RDD estimates of merit-aid eligibility on enrollment in graduation year, any enrollment, and initial degree choice.
- `2b_intensive_margin_analysis_09_14_final.Rmd`: long-run outcomes: persistence, total years enrolled, ECTS, completion, master's enrollment.
- `2c_robustness_09_14_final.Rmd`: bandwidth and specification robustness checks.
- `3a_ove_expensive_urban_areas.qmd`: OVE 2010 descriptive evidence on study costs in high-rent urban areas.
- `3b_heterogeneity_09_14_final.Rmd`: heterogeneous effects by gender, parental SES, parental income, HS track, and grant level.
- `4_student_survey_impact_work_studies.qmd`: OVE 2013 evidence on the impact of paid work on student outcomes, by Bac mention.
- `5_paper_figures_tables.Rmd`: assembles the paper's figures and tables from the raw-data files in `out/figures/raw_data/` and `out/tables/raw_data/`.
- `mean_y_all_outcomes.R`, `oucome_lab_crosswalk.R`: helpers used across the analysis files.

### Shared functions — `src/code_lib/`

Functions and themes shared across the cleaning and analysis scripts (e.g. `useful_functions.R`, `common_initial_loading.R`, RDD estimation/output helpers, table-formatting helpers, plotting helpers).

## Figures and tables

- **The figures and tables themselves** are under `out/figures/` (as `.pdf`) and `out/tables/` (as `.tex`).
- **Reproducing them without the microdata:** open `replication_files.Rproj` in RStudio and run `src/03_analysis/5_paper_figures_tables.Rmd`. It rebuilds every figure and table in the paper from the raw-data CSV/RDS files shipped under `out/figures/raw_data/` and `out/tables/raw_data/`, so this works without DEPP or OVE access.
- **Re-running the full pipeline:** source `src/master_file.R` instead. This requires access to both data sources above, and you'll need to update the relevant paths inside each notebook to match your local environment.
