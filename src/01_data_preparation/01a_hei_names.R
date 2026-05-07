
# Packages ----------------------------------------------------------------

library(arrow)
library(tidyverse)
library(data.table)
library(janitor)
library(here)
library(readxl)

# Names of higher education institutions ----------------------------------


# load AGLAE datasets which contain HEI ids and their names
aglae_0809 <- open_dataset(here("modified_data/anonym/aglae/parquet/")) |> 
  filter(year_aglae == 2008)
aglae_0910 <- open_dataset(here("modified_data/anonym/aglae/parquet/")) |> 
  filter(year_aglae == 2009)
aglae_1011 <- open_dataset(here("modified_data/anonym/aglae/parquet/")) |> 
  filter(year_aglae == 2010)
aglae_1112 <- open_dataset(paste0("/home/depp/projets_inter/SPO-Meritesup/SPO-Meritesup-echanges/2025-11_admin_data/Livrables/aglae_bx_allcrypt", "/bx11c_crypt.parquet"))

# keep only HEI id and label
etab_sup_aglae_08 <- aglae_0809 %>%
  count(etabli, lib1etab, lib2etab) %>%
  select(-n) %>%
  mutate(etabli_lab = paste(lib1etab, ifelse(!is.na(lib2etab), lib2etab, ""))) %>%
  select(etabli, etabli_lab) |> 
  collect() |> 
  mutate(etabli_lab = trimws(etabli_lab, "right"))

etab_sup_aglae_09 <- aglae_0910 %>%
  count(etabli, lib1etab, lib2etab) %>%
  select(-n) %>%
  mutate(etabli_lab = paste(lib1etab, ifelse(!is.na(lib2etab), lib2etab, ""))) %>%
  select(etabli, etabli_lab) |> 
  collect() |> 
  mutate(etabli_lab = trimws(etabli_lab, "right"))

etab_sup_aglae_10 <- aglae_1011 %>%
  count(etabli, lib1etab, lib2etab) %>%
  select(-n) %>%
  mutate(etabli_lab = paste(lib1etab, ifelse(!is.na(lib2etab), lib2etab, ""))) %>%
  select(etabli, etabli_lab) |> 
  collect() |> 
  mutate(etabli_lab = trimws(etabli_lab, "right"))

etab_sup_aglae_11 <- aglae_1112 %>%
  count(etabli, lib1etab, lib2etab) %>%
  select(-n) %>%
  mutate(etabli_lab = paste(lib1etab, ifelse(!is.na(lib2etab), lib2etab, ""))) %>%
  select(etabli, etabli_lab) |> 
  collect() |> 
  mutate(etabli_lab = trimws(etabli_lab, "right"))

# bind all together
etab_sup_aglae <- bind_rows(etab_sup_aglae_08,
                            etab_sup_aglae_09,
                            etab_sup_aglae_10,
                            etab_sup_aglae_11)

# keep unique values
etab_sup_aglae <- etab_sup_aglae %>%
  unique()
setDT(etab_sup_aglae)

# keep only 1 row per HEI id
etab_sup_aglae[, .N, by = etabli] %>% count(N)
etab_sup_aglae[, nb := 1:.N, by = etabli]
etab_sup_aglae %>% count(nb)
etab_sup_aglae <- etab_sup_aglae[nb == 1]

# export
fwrite(etab_sup_aglae, here("modified_data/other/etab_sup_aglae.csv"))
# ----

code_uai_etab <- fread(here("original_data/other/code_UAI_etab.csv")) %>% clean_names()
code_uai_etab[, .N, by = code_uai] %>% count(N)
code_uai_etab[, nb := .N, by = code_uai]
code_uai_etab[nb > 1] %>% select(nom)
code_uai_etab[, id := 1:.N, by = code_uai]
code_uai_etab <- bind_rows(code_uai_etab[nb == 1],
                           code_uai_etab[nb > 1 & id == 1])
code_uai_etab[, .N, by = code_uai] %>% count(N)
fwrite(code_uai_etab, here("modified_data/other/code_uai_etab_clean.csv"))

etab_sup <- fread(here("original_data/other/fr-esr-principaux-etablissements-enseignement-superieur.csv")) %>% clean_names()

onisep_uai <- read_excel(here("original_data/other/onisep_uai.xlsx")) %>% clean_names()
setDT(onisep_uai)

etab_sup_aglae <- fread(here("modified_data/other/etab_sup_aglae.csv"))

uai_all <- bind_rows(onisep_uai %>%
                       mutate(source = "onisep") %>%
                       select(ETABLI = code_uai,
                              etabli_lab = nom,
                              source),
                     etab_sup_aglae %>%
                       mutate(source = "aglae") %>%
                       select(ETABLI = etabli,
                              etabli_lab,
                              source))
setDT(uai_all)
uai_all[, .N, by = ETABLI] %>% count(N)
uai_all <- uai_all %>% unique()
uai_all[, nb := .N, by = ETABLI]
uai_all <- uai_all %>%
  group_by(ETABLI) %>%
  mutate(source_both = ifelse(sum(source == "onisep") == nb, "onisep",
                              ifelse(sum(source == "aglae") == nb, "aglae", "both"))) %>%
  ungroup()
setDT(uai_all)

uai_all[nb > 1 & source_both == "onisep", id := 1:.N, by = ETABLI]

uai_all <- bind_rows(uai_all[nb == 1],
                     uai_all[nb > 1 & source_both == "both" & source == "aglae"],
                     uai_all[nb > 1 & source_both == "onisep" & id == 1])

# manually add schools
uai_all <- bind_rows(uai_all,
                     data.frame(ETABLI = c("0755722M", "0771077C", "0180974L", "0492202C", "0922706S", "0942340H", "0942341J", "0342255S", "0352756F", "0596870X", "0912330N", "0062126D", "0383493R", "0755698L", "0755700N", "0754954C", "0755325F", "0755763G", "0781981E", "0811332H", "0922757X", "0292355C", "0442852L", "0597060D", "0912381U", "0922750P", "0251985X", "0597131F", "0912423P", "0371771Z", "0332813D", "0694316S", "0271691S", "0731563C", "0922795N", "031P0002", "0922793L", "0755902H", "035P0002", "0291792R", "0595821G", "056P0002", "031P0001", "0952262T", "014P0001", "0531006F", "0561996R", "0912408Y", "0342318K", "0492499A", "0952226D", "0771204R", "0241056T", "0623631K", "0811267M", "035P0003", "0333405X", "0021502X", "033P0006", "031P0003", "069P0005", "059P0001", "044P0001", "0756148A", "056P0003", "035P0005", "033P0005", "034P0004", "073P0001", "073P0002", "073P0005", "078P0001", "0022134J", "0597139P", "0632086A", "0912403T", "0912407X", "0755890V"),
                                etabli_lab = c("France Business School", 
                                               "Institut européen d'administration des affaires (INSEAD)",
                                               "Institut national des sciences appliquées Centre Val de Loire - site de Bourges (INSA CVL)",
                                               "Ecole supérieure et d'application du génie du Nantes",
                                               "Ecole nationale supérieure maritime (ENSM)",
                                               "École spéciale des travaux publics du bâtiment et de l'industrie - campus de Cachan (ESTP Paris)",
                                               "Ecole supérieure d'ingénieurs en informatique et génie des télécommunications",
                                               "Languedoc-Roussillon Universités",
                                               "Université Bretagne Loire",
                                               "Université Lille Nord de France",
                                               "Université d'Évry-Val-d'Essonne",
                                               "Université Côte d'Azur",
                                               "Université Grenoble Alpes",
                                               "Université Paris Lumières",
                                               "École des hautes études en sciences sociales (EHESS)",
                                               "Ecole supérieure des ressources humaines (Sup des RH)",
                                               "ESGRH",
                                               "International School of Management",
                                               "CFA Sup de Vente CCI Paris Ile-de-France - campus Paris 17e (CFA SUP DE V)",
                                               "Institut supérieur de promotion industrielle (IPI)",
                                               "Institut des Hautes Etudes à Paris",
                                               "IMT Atlantique - Campus Brest et Rennes",
                                               "IMT Atlantique - Campus Nantes",
                                               "Ecole nationale supérieure Mines-Télécom Lille Douai - site de Lille, Université de Lille (IMT Lille Douai)",
                                               "Ecole nationale de la statistique et de l'administration économique (ENSAE Paris)",
                                               "CentraleSupélec Campus de Châtenay",
                                               "Communauté d'universités et établissements Université Bourgogne - Franche-Comté",
                                               "Institut National des Sciences Appliquées Hauts-de-France de Valenciennes",
                                               "Ecole Normale Supérieure Paris-Saclay",
                                               "ESG Tours",
                                               "Centre de Formation d'Apprentis - Campus du Lac Bordeaux",
                                               "Ecole de commerce et de management Mbway",
                                               "Centre de formation d'apprentis Ecole supérieure de la CCI Portes de Normandie",
                                               "Centre d'études de formation Alpes Savoie",
                                               "La Ccompagnie De Formation",
                                               "Introuvable",
                                               "Ipac Bachelor Factory",
                                               "CFA de la Chambre de Commerce et d'Industrie de Paris Ile-de-France - SUP DE VENTE site de Paris Champerret",
                                               "Introuvable",
                                               "IFAC-CCI MBO Campus des métiers",
                                               "CFA TERTIA - Grand Hainaut Site de Valenciennes",
                                               "Introuvable",
                                               "Introuvable",
                                               "Site de formation d'apprentis - CCI de Paris IDF - Sup de Vente - site Pontoise",
                                               "Introuvable",
                                               "ESUP - école supérieure de commerce et de management",
                                               "Groupe AFTEC - MBWAY Vannes",
                                               "Université Paris-Saclay",
                                               "Centre de formation d'apprentis Purple Campus Pérols",
                                               "Win Sport School Angers",
                                               "ESIEE IT",
                                               "Lycée professionnel privé Pigier Melun",
                                               "Centre de formation d'apprentis Ecoles CCI de la Dordogne",
                                               "CFA SIADEP - Siège de Lens",
                                               "Centre de formation d'apprentis IFA 81 CCI Tarn",
                                               "Introuvable",
                                               "Ecole technique privée Pigier Performance",
                                               "CFA de la CCIA",
                                               "Introuvable",
                                               "Introuvable",
                                               "Introuvable",
                                               "Introuvable",
                                               "Introuvable",
                                               "ESSYM Ecole Supérieur des Systèmes de Management",
                                               "Introuvable",
                                               "Introuvable",
                                               "Introuvable",
                                               "Introuvable",
                                               "Introuvable",
                                               "Introuvable",
                                               "Introuvable",
                                               "Introuvable",
                                               "Ecole d'ingénieurs des sciences aérospatiales (ELISA Aérospace)",
                                               "Centrale Lille Institut - VILLENEUVE-D'ASCQ",
                                               "Ecole d'ingénieurs SIGMA Clermont",
                                               "Institut polytechnique Paris",
                                               "Télécom Paris",
                                               "Sorbonne Université"), source = "manual"))
# export
fwrite(uai_all, here("modified_data/other/uai_all.csv"))
