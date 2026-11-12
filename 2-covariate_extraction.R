#----------------------------------------------#
# Determining longitudinal immunophenotypes
# Code for covariate extraction
# Roemer J. Janse - 2026/11/09
#----------------------------------------------#

# 0. Set-up ----
# Load packages
pacman::p_load("dplyr",          # Data wrangling
               "magrittr",       # Efficient pipelines
               "stringr",        # Working with character strings
               "purrr",          # Functional programming
               "rio",            # Import foreign data
               "here",           # Relative local file locations
               "conflicted"      # Resolving package conflicts
)

# Resolve function conflicts
conflicts_prefer(dplyr::filter) # Between stats and dplyr

# Load functions
walk(list.files(here("funs")), \(x) source(paste0(here("funs"), "/", x)))

# Set file path
path <- psp("/Volumes/DATA/LAB/lab_research/RES-Folder-UPOD/PICTS/E_ResearchData/2_ResearchData/ktx-immunophenotypes/dataframes/",
            "L:/lab_research/RES-Folder-UPOD/PICTS/E_ResearchData/2_ResearchData/ktx-immunophenotypes/dataframes/")

# 1. Load data ----
# Load spine data
loadr("cohort_selection.Rdata")

# Load diagnosis data
loadr("cleaned_dgn.Rdata")

# Load medication data
loadr("cleaned_med.Rdata")

# Load lab data
loadr("cleaned_lab.Rdata")

# Load DBC data
loadr("cleaned_dbc.Rdata")

# Load surgery data
loadr("cleaned_sur.Rdata")

# Load radiology data
loadr("cleaned_rad.Rdata")

# Load hospitalisation data
loadr("cleaned_hsp.Rdata")

# Minimal spine data required for linking
dat_link <- select(dat_spine, id, dt_ktx)

# 1. Medical history ----
## 1.1. Cardiovascular disease ----
### 1.1.1. Hypertension ----
# ICD10: I10-I15
# ATC: antihypertensive agent use
# Two consecutive measurements 90 days apart of SBP >=140 or DBP >=90
# DBC code for hypertension
# Hospitalisation for hypertension

# Diagnoses
vec_htn_icd <- extract_icd("hypertens|^I1[0-5]")

# Medication
vec_htn_atc <- extract_atc("^C0[23789]")

# DBC codes
vec_htn_dbc <- extract_dbc("hypertens", 
                   nottext = "ocul")

# Hospitalisation
vec_htn_hsp <- extract_hsp("hypertens|hoge bloeddruk")

# Combine
vec_htn <- unique(c(vec_htn_icd, vec_htn_atc, vec_htn_dbc, vec_htn_hsp))

# Remove individual vectors
rm(vec_htn_atc, vec_htn_dbc, vec_htn_hsp, vec_htn_icd)

### 1.1.2. Heart failure ----
# ICD10: I11.0, I13.0, I13.2, I50
# Procedures: LVAD, left ventricular assist device
# DBC code for heart failure, cardiomyopathy
# Hospitalisation for heart failure, cardiomyopathy

# Diagnoses
vec_hf_icd <- extract_icd("hartfalen|^I1(10|3[02])|^I50")

# LVAD
# None identified

# DBC
vec_hf_dbc <- extract_dbc("hartfalen")

# Hospitalisation
vec_hf_hsp <- extract_hsp("hartfalen")

# Combine
vec_hf <- unique(c(vec_hf_icd, vec_hf_dbc, vec_hf_hsp))

# Remove individual vectors
rm(vec_hf_icd, vec_hf_dbc, vec_hf_hsp)

### 1.1.3. Cerebrovascular disease ----
# ICD10: I60-I64
# Procedures: carotid endarterectomy, carotid artery stenting
# DBC
# Hospitalisation
# Text: stroke, transient ischemic attack, beroerte

# ICD
vec_cbd_icd <- extract_icd("I6[0-4]")

# Procedures
# None identified

# DBC
vec_cbd_dbc <- extract_dbc("tia|beroerte|cva")

# Hospitalisation
vec_cbd_hsp <- extract_hsp("tia",
                           nottext = "screening")

# Combine
vec_cbd <- unique(c(vec_cbd_icd, vec_cbd_dbc, vec_cbd_hsp))

# Remove individual vectors
rm(vec_cbd_icd, vec_cbd_dbc, vec_cbd_hsp)

### 1.1.4. Coronary artery disease ----
# ICD10: I21-I23
# Procedures: coronary artery bypass grafting, percutaneous coronary intervention
# DBC
# Hospitalisation

# ICD10
vec_cad_icd <- extract_icd("I2[1-3]")

# Procedures
vec_cad_sur <- extract_sur("cabg|ptca|coronaire interventie|cor.arterien")

# DBC
vec_cad_dbc <- extract_dbc("coron|cabg|ptca")

# Hospitalisation
vec_cad_hsp <- extract_hsp("pci|cabg")

# Combine
vec_cad <- unique(c(vec_cad_icd, vec_cad_sur, vec_cad_dbc, vec_cad_hsp))

# Remove individual vectors
rm(vec_cad_icd, vec_cad_sur, vec_cad_dbc, vec_cad_hsp)

### 1.1.5. Arrhythmias & conduction disorders ----
# ICD10: I44-49, Z99.4, Z95.0, T82.1, Z45.0
# Procedures: ICD/pacemaker, cardiac defibrillation, pulmonary vein isolation (ablation), cardioversion
# DBC
# Hospitalisation
# Text: geleidingsstoornis, hartritmestoornis, arritmie fibrilleren, ritmestoornis

# ICD
vec_arr_icd <- extract_icd("^I4[4-9]|^Z9(94|50)|T821|Z450")

# Surgery
vec_arr_sur <- extract_sur("icd|defib|ritme|versie|kath\\.ablatie|cryoablatie|pacemaker",
                           nottext = "reanimatie")

# DBC
vec_arr_dbc <- extract_dbc("icd|ritme|fibri|flutter")

# Hospitalisation
vec_arr_hsp <- extract_hsp("icd|cardioversie|ablatie|fibri|flutter")

# Combine
vec_arr <- unique(c(vec_arr_icd, vec_arr_sur, vec_arr_dbc, vec_arr_hsp))

# Remove indiviudal vectors
rm(vec_arr_icd, vec_arr_sur, vec_arr_dbc, vec_arr_hsp)

### 1.1.6. Peripheral artery disease ----
# ICD10: I70.2
# Procedures: revascularization, bypass of peripheral vessels, stent
# Text: peripheral arterial atherosclerosis, stenosis of peripheral arteries, leg ulcers, intermittent claudication

# ICD
vec_pad_icd <- extract_icd("^I702")

# Procedures
vec_pad_sur <- extract_sur("(?<=perif\\.vaten).+stent|(?<=femor).+bypass")

# DBC
vec_pad_dbc <- extract_dbc("perifeer vaatlijden|ulcus cruris|claudicatio")

# Hospitalisations
vec_pad_hsp <- extract_hsp("perifeer|fontaine")

# Combine
vec_pad <- unique(c(vec_pad_icd, vec_pad_sur, vec_pad_dbc, vec_pad_hsp))

# Remove indiviudal vectors
rm(vec_pad_icd, vec_pad_sur, vec_pad_dbc, vec_pad_hsp)

### 1.1.7. Cardiovascular disease ----
# Combine all above vectors
vec_cvd <- unique(c(vec_arr, vec_cad, vec_cbd, vec_hf, vec_htn, vec_pad))

## 1.2. Diabetes mellitus ----
# ICD10: E10-14
# ATC: A10
# Lab: HbA1c >= 48 mmol/mol of 6.5%, fasting glucose >= 7, or non-fasting glucose >= 11.1
# Text: diabetes mellitus

# ICD
vec_dm_icd <- extract_icd("diabetes|^E1[0-4]")

# ATC
vec_dm_atc <- extract_atc("^A10")

# Lab
vec_dm_lab <- dat_lab %>%
  # Keep only relevant tests
  filter(code %in% c("glucose-nuchter-bl", "glucose-bl", "hba1c-bl", "hba1c_2-bl", "1075")) %>%
  # Set boundary
  mutate(boundary = recode_values(code,
                                  "glucose-nuchter-bl" ~ 7,
                                  "glucose-bl" ~ 11.1,
                                  "hba1c-bl" ~ 6.5,
                                  "hba1c_2-bl" ~ 48,
                                  "1075" ~ 6.5,
                                  default = NA)) %>%
  # Keep only valid values (above boundary & before index)
  filter(result >= boundary & dt_lab <= dt_ktx) %>%
  # Keep only unique IDs
  distinct(id) %>%
  # Extract ID
  extract2("id")

# DBC
vec_dm_dbc <- extract_dbc("diabetes",
                          nottext = "zwanger")

# Hospitalisation
vec_dm_hsp <- extract_hsp("diab")

# Combine
vec_dm <- unique(c(vec_dm_icd, vec_dm_atc, vec_dm_lab, vec_dm_dbc, vec_dm_hsp))

# Remove unique vectors
rm(vec_dm_icd, vec_dm_atc, vec_dm_lab, vec_dm_dbc, vec_dm_hsp)

## 