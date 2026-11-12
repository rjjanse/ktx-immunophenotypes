#----------------------------------------------#
# Determining longitudinal immunophenotypes
# Code for data cleaning
# Roemer J. Janse - 2026/07/28
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
path <- psp("/Volumes/DATA/LAB/lab_research/RES-Folder-UPOD/PICTS/E_ResearchData/2_ResearchData/",
            "L:/lab_research/RES-Folder-UPOD/PICTS/E_ResearchData/2_ResearchData/")

# Set path for saving cleaned data frames
path_dataframes <- psp("/Volumes/DATA/LAB/lab_research/RES-Folder-UPOD/PICTS/E_ResearchData/2_ResearchData/ktx-immunophenotypes/dataframes/",
                       "L:/lab_research/RES-Folder-UPOD/PICTS/E_ResearchData/2_ResearchData/ktx-immunophenotypes/dataframes/")

# 1. Spine data ----
# Load data
dat_spine <- importr("20260805/patsel_20260805.sas7bdat")

# Clean data
dat_spine %<>%
  # Set column names to lowercase
  set_colnames(tolower(colnames(.))) %>%
  # Drop unnecessary columns
  select(-c(t_center,        # Transplantation centre
            r_etnr,          # Eurotransplant nr.
            r_sex,           # UPOD sex registry has precedence (one mismatch)
            t_lki,           # Left kidney ID
            t_rki,           # Right kidney ID
            d_reg_nr,        # Donor ID
            tdate,           # Same as t_date
            t_urg_at,        # Urgency status (not complete)
            etid             # Eurotransplant ID
  )) %>%
  # Clean up column names
  rename(id = studyid_0988,
         dt_ktx = t_date,
         r_pkd = r_ki_pd,
         d_rhesus = d_rh,
         r_rhesus = r_rh,
         dt_fail = t_fd,
         fail_cause = t_fc,
         dt_dialysis = r_dfd, 
         r_age = age,
         r_female = gender, 
         d_female = d_sex,
         death = deceasedboolean,
         dt_death = deceaseddatetime,
         r_birthyear = geboortejaar,
         dt_lastcontact = last_contact_dt) %>%
  # Change variables
  mutate(# Set dates to Date format
         across(starts_with("dt_"),  \(x) as.Date(x, format = "%d-%m-%Y")),
         # For all character columns, set empty text to NA
         across(!starts_with("dt_"), \(x) if_else(x == "", NA, x)),
         # Set missings in height and weight (indicated with "" or 1) to missing
         # Additionally set columns to numeric
         across(contains("eight"), \(x) as.numeric(if_else(x %in% c("", "0", "1"), NA, x))),
         # Set ages to numeric
         across(contains("age"), as.numeric),
         # Calculate BMI
         d_bmi = d_weight / (d_height / 100) ^ 2,   # Donor
         r_bmi = r_weight / (r_height / 100) ^ 2,   # Recipient
         # Set rhesus status to 1 (positive) or 0 (negative)
         across(c(r_rhesus, d_rhesus), \(x) as.numeric(if_else(x == "Pos", 1, 0, missing = NA))),
         # Set donor sex
         d_female = as.numeric(if_else(d_female == "F", 1, 0)),
         # Set recipient sex
         r_female = as.numeric(if_else(r_female == "female", 1, 00))
  )
        
# Save spine data
saver("cleaned_spine.Rdata", dat_spine, prefix_path = path_dataframes)

# 2. Other longitudinal data ----
## 2.1. Medication ----
# Load data
dat_med <- importr("20260521/med_20260519.sas7bdat")

# Drop column names
for(i in colnames(dat_med)) attributes(dat_med[[i]]) <- NULL

# Clean data
dat_med %<>%
  # To tibble
  as_tibble() %>%
  # Set column names to lowercase
  set_colnames(tolower(colnames(.))) %>%
  # Rename relevant columns
  rename(id = studyid_0988,
         dt_ktx = tdate,
         type = categorie,
         atc = atc_code,
         dosage = dosis,
         formula = toedieningsweg,
         dt_start = periodofuse_valueperiod_start,
         dt_end = periodofuse_valueperiod_end) %>%
  # Clean variables
  mutate(# Set dates to date format
         across(c(dt_ktx, dt_start, dt_end), \(x) as.Date(as.POSIXct(x))),
         # Set empty to missing
         across(c(type, atc, dosage, formula), \(x) if_else(x == "", NA, x))) %>%
  # Drop variables not of interest
  select(-c(omschrijving, atc_omschrijving)) %>%
  # Drop missing ATCs
  filter(!is.na(atc))

# Save medication data
saver("cleaned_med.Rdata", dat_med, prefix_path = path_dataframes)

## 2.2. Laboratory data ----
# Load data
dat_lab <- importr("20260521/lab_20260519.sas7bdat")

# Drop column names
for(i in colnames(dat_lab)) attributes(dat_lab[[i]]) <- NULL

# Clean data
dat_lab %<>%
  # To tibble
  as_tibble() %>%
  # Set column names to lowercase
  set_colnames(tolower(colnames(.))) %>%
  # Rename relevant columns
  rename(id = studyid_0988,
         dt_ktx = tdate,
         category = category3_display_original,
         test = code_display_original,
         code = code2_display_original,
         dt_lab = afname_dt,
         ref = referencerange_text,
         result = valuequantity_value,
         unit = valuequantity_code_original,
         specialty = specialty_specialtyagb_display) %>%
  # Clean variables
  mutate(# Set test to lower
         test = tolower(test),
         # Set code to lower
         code = tolower(code),
         # Set dates to date format
         across(c(dt_ktx, dt_lab), \(x) as.Date(as.POSIXct(x))),
         # Determine lower and upper reference bound
         ref_lb = as.numeric(str_extract(ref, "\\d+.*\\d*(?=\\s-)")),
         ref_ub = as.numeric(str_extract(ref, "(?<=-\\s)\\d+.*\\d*")),
         # Set empty to missing
         across(c(test), \(x) if_else(x == "", NA, x))) %>%
  # Drop variables not of interest
  select(-c(dagen_verschil_transpl, code_code_original, interpretation_display, 
            valuequantity_comparator, valuequantity_unit, ref)) %>%
  # Drop missing results
  filter(!is.na(result))

# Save lab data
saver("cleaned_lab.Rdata", dat_lab, prefix_path = path_dataframes)

## 2.3. Hospitalisations ----
# Load data
dat_hsp <- importr("20260521/hos_20260519.sas7bdat")

# Drop column names
for(i in colnames(dat_hsp)) attributes(dat_hsp[[i]]) <- NULL

# Clean data
dat_hsp %<>%
  # To tibble
  as_tibble() %>%
  # Set column names to lowercase
  set_colnames(tolower(colnames(.))) %>%
  # Rename relevant columns
  rename(id = studyid_0988,
         dt_ktx = tdate,
         specialty = hos_admit_spec_code,
         type = hos_type,
         dt_adm_spec = hos_start_dt,
         dt_dis_spec = hos_end_dt,
         reason = hos_reden_text,
         ic = hos_ic_opname) %>%
  # Drop variables not of interest
  select(-hos_reden_display) %>%
  # Clean variables
  mutate(# Set reason to lower
         reason = tolower(reason),
         # Set POSIXct dates back
         across(c(dt_adm_spec, dt_dis_spec, dt_ktx), as.POSIXct),
         # Get dates from POXIXct
         dt_adm = as.Date(dt_adm_spec),
         dt_dis = as.Date(dt_dis_spec),
         # Set ktx to date
         dt_ktx = as.Date(dt_ktx))

# Save hosp data
saver("cleaned_hsp.Rdata", dat_hsp, prefix_path = path_dataframes)

## 2.4. Diagnoses ----
# Load data
dat_dgn <- importr("20260521/diag_20260519.sas7bdat")

# Drop column names
for(i in colnames(dat_dgn)) attributes(dat_dgn[[i]]) <- NULL

# Clean data
dat_dgn %<>%
  # To tibble
  as_tibble() %>%
  # Set column names to lowercase
  set_colnames(tolower(colnames(.))) %>%
  # Rename variables
  rename(id = studyid_0988,
         dt_ktx = tdate,
         specialty = specialism,
         icd10 = code_code,
         dt_dgn = onset) %>%
  # Clean data
  mutate(# Set dates to date format
         across(c(dt_ktx, dt_dgn), \(x) as.Date(as.POSIXct(x))),
         # If icd10 is empty, get text from free text field
         # Append with _ to avoid faulty icd10 code identification later on
         # We get a warning NAs introduced by coercion but this is the mechanism that we require to check for proper ICD codes
         icd10 = if_else(icd10 == "" | !is.na(as.numeric(icd10)), paste0("_", tolower(code_display_original)), icd10),
         # Drop . from ATC
         icd10 = str_replace(icd10, "\\.", "")) %>%
  # Keep only relevant variables
  select(id, dt_ktx, specialty, icd10, dt_dgn) %>%
  # Drop palliative care & unclear/no diagnosis
  filter(!str_detect(icd10, "palliatie|slecht omschreven aandoening/geen diagnose"))

# Save diagnoses data
saver("cleaned_dgn.Rdata", dat_dgn, prefix_path = path_dataframes)

## 2.5. Procedures (radiology) ----
# Load data
dat_rad <- importr("20260521/verr_rad_20260519.sas7bdat")

# Drop column names
for(i in colnames(dat_rad)) attributes(dat_rad[[i]]) <- NULL

# Clean data
dat_rad %<>%
  # To tibble
  as_tibble() %>%
  # Set column names to lowercase
  set_colnames(tolower(colnames(.))) %>%
  # Rename variables
  rename(id = studyid_0988,
         dt_ktx = tdate,
         proc = code_coding_display,
         dt_proc = performedperiod_start,
         specialty = uitvoerend_speccode,
         referrer = verwijzer_speccode,
         type = category_display) %>%
  # Clean data
  mutate(# Set procedure to lowercase
         proc = tolower(proc),
         # Set dates to date format
         across(c(dt_ktx, dt_proc), \(x) as.Date(as.POSIXct(x))),
         # Set NA proc to NA
         if_else(proc == "", NA, proc)) %>%
  # Select relevant variables
  select(id, dt_ktx, proc, specialty, dt_proc, referrer, type) %>%
  # Drop unknown procedures
  filter(!is.na(proc))

# Save procedure radiology data
saver("cleaned_rad.Rdata", dat_rad, prefix_path = path_dataframes)

## 2.6. Procedures (surgery) ----
# Load data
dat_sur <- importr("20260521/verr_ok_20260519.sas7bdat")

# Drop column names
for(i in colnames(dat_sur)) attributes(dat_sur[[i]]) <- NULL

# Clean data
dat_sur %<>%
  # To tibble
  as_tibble() %>%
  # Set column names to lowercase
  set_colnames(tolower(colnames(.))) %>%
  # Rename variables
  rename(id = studyid_0988,
         dt_ktx = tdate,
         proc = code_coding_display,
         dt_proc_start_spec = cluster_start,
         dt_proc_end_spec = cluster_end,
         specialty = uitvoerend_speccode) %>%
  # Clean data
  mutate(# Set procedure to lowercase
         proc = tolower(proc),
         # Set KTx date to date format
         dt_ktx = as.Date(as.POSIXct(dt_ktx)),
         # Set dates to POSIXct format
         across(c(dt_proc_start_spec, dt_proc_end_spec), as.POSIXct),
         # Get dates for start and end too
         dt_proc_start = as.Date(dt_proc_start_spec),
         dt_proc_end = as.Date(dt_proc_end_spec),
         # Calculate duration in hours
         dur = round(as.numeric((dt_proc_end_spec - dt_proc_start_spec) / 3600), 1)) %>%
  # Keep only relevant variables
  select(id, dt_ktx, proc, dt_proc_start, dt_proc_end, dt_proc_start_spec, dt_proc_end_spec, dur, specialty)

# Save procedure surgery data
saver("cleaned_sur.Rdata", dat_sur, prefix_path = path_dataframes)

## 2.7. DBC codes ----
# Load data
dat_dbc <- importr("20260521/dbc_20260519.sas7bdat")

# Drop column names
for(i in colnames(dat_dbc)) attributes(dat_dbc[[i]]) <- NULL

# Clean data
dat_dbc %<>%
  # To tibble
  as_tibble() %>%
  # Set column names to lowercase
  set_colnames(tolower(colnames(.))) %>%
  # Rename variables
  rename(id = studyid_0988,
         dt_ktx = tdate,
         dbc = dbcdiagnosis,
         dt_dbc_start = period_start,
         dt_dbc_end = period_end,
         specialty = specialty1_organization_value,
         subspecialty = specialty2_organization_value,
         icd10 = medicalcode) %>%
  # Clean data
  mutate(# Set DBC to lowercase
         dbc = tolower(dbc),
         # Set dates to date format
         across(c(dt_ktx, dt_dbc_start, dt_dbc_end), \(x) as.Date(as.POSIXct(x))),
         # Drop . from ICD
         icd10 = str_replace(icd10, "\\.", ""),
         # Set empty to missing
         across(c(dbc, icd10, specialty, subspecialty), \(x) if_else(x == "", NA, x))) %>%
  # Keep only relevant variables
  select(id, dt_ktx, dbc, dt_dbc_start, dt_dbc_end, specialty, subspecialty, icd10) %>%
  # Drop NAs in DBC
  filter(!is.na(dbc))

# Save dbc data
saver("cleaned_dbc.Rdata", dat_dbc, prefix_path = path_dataframes)

# 3. Specialty tables ----
# Get procedures (they have translation tables)
dat_proc_sur <- importr("20260521/verr_ok_20260519.sas7bdat")
dat_proc_rad <- importr("20260521/verr_rad_20260519.sas7bdat")

# Combine with only relevant columns
dat_specialty_key <- 
  # Combine data
  bind_rows(
    # Surgery data
    dat_proc_sur %>% 
      # Rename variables
      rename(specialty = UITVOEREND_SPECCODE,
             key = UITVOEREND_SPECIALISME) %>%
      # Keep only relevant variables
      select(specialty, key),
    # Radiology data performing specialty
    dat_proc_rad %>%
      # Rename variables
      rename(specialty = UITVOEREND_SPECCODE,
             key = UITVOEREND_SPECIALISME) %>%
      # Keep only relevant variables
      select(specialty, key),
    # Radiology data referring specialty
    dat_proc_rad %>%
      # Rename variables
      rename(specialty = VERWIJZER_SPECCODE,
             key = VERWIJZER_SPECIALISME) %>%
      # Keep only relevant variables
      select(specialty, key)
  ) %>%
  # Keep only uniques
  distinct()

# Save specialty table to excel
export(dat_specialty_key, paste0("/Users/rjanse5/Library/CloudStorage/OneDrive-UMCUtrecht/Documenten/Projects/202602 - KTx immunophenotyping/sheets/specialty_key.xlsx"))
