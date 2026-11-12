#----------------------------------------------#
# Determining longitudinal immunophenotypes
# Code for cohort selection
# Roemer J. Janse - 2026/11/06
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

# Load selection
loadr("cleaned_spine.Rdata")

# Total population
n_distinct(dat_spine[["id"]]) # n = 1245

# 1. Select population ----
# First transplantation
dat_spine %<>% 
  # Arrange on transplantation date
  arrange(id, dt_ktx) %>%
  # Group per individual
  group_by(id) %>%
  # Keep first row per person
  slice(1L) %>%
  # Remove grouping structure
  ungroup()

# Count population
n_distinct(dat_spine[["id"]]) # n = 1245

# Time period
dat_spine %<>% filter(dt_ktx >= as.Date("2006-01-01"))

# Count population
n_distinct(dat_spine[["id"]]) # n = 1103

# Recipient age
dat_spine %<>% filter(r_age >= 18)

# Count population
n_distinct(dat_spine[["id"]]) # n = 1097

# Donor age
dat_spine %<>% filter(d_age >= 18)

# Count population
n_distinct(dat_spine[["id"]]) # n = 1087

# Save cohort
saver("cohort_selection.Rdata", dat_spine)
