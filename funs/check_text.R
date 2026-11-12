# Function to check DBC text fields
check_dbc <- function(text) dat_dbc[["dbc"]][str_detect(dat_dbc[["dbc"]], text)] %>% unique()

# Function to check hospitalisation text fields
check_hsp <- function(text) dat_hsp[["reason"]][str_detect(dat_hsp[["reason"]], text)] %>% unique()

# Function to check diagnosis text fields
check_dgn <- function(text) dat_dgn[["icd10"]][str_detect(dat_dgn[["icd10"]], text)] %>% unique()

# Function to check radiology text fields
check_rad <- function(text) dat_rad[["proc"]][str_detect(dat_rad[["proc"]], text)] %>% unique()

# Function to check surgery text fields
check_sur <- function(text) dat_sur[["proc"]][str_detect(dat_sur[["proc"]], text)] %>% unique()

# Function to check lab test text fields
check_lab <- function(text) dat_lab[["test"]][str_detect(dat_lab[["test"]], text)] %>% unique()
