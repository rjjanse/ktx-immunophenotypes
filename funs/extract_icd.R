# Function to extract comorbidities from ICD10 codes
extract_icd <- function( codes,
                        .data = dat_dgn,
                        spine = dat_link,
                        lookback = Inf){
  # Get vector of individuals with comorbidity
  vec_id <- spine %>%
    # Link comorbidities
    left_join(.data %>%
                # Keep only relevant ICD-10 codes
                filter(str_detect(icd10, codes)), 
              # Join by ID and transplantation date
              c("id", "dt_ktx")) %>%
    # Keep only those prior to index and before lookback horizon
    filter(dt_dgn >= dt_ktx - lookback & dt_dgn <= dt_ktx) %>%
    # Drop empty ICD
    filter(!is.na(icd10)) %>%
    # Keep only unique ID
    distinct(id) %>%
    # Extract to vector
    extract2("id")
  
  # Return IDs
  return(vec_id)
}
