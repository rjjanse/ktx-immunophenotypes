# Function to extract medication from ATC codes
extract_atc <- function(codes,
                        .data = dat_med,
                        spine = dat_link,
                        active = FALSE,
                        lookback = Inf){
  # Get vector of individuals with comorbidity
  vec_id <- spine %>%
    # Link comorbidities
    left_join(.data %>%
                # Keep only relevant ICD-10 codes
                filter(str_detect(atc, codes)), 
              # Join by ID and transplantation date
              c("id", "dt_ktx")) %>%
    # Keep only those within the pre-specified period
    filter(
      # For active, start should be on/before index and end should be on/after index
      (active & dt_start <= dt_ktx & dt_end >= dt_ktx) |
      # For any:
      (!active & (
        # OR end is between lookback and index, regardless of start
        (dt_end >= dt_ktx - lookback & dt_end <= dt_ktx) |
        # OR end is after index, but start is before
        (dt_start <= dt_ktx & dt_end >= dt_ktx)))) %>%
    # Drop empty ATCs
    filter(!is.na(atc)) %>%
    # Keep only unique ID
    distinct(id) %>%
    # Extract to vector
    extract2("id")
  
  # Return IDs
  return(vec_id)
}
