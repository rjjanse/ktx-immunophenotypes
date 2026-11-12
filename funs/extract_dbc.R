# Function to extract diagnosis from DBC codes
extract_dbc <- function(text,
                        nottext = " ",
                        .data = dat_dbc,
                        spine = dat_link,
                        active = FALSE,
                        lookback = Inf){
  # Get vector of individuals with comorbidity
  vec_id <- spine %>%
    # Link comorbidities
    left_join(.data %>%
                # Keep only relevant DBC codes
                # Also drop irrelevant codes if nottext is specified beyond empty space
                filter(str_detect(dbc, text) & (!str_detect(dbc, nottext) | nottext == " ")),
              # Join by ID and transplantation date
              c("id", "dt_ktx")) %>%
    # Keep only those within the pre-specified period
    filter(
      # For active, start should be on/before index and end should be on/after index
      (active & dt_dbc_start <= dt_ktx & dt_dbc_end >= dt_ktx) |
        # For any:
        (!active & (
          # OR end is between lookback and index, regardless of start
          (dt_dbc_end >= dt_ktx - lookback & dt_dbc_end <= dt_ktx) |
            # OR end is after index, but start is before
            (dt_dbc_start <= dt_ktx & dt_dbc_end >= dt_ktx)))) %>%
    # Drop empty DBCs
    filter(!is.na(dbc)) %>%
    # Keep only unique ID
    distinct(id) %>%
    # Extract to vector
    extract2("id")
  
  # Return IDs
  return(vec_id)
}
