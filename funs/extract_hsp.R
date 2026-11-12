# Function to extract hospitalisation
extract_hsp <- function(text,
                        nottext = " ",
                        .data = dat_hsp,
                        spine = dat_link,
                        active = FALSE,
                        lookback = Inf){
  # Get vector of individuals with comorbidity
  vec_id <- spine %>%
    # Link comorbidities
    left_join(.data %>%
                # Keep only relevant DBC codes
                # Also drop irrelevant codes if nottext is specified beyond empty space
                filter(str_detect(reason, text) & (!str_detect(reason, nottext) | nottext == " ")),
              # Join by ID and transplantation date
              c("id", "dt_ktx")) %>%
    # Keep only those within the pre-specified period
    filter(
      # For active, start should be on/before index and end should be on/after index
      (active & dt_adm <= dt_ktx & dt_dis >= dt_ktx) |
        # For any:
        (!active & (
          # OR end is between lookback and index, regardless of start
          (dt_dis >= dt_ktx - lookback & dt_dis <= dt_ktx) |
            # OR end is after index, but start is before
            (dt_adm <= dt_ktx & dt_dis >= dt_ktx)))) %>%
    # Drop empty procedures
    filter(!is.na(reason)) %>%
    # Keep only unique ID
    distinct(id) %>%
    # Extract to vector
    extract2("id")
  
  # Return IDs
  return(vec_id)
}
