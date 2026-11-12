# Easy save function
saver <- function(file, ..., prefix_path = path) save(..., file = paste0(prefix_path, file))
