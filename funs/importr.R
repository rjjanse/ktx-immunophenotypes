# Shortcut function to import data with a pre-specified path, named path
importr <- function(file, prefix_path = path) import(paste0(prefix_path, file))
