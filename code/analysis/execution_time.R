# This file is for generating execution time plots

library(tidyverse)

convert_to_secs <- function(timing_string) {
  mins <- as.numeric(sub(".*\\t(\\d+)m.*", "\\1", timing_string))
  secs <- as.numeric(sub(".*m([0-9.]+)s", "\\1", sys_timings))
  return(mins*60 + secs)
}

read_timings_file <- function(timings_path) {
  pipeline = sub("_.*", "", basename(dirname(timings_path)))
  sample_number = as.numeric(sub(".*_(\\d+)\\.qlog$", "\\1", timings_path))
  
  full_timings<- readLines(timings_path) #usr + sys
  user_timings <- grep("^(user)\\t", full_timings, value = TRUE)
  sys_timings <- grep("^(sys)\\t", full_timings, value = TRUE)
  
  total_secs <- convert_to_secs(user_timings) + convert_to_secs(sys_timings)
  
  return(data.frame(pipeline = pipeline, 
                    sample_number = sample_number, 
                    total_secs = total_secs))
}

all_timings_paths <- list.files("timings", pattern = ".qlog", recursive = TRUE, 
                                full.names = TRUE)

all_timings <- purrr::map_dfr(all_timings_paths, read_timings_file)

ggplot(all_timings, aes(x = sample_number, y = total_secs, color = pipeline)) + 
  geom_point()
