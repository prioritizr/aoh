# Initialization
## load packages
library(rredlist)
library(readr)
library(dplyr)

## define parameters
rl_categories <- c(
  "DD",
  "LC",
  "NT",
  "VU",
  "EN",
  "CR",
  "EW",
  "EX"
)

# Main processing
## download data
results <-
  lapply(rl_categories, function(x) {
    y <- rl_sp_category(x)$result
    y$category <- x
    ## wait
    Sys.sleep(2)
    # return result
    y
  }) %>%
  bind_rows()

# Exports
write_csv(results, "iucn-species-list.csv")
