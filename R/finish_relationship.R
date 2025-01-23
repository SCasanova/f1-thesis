
# Imports ---------------------------------------------------------------

# For data manipulation
library(tidyverse)
# To extract F1 data
library(f1dataR)
# RBO function for comparison
source('R/rbo.R')


# Result RBO computation -------------------------------------------------------------


results_rbo <- function(season, p = 0.9, ...){
  
  # Get the names (IDs) for 2022 races
  race_list <- load_schedule(season) |> 
    filter(date < Sys.Date()) |> 
    select(season, round, circuit_id)
  
  results <- 
    # We use lapply so we are able to store different length results if required
    lapply(
      # Iterate over all races
      1:nrow(race_list),
      function(i) {
        # Get race results
        load_results(season, i) |>
          mutate(laps = parse_number(laps)) |> 
          # Obtain an ordered vector of positions
          pull(driver_id)
      }
    )
  
  # Name each list item with the correct race ID for cross-year comparisons
  names(results) <- race_list$circuit_id
  
  
  rbo_matrix <- 
    sapply(
      # Iterate over whole list length
      1:length(results),
      function(i) {
        # Iterate over whole list length, again, for pairwise calculations
        sapply(1:length(results),
               function(j) {
                 # Compute RBO for ith and jth element fo our list
                 rbo(results[[i]], results[[j]], p)
               })
      }
    ) |>
    data.frame()
  
  # Assign the correct row and column names for interpretation
  colnames(rbo_matrix) <- race_list$circuit_id
  rownames(rbo_matrix) <- race_list$circuit_id
  
  rbo_long <- rbo_matrix |> 
    rownames_to_column("race1") |> 
    # Convert to pair list format
    pivot_longer(c(-race1), names_to = "race2", values_to = "rbo")
  
  # Create a unique pair id (alphabeticaly sorted)
  rbo_long$ordered_id <- apply(rbo_long[, c('race1', 'race2')], 1, 
                         function(x) paste(sort(x), collapse=""))
  
  # Eliminate symetric entires
  rbo_long <- rbo_long[!duplicated(rbo_long$ordered_id), ]
  
  # Output matrix form and pairwise form
  list(
    matrix = rbo_matrix,
    pairwise = rbo_long
  )
  
}







