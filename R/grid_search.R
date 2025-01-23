
source('R/finish_relationship.R')


#' Perform a p grid search on RBO for f1
#' 
#' @param p A number between 0 and 1
#' @returns Various metrics to maximize or analyze


grid_search_p <- function(p){
  
  # Comput RBO for 2022, 2023, and 2024 with parameter p and exclude same
  # race comparisons
  rbo_2022 <- results_rbo(2022, p)$pairwise 
  
  rbo_2023 <- results_rbo(2023, p)$pairwise 
  
  # rbo_2024 <- results_rbo(2024, p)$pairwise 
  
  # Add 2022 as base year and 2023 as n+1 year
  pair_1 <- inner_join(
    rbo_2022,
    rbo_2023 |> 
      select(ordered_id, rbo_n1 = rbo),
    by = c('ordered_id')
  ) 
  
  # Add 2023 as base year and 2024 as n+1 year
  # pair_2 <- inner_join(
  #   rbo_2023,
  #   rbo_2024 |> 
  #     select(ordered_id, rbo_n1 = rbo),
  #   by = c('ordered_id')
  # ) 
  
  # Test a 2-year relationship
  
  # pair_3 <- inner_join(
  #   rbo_2022,
  #   rbo_2024 |> 
  #     select(ordered_id, rbo_n1 = rbo),
  #   by = c('ordered_id')
  # ) 
  
  # Combine results and compute L1 and L2 norms (pre)
  combined <- bind_rows(
    pair_1,
    # pair_2,
    # pair_3
  ) |> 
    mutate(
      diff = rbo-rbo_n1,
      n1 = abs(diff),
      n2 = (diff)^2
    )
  
  # Get our test maximizing function outputs
  results <- tibble(
    p = p,
    mae = mean(combined$n1),
    mse = mean(combined$n2) |> sqrt(),
    # n_inf = max(combined$diff),
    cor = cor(combined$rbo, combined$rbo_n1),
    kendall = cor(combined$rbo, combined$rbo_n1, method = 'kendall'),
    variance = cov(combined$rbo, combined$rbo_n1)
  ) 
  
  results
  
}


#' Perform a p grid search on RBO based on point structure
#' 
#' @param p A number between 0 and 1
#' @returns the L2 norm for various values


grid_search_points <- function(p){
  
  points <- tibble::tibble(
    pos = seq(1, 20, 2),
    points = c(25,18,15,12,10,8,6,4,2,1)
  ) |> 
    mutate(cummulative = cumsum(points)/sum(points),
           weight = purrr::map_dbl(pos, function(x)rbo_weight(p, x)),
           diff = cummulative-weight)
  
  results <- tibble::tibble(
    p = p,
    l1 = abs(points$diff) |> sum(),
    l2 = (points$diff)^2 |> sum()
  )
  
  
  results
  
  
}


