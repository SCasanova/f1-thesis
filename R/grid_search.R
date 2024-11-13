
#' Perform a p grid search on RBO for f1
#' 
#' @param p A number between 0 and 1
#' @returns Various metrics to maximize or analyze


grid_search_p <- function(p){
  
  # Comput RBO for 2022, 2023, and 2024 with parameter p
  rbo_2022 <- results_rbo(2022, p)
  
  rbo_2023 <- results_rbo(2023, p)
  
  rbo_2024 <- results_rbo(2024, p)
  
  # Add 2022 as base year and 2023 as n+1 year
  pair_1 <- inner_join(
    rbo_2022$pairwise,
    rbo_2023$pairwise |> 
      select(ordered_id, rbo_n1 = rbo),
    by = c('ordered_id')
  ) 
  
  # Add 2023 as base year and 2024 as n+1 year
  pair_2 <- inner_join(
    rbo_2023$pairwise,
    rbo_2024$pairwise |> 
      select(ordered_id, rbo_n1 = rbo),
    by = c('ordered_id')
  ) 
  
  # Combine results and compute L1 and L2 norms (pre)
  combined <- bind_rows(
    pair_1,
    pair_2
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