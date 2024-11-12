
# Imports -----------------------------------------------------------------

library(patchwork)
library(latex2exp)
source('R/finish_relationship.R')


# Test RBO ----------------------------------------------------------------

# Compute rbo for 2022 and 2023 as test
rbo_2022 <- results_rbo(2022)

rbo_2023 <- results_rbo(2023)


inner_join(
  rbo_2022$pairwise,
  rbo_2023$pairwise |> 
    select(ordered_id, rbo_23 = rbo),
  by = c('ordered_id')
) 


# Grid search for p -------------------------------------------------------

# Create our grid testing values for parameter p
p_grid <- seq(0.85,0.9999,0.0005)


# Function that will perform our grid search
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

# Apply the function on the p values we have selected
grid_results <- purrr::map_df(p_grid, grid_search_p)


# Generate all the plots to visualize
covariance_plot <- ggplot(grid_results, aes(p, variance))+
  geom_line()

n2_plot <- ggplot(grid_results, aes(p, mse))+
  geom_line()

kendall_plot <- ggplot(grid_results, aes(p, kendall))+
  geom_line()

cor_plot <- ggplot(grid_results, aes(p, cor))+
  geom_line()

(covariance_plot+n2_plot)/(kendall_plot+cor_plot)

# Get the precise result
max_p <- grid_results |> 
  filter(cor == max(cor))

# Generate figure for paper
ggplot(grid_results, aes(p, cor))+
  geom_line(size = 1.3) +
  ggthemes::theme_fivethirtyeight()+
  theme(
    plot.background = element_blank(),
    panel.background = element_blank(),
    axis.title = element_text()
  )+
  labs(
    x = 'Parameter p',
    y = 'Year-to-year correlation (Pearson)'
  )+
  annotate("text", 
           x=0.91, 
           y=0.5, 
           label=TeX(paste0("$p_{max}$ = ", round(max_p$p, 2))), 
           hjust = 0,
           size= 6,
           parse=TRUE)

# Save
ggsave(
  'figures/p_max.png',
  height = 5.5,
  width = 6,
  device = 'png',
  dpi = 'retina',
  bg = 'transparent'
)


# Have p follow points structure ------------------------------------------





