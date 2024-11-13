
# Imports -----------------------------------------------------------------

library(patchwork)
library(latex2exp)
source('R/rbo.R')
source('R/finish_relationship.R')
source('R/grid_seach.R')


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

# Grid seach for best p

points <- tibble::tibble(
  pos = seq(1, 20, 2),
  points = c(25,18,15,12,10,8,6,4,2,1)
) |> 
  mutate(cummulative = cumsum(points)/sum(points),
         weight = purrr::map_dbl(pos, function(x)rbo_weight(0.923, x)))

ggplot(points, aes(pos, cummulative))+
  geom_line()+
  geom_line(aes(y = weight))

