## Create a function that output a tree table OR a graph

make_trees <- function(.input_ba, .input_model = FALSE, .table = FALSE){
  
  stopifnot(is.logical(.table))
  
  n <- 10 + round(.input_ba / 2, 0)
  
  set.seed(1)
  ba_round <- 1000
  while(ba_round != .input_ba) {
    vec_dbh <- sample(50:1000, n) / 10
    ba <- round(sum(vec_dbh^2 / 10000 * pi), 2)
    ba_round <- round(ba / 5) * 5
  }
  
  tree <- tibble(
    dbh          = vec_dbh, 
    height_calc  = exp(0.893 + 0.760 * log(dbh) - 0.0340 * log(dbh^2)),
    height_error = exp(rnorm(n = n, mean = 0, sd = 0.243)),
    height_top   = height_calc * height_error,
    dist1        = sample(0:800, n, replace = T) / 100,
    dist2        = sample(0:1784, n, replace = T) / 100,
    distance     = if_else(dbh < 30, dist1, dist2),
    azimuth      = sample(0:360, n, replace = T)
  ) %>%
    select(-dist1, -dist2)
  
  gr_list <- list(
    geom_line(aes(y = height_calc), alpha = 0.5),
    geom_segment(aes(xend = dbh, y = h_min, yend = h_max), alpha = 0.2)
  )
  
  gr_tree <- tree %>%
    mutate(
      h_min = if_else(height_error > 0, height_calc, height_top ),
      h_max = if_else(height_error > 0, height_top , height_calc),
    ) %>%
    ggplot(aes(x = dbh)) +
    geom_point(aes(y = height_top), size = 2) +
    { if (.input_model == "Show model") gr_list } +
    labs(
      x = "Diameter at breast height (cm)",
      y = "Tree total height (m)",
      title = bquote("Plot basal area" ~ (m^2/ha) == .(ba))
    )
  
  if (.table) return(tree) else return(gr_tree)
  
}



## Test function
library(tidyverse)
input <- list(basal_area = 40, show_model = "Show model")
make_trees(.input_ba = input$basal_area, .table = TRUE)
make_trees(.input_ba = input$basal_area, .input_model = "Show model", .table = FALSE)
