## BG pilot data analysis 2017
## Recheck 2020 for NFI module

## Setup scripts include:
##   01-1 libraries, 
##   01-2 default theme and style for plots,
##   01-3 path to data and results and 
##   01-4 custom functions.


## Check if plot style assigned correctly
stopifnot(is.character(set_plot_style), set_plot_style %in% c("light", "dark", "print"))


## Custom hack ggplot function
if (set_plot_style == "light") {
  
  custom_palette <- c(
    "#111111", "#E69F00", "#56B4E9", 
    "#009E73", "#D55E00", "#0072B2", 
    "#F0E442", "#CC79A7", "#C3D7A4"
  )

  ggplot <- function(...) ggplot2::ggplot(...) +
    scale_color_manual(values = custom_palette) +
    scale_fill_manual(values = custom_palette) +
    theme_bw()
  
  col_bg  <- "white"
  col_txt <- "black"
  
  scales::show_col(custom_palette)
  
} else if (set_plot_style == "dark") {
  
  dark_theme_used <- 1
  
  custom_palette <- c(
    "#999999", "#E69F00", "#56B4E9", 
    "#009E73", "#D55E00", "#0072B2", 
    "#F0E442", "#CC79A7", "#C3D7A4"
  )
  
  ggplot <- function(...) ggplot2::ggplot(...) +
    scale_color_manual(values = custom_palette) +
    scale_fill_manual(values = custom_palette) +
    dark_theme_bw()
  
  col_bg  <- "black"
  col_txt <- "white"
  
  par(bg="black")
  scales::show_col(custom_palette)
  par(bg="white")
  
} else if (set_plot_style == "print") {
  
  custom_palette <- c(
    "#999999", "#000000", "#777777", 
    "#222222", "#656565", "#343434"
  )
  
  ggplot <- function(...) ggplot2::ggplot(...) +
    scale_color_manual(values = custom_palette) +
    scale_fill_manual(values = custom_palette) +
    theme_bw()
  
  col_bg <- "white"
  col_txt <- "black"
  
  scales::show_col(custom_palette)
  
} ## END IF

## Reset geom default in case dark theme has been use before
if (exists("dark_theme_used") & set_plot_style != "dark") { 
  invert_geom_defaults() 
  rm(dark_theme_used)
}


## Reset ggplot if needed
#rm(ggplot)

