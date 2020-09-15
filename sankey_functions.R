# load in packages
library(shiny)
library(shinyWidgets)
library(dplyr)
library(titanic)
library(ggplot2)
library(ggforce)
library(RColorBrewer)
library(scales)
library(tidyverse)
library(dsmodules)


###================================================================================
### plot function to prepare data and create sankey plot
###================================================================================

prepare_data <- function(df, col_vars, fill_var = ""){
  
  if(fill_var == ""){
    fill_var <- NULL
    }
  
  if(is.null(fill_var)){
    groupby <- col_vars
  } else if(fill_var %in% col_vars){
    groupby <- col_vars
  } else {
    groupby <- c(col_vars, fill_var)
  }
  
  dat_prelim <- df %>% 
    select(groupby) %>% 
    replace(is.na(.), "missing") %>% 
    group_by_at(groupby) %>% 
    summarise(Freq = n()) %>%
    ungroup() %>% 
    mutate(id = row_number())
  
  if(is.null(fill_var)){
  dat_plot <- dat_prelim %>% 
    tidyr::gather(key = "x",
                  value = "stratum",
                  factor_key = TRUE,
                  col_vars)
  } else {
    dat_plot <- dat_prelim %>% 
      tidyr::gather(key = "x",
                    value = "stratum",
                    factor_key = TRUE,
                    col_vars) %>%
      left_join(dat_prelim %>%
                  mutate_(fill = fill_var) %>%
                  select(id, fill),
                by = "id")
  }
  return(dat_plot)
}

create_sankey_plot <- function(df, fill_var = "", palette = NULL, manualcols = NULL, labels = NULL, stratum_colour = "black"){
  
  if(fill_var == ""){
    fill_var <- NULL
  }
  
  
  if(stratum_colour == "black"){
    stratum_font_colour = "white"
  } else {
    stratum_font_colour = "black"
    
  }
  
  stratum_line_colour = "black"
  
  stratum_width <- 0.4
  stratum_angle <- 0
  legend_position <- "right"
  alpha <- 0.7
  
  gg <- ggplot(df, aes(x = x, id = id, split = stratum, value = Freq)) +
        geom_parallel_sets(alpha = alpha, axis.width = stratum_width) +
        geom_parallel_sets_axes(axis.width = stratum_width, 
                                fill = stratum_colour,
                                colour = stratum_line_colour) +
        geom_parallel_sets_labels(colour = stratum_font_colour, angle = stratum_angle) +
        theme_minimal() +
        theme(
          legend.position = legend_position,
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 14, face = "bold"),
          axis.title.x  = element_blank()
          )
  
  if(!is.null(fill_var)){
    gg <-   gg <- ggplot(df, aes(x = x, id = id, split = stratum, value = Freq)) +
      geom_parallel_sets(aes(fill = fill), alpha = alpha, axis.width = stratum_width) +
      geom_parallel_sets_axes(axis.width = stratum_width, 
                              fill = stratum_colour,
                              colour = stratum_line_colour) +
      geom_parallel_sets_labels(colour = stratum_font_colour, angle = stratum_angle) +
      scale_fill_discrete(name = fill_var) +
      theme_minimal() +
      theme(
        legend.position = legend_position,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.title.x  = element_blank()
      )
  }
  
  if(!is.null(palette)){
    gg <- gg +
      scale_fill_brewer(name = fill_var, palette = palette) +
      scale_color_brewer(palette = palette)
  }
  
  if(!is.null(manualcols)){
    gg <- gg +
      scale_fill_manual(name = fill_var, values = manualcols) +
      scale_color_manual(values = manualcols)
  }
  
  if(!is.null(labels)){
    gg <- gg +
      scale_x_discrete(labels = labels)
  }
  
  return(gg)
}
