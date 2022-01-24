# Global.R - Bastian I. Hougaard
# Whenever you create a new module, remember to add it to Global.R.
# Global.R is ONLY for configuring global variables. Importing datasets in here
# is NOT recommended. You should import data from inside a module instead, 
# so your dataset is reactive.

# 1) IMPORT LIBRARIES
# When you import libraries below, they become available through-out your
# whole R Shiny application. Importing your libraries here also means
# you have a single location to watch what packages your application uses.
library(tidyverse)
library(plotly)
library(shiny)
library(shinyjs)
library(dplyr)

# 2) SET R OPTIONS
# Sometimes R defaults are not ideal for the app we use.
# shiny.fullstacktrace: prints full error messages from R Shiny for debugging.
# digits.secs: Show milliseconds in timestamps.
# shiny.maxRequestSize: increase maximum file upload size to 50MB.
options(shiny.fullstacktrace=TRUE)
options("digits.secs"=6)
options(shiny.maxRequestSize=50*1024^2)

# 3) IMPORT MODULES
# Whenever you create a new module, add it to the list below.
# Importing your module makes them callable in your R Shiny application.
source("plot_ggplot_module.R", local = T)
source("plot_violin_scatter.R", local = T)
source("data_import_module.R", local = T)
source("page_linked_plots_module.R", local = T)
source("plot_linked_module.R", local = T)