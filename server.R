library(shiny)
library(reactable)
library(tidyverse)

server <- function(input, output, session) {
 
  source("server_sidebar.R", local = TRUE)
  source("server_dashboard.R", local = TRUE)
  source("server_reefs.R", local = TRUE)
  source("server_nrm.R", local = TRUE)
  source("server_sector.R", local = TRUE)
  
}


