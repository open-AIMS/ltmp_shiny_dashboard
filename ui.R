      
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(tidyverse)
library(fansi)
## library(promises)
library(processx)
## library(future)
library(DBI)
library(RSQLite)
library(digest)
## source("https://raw.githubusercontent.com/open-AIMS/ltmp_dashboard/refs/heads/main/R/backend_functions.R")
source("/home/mlogan/dev/R/backend_functions.R")
              
## future::plan(multisession)
source("dashboard_config.R", local = FALSE)    ##PUT THIS BACK AFTER TESTING
source("ui_header.R")
source("ui_sidebar.R")
source("ui_body.R")
                                        
shinyUI(
  dashboardPage(
    useShinyjs(),
    header = header,
    sidebar = sidebar,
    body = body,
  )
)


                                                        
