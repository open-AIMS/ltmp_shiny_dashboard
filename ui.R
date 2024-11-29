     
library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(fansi)
## library(promises)
library(processx)
## library(future)
library(DBI)
library(RSQLite)
 
## future::plan(multisession)
source("dashboard_config.R", local = FALSE)
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
             
              
    
                 
     
