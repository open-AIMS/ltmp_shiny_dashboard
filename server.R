library(shiny)
library(reactable)
library(tidyverse)

server <- function(input, output, session) {
 
  source("server_sidebar.R", local = TRUE)
  source("server_dashboard.R", local = TRUE)
  source("server_reefs.R", local = TRUE)
  source("server_nrm.R", local = TRUE)
  source("server_sector.R", local = TRUE)
  
  ## ## source("server_nrm_pt.R", local = TRUE)
}

## server <- function(input, output, session) {
##   output$plot <- renderPlot({
##     set.seed(123)
##     data <- rnorm(input$num)
##     hist(data, main = "Histogram of Random Data", 
##          xlab = "Value", col = "skyblue", border = "white")
##   })
## }
