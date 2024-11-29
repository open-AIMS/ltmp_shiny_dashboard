
nrm_tab_lookup <- list(
  "Photo-transects" =  list(
    data_type = "photo-transect",
    outputId = "nrm_pt",
    family = "binomial",
    shelfs = c("Inshore", "Offshore"),
    groups = c("HARD CORAL", "SOFT CORAL",
               "ALGAE", "MACROALGAE")
  ),
  "Manta tow" =  list(
    data_type = "manta",
    outputId = "nrm_manta",
    family = "beta",
    shelfs = c("Offshore"),
    groups = c("HARD CORAL")
  ),
  "Juveniles" =  list(
    data_type = "juveniles",
    outputId = "nrm_juveniles",
    family = "binomial",
    shelfs = c("Inshore", "Offshore"),
    groups = c("HARD CORAL")
  ),
  "Fish" =  list(
    data_type = "fish",
    outputId = "nrm_fish",
    family = "fish",
    shelfs = c("Inshore", "Offshore"),
    groups = c("Harvested", "Herbivores", "Coral Trout",
               "Large fishes", "Damselfishes")
  )
)

observeEvent(input$nrm_panel, {     ## when change panels
  tab_name <- input$nrm_panel
  tab_id <- nrm_tab_lookup[[tab_name]]$outputId
  current_candidates <- config_$models |>
    filter(data_scale == "nrm",
           data_type == nrm_tab_lookup[[tab_name]]$data_type)
           ## domain_name == input$nrm_selector)
  ## Render the content of the panel
  output[[paste0(tab_id, "_panel")]] <- renderUI({

    fluidRow(
      ## Selector box
      box(
        class = "nrm-pt-panel-box",
        status = "info",
        width = 2,
        solidHeader = TRUE,
        column(width = 12,
               style = "display:flex",
           selectInput(paste0(tab_id, "_nrm_selector"),
                       "Select NRM:",
                       choices = config_$models |>
                         filter(data_scale == "nrm",
                                data_type == data_type) |>
                         pull(domain_name) |>
                         unique() |>
                       sort(), width = "90%"),
           actionButton(inputId = "run_reef_refresh",
                        class = "refresh_button",
                        label = "",
                        icon = icon("rotate-right")),
           ),
        column(width = 12, selectInput(paste0(tab_id, "_shelf_selector"),
                                       "Select Shelf:",
                                       ## choices = nrm_tab_lookup[[tab_name]]$shelfs)),
                                       choices = current_candidates |>
                                      pull(shelf) |> unique())),
        column(width = 12, selectInput(paste0(tab_id, "_group_selector"),
                                       "Select group:",
                                       ## choices = nrm_tab_lookup[[tab_name]]$groups)),
                                       choices = current_candidates |>
                                      pull(group) |> unique())),
        ),
      ## Figure display box
      box(
        class = "panel-box",
        status = "info",
        width = 10,
        solidHeader = TRUE,
        tabsetPanel(
          id = "nrm_plot_panel",
          tabPanel(
            title = "Raw data",
            icon = icon("chart-line"),
            imageOutput(outputId = paste0(tab_id, "_raw_fig"), height = "600px"),
            textOutput(outputId = paste0(tab_id, "_raw_fig_cap"))
            ),
          tabPanel(
            title = "Partial plots",
            icon = icon("chart-area"),
            imageOutput(outputId = paste0(tab_id, "_fig"), height = "600px"),
            ),
          tabPanel(
            title = "Annual estimates",
            icon = icon("table"),
            reactableOutput(outputId = paste0(tab_id, "_annual_tbl")),
            downloadButton(paste0(tab_id, "_annual_download_data"), "Download as csv"),
            downloadButton(paste0(tab_id, "_annual_download_data_posteriors"), "Download posteriors as csv")
            ),
          tabPanel(
            title = "Annual group estimates",
            icon = icon("database"),
            reactableOutput(outputId = paste0(tab_id, "_annual_group_tbl")),
            downloadButton(paste0(tab_id, "_annual_group_download_data"), "Download as csv"),
            downloadButton(paste0(tab_id, "_annual_group_download_data_posteriors"), "Download posteriors as csv")
            ),
          tabPanel(
            title = "Annual comparison estimates",
            icon = icon("database"),
            reactableOutput(outputId = paste0(tab_id, "_annual_comp_tbl")),
            downloadButton(paste0(tab_id, "_annual_comp_download_data"), "Download as csv"),
            downloadButton(paste0(tab_id, "_annual_comp_download_data_posteriors"), "Download posteriors as csv")
            ),
          tabPanel(
            title = "Raw aggreations",
            icon = icon("database"),
            reactableOutput(outputId = paste0(tab_id, "_raw_sum_tbl")),
            downloadButton(paste0(tab_id, "_raw_sum_download_data"), "Download as csv")
            ),
          tabPanel(
            title = "Raw data",
            icon = icon("database"),
            reactableOutput(outputId = paste0(tab_id, "_raw_data_tbl")),
            downloadButton(paste0(tab_id, "_raw_data_download_data"), "Download as csv")
          ),
          )
      )
    )
  })  # end of renderUI

  observeEvent(c(input[[paste0(tab_id, "_nrm_selector")]]), {
    nrm_selector <- input[[paste0(tab_id, "_nrm_selector")]]
    data_type <- nrm_tab_lookup[[tab_name]]$data_type
    shelfs <- config_$models |>
      filter(data_type == data_type,
             data_scale == "nrm",
             domain_name == nrm_selector) |>
      pull(shelf) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_shelf_selector"),
                      choices = shelfs)
  }
  )

  observeEvent(c(
    input[[paste0(tab_id, "_nrm_selector")]],
    input[[paste0(tab_id, "_shelf_selector")]],
    input[[paste0(tab_id, "_group_selector")]]), {

      nrm_selector <- input[[paste0(tab_id, "_nrm_selector")]]
      shelf_selector <- input[[paste0(tab_id, "_shelf_selector")]]
      group_selector <- input[[paste0(tab_id, "_group_selector")]]
      data_type <- nrm_tab_lookup[[tab_name]]$data_type

      ## Raw summaries
      output[[paste0(tab_id, '_fig')]] <- renderImage({
        outfile <- paste0("www/figures/gg_",
                          data_type,
                          "_nrm_",
                          nrm_selector,
                          "_",
                          group_selector,
                          "_",
                          " ",  ## ghost zone
                          "_",
                          " ",  ## ghost depth
                          "_",
                          shelf_selector,
                          "_",
                          ".png")
        list(src = outfile,
             contentType =  "image/png",
             height = "600px",
             alt =  "this is alternative text")
      }, deleteFile = FALSE)

      ## Partial plots summaries
      output[[paste0(tab_id, '_raw_fig')]] <- renderImage({
        outfile <- paste0("www/figures/gg_raw_sum_",
                          data_type,
                          "_nrm_",
                          nrm_selector,
                          "_",
                          group_selector,
                          "_",
                          " ",  ## ghost zone
                          "_",
                          " ",  ## ghost depth
                          "_",
                          shelf_selector,
                          "_",
                          ".png")
        list(src = outfile,
             contentType =  "image/png",
             height = "600px",
             alt =  "this is alternative text")
      }, deleteFile = FALSE)

      output[[paste0(tab_id, '_raw_fig_cap')]] <- renderText({
        paste0("www/figures/gg_raw_sum_",
               data_type,
               "_nrm_",
               nrm_selector,
               "_",
               group_selector,
               "_",
               " ",  ## ghost zone
               "_",
               " ",  ## ghost depth
               "_",
               shelf_selector,
               "_",
               ".png")
      })

      ## Annual summaries
      nm <- paste0("www/data/modelled/",
                   data_type,
                   "_nrm_",
                   nrm_selector,
                   "_",
                   group_selector,
                   "_",
                   " ",  ## ghost zone
                   "_",
                   " ",  ## ghost depth
                   "_",
                   shelf_selector,
                   "__",
                   "binomial_",
                   "year_sum",
                   ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm)) data <- readRDS(file = nm)
        output[[paste0(tab_id, "_annual_tbl")]] <- reactable::renderReactable({
          make_table(data, type = "annual")
        })
        output[[paste0(tab_id, "_annual_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0("annual.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            write.csv(data, file)
          }
        )
      }

      ## Annual group summaries
      nm2 <- paste0("www/data/modelled/",
                    data_type,
                    "_nrm_",
                    nrm_selector,
                    "_",
                    group_selector,
                    "_",
                    " ",  ## ghost zone
                    "_",
                    " ",  ## ghost depth
                    "_",
                    shelf_selector,
                    "__",
                    "binomial_",
                    "year_group_sum",
                    ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm2)) data2 <- readRDS(file = nm2)
        output[[paste0(tab_id, "_annual_group_tbl")]] <- reactable::renderReactable({
          make_table(data2, type = "annual_group")
        })
        output[[paste0(tab_id, "_annual_group_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0("annual_group.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            write.csv(data2, file)
          }
        )
      }

      ## Annual comparison summaries
      nm3 <- paste0("www/data/modelled/",
                    data_type,
                    "_nrm_",
                    nrm_selector,
                    "_",
                    group_selector,
                    "_",
                    " ",  ## ghost zone
                    "_",
                    " ",  ## ghost depth
                    "_",
                    shelf_selector,
                    "__",
                    "binomial_",
                    "yearcomp_sum",
                    ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm3)) data3 <- readRDS(file = nm3)
        output[[paste0(tab_id, "_annual_comp_tbl")]] <- reactable::renderReactable({
          make_table(data3, type = "annual_comp")
        })
        output[[paste0(tab_id, "_annual_comp_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0("annual_comp.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            write.csv(data3, file)
          }
        )
      }

    ## Raw summaries
    nm4 <- paste0("www/data/modelled/",
                 data_type,
                 "_nrm_",
                 nrm_selector,
                 "_",
                 group_selector,
                 "_",
                 " ",  ## ghost zone
                 "_",
                 " ",  ## ghost depth
                 "_",
                 shelf_selector,
                 "__",
                 "raw_sums",
                 ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm4)) data4 <- readRDS(file = nm4)
        output[[paste0(tab_id, "_raw_sum_tbl")]] <- reactable::renderReactable({
          make_table(data4, type = "raw_sum")
        })
        output[[paste0(tab_id, "_raw_sum_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0("raw_sum.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            write.csv(data4, file)
          }
        )
      }

      ## Raw data
      nm5 <- paste0("www/data/modelled/",
                    data_type,
                    "_nrm_",
                    nrm_selector,
                    "_",
                    group_selector,
                    "_",
                    " ",  ## ghost zone
                    "_",
                    " ",  ## ghost depth
                    "_",
                    shelf_selector,
                    "__",
                    "raw_data",
                    ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm5)) data5 <- readRDS(file = nm5)
        output[[paste0(tab_id, "_raw_data_tbl")]] <- reactable::renderReactable({
          make_table(data5, type = "raw_data")
        })
        output[[paste0(tab_id, "_raw_data_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0("raw_data.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            write.csv(data5, file)
          }
        )
      }

    })

})


make_table <- function(data, type = "annual") {
  dv <- data
  if (type == "annual") {
   dv <- dv |> 
    arrange(desc(fYEAR)) |> 
    dplyr::select(REPORT_YEAR, DATE, median, everything(), -fYEAR) 
  }
  if (type == "annual_group") {
   dv <- dv |> 
    arrange(desc(fYEAR)) |> 
    dplyr::select(REPORT_YEAR, GROUP = fGROUP, DATE, median, everything(), -fYEAR) 
  }
  if (type == "raw_sum") {
   dv <- dv |> 
    arrange(desc(fYEAR))  
  }
  dv <- dv |>
    reactable(
      compact = TRUE, bordered = TRUE, resizable = TRUE,
      highlight = TRUE,
      wrap = FALSE,
      filterable = TRUE,
      defaultColDef = colDef(filterMethod = JS("function(rows, columnId, filterValue) {
        return rows.filter(function(row) {
          return row.values[columnId].indexOf(filterValue) !== -1
        })
      }")),
      ##pagination = FALSE, height = 600,
      ## defaultColDef = colDef(style = "white-space: nowrap;"),
      theme = reactableTheme(
        headerStyle = list(color = "white", backgroundColor = "rgb(81, 127, 185)"),
        borderWidth = "1pt",
        borderColor = "rgb(85, 85, 85)",
        style = list(fontFamily = "Helvetica, Arial, sans-serif", fontSize = "10px")
      )
    )
}
