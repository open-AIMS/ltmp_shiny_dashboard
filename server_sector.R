sector_tab_lookup <- list(
  "Photo-transects" =  list(
    data_type = "photo-transect",
    outputId = "sector_pt",
    family = "binomial",
    ## shelfs = c("Inshore", "Offshore"),
    groups = c("HARD CORAL", "SOFT CORAL",
               "ALGAE", "MACROALGAE")
  ),
  "Manta tow" =  list(
    data_type = "manta",
    outputId = "sector_manta",
    family = "beta",
    ## shelfs = c("Offshore"),
    groups = c("HARD CORAL")
  ),
  "Juveniles" =  list(
    data_type = "juveniles",
    outputId = "sector_juveniles",
    family = "binomial",
    ## shelfs = c("Inshore", "Offshore"),
    groups = c("HARD CORAL")
  ),
  "Fish" =  list(
    data_type = "fish",
    outputId = "sector_fish",
    family = "poisson",
    ## shelfs = c("Inshore", "Offshore"),
    groups = c("Harvested", "Herbivores", "Coral Trout",
               "Large fishes", "Damselfishes")
  )
)



observeEvent(input$sector_panel, {     ## when change panels
  tab_name <- input$sector_panel
  tab_id <- sector_tab_lookup[[tab_name]]$outputId
  current_candidates <- config_$models |>
    filter(data_scale == "Sector",
           data_type == sector_tab_lookup[[tab_name]]$data_type)
           ## domain_name == input$sector_selector)
  ## Render the content of the panel
  output[[paste0(tab_id, "_panel")]] <- renderUI({

    fluidRow(
      ## Selector box
      box(
        class = "sector-pt-panel-box",
        status = "info",
        width = 2,
        solidHeader = TRUE,
        column(width = 12,
               style = "display:flex",
           selectInput(paste0(tab_id, "_sector_selector"),
                       "Select Sector:",
                       choices = config_$models |>
                         filter(data_scale == "Sectors",
                                data_type == data_type) |>
                         pull(domain_name) |>
                         unique() |>
                       sort(), width = "90%"),
           actionButton(inputId = "sector_run_reef_refresh",
                        class = "refresh_button",
                        label = "",
                        icon = icon("rotate-right")),
           ),
        ## column(width = 12, selectInput(paste0(tab_id, "_shelf_selector"),
        ##                                "Select Shelf:",
        ##                                ## choices = sector_tab_lookup[[tab_name]]$shelfs)),
        ##                                choices = current_candidates |>
        ##                               pull(shelf) |> unique())),
        ## {if (sector_tab_lookup[[tab_name]]$data_type != "manta") {
           column(width = 12, selectInput(paste0(tab_id, "_group_selector"),
                                          "Select group:",
                                          ## choices = sector_tab_lookup[[tab_name]]$groups)),
                                          choices = current_candidates |>
                                            pull(group) |> unique()))
         ## }},
        ),
      ## Figure display box
      box(
        class = "panel-box",
        status = "info",
        width = 10,
        solidHeader = TRUE,
        tabsetPanel(
          id = "sector_plot_panel",
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
            reactableOutput(outputId = paste0(tab_id, "_all_annual_comp_tbl")),
            downloadButton(paste0(tab_id, "_all_annual_comp_download_data"), "Download as csv"),
            downloadButton(paste0(tab_id, "_all_annual_comp_download_data_posteriors"), "Download posteriors as csv")
            ),
          tabPanel(
            title = "Comparison to most recent",
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

  observeEvent(input$sector_run_reef_refresh, {
    tab_name <- input$sector_panel
    ## alert(tab_name)
    data_types <- sector_tab_lookup[[tab_name]]$data_type
    if(!does_db_table_exist("models")) {
      config_$models <- get_config_models()
    } else {
      config_$models <- get_db_model_data(method = NULL, scale = "sector", domain = NULL) |>
        filter(data_type == data_types)
    }
    ## alert(paste0("data_types:", data_types))
    ## config_$models <- get_config_models()
    ## config_$models <- get_db_summary_table(method = data_types , scale = "sector")
    ## assign("config_", config_, envir = .GlobalEnv)
    cat(file = stderr(), paste("refresh pressed:", config_$models), "\n")
    tab_id <- sector_tab_lookup[[tab_name]]$outputId
    ## alert(config_$models== "sector"))
    current_candidates <- config_$models |>
      ## filter(data_scale == "Sectors",
      filter(data_scale == "sector",
             ## data_type == sector_tab_lookup[[tab_name]]$data_type)
             data_type == data_types)
    ## alert(current_candidates |> pull(domain_name) |> unique())
    ## cat(file = stderr(), paste("refresh pressed:", current_candidates |> pull(domain_name) |> unique()), "\n")
    ## alert(current_candidates)
    updateSelectInput(session, paste0(tab_id, "_sector_selector"),
                      choices = current_candidates |>
                        pull(domain_name) |> unique())
  }
  )

  observeEvent(c(input[[paste0(tab_id, "_sector_selector")]]), {
    sector_selector <- input[[paste0(tab_id, "_sector_selector")]]
    data_types <- sector_tab_lookup[[tab_name]]$data_type
    ## shelfs <- config_$models |>
    ##   filter(data_type == data_types,
    ##          data_scale == "Sectors",
    ##          domain_name == sector_selector) |>
    ##   pull(shelf) |>
    ##   unique()
    ## updateSelectInput(session, paste0(tab_id, "_shelf_selector"),
    ##                   choices = shelfs)
    ## alert(paste0("data_type:", data_types))
    ## alert(paste0("config_$models:",config_$models |>
    ##                                filter(data_type == "manta",
    ##                                       data_scale == "Sectors")))
    groups <- config_$models |>
      filter(data_type == data_types,
             data_scale == "sector",
             domain_name == sector_selector) |>
      pull(group) |>
      unique()
    ## alert(groups)
    updateSelectInput(session, paste0(tab_id, "_group_selector"),
                      choices = groups)
  }
  )

  observeEvent(c(
    input[[paste0(tab_id, "_sector_selector")]],
    ## input[[paste0(tab_id, "_shelf_selector")]],
    input[[paste0(tab_id, "_group_selector")]]), {

      sector_selector <- input[[paste0(tab_id, "_sector_selector")]]
      ## shelf_selector <- input[[paste0(tab_id, "_shelf_selector")]]
      group_selector <- input[[paste0(tab_id, "_group_selector")]]
      data_type <- sector_tab_lookup[[tab_name]]$data_type

      ## Raw summaries
      output[[paste0(tab_id, '_fig')]] <- renderImage({
        outfile <- paste0("www/figures/gg_",
                          data_type,
                          "_Sectors_",
                          sector_selector,
                          "_",
                          group_selector,
                          "_",
                          " ",  ## ghost zone
                          "_",
                          " ",  ## ghost depth
                          "_",
                          " ",#shelf_selector,
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
                          "_Sectors_",
                          sector_selector,
                          "_",
                          group_selector,
                          "_",
                          " ",  ## ghost zone
                          "_",
                          " ",  ## ghost depth
                          "_",
                          " ", #shelf_selector,
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
               "_Sectors_",
               sector_selector,
               "_",
               group_selector,
               "_",
               " ",  ## ghost zone
               "_",
               " ",  ## ghost depth
               "_",
               " ", #shelf_selector,
               "_",
               ".png")
      })

      ## Raw data
      nm5 <- paste0("www/data/modelled/",
                    data_type,
                    "_Sectors_",
                    sector_selector,
                    "_",
                    group_selector,
                    "_",
                    " ",  ## ghost zone
                    "_",
                    " ",  ## ghost depth
                    "_",
                    " ", #shelf_selector,
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
        if(file.exists(nm5)) {
          ## data5 <- readRDS(file = nm5)
          ## alert(names(data5))
          if (data_type == "manta") {
            raw_bits <- data5 |>
              mutate(Sector = sector_selector) |> 
              dplyr::select(Sector) |>
              mutate(GROUP = group_selector,
                     FAMILY = sector_tab_lookup[[tab_name]]$family) |>
              distinct() 
          } else {
            raw_bits <- data5 |>
              mutate(Sector = sector_selector) |> 
              ## dplyr::select(Sector, Shelf) |>
              dplyr::select(Sector) |>
              mutate(GROUP = group_selector,
                     FAMILY = sector_tab_lookup[[tab_name]]$family) |>
              distinct() 
          }
        }
      }
      ## Annual summaries
      nm <- paste0("www/data/modelled/",
                   data_type,
                   "_Sectors_",
                   sector_selector,
                   "_",
                   group_selector,
                   "_",
                   " ",  ## ghost zone
                   "_",
                   " ",  ## ghost depth
                   "_",
                   " ", #shelf_selector,
                   "__",
                   ## "binomial_",
                   sector_tab_lookup[[tab_name]]$family,
                   "_",
                   "year_sum",
                   ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm)) {
          data <- readRDS(file = nm)
          data <- data |>
            mutate(Sector = sector_selector) |> 
            left_join(raw_bits, by = "Sector")
        }
        output[[paste0(tab_id, "_annual_tbl")]] <- reactable::renderReactable({
          make_table(data, type = "annual")
        })
        output[[paste0(tab_id, "_annual_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0(gsub("__.*", "_", basename(nm)), "annual_summary.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            write.csv(data, file)
          }
        )
        output[[paste0(tab_id, "_annual_download_data_posteriors")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0(gsub("__([^_]*)_.*", "__\\1_", basename(nm)), "annual_posteriors.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            data <- readRDS(gsub("sum.rds", "posteriors.rds", nm)) ## |>
              ## mutate(AIMS_REEF_NAME = reefs_selector,
              ##        DATA_TYPE = data_type,
              ##        )
            write_csv(data, file)
          }
        )
      }

      ## Annual group summaries
      nm2 <- paste0("www/data/modelled/",
                    data_type,
                    "_Sectors_",
                    sector_selector,
                    "_",
                    group_selector,
                    "_",
                    " ",  ## ghost zone
                    "_",
                    " ",  ## ghost depth
                    "_",
                    " ", #shelf_selector,
                    "__",
                    ## "binomial_",
                   sector_tab_lookup[[tab_name]]$family,
                   "_",
                    "year_group_sum",
                    ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm2)){
          data2 <- readRDS(file = nm2)
          data2 <- data2 |>
            mutate(Sector = sector_selector) |> 
            ## left_join(raw_bits, by = c("GROUP", "Sector"))
            left_join(raw_bits |> dplyr::select(-GROUP), by = c("Sector"))
        }
        output[[paste0(tab_id, "_annual_group_tbl")]] <- reactable::renderReactable({
          make_table(data2, type = "annual_group")
        })
        output[[paste0(tab_id, "_annual_group_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            ## paste0("annual_group.csv")
            paste0(gsub("__.*", "_", basename(nm2)), "annual_group_summary.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            write.csv(data2, file)
          }
        )
        output[[paste0(tab_id, "_annual_group_download_data_posteriors")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0(gsub("__([^_]*)_.*", "__\\1_", basename(nm2)), "annual_group_posteriors.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            data <- readRDS(gsub("sum.rds", "posteriors.rds", nm2)) ## |>
              ## mutate(AIMS_REEF_NAME = reefs_selector,
              ##        DATA_TYPE = data_type,
              ##        )
            write_csv(data, file)
          }
        )
      }
      
      ## All Annual comparison summaries
      nm3a <- paste0("www/data/modelled/",
                    data_type,
                    "_Sectors_",
                    sector_selector,
                    "_",
                    group_selector,
                    "_",
                    " ",  ## ghost zone
                    "_",
                    " ",  ## ghost depth
                    "_",
                    " ", #shelf_selector,
                    "__",
                    ## "binomial_",
                   sector_tab_lookup[[tab_name]]$family,
                   "_",
                    "all_yearcomp_sum",
                    ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm3a)) {
          data3a <- readRDS(file = nm3a)
          data3a <- data3a |>
            mutate(Sector = sector_selector) |> 
            left_join(raw_bits, by = "Sector")
        }
        output[[paste0(tab_id, "_all_annual_comp_tbl")]] <- reactable::renderReactable({
          make_table(data3a, type = "all_annual_comp")
        })
        output[[paste0(tab_id, "_all_annual_comp_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0(gsub("__.*", "_", basename(nm3)), "all_annual_comp_summary.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            write.csv(data3a, file)
          }
        )
        output[[paste0(tab_id, "_all_annual_comp_download_data_posteriors")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0(gsub("__([^_]*)_.*", "__\\1_", basename(nm)), "all_annual_comp_posteriors.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            data <- readRDS(gsub("sum.rds", "posteriors.rds", nm3a)) ## |>
              ## mutate(AIMS_REEF_NAME = reefs_selector,
              ##        DATA_TYPE = data_type,
              ##        )
            write_csv(data, file)
          }
        )
      }

      ## Annual comparison summaries (Comparison to most recent year)
      nm3 <- paste0("www/data/modelled/",
                    data_type,
                    "_Sectors_",
                    sector_selector,
                    "_",
                    group_selector,
                    "_",
                    " ",  ## ghost zone
                    "_",
                    " ",  ## ghost depth
                    "_",
                    " ", #shelf_selector,
                    "__",
                    ## "binomial_",
                   sector_tab_lookup[[tab_name]]$family,
                   "_",
                    "yearcomp_sum",
                    ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm3)) {
          data3 <- readRDS(file = nm3)
          data3 <- data3 |>
            mutate(Sector = sector_selector) |> 
            left_join(raw_bits, by = "Sector")
        }
        output[[paste0(tab_id, "_annual_comp_tbl")]] <- reactable::renderReactable({
          make_table(data3, type = "annual_comp")
        })
        output[[paste0(tab_id, "_annual_comp_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0(gsub("__.*", "_", basename(nm3)), "annual_comp_summary.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            write.csv(data3, file)
          }
        )
        output[[paste0(tab_id, "_annual_comp_download_data_posteriors")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0(gsub("__([^_]*)_.*", "__\\1_", basename(nm)), "annual_comp_posteriors.csv")
          },
          content = function(file) {
            ## Write the dataset to the `file` that will be downloaded
            data <- readRDS(gsub("sum.rds", "posteriors.rds", nm3)) ## |>
              ## mutate(AIMS_REEF_NAME = reefs_selector,
              ##        DATA_TYPE = data_type,
              ##        )
            write_csv(data, file)
          }
        )
      }

    ## Raw summaries
    nm4 <- paste0("www/data/modelled/",
                 data_type,
                 "_Sectors_",
                 sector_selector,
                 "_",
                 group_selector,
                 "_",
                 " ",  ## ghost zone
                 "_",
                 " ",  ## ghost depth
                 "_",
                 " ", #shelf_selector,
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


    })

})


