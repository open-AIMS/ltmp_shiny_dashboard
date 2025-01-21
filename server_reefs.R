

reefs_tab_lookup <- list(
  "Photo-transects" =  list(
    data_type = "photo-transect",
    outputId = "reefs_pt",
    family = "binomial"
    ## zones = c(" "),
    ## depths = c("2", "5", "9"),
    ## shelfs = c(" "),
    ## groups = c("HARD CORAL", "SOFT CORAL",
    ##            "ALGAE", "MACROALGAE")
  ),
  "Manta tow" =  list(
    data_type = "manta",
    outputId = "reefs_manta",
    family = "beta"
    ## shelfs = c("Offshore"),
    ## groups = c("HARD CORAL")
  ),
  "Juveniles" =  list(
    data_type = "juveniles",
    outputId = "reefs_juveniles",
    family = "binomial"
    ## shelfs = c("Inshore", "Offshore"),
    ## groups = c("HARD CORAL")
  ),
  "Fish" =  list(
    data_type = "fish",
    outputId = "reefs_fish",
    family = "poisson"
    ## shelfs = c("Inshore", "Offshore"),
    ## groups = c("Harvested", "Herbivores", "Coral Trout",
    ##            "Large fishes", "Damselfishes")
  )
)

observeEvent(input$run_reef_refresh, {
  tab_name <- input$reefs_panel
  data_types <- reefs_tab_lookup[[tab_name]]$data_type
  ## alert(data_types)
  if(!does_db_table_exist("models")) {
    config_$models <- get_config_models()
    ## cat(file = "/home/mlogan/data/con.txt", paste("refresh pressed:", config_$models), "\n", append = FALSE)
    ## write_csv(file = "/home/mlogan/data/con.csv", config_$models)
  } else {
    ##   config_$models <- get_db_summary_table(method = data_types , scale = "reef")
    ## write_csv(file = "/home/mlogan/data/con1.csv", config_$models)
    ## config_$models <- get_db_model_data(method = data_types, scale = "reef", domain = NULL)
    config_$models <- get_db_model_data(method = NULL, scale = "reef", domain = NULL) |>
      filter(data_type == data_types)
    ## write_csv(file = "/home/mlogan/data/con2.csv", config_$models)
    ## cat(file = "/home/mlogan/data/con1.txt", paste("refresh pressed:", config_$models), "\n", append = FALSE)
  }
    ## alert(config_$models |>
    ##   filter(data_type == "manta",
    ##          data_scale == "reef",
    ##          domain_name == "Reef 14-133"))
  assign("config_", config_, envir = .GlobalEnv)
  ## alert(config_$models |> filter(domain_name == "Reef 14-133"))
  cat(file = stderr(), paste("refresh pressed:", config_$models), "\n")
  tab_id <- reefs_tab_lookup[[tab_name]]$outputId
  current_candidates <- config_$models |>
    filter(data_scale == "reef",
           data_type == reefs_tab_lookup[[tab_name]]$data_type)
  ## cat(file = stderr(), paste("refresh pressed:", current_candidates |> pull(domain_name) |> unique()), "\n")
  ## alert(current_candidates)
  updateSelectInput(session, paste0(tab_id, "_reefs_selector"),
                    choices = current_candidates |>
                      pull(domain_name) |> unique())
}
)


observeEvent(input$reefs_panel, {     ## when change panels
  ## config_mod_file <- paste0(config_$data_path, "models.csv")
  tab_name <- input$reefs_panel
  tab_id <- reefs_tab_lookup[[tab_name]]$outputId
  ## current_candidates <- config_$models |>
  ##   filter(data_scale == "reef",
  ##          data_type == reefs_tab_lookup[[tab_name]]$data_type,
  ##          domain_name == input$reefs_selector)
  current_candidates <- config_$models |>
    filter(data_scale == "reef",
           data_type == reefs_tab_lookup[[tab_name]]$data_type)
  ## Render the content of the panel
  data_type <- reefs_tab_lookup[[tab_name]]$data_type
  ## alert(current_candidates)
  ## alert(data_type)
  output[[paste0(tab_id, "_panel")]] <- renderUI({

    fluidRow(
      ## Selector box
      box(
        class = "reefs-pt-panel-box",
        status = "info",
        width = 2,
        solidHeader = TRUE,
        column(width = 12,
               style = "display:flex",
           selectInput(paste0(tab_id, "_reefs_selector"),
                       "Select Reef:",
                       choices = config_$models |>
                         filter(data_scale == "reef",
                                data_type == data_type) |>
                         pull(domain_name) |>
                         unique() |>
                       sort(), width = "90%"),
           actionButton(inputId = "run_reef_refresh",
                        class = "refresh_button",
                        label = "",
                        icon = icon("rotate-right")),
           ),
        column(width = 12,
               selectInput(paste0(tab_id, "_zone_selector"),
                           "Select Zone:",
                           choices = current_candidates |>
                           pull(reef_zone) |> unique())),
        column(width = 12,
               selectInput(paste0(tab_id, "_depth_selector"),
                           "Select Depth:",
                           choices = current_candidates |>
                             pull(depth) |> unique())),
        ## column(width = 12,
        ##        selectInput(paste0(tab_id, "_shelf_selector"),
        ##                    "Select Shelf:",
        ##                    choices = current_candidates |>
        ##                    pull(shelf) |> unique())),
        column(width = 12,
               selectInput(paste0(tab_id, "_group_selector"),
                           "Select group:",
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
          id = "reefs_plot_panel",
          tabPanel(
            title = "Raw data",
            icon = icon("chart-line"),
            imageOutput(outputId = paste0(tab_id, "_raw_fig"), height = "600px"),
            textOutput(outputId = paste0(tab_id, "_raw_fig_cap")),
            imageOutput(outputId = paste0(tab_id, "_raw_group_fig"), height = "600px"),
            ),
          tabPanel(
            title = "Partial plots",
            icon = icon("chart-area"),
            imageOutput(outputId = paste0(tab_id, "_fig"), height = "600px"),
            imageOutput(outputId = paste0(tab_id, "_group_fig"), height = "600px"),
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

  observeEvent(c(input[[paste0(tab_id, "_reefs_selector")]]), {
    reefs_selector <- input[[paste0(tab_id, "_reefs_selector")]]
    data_type <- reefs_tab_lookup[[tab_name]]$data_type
 ## alert(reefs_selector)
    zones <- config_$models |>
      filter(data_type == data_type,
             data_scale == "reef",
             domain_name == reefs_selector) |>
      pull(reef_zone) |>
      unique()
    ## alert(config_$models |>
    ##   filter(data_type == data_type,
    ##          data_scale == "reef",
    ##          domain_name == reefs_selector))
    updateSelectInput(session, paste0(tab_id, "_zone_selector"),
                      choices = zones)
    depths <- config_$models |>
      filter(data_type == data_type,
             data_scale == "reef",
             domain_name == reefs_selector) |>
      pull(depth) |>
      unique()
    ## alert(depths)
    updateSelectInput(session, paste0(tab_id, "_depth_selector"),
                      choices = depths)

    groups <- config_$models |>
      filter(data_type == data_type,
             data_scale == "reef",
             domain_name == reefs_selector) |>
      pull(group) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_group_selector"),
                      choices = groups)
  }
  )

  observeEvent(input[[paste0(tab_id, "_zone_selector")]], {
    reefs_selector <- input[[paste0(tab_id, "_reefs_selector")]]
    data_type <- reefs_tab_lookup[[tab_name]]$data_type
    depths <- config_$models |>
      filter(data_type == data_type,
             data_scale == "reef",
             domain_name == reefs_selector,
             reef_zone == input[[paste0(tab_id, "_zone_selector")]]
             ) |>
      pull(depth) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_depth_selector"),
                      choices = depths)
  }
  )
  
  observeEvent(c(
    input[[paste0(tab_id, "_reefs_selector")]],
    input[[paste0(tab_id, "_shelf_selector")]],
    input[[paste0(tab_id, "_depth_selector")]],
    input[[paste0(tab_id, "_zone_selector")]],
    input[[paste0(tab_id, "_group_selector")]]), {

      reefs_selector <- input[[paste0(tab_id, "_reefs_selector")]]
      shelf_selector <- " " #input[[paste0(tab_id, "_shelf_selector")]]
      zone_selector <- input[[paste0(tab_id, "_zone_selector")]]
      depth_selector <- input[[paste0(tab_id, "_depth_selector")]]
      group_selector <- input[[paste0(tab_id, "_group_selector")]]
      data_type <- reefs_tab_lookup[[tab_name]]$data_type

      ## Raw summaries
      output[[paste0(tab_id, '_fig')]] <- renderImage({
        outfile <- paste0("www/figures/gg_",
                          data_type,
                          "_reef_",
                          reefs_selector,
                          "_",
                          group_selector,
                          "_",
                          zone_selector,
                          "_",
                          depth_selector,
                          "_",
                          shelf_selector,
                          "_",
                          ".png")
        list(src = outfile,
             contentType =  "image/png",
             height = "600px",
             alt =  "this is alternative text")
      }, deleteFile = FALSE)

      ## Raw group summaries
      output[[paste0(tab_id, '_group_fig')]] <- renderImage({
        outfile <- paste0("www/figures/gg_group_",
                          data_type,
                          "_reef_",
                          reefs_selector,
                          "_",
                          group_selector,
                          "_",
                          zone_selector,
                          "_",
                          depth_selector,
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
                          "_reef_",
                          reefs_selector,
                          "_",
                          group_selector,
                          "_",
                          zone_selector,
                          "_",
                          depth_selector,
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
               "_reef_",
               reefs_selector,
               "_",
               group_selector,
               "_",
               zone_selector,
               "_",
               depth_selector,
               "_",
               shelf_selector,
               "_",
               ".png")
      })

      output[[paste0(tab_id, '_raw_group_fig')]] <- renderImage({
        outfile <- paste0("www/figures/gg_raw_group_sum_",
                          data_type,
                          "_reef_",
                          reefs_selector,
                          "_",
                          group_selector,
                          "_",
                          zone_selector,
                          "_",
                          depth_selector,
                          "_",
                          shelf_selector,
                          "_",
                          ".png")
        list(src = outfile,
             contentType =  "image/png",
             height = "600px",
             alt =  "this is alternative text")
      }, deleteFile = FALSE)

      ## Raw data
      nm5 <- paste0("www/data/modelled/",
                    data_type,
                    "_reef_",
                    reefs_selector,
                    "_",
                    group_selector,
                    "_",
                    zone_selector,
                    "_",
                    depth_selector,
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
        if(file.exists(nm5)) {
          raw_bits <- data5 |>
            dplyr::select(Sector, Shelf, NRM_region) |>
            mutate(AIMS_REEF_NAME = reefs_selector,
                   GROUP = group_selector,
                   DEPTH = depth_selector,
                   FAMILY = reefs_tab_lookup[[tab_name]]$family) |>
            distinct() 
        }
      }

      ## Annual summaries
      nm <- paste0("www/data/modelled/",
                   data_type,
                   "_reef_",
                   reefs_selector,
                   "_",
                   group_selector,
                   "_",
                   zone_selector,
                   "_",
                   depth_selector,
                   "_",
                   shelf_selector,
                   "__",
                   reefs_tab_lookup[[tab_name]]$family,
                   "_",
                   "year_sum",
                   ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm)) {
          data <- readRDS(file = nm)
          data <- data |>
            mutate(AIMS_REEF_NAME = reefs_selector) |> 
            left_join(raw_bits, by = "AIMS_REEF_NAME")
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
            write_csv(data, file)
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
                    "_reef_",
                    reefs_selector,
                    "_",
                    group_selector,
                    "_",
                    zone_selector,
                    "_",
                    depth_selector,
                    "_",
                    shelf_selector,
                    "__",
                   reefs_tab_lookup[[tab_name]]$family,
                   "_",
                    "year_group_sum",
                    ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm2)) data2 <- readRDS(file = nm2)
        output[[paste0(tab_id, "_annual_group_tbl")]] <- reactable::renderReactable({
          make_table(data2, type = "annual_group")
        })

        if(file.exists(nm2)) {
          data2 <- data2 |>
            mutate(AIMS_REEF_NAME = reefs_selector) |> 
            left_join(raw_bits, by = "AIMS_REEF_NAME")
        }
        output[[paste0(tab_id, "_annual_group_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            paste0(gsub("__.*", "_", basename(nm)), "annual_group_summary.csv")
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
                    "_reef_",
                    reefs_selector,
                    "_",
                    group_selector,
                    "_",
                    zone_selector,
                    "_",
                    depth_selector,
                    "_",
                    shelf_selector,
                    "__",
                   reefs_tab_lookup[[tab_name]]$family,
                   "_",
                    "all_yearcomp_sum",
                    ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm3a)) data3a <- readRDS(file = nm3a)
        output[[paste0(tab_id, "_all_annual_comp_tbl")]] <- reactable::renderReactable({
          make_table(data3a, type = "all_annual_comp")
        })
        if(file.exists(nm3a)) {
          data3a <- data3a |>
            mutate(AIMS_REEF_NAME = reefs_selector) |> 
            left_join(raw_bits, by = "AIMS_REEF_NAME")
        }
        output[[paste0(tab_id, "_all_annual_comp_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            ## paste0("annual_comp.csv")
            paste0(gsub("__.*", "_", basename(nm)), "all_nnual_comp_summary.csv")
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
                    "_reef_",
                    reefs_selector,
                    "_",
                    group_selector,
                    "_",
                    zone_selector,
                    "_",
                    depth_selector,
                    "_",
                    shelf_selector,
                    "__",
                   reefs_tab_lookup[[tab_name]]$family,
                   "_",
                    "yearcomp_sum",
                    ".rds") 
      if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
        if(file.exists(nm3)) data3 <- readRDS(file = nm3)
        output[[paste0(tab_id, "_annual_comp_tbl")]] <- reactable::renderReactable({
          make_table(data3, type = "annual_comp")
        })
        if(file.exists(nm3)) {
          data3 <- data3 |>
            mutate(AIMS_REEF_NAME = reefs_selector) |> 
            left_join(raw_bits, by = "AIMS_REEF_NAME")
        }
        output[[paste0(tab_id, "_annual_comp_download_data")]] <- downloadHandler(
          filename = function() {
            ## Use the selected dataset as the suggested file name
            ## paste0("annual_comp.csv")
            paste0(gsub("__.*", "_", basename(nm)), "annual_comp_summary.csv")
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
                 "_reef_",
                 reefs_selector,
                 "_",
                 group_selector,
                 "_",
                 zone_selector,
                 "_",
                 depth_selector,
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
        if(file.exists(nm4)) {
          data4 <- data4 |>
            mutate(AIMS_REEF_NAME = reefs_selector) |> 
            left_join(raw_bits, by = "AIMS_REEF_NAME")
        }
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
