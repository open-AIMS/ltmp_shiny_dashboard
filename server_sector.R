sector_tab_lookup <- list(
  "Photo-transects" =  list(
    data_type = "photo-transect",
    outputId = "sector_pt",
    family = "binomial",
    ## shelfs = c("Inshore", "Offshore"),
    groups = c("HARD CORAL", "SOFT CORAL",
               "ALGAE", "MACROALGAE"),
    response = ""
  ),
  "Manta tow" =  list(
    data_type = "manta",
    outputId = "sector_manta",
    family = "beta",
    ## shelfs = c("Offshore"),
    groups = c("HARD CORAL"),
    response = ""
  ),
  "Juveniles" =  list(
    data_type = "juveniles",
    outputId = "sector_juveniles",
    family = "binomial",
    ## shelfs = c("Inshore", "Offshore"),
    groups = c("HARD CORAL"),
    response = ""
  ),
  "Fish" =  list(
    data_type = "fish",
    outputId = "sector_fish",
    family = "poisson",
    ## shelfs = c("Inshore", "Offshore"),
    groups = c("Harvested", "Herbivores", "Coral Trout",
               "Large fishes", "Damselfishes"),
    response = ""
  )
)


observeEvent(input$sector_run_reef_refresh, {
  tab_name <- input$sector_panel
  tab_id <- sector_tab_lookup[[tab_name]]$outputId
  ## alert(tab_name)
  data_types <- sector_tab_lookup[[tab_name]]$data_type
  current_candidates <- get_candidates(tab_name, data_types, scale = "sector")
  
  ## add an "all sectors" candidate
  current_candidates <-
    bind_rows(
      current_candidates |>
      slice(1) |> 
      mutate(across(everything(), \(x) NA)) |>
      mutate(domain_name = "All Sectors"),
      current_candidates) 
  updateSelectInput(session, paste0(tab_id, "_sector_selector"),
                    choices = current_candidates |>
                      pull(domain_name) |> unique())
}
)

observeEvent(input$sector_panel, {     ## when change panels
  tab_name <- input$sector_panel
  tab_id <- sector_tab_lookup[[tab_name]]$outputId
  data_types <- reefs_tab_lookup[[tab_name]]$data_type
  current_candidates <- get_candidates(tab_name, data_types, scale = "sector", domain = NULL)

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
                       choices = ## config_$models |>
                         ## filter(data_scale == "reef",
                         ##        data_type == data_type) |>
                         current_candidates |> 
                         pull(domain_name) |>
                         unique() |>
                         sort(),
                       width = "90%"),
                       ## choices = config_$models |>
                       ##   filter(data_scale == "Sectors",
                       ##          data_type == data_type) |>
                       ##   pull(domain_name) |>
                       ##   unique() |>
                       ## sort(), width = "90%"),
           actionButton(inputId = "sector_run_reef_refresh",
                        class = "refresh_button",
                        label = "",
                        icon = icon("rotate-right")),
           ),
           column(width = 12, selectInput(paste0(tab_id, "_group_selector"),
                                          "Select group:",
                                          ## choices = sector_tab_lookup[[tab_name]]$groups)),
                                          choices = current_candidates |>
                                            pull(group) |> unique())),
          column(width = 12,
                            selectInput(paste0(tab_id, "_response_selector"),
                                        "Select response type:",
                                        ## choices = reefs_tab_lookup[[tab_name]]$model_type
                                        choices = current_candidates |>
                                          pull(model_type) |> unique()
                                        )
                          )
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


  observeEvent(c(input[[paste0(tab_id, "_sector_selector")]]), {
    sector_selector <- input[[paste0(tab_id, "_sector_selector")]]
    tab_id <- sector_tab_lookup[[tab_name]]$outputId
    data_types <- sector_tab_lookup[[tab_name]]$data_type
    current_candidates <- get_candidates(tab_name, data_types,
                                         scale = "sector",
                                         domain = sector_selector)
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
    ## groups <- config_$models |>
    ##   filter(data_type == data_types,
    ##          data_scale == "sector",
    ##          domain_name == sector_selector) |>
    ##   pull(group) |>
    ##   unique()
    ## ## alert(groups)
    ## updateSelectInput(session, paste0(tab_id, "_group_selector"),
    ##                   choices = current_candidates |>
    ##                   pull(domain_name) |> unique())
    groups <-
      current_candidates |> 
      pull(group) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_group_selector"),
                      choices = groups)
    model_type <-
      current_candidates |> 
      pull(model_type) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_response_selector"),
                      choices = model_type)
  }
  )

  observeEvent(c(
    input[[paste0(tab_id, "_sector_selector")]],
    ## input[[paste0(tab_id, "_shelf_selector")]],
    input[[paste0(tab_id, "_group_selector")]],
    input[[paste0(tab_id, "_response_selector")]]
  ), {

      sector_selector <- input[[paste0(tab_id, "_sector_selector")]]
      ## shelf_selector <- input[[paste0(tab_id, "_shelf_selector")]]
      group_selector <- input[[paste0(tab_id, "_group_selector")]]
      data_type <- sector_tab_lookup[[tab_name]]$data_type
      response_selector <- input[[paste0(tab_id, "_response_selector")]]

      all_sectors <- TRUE
      file_str_fig_path <- "www/figures/"
      file_str_fig_body <- paste0( 
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
                          response_selector,
                          "_ ")

      ## Raw summary figures
      if (sector_selector != "All Sectors") {
        output[[paste0(tab_id, '_raw_fig')]] <- renderImage({
          list(src = paste0(file_str_fig_path, "gg_raw_sum_", file_str_fig_body, ".png"),
               contentType =  "image/png",
               height = "600px",
               alt =  "this is alternative text")
        }, deleteFile = FALSE)
        output[[paste0(tab_id, '_raw_fig_cap')]] <- renderText({
          paste0(file_str_fig_path, "gg_raw_sum_", file_str_fig_body, ".png")
        })
      } else if (all_sectors) {
        output[[paste0(tab_id, '_raw_fig')]] <- NULL
        output[[paste0(tab_id, '_raw_fig_cap')]] <- renderText( {
          "Nothing to display when All Sectors selected"})
      }

      ## Partial plots summaries
      if (sector_selector != "All Sectors") {
        output[[paste0(tab_id, '_fig')]] <- renderImage({
          list(src = paste0(file_str_fig_path, "gg_", file_str_fig_body, ".png"),
               contentType =  "image/png",
               height = "600px",
               alt =  "this is alternative text")
        }, deleteFile = FALSE)
      } else if (all_sectors) {
        output[[paste0(tab_id, '_fig')]] <- NULL
        output[[paste0(tab_id, '_group_fig')]] <- NULL
      }

      ## Get the nested model tibble - this must be placed here before others
      if (sector_selector != "All Sectors") {
        model_file <- paste0("www/data/modelled/",
                             data_type, "_", "Sectors", "_", sector_selector, ".rds")
        if (file.exists(model_file)) {
          model_tbl <- readRDS(model_file)
          family_type <- model_tbl |>
            filter(VARIABLE == group_selector,
                   model_type == response_selector,
                   selected) |>
            pull(family_type)
        }
        file_str_data_path <- "www/data/modelled/"
        file_str_data_body <- paste0( 
          data_type,
          "_Sectors_",
          sector_selector,
          "_",
          group_selector,
          "_",
          family_type,
          "_",
          " ",  ## ghost zone
          "_",
          " ",  ## ghost depth
          "_",
          " ", #shelf_selector,
          "_",
          response_selector,
          "_ _")
        nm5 <- paste0(file_str_data_path, file_str_data_body, "raw_data", ".rds")
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm5)) data5 <- readRDS(file = nm5)
          output[[paste0(tab_id, "_raw_data_tbl")]] <- reactable::renderReactable({
            make_table(data5, type = "raw_data")
          })
          output[[paste0(tab_id, "_raw_data_download_data")]] <- downloadHandler(
            filename = function() {
              paste0(gsub("raw_data.rds", "raw_data.csv",
                          gsub("__.*", "_", basename(nm5))))
            },
            content = function(file) {
              write.csv(data5, file)
            }
          )
          if(file.exists(nm5)) {
            raw_bits <- data5 |>
              mutate(Sector = sector_selector) |> 
              dplyr::select(Sector) |>
              mutate(GROUP = group_selector,
                     FAMILY = family_type) |>
              distinct() 
          }
        }
      } else if (all_sectors) {
        add_data5 <- get_candidates(tab_name, data_type, scale = "sector")  |> 
          filter(selected_flag == 1) |>
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   ## data_scale,
                                   "Sectors",
                                   domain_name,
                                   group_selector,
                                   family_type, " ", " ",
                                   " ", response_selector, " ",
                                   "raw_data.rds", sep = "_"))) |>
          mutate(raw = map(.x = nm,
                           .f = ~ {
                             if (file.exists(.x)) {
                               readRDS(.x)
                             } else NULL
                           })) |>
          dplyr::select(nm, raw) |>
          unnest("raw")
        output[[paste0(tab_id, "_raw_data_tbl")]] <- NULL
        nm_5 <- find_common_pattern(add_data5$nm) 
        output[[paste0(tab_id, "_raw_data_download_data")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("raw_data.rds", "raw_data.csv",
                        gsub("__.*", "_", basename(nm_5))))
          },
          content = function(file) {
            write.csv(add_data5 |> dplyr::select(-nm), file)
          }
        )
      }

      ## Annual summaries
      if (sector_selector != "All Sectors") {
        nm <- paste0(file_str_data_path, file_str_data_body, "year_sum", ".rds")
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
              paste0(gsub("sum.rds", "annual_summary.csv",
                          gsub("__.*", "_", basename(nm))))
            },
            content = function(file) {
              write.csv(data, file)
            }
          )
          output[[paste0(tab_id, "_annual_download_data_posteriors")]] <- downloadHandler(
            filename = function() {
              paste0(gsub("sum.rds", "annual_posteriors.csv",
                          gsub("__([^_]*)_.*", "__\\1_", basename(nm))))
            },
            content = function(file) {
              data <- readRDS(gsub("sum.rds", "posteriors.rds", nm)) ## |>
              write_csv(data, file)
            }
          )
        }
      } else if (all_sectors) {
        add_data <- get_candidates(tab_name, data_type, scale = "sector") |> 
          filter(selected_flag == 1) |>
          filter(group == group_selector,
                 ## reef_zone == zone_selector,
                 model_type == response_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   ## data_scale,
                                   "Sectors",
                                   domain_name,
                                   group,
                                   family_type, " ", " ",
                                   " ", model_type, " ",
                                   "year_sum.rds", sep = "_"))) |> 
          dplyr::select(data_type, data_scale, domain_name,
                        group, family_type, reef_zone, depth,
                        shelf, model_type, nm
                        )

        output[[paste0(tab_id, "_annual_tbl")]] <- NULL
        nm_2 <- find_common_pattern(add_data$nm) 

        output[[paste0(tab_id, "_annual_download_data")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("sum.rds", "annual_summary.csv",
                        gsub("__.*", "_", basename(nm_2))))
          },
          content = function(file) {
            add_data_2 <- add_data |> 
              mutate(dat =  map(.x = nm, .f = ~readRDS(.x))) |>
              unnest(dat)  
            write_csv(add_data_2, file)
          }
        )
        output[[paste0(tab_id, "_annual_download_data_posteriors")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("sum.rds", "annual_posteriors.csv",
                        gsub("__([^_]*)_.*", "__\\1_", basename(nm_2))))
          },
          content = function(file) {
            add_data_3 <- add_data |> 
              mutate(dat =  map(.x = gsub("sum.rds", "posteriors.rds", nm),
                                .f = ~readRDS(.x))) |>
              unnest(dat)  |>
              dplyr::select(-nm)
            write_csv(add_data_3, file)
          }
        )
      }

      ## Annual group summaries
      if (sector_selector != "All Sectors") {
        nm2 <- paste0(file_str_data_path, file_str_data_body, "year_group_sum", ".rds")
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm2)){
            data2 <- readRDS(file = nm2)
            data2 <- data2 |>
              mutate(Sector = sector_selector) |> 
              left_join(raw_bits |> dplyr::select(-GROUP), by = c("Sector"))
          }
          output[[paste0(tab_id, "_annual_group_tbl")]] <- reactable::renderReactable({
            make_table(data2, type = "annual_group")
          })
          output[[paste0(tab_id, "_annual_group_download_data")]] <- downloadHandler(
            filename = function() {
              paste0(gsub("year_group_sum.rds", "year_group_summary.csv",
                          gsub("__.*", "_", basename(nm2))))
              ## paste0(gsub("__.*", "_", basename(nm2)), "annual_group_summary.csv")
            },
            content = function(file) {
              write.csv(data2, file)
            }
          )
          output[[paste0(tab_id, "_annual_group_download_data_posteriors")]] <- downloadHandler(
            filename = function() {
              paste0(gsub("year_group_sum.rds", "annual_group_posteriors.csv",
                          gsub("__.*", "_", basename(nm2))))
              ## paste0(gsub("__([^_]*)_.*", "__\\1_", basename(nm2)), "annual_group_posteriors.csv")
            },
            content = function(file) {
              data <- readRDS(gsub("sum.rds", "posteriors.rds", nm2)) ## |>
              write_csv(data, file)
            }
          )
        }
      } else if (all_sectors) {
        output[[paste0(tab_id, "_annual_group_tbl")]] <- NULL 
        add_data2 <- get_candidates(tab_name, data_type, scale = "sector") |> 
          filter(selected_flag == 1) |>
          filter(group == group_selector,
                 model_type == response_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   ## data_scale,
                                   "Sectors",
                                   domain_name,
                                   group,
                                   family_type, reef_zone, depth,
                                   shelf, model_type, " ",
                                   "year_group_sum.rds", sep = "_"))) |> 
          dplyr::select(data_type, data_scale, domain_name,
                        group, family_type, reef_zone, depth,
                        shelf, model_type, nm
                        )

        nm2_2 <- find_common_pattern(add_data2$nm) 

        output[[paste0(tab_id, "_annual_group_download_data")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("sum.rds", "annual_group_summary.csv",
                        gsub("__.*", "_", basename(nm2_2))))
          },
          content = function(file) {
            add_data2_2 <- add_data2 |> 
              mutate(dat =  map(.x = nm, .f = ~readRDS(.x))) |>
              unnest(dat)  
            write_csv(add_data2_2 |> dplyr::select(-nm), file)
          }
        )
        output[[paste0(tab_id, "_annual_group_download_data_posteriors")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("sum.rds", "annual_group_posteriors.csv",
                        gsub("__([^_]*)_.*", "__\\1_", basename(nm2_2))))
          },
          content = function(file) {
            add_data2_3 <- add_data2 |> 
              mutate(dat =  map(.x = gsub("sum.rds", "posteriors.rds", nm),
                                .f = ~readRDS(.x))) |>
              unnest(dat)  |>
              dplyr::select(-nm)
            write_csv(add_data2_3, file)
          }
        )
      }

      ## All Annual comparison summaries
      if (sector_selector != "All Sectors") {
        nm3a <- paste0(file_str_data_path, file_str_data_body, "all_yearcomp_sum", ".rds")
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
              paste0(gsub("all_yearcomp_sum.rds", "all_annual_comp_summary.csv",
                          gsub("__.*", "_", basename(nm3a))))
              ## paste0(gsub("__.*", "_", basename(nm3)), "all_annual_comp_summary.csv")
            },
            content = function(file) {
              write.csv(data3a, file)
            }
          )
          output[[paste0(tab_id, "_all_annual_comp_download_data_posteriors")]] <- downloadHandler(
            filename = function() {
              paste0(gsub("all_yearcomp_sum.rds", "all_annual_comp_posteriors.csv",
                          gsub("__([^_]*)_.*", "__\\1_", basename(nm3a))))
              ## paste0(gsub("__([^_]*)_.*", "__\\1_", basename(nm)), "all_annual_comp_posteriors.csv")
            },
            content = function(file) {
              data <- readRDS(gsub("sum.rds", "posteriors.rds", nm3a)) ## |>
              write_csv(data, file)
            }
          )
        }
      } else if (all_sectors) {
        add_data_3 <- get_candidates(tab_name, data_type, scale = "sector") |> 
          filter(selected_flag == 1) |>
          filter(group == group_selector,
                 model_type == response_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   ## data_scale,
                                   "Sectors",
                                   domain_name,
                                   group,
                                   family_type, reef_zone, depth,
                                   shelf, model_type, " ",
                                   "all_yearcomp_sum.rds", sep = "_"))) |> 
          dplyr::select(data_type, data_scale, domain_name,
                        group, family_type, reef_zone, depth,
                        shelf, model_type, nm
                        )
        ## alert(add_data_3$nm)

        output[[paste0(tab_id, "_all_annual_comp_tbl")]] <- NULL
        ## output[[paste0(tab_id, "_all_annual_comp_tbl")]] <- reactable::renderReactable({
        ##   ## reactable(add_data3)
        ##   make_table(add_data_3a |>
        ##              dplyr::select(-nm), type = "other")
        ## })
        nm_3a <- find_common_pattern(add_data_3$nm) 

        output[[paste0(tab_id, "_all_annual_comp_download_data")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("all_yearcomp_sum.rds", "all_annual_yearcomp_summary.csv",
                        gsub("__.*", "_", basename(nm_3a))))
          },
          content = function(file) {
            add_data_3a <- add_data_3 |> 
              mutate(dat =  map(.x = nm, .f = ~ {
                if (!file.exists(.x)) return(NULL)
                readRDS(.x)
              })) |>
              unnest(dat)  
            write_csv(add_data_3a |> dplyr::select(-nm), file)
          }
        )
        output[[paste0(tab_id, "_all_annual_comp_download_data_posteriors")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("all_yearcomp_sum.rds", "all_annual_yearcomp_posteriors.csv",
                        gsub("__([^_]*)_.*", "__\\1_", basename(nm_3a))))
          },
          content = function(file) {
            add_data_3b <- add_data_3 |> 
              mutate(dat =  map(.x = gsub("sum.rds", "posteriors.rds", nm),
                                .f = ~readRDS(.x))) |>
              unnest(dat)  |>
              dplyr::select(-nm)
            write_csv(add_data_3b, file)
          }
        )
      }


      if (sector_selector != "All Sectors") {
        nm3 <- paste0(file_str_data_path, file_str_data_body, "yearcomp_sum", ".rds")
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
                paste0(gsub("yearcomp_sum.rds", "annual_comp_summary.csv",
                            gsub("__.*", "_", basename(nm3))))
              ## paste0(gsub("__.*", "_", basename(nm3)), "annual_comp_summary.csv")
            },
            content = function(file) {
              write.csv(data3, file)
            }
          )
          output[[paste0(tab_id, "_annual_comp_download_data_posteriors")]] <- downloadHandler(
            filename = function() {
              paste0(gsub("yearcomp_sum.rds", "annual_comp_posteriors.csv",
                          gsub("__([^_]*)_.*", "__\\1_", basename(nm3))))
              ## paste0(gsub("__([^_]*)_.*", "__\\1_", basename(nm3)), "annual_comp_posteriors.csv")
            },
            content = function(file) {
              data <- readRDS(gsub("sum.rds", "posteriors.rds", nm3)) ## |>
              write_csv(data, file)
            }
          )
        }
      } else if (all_sectors) {
        add_data_3_1 <- get_candidates(tab_name, data_type, scale = "sector") |> 
          filter(selected_flag == 1) |>
          filter(group == group_selector,
                 model_type == response_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   ## data_scale,
                                   "Sectors",
                                   domain_name,
                                   group,
                                   family_type, reef_zone, depth,
                                   shelf, model_type, " ",
                                   "yearcomp_sum.rds", sep = "_"))) |> 
          dplyr::select(data_type, data_scale, domain_name,
                        group, family_type, reef_zone, depth,
                        shelf, model_type, nm
                        )

        output[[paste0(tab_id, "_annual_comp_tbl")]] <- NULL
        nm_3_1a <- find_common_pattern(add_data_3_1$nm) 

        output[[paste0(tab_id, "_annual_comp_download_data")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("yearcomp_sum.rds", "annual_yearcomp_summary.csv",
                        gsub("__.*", "_", basename(nm_3_1a))))
          },
          content = function(file) {
            add_data_3_1a <- add_data_3_1 |> 
              mutate(dat =  map(.x = nm, .f = ~ {
                if (!file.exists(.x)) return(NULL)
                readRDS(.x)
              })) |>
              unnest(dat)  
            write_csv(add_data_3_1a |> dplyr::select(-nm), file)
          }
        )
        output[[paste0(tab_id, "_annual_comp_download_data_posteriors")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("yearcomp_sum.rds", "annual_yearcomp_posteriors.csv",
                        gsub("__([^_]*)_.*", "__\\1_", basename(nm_3_1a))))
          },
          content = function(file) {
            add_data_3_1b <- add_data_3_1a |> 
              mutate(dat =  map(.x = gsub("sum.rds", "posteriors.rds", nm),
                                .f = ~readRDS(.x))) |>
              unnest(dat)  |>
              dplyr::select(-nm)
            write_csv(add_data_3_1b, file)
          }
        )
      }

      ## Raw summaries
      if (sector_selector != "All Sectors") {
        nm4 <- paste0(file_str_data_path, file_str_data_body, "raw_sums", ".rds")
        
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm4)) data4 <- readRDS(file = nm4)
          output[[paste0(tab_id, "_raw_sum_tbl")]] <- reactable::renderReactable({
            make_table(data4, type = "raw_sum")
          })
          output[[paste0(tab_id, "_raw_sum_download_data")]] <- downloadHandler(
            filename = function() {
              paste0(gsub("raw_sums.rds", "raw_sums.csv",
                          gsub("__.*", "_", basename(nm4))))
              ## paste0("raw_sum.csv")
            },
            content = function(file) {
              write.csv(data4, file)
            }
          )
        }
      } else if (all_sectors) {
        add_data4 <- get_candidates(tab_name, data_type, scale = "sector") |> 
          filter(selected_flag == 1) |>
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   ## data_scale,
                                   "Sectors",
                                   domain_name,
                                   group_selector,
                                   family_type, " ", " ",
                                   " ", response_selector, " ",
                                   "raw_sums.rds", sep = "_"))) |>
          mutate(raw = map(.x = nm,
                           .f = ~ {
                             if (file.exists(.x)) {
                               readRDS(.x)
                             } else NULL
                           })) |>
          dplyr::select(nm, raw) |>
          unnest("raw")
        output[[paste0(tab_id, "_raw_sum_tbl")]] <- NULL
        nm_4 <- find_common_pattern(add_data4$nm) 
        output[[paste0(tab_id, "_raw_sum_download_data")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("raw_sums.rds", "raw_sums.csv",
                        gsub("__.*", "_", basename(nm_4))))
          },
          content = function(file) {
            write.csv(add_data4 |> dplyr::select(-nm), file)
          }
        )
      }
 
        

      if (1 == 2) {
        


      

    nm4 <- paste0("www/data/modelled/",
                 data_type,
                 "_Sectors_",
                 sector_selector,
                 "_",
                 group_selector,
                 "_",
                 family_type,
                 "_",
                 " ",  ## ghost zone
                 "_",
                 " ",  ## ghost depth
                 "_",
                 " ", #shelf_selector,
                 "_",
                 response_selector,
                 "_ _",
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


      }
    })

})


