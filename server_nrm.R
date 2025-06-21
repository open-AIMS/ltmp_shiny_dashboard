
nrm_tab_lookup <- list(
  "Photo-transects" =  list(
    data_type = "photo-transect",
    outputId = "nrm_pt",
    family = "binomial",
    shelfs = c("Inshore", "Offshore"),
    groups = c("HARD CORAL", "SOFT CORAL",
               "ALGAE", "MACROALGAE"),
    response = ""
  ),
  "Manta tow" =  list(
    data_type = "manta",
    outputId = "nrm_manta",
    family = "beta",
    shelfs = c("Offshore"),
    groups = c("HARD CORAL"),
    response = ""
  ),
  "Juveniles" =  list(
    data_type = "juveniles",
    outputId = "nrm_juveniles",
    family = "binomial",
    shelfs = c("Inshore", "Offshore"),
    groups = c("HARD CORAL"),
    response = ""
  ),
  "Fish" =  list(
    data_type = "fish",
    outputId = "nrm_fish",
    family = "fish",
    shelfs = c("Inshore", "Offshore"),
    groups = c("Harvested", "Herbivores", "Coral Trout",
               "Large fishes", "Damselfishes"),
    sub_model = c("restricted", "extended"),
    response = ""
  )
)

observeEvent(input$nrm_run_reef_refresh, {
  tab_name <- input$nrm_panel
  tab_id <- nrm_tab_lookup[[tab_name]]$outputId
  data_types <- nrm_tab_lookup[[tab_name]]$data_type
  current_candidates <- get_candidates(tab_name, data_types, scale = "nrm")
  ## add an "all reefs" candidate
  current_candidates <-
    bind_rows(
      current_candidates |>
      slice(1) |> 
      mutate(across(everything(), \(x) NA)) |>
      mutate(domain_name = "All NRMs"),
      current_candidates)
  updateSelectInput(session, paste0(tab_id, "_nrm_selector"),
                    choices = current_candidates |>
                      pull(domain_name) |> unique())
}
)

observeEvent(input$nrm_panel, {     ## when change panels
  tab_name <- input$nrm_panel
  tab_id <- nrm_tab_lookup[[tab_name]]$outputId
  data_type <- nrm_tab_lookup[[tab_name]]$data_type

  current_candidates <- get_candidates(tab_name,
                                       data_type,
                                       scale = "nrm",
                                       domain = NULL) 
  ## if (is.null(config_$models))
  ##   config_$models <- blank_candidate_models()

  ## current_candidates <- config_$models |>
  ##   filter(data_scale == "nrm",
  ##          data_type == nrm_tab_lookup[[tab_name]]$data_type,
  ##          !is.na(path))
  ##          ## domain_name == input$nrm_selector)
  ## ## Render the content of the panel
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
                       choices = ## config_$models |>
                         ## filter(data_scale == "nrm",
                         ##        data_type == data_type) |>
                         current_candidates |> 
                         pull(domain_name) |>
                         unique() |>
                       sort(), width = "90%"),
           actionButton(inputId = "nrm_run_reef_refresh",
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
          column(width = 12,
                            selectInput(paste0(tab_id, "_response_selector"),
                                        "Select response type:",
                                        ## choices = reefs_tab_lookup[[tab_name]]$model_type
                                        choices = current_candidates |>
                                          pull(model_type) |> unique()
                                        )
                          ),
          column(width = 12,
                            selectInput(paste0(tab_id, "_sub_model_selector"),
                                        "Select sub model:",
                                        ## choices = reefs_tab_lookup[[tab_name]]$model_type
                                        choices = current_candidates |>
                                          pull(sub_model) |> unique()
                                        )
                          )
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
            reactableOutput(outputId = paste0(tab_id, "_all_annual_comp_tbl")),
            downloadButton(paste0(tab_id, "_all_annual_comp_download_data"), "Download as csv"),
            downloadButton(paste0(tab_id, "_all_annual_comp_download_data_posteriors"), "Download posteriors as csv")
            ),
          tabPanel(
            title = "comparison to most recent",
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
    shelfs <- ## config_$models |>
      ## filter(data_type == data_type,
      ##        data_scale == "nrm",
      ##        domain_name == nrm_selector) |>
      current_candidates |> 
      pull(shelf) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_shelf_selector"),
                      choices = shelfs)
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
    sub_model <-
      current_candidates |> 
      filter(domain_name == nrm_selector) |> 
      pull(sub_model) |>
      unique()
    ## alert(sub_model)
    updateSelectInput(session, paste0(tab_id, "_sub_model_selector"),
                      choices = sub_model,
                      selected = sub_model[1])
  }
  )

    
  observeEvent(c(
    input[[paste0(tab_id, "_nrm_selector")]],
    input[[paste0(tab_id, "_shelf_selector")]],
    input[[paste0(tab_id, "_group_selector")]],
    input[[paste0(tab_id, "_response_selector")]],
    input[[paste0(tab_id, "_sub_model_selector")]]
  ), {

      nrm_selector <- input[[paste0(tab_id, "_nrm_selector")]]
      shelf_selector <- input[[paste0(tab_id, "_shelf_selector")]]
      group_selector <- input[[paste0(tab_id, "_group_selector")]]
      data_type <- nrm_tab_lookup[[tab_name]]$data_type
      response_selector <- input[[paste0(tab_id, "_response_selector")]]
      sub_model_selector <- input[[paste0(tab_id, "_sub_model_selector")]]

      all_nrms <- TRUE
      file_str_fig_path <- "www/figures/"
      file_str_fig_body <- paste0( 
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
        response_selector,
        "_",
        sub_model_selector
      )
      ## Raw summary figures
      if (nrm_selector != "All NRMs")  {
        output[[paste0(tab_id, '_raw_fig')]] <- renderImage({
          ## list(src = outfile,
          list(src = paste0(file_str_fig_path, "gg_raw_sum_", file_str_fig_body, ".png"),
               contentType =  "image/png",
               height = "600px",
               alt =  "this is alternative text")
        }, deleteFile = FALSE)
        output[[paste0(tab_id, '_raw_fig_cap')]] <- renderText({
          paste0(file_str_fig_path, "gg_raw_sum_", file_str_fig_body, ".png")
        })
        output[[paste0(tab_id, '_raw_group_fig')]] <- renderImage({
          ## list(src = gsub("gg_raw_sum", "gg_raw_group_sum", outfile),
          list(src = paste0(file_str_fig_path, "gg_raw_group_sum_", file_str_fig_body, ".png"),
               contentType =  "image/png",
               height = "600px",
               alt =  "this is alternative text")
        }, deleteFile = FALSE)
      } else if (all_nrms) {
        output[[paste0(tab_id, '_raw_fig')]] <- NULL
        output[[paste0(tab_id, '_raw_fig_cap')]] <- renderText( {
          "Nothing to display when All NRMs selected"})
        output[[paste0(tab_id, '_raw_group_fig')]] <- NULL 
      }

      ## Partial plots summaries
      if (nrm_selector != "All NRMs") {
        output[[paste0(tab_id, '_fig')]] <- renderImage({
          list(src = paste0(file_str_fig_path, "gg_", file_str_fig_body, ".png"),
               contentType =  "image/png",
               height = "600px",
               alt =  "this is alternative text")
        }, deleteFile = FALSE)
        output[[paste0(tab_id, '_group_fig')]] <- renderImage({
          list(src = paste0(file_str_fig_path, "gg_group_", file_str_fig_body, ".png"),
               contentType =  "image/png",
               height = "600px",
               alt =  "this is alternative text")
        }, deleteFile = FALSE)
      } else if (all_nrms) {
        output[[paste0(tab_id, '_fig')]] <- NULL
        output[[paste0(tab_id, '_group_fig')]] <- NULL
      }

      ## Get the nested model tibble - this must be placed here before others
      if (nrm_selector != "All NRMs") {
        model_file <- paste0("www/data/modelled/",
                             data_type, "_", "nrm", "_", nrm_selector, ".rds")
        if (file.exists(model_file)) {
          model_tbl <- readRDS(model_file) |> 
            mutate(sub_model = ifelse(is.na(sub_model), " ", sub_model))
          family_type <- model_tbl |>
            unnest("splits") |>
            separate(splits, into = c("reef_zone", "depth", "shelf"), sep = "_") |> 
            filter(VARIABLE == group_selector,
                   shelf == shelf_selector,
                   model_type == response_selector,
                   sub_model == sub_model_selector,
                   selected) |>
            pull(family_type)
        }
        file_str_data_path <- "www/data/modelled/"
        file_str_data_body <- paste0( 
          data_type,
          "_nrm_",
          nrm_selector,
          "_",
          group_selector,
          "_",
          family_type,
          "_",
          " ",  ## ghost zone
          "_",
          " ",  ## ghost depth
          "_",
          shelf_selector,
          "_",
          response_selector,
          ## "_ _")
          "_",
          sub_model_selector,
          "_")

        ## Raw data
        nm5 <- paste0(file_str_data_path, file_str_data_body, "raw_data", ".rds")
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm5)) data5 <- readRDS(file = nm5)
          output[[paste0(tab_id, "_raw_data_tbl")]] <- reactable::renderReactable({
            make_table(data5, type = "raw_data")
          })
            output[[paste0(tab_id, "_raw_data_download_data")]] <- downloadHandler(
            filename = function() {
              ## Use the selected dataset as the suggested file name
              paste0(gsub("raw_data.rds", "raw_data.csv",
                          gsub("__.*", "_", basename(nm5))))
              ## paste0("raw_data.csv")
            },
            content = function(file) {
              ## Write the dataset to the `file` that will be downloaded
              write.csv(data5, file)
            }
          )
          if(file.exists(nm5)) {
            ## alert(colnames(data5))
            ## alert(family_type)
            raw_bits <- data5 |>
              dplyr::select(Sector, Shelf, NRM_region) |>
              mutate(AIMS_REEF_NAME = nrm_selector,
                     GROUP = group_selector,
                     SHELF = shelf_selector,
                     ## FAMILY = nrm_tab_lookup[[tab_name]]$family) |>
                     FAMILY = family_type) |>
              distinct() 
          }
        }
      } else if (all_nrms) {
        add_data5 <- get_candidates(tab_name, data_type, scale = "nrm") |>
          filter(selected_flag == 1) |>
          filter(group == group_selector,
                  shelf == shelf_selector,
                 model_type == response_selector
                 ) |> 
        ## write_csv(add_data5, file = "~/data/A.csv")
        ## add_data5 <- add_data5 |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   ## data_scale,
                                   "nrm",
                                   domain_name,
                                   group_selector,
                                   family_type,
                                   " ",
                                   " ",
                                   shelf_selector, response_selector, sub_model_selector,
                                   "raw_data.rds", sep = "_"))) |>
          mutate(raw = map(.x = nm,
                           .f = ~ {
                             if (file.exists(.x)) {
                               readRDS(.x)
                             } else NULL
                           })) |>
          dplyr::select(nm, raw) |>
          unnest("raw") |>
          distinct()
        ## write_csv(add_data5, file = "~/data/B.csv")
        output[[paste0(tab_id, "_raw_data_tbl")]] <- NULL
        nm_5 <- find_common_pattern(add_data5$nm) 
        output[[paste0(tab_id, "_raw_data_download_data")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("raw_data.rds", "raw_data.csv",
                        gsub("__.*", "_", basename(nm_5))))
            ## paste0("raw_data.csv")
          },
          content = function(file) {
            write.csv(add_data5 |> dplyr::select(-nm), file)
          }
        )
      }

      ## Annual summaries
      if (nrm_selector != "All NRMs") {
        nm <- paste0(file_str_data_path, file_str_data_body, "year_sum", ".rds")
        ## alert(nm)
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm)) {
            data <- readRDS(file = nm)
            data <- data |>
              mutate(NRM_region = nrm_selector) |> 
              left_join(raw_bits, by = "NRM_region")
          }

          output[[paste0(tab_id, "_annual_tbl")]] <- reactable::renderReactable({
            make_table(data, type = "annual")
          })
          
          output[[paste0(tab_id, "_annual_download_data")]] <- downloadHandler(
            filename = function() {
              ## Use the selected dataset as the suggested file name
              paste0(gsub("sum.rds", "annual_summary.csv",
                          gsub("__.*", "_", basename(nm))))
            },
            content = function(file) {
              ## Write the dataset to the `file` that will be downloaded
              write_csv(data, file)
            }
          )
          output[[paste0(tab_id, "_annual_download_data_posteriors")]] <- downloadHandler(
            filename = function() {
              ## Use the selected dataset as the suggested file name
              paste0(gsub("sum.rds", "annual_posteriors.csv",
                          gsub("__([^_]*)_.*", "__\\1_", basename(nm))))
            },
            content = function(file) {
              ## Write the dataset to the `file` that will be downloaded
              data <- readRDS(gsub("sum.rds", "posteriors.rds", nm)) ## |>
              write_csv(data, file)
            }
          )
        }
      } else if (all_nrms) {
        add_data <- get_candidates(tab_name, data_type, scale = "nrm") |> 
          filter(selected_flag == 1) |>
          filter(group == group_selector,
                 ## reef_zone == zone_selector,
                 ## depth == depth_selector,
                 shelf == shelf_selector,
                 model_type == response_selector,
                 sub_model == sub_model_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   ## data_scale,
                                   "nrm",
                                   domain_name,
                                   group,
                                   family_type,
                                   " ", ## reef_zone,
                                   " ", ## depth,
                                   shelf,
                                   model_type, sub_model,
                                   "year_sum.rds", sep = "_"))) |> 
          dplyr::select(data_type, data_scale, domain_name,
                        group, family_type, reef_zone, depth,
                        shelf, model_type, sub_model, nm
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
      if (nrm_selector != "All NRMs") {
        nm2 <- paste0(file_str_data_path, file_str_data_body, "year_group_sum", ".rds")
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm2)) {
            data2 <- readRDS(file = nm2)
            data2 <- data2 |>
              mutate(NRM_region = nrm_selector) |> 
              left_join(raw_bits |> dplyr::select(-GROUP), by = "NRM_region")
            ## output[[paste0(tab_id, "_annual_group_tbl")]] <- reactable::renderReactable({
            ##   make_table(data2, type = "annual_group")
            ## })
            output[[paste0(tab_id, "_annual_group_tbl")]] <- NULL

            ## output[[paste0(tab_id, "_annual_group_download_data")]] <- downloadHandler(
            ##   filename = function() {
            ##   paste0(gsub("year_group_sum.rds", "year_group_summary.csv",
            ##               gsub("__.*", "_", basename(nm2))))
            ##   },
            ##   content = function(file) {
            ##     write.csv(data2, file)
            ##   }
            ## )
            ## output[[paste0(tab_id, "_annual_group_download_data_posteriors")]] <- downloadHandler(
            ##   filename = function() {
            ##   paste0(gsub("year_group_sum.rds", "annual_group_posteriors.csv",
            ##               gsub("__.*", "_", basename(nm2))))
            ##   },
            ##   content = function(file) {
            ##     data <- readRDS(gsub("sum.rds", "posteriors.rds", nm2)) ## |>
            ##     write_csv(data, file)
            ##   }
            ## )
          }
        }
      }

      ## All Annual comparison summaries
      if (nrm_selector != "All NRMs") {
        nm3a <- paste0(file_str_data_path, file_str_data_body, "all_yearcomp_sum", ".rds")
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm3a)) {
            data3a <- readRDS(file = nm3a)
            data3a <- data3a |>
              mutate(NRM_region = nrm_selector) |> 
              left_join(raw_bits, by = "NRM_region")
            output[[paste0(tab_id, "_all_annual_comp_tbl")]] <- reactable::renderReactable({
              make_table(data3a, type = "all_annual_comp")
            })
            output[[paste0(tab_id, "_all_annual_comp_download_data")]] <- downloadHandler(
              filename = function() {
                paste0(gsub("all_yearcomp_sum.rds", "all_annual_comp_summary.csv",
                            gsub("__.*", "_", basename(nm3a))))
              },
              content = function(file) {
                write.csv(data3a, file)
              }
            )
            output[[paste0(tab_id, "_all_annual_comp_download_data_posteriors")]] <- downloadHandler(
              filename = function() {
                paste0(gsub("all_yearcomp_sum.rds", "all_annual_comp_posteriors.csv",
                            gsub("__([^_]*)_.*", "__\\1_", basename(nm3a))))
              },
              content = function(file) {
                data <- readRDS(gsub("sum.rds", "posteriors.rds", nm3a)) ## |>
                write_csv(data, file)
              }
            )
          }
        }
      } else if (all_nrms) {
        add_data_3 <- get_candidates(tab_name, data_type, scale = "nrm") |> 
          filter(selected_flag == 1) |>
          filter(group == group_selector,
                 ## reef_zone == zone_selector,
                 ## depth == depth_selector,
                 shelf == shelf_selector,
                 model_type == response_selector,
                 sub_model == sub_model_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   ## data_scale,
                                   "nrm",
                                   domain_name,
                                   group,
                                   family_type,
                                   " ", #reef_zone,
                                   " ", #depth,
                                   shelf, model_type, sub_model,
                                   "all_yearcomp_sum.rds", sep = "_"))) |> 
          dplyr::select(data_type, data_scale, domain_name,
                        group, family_type, reef_zone, depth,
                        shelf, model_type, sub_model, nm
                        )
        alert(add_data_3$nm)

        output[[paste0(tab_id, "_all_annual_comp_tbl")]] <- NULL
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

      ## Annual comparison summaries (Comparison to most recent year)
      if (nrm_selector != "All NRMs") {
        nm3 <- paste0(file_str_data_path, file_str_data_body, "yearcomp_sum", ".rds")
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm3)) {
            data3 <- readRDS(file = nm3)
            data3 <- data3 |>
              mutate(NRM_region = nrm_selector) |> 
              left_join(raw_bits, by = "NRM_region")
            output[[paste0(tab_id, "_annual_comp_tbl")]] <- reactable::renderReactable({
              make_table(data3, type = "annual_comp")
            })
            output[[paste0(tab_id, "_annual_comp_download_data")]] <- downloadHandler(
              filename = function() {
                paste0(gsub("yearcomp_sum.rds", "annual_comp_summary.csv",
                            gsub("__.*", "_", basename(nm3))))
              },
              content = function(file) {
                write.csv(data3, file)
              }
            )
            output[[paste0(tab_id, "_annual_comp_download_data_posteriors")]] <- downloadHandler(
              filename = function() {
                paste0(gsub("yearcomp_sum.rds", "annual_comp_posteriors.csv",
                            gsub("__([^_]*)_.*", "__\\1_", basename(nm3))))
              },
              content = function(file) {
                data <- readRDS(gsub("sum.rds", "posteriors.rds", nm3)) ## |>
                write_csv(data, file)
              }
            )
          }
        }
      } else if (all_nrms) {
        add_data_3_1 <- get_candidates(tab_name, data_type, scale = "nrm") |> 
          filter(selected_flag == 1) |>
          filter(group == group_selector,
                 ## reef_zone == zone_selector,
                 ## depth == depth_selector,
                 shelf == shelf_selector,
                 model_type == response_selector,
                 sub_model == sub_model_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   "nrm", #data_scale,
                                   domain_name,
                                   group,
                                   family_type,
                                   " ", #reef_zone,
                                   " ", #depth,
                                   shelf,
                                   model_type, sub_model,
                                   "yearcomp_sum.rds", sep = "_"))) |> 
          dplyr::select(data_type, data_scale, domain_name,
                        group, family_type, reef_zone, depth,
                        shelf, model_type, sub_model, nm
                        )
        add_data_3_1a <- add_data_3_1 |> 
          mutate(dat =  map(.x = nm, .f = ~ {
            if (!file.exists(.x)) return(NULL)
            readRDS(.x)
          })) |>
          unnest(dat)  

        output[[paste0(tab_id, "_annual_comp_tbl")]] <- NULL
        nm_3_1a <- find_common_pattern(add_data_3_1a$nm) 

        output[[paste0(tab_id, "_annual_comp_download_data")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("yearcomp_sum.rds", "annual_yearcomp_summary.csv",
                        gsub("__.*", "_", basename(nm_3_1a))))
          },
          content = function(file) {
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

      ## Raw aggregations summaries
      if (nrm_selector != "All NRMs") {
        nm4 <- paste0(file_str_data_path, file_str_data_body, "raw_sums", ".rds")
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm4)) data4 <- readRDS(file = nm4)
          output[[paste0(tab_id, "_raw_sum_tbl")]] <- reactable::renderReactable({
            make_table(data4, type = "raw_sum")
          })
          if(file.exists(nm4)) {
            data4 <- data4 |>
              mutate(NRM_region = nrm_selector) |> 
              left_join(raw_bits, by = "NRM_region")
          }
          output[[paste0(tab_id, "_raw_sum_download_data")]] <- downloadHandler(
            filename = function() {
              paste0(gsub("raw_sums.rds", "raw_sums.csv",
                          gsub("__.*", "_", basename(nm4))))
            },
            content = function(file) {
              write.csv(data4, file)
            }
          )
        }
      } else if (all_nrms) {
        add_data4 <- get_candidates(tab_name, data_type, scale = "nrm") |> 
          filter(selected_flag == 1) |>
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type,
                                   "nrm", #data_scale,
                                   domain_name,
                                   group_selector,
                                   family_type,
                                   " ", #zone_selector,
                                   " ", #depth_selector,
                                   shelf_selector, response_selector, sub_model_selector,
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

find_common_pattern <- function(strings) {
  # Split each string into parts based on "_"
  split_strings <- strsplit(strings, "_", fixed = TRUE)
  # Find the length of the shortest split (to avoid index errors)
  min_length <- min(sapply(split_strings, length))
  
  # Compare elements at each position
  result <- sapply(seq_len(min_length), function(i) {
    elements <- sapply(split_strings, `[`, i)
    if (all(elements == elements[1])) {
      return(elements[1])  # Keep common elements
    } else {
      return(" ")  # Replace differing elements with "*"
    }
  })
  
  # Collapse into a single string
  paste(result, collapse = "_")
}
