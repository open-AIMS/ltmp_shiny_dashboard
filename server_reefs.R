

reefs_tab_lookup <- list(
  "Photo-transects" =  list(
    data_type = "photo-transect",
    outputId = "reefs_pt",
    family = "binomial",
    ## zones = c(" "),
    ## depths = c("2", "5", "9"),
    ## shelfs = c(" "),
    ## groups = c("HARD CORAL", "SOFT CORAL",
    ##            "ALGAE", "MACROALGAE")
    response = ""
  ),
  "Manta tow" =  list(
    data_type = "manta",
    outputId = "reefs_manta",
    family = "beta",
    ## shelfs = c("Offshore"),
    ## groups = c("HARD CORAL")
    response = ""
  ),
  "Juveniles" =  list(
    data_type = "juveniles",
    outputId = "reefs_juveniles",
    family = "poisson",
    ## shelfs = c("Inshore", "Offshore"),
    ## groups = c("HARD CORAL")
    response = ""
  ),
  "Fish" =  list(
    data_type = "fish",
    outputId = "reefs_fish",
    family = "poisson",
    ## shelfs = c("Inshore", "Offshore"),
    groups = c("Harvested", "Herbivores", "Coral Trout",
               "Large fishes", "Damselfishes"),
    sub_model = c("restricted", "extended"),
    responses = c("ABUNDANCE", "Biomass")
  )
)

observeEvent(input$run_reef_refresh, {
  tab_name <- input$reefs_panel
  tab_id <- reefs_tab_lookup[[tab_name]]$outputId
  data_types <- reefs_tab_lookup[[tab_name]]$data_type
  current_candidates <- get_candidates(tab_name, data_types, scale = "reef")
  ## add an "all reefs" candidate
  current_candidates <-
    bind_rows(
      current_candidates |>
      slice(1) |> 
      mutate(across(everything(), \(x) NA)) |>
      mutate(domain_name = "All Reefs"),
      current_candidates)
  updateSelectInput(session, paste0(tab_id, "_reefs_selector"),
                    choices = current_candidates |>
                      pull(domain_name) |> unique())
}
)


observeEvent(input$reefs_panel, {     ## when change panels
  tab_name <- input$reefs_panel
  tab_id <- reefs_tab_lookup[[tab_name]]$outputId
  data_type <- reefs_tab_lookup[[tab_name]]$data_type
  current_candidates <- get_candidates(tab_name,
                                       data_type,
                                       scale = "reef",
                                       domain = NULL) 
 ## alert(reefs_tab_lookup[[tab_name]]$data_type)
  write_csv(current_candidates, file = paste0(config_$data_path, "AAAA.csv")) 
  ## Render the content of the panel
  data_type <- reefs_tab_lookup[[tab_name]]$data_type
  ## alert(colnames(current_candidates))
  ## alert(data_type)
  write_csv(current_candidates, file = "../data/tempA.csv")
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
                       choices = ## config_$models |>
                         ## filter(data_scale == "reef",
                         ##        data_type == data_type) |>
                         current_candidates |> 
                         pull(domain_name) |>
                         unique() |>
                         sort(),
                       width = "90%"),
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
        ## if (data_type == "fish") {
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
          ## } else {
          ## column(width = 12,
          ##        shinyjs::disabled(
          ##                   selectInput(paste0(tab_id, "_response_selector"),
          ##                               "Select response:",
          ##                               choices = reefs_tab_lookup[[tab_name]]$responses
          ##                               ## choices = current_candidates |>
          ##                               ##   pull(reef_zone) |> unique()))
          ##                               )
          ##                 ),
          ##        )
          ## }
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
            downloadButton(paste0(tab_id, "_annual_download_data_posteriors"), "Download posteriors as csv"),
            "When downloading 'All Reefs' posteriors, please be patient.  It may take a minute or so to bind all the posteriors from each reef together, before it can begin the download."
            ## actionButton("generate_zip", "Generate ZIP"),
            ##  uiOutput("download_ui")  # Placeholder for the download link

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
            downloadButton(paste0(tab_id, "_all_annual_comp_download_data_posteriors"), "Download posteriors as csv"),
            "When downloading 'All Reefs' posteriors, please be patient.  It may take a minute or so to bind all the posteriors from each reef together, before it can begin the download."
            ),
          tabPanel(
            title = "Comparison to most recent",
            icon = icon("database"),
            reactableOutput(outputId = paste0(tab_id, "_annual_comp_tbl")),
            downloadButton(paste0(tab_id, "_annual_comp_download_data"), "Download as csv"),
            downloadButton(paste0(tab_id, "_annual_comp_download_data_posteriors"), "Download posteriors as csv"),
            "When downloading 'All Reefs' posteriors, please be patient.  It may take a minute or so to bind all the posteriors from each reef together, before it can begin the download."
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
  ## tab_name <- input$reefs_panel
    reefs_selector <- input[[paste0(tab_id, "_reefs_selector")]]
    data_type <- reefs_tab_lookup[[tab_name]]$data_type
    current_candidates <- get_candidates(tab_name, data_type,
                                         scale = "reef",
                                         domain = reefs_selector) 

    zones <-
      current_candidates |> 
      pull(reef_zone) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_zone_selector"),
                      choices = zones)
    depths <-
      current_candidates |> 
      pull(depth) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_depth_selector"),
                      choices = depths)
    groups <-
      current_candidates |> 
      filter(domain_name == reefs_selector) |> 
      pull(group) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_group_selector"),
                      choices = groups, selected = groups[1])

    model_type <-
      current_candidates |> 
      pull(model_type) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_response_selector"),
                      choices = model_type)

    sub_model <-
      current_candidates |> 
      filter(domain_name == reefs_selector) |> 
      pull(sub_model) |>
      unique()
    updateSelectInput(session, paste0(tab_id, "_sub_model_selector"),
                      choices = sub_model,
                      selected = sub_model[1])
  }
  )

  ## observeEvent(input[[paste0(tab_id, "_zone_selector")]], {
  ##   reefs_selector <- input[[paste0(tab_id, "_reefs_selector")]]
  ##   data_type <- reefs_tab_lookup[[tab_name]]$data_type
  ##   depths <- config_$models |>
  ##     filter(data_type == data_type,
  ##            data_scale == "reef",
  ##            domain_name == reefs_selector,
  ##            reef_zone == input[[paste0(tab_id, "_zone_selector")]]
  ##            ) |>
  ##     pull(depth) |>
  ##     unique()
  ##   updateSelectInput(session, paste0(tab_id, "_depth_selector"),
  ##                     choices = depths)
  ## }
  ## )

  if (1 == 1) {
  observeEvent(c(
    input[[paste0(tab_id, "_reefs_selector")]],
    input[[paste0(tab_id, "_shelf_selector")]],
    input[[paste0(tab_id, "_depth_selector")]],
    input[[paste0(tab_id, "_zone_selector")]],
    input[[paste0(tab_id, "_group_selector")]],
    input[[paste0(tab_id, "_response_selector")]],
    input[[paste0(tab_id, "_sub_model_selector")]]
    ), {

      reefs_selector <- input[[paste0(tab_id, "_reefs_selector")]]
      shelf_selector <- " " #input[[paste0(tab_id, "_shelf_selector")]]
      zone_selector <- input[[paste0(tab_id, "_zone_selector")]]
      depth_selector <- input[[paste0(tab_id, "_depth_selector")]]
      group_selector <- input[[paste0(tab_id, "_group_selector")]]
      data_type <- reefs_tab_lookup[[tab_name]]$data_type
      ## if(data_type == "fish") response_selector <- input[[paste0(tab_id, "_response_selector")]]
      response_selector <- input[[paste0(tab_id, "_response_selector")]]
      sub_model_selector <- input[[paste0(tab_id, "_sub_model_selector")]]
      ## alert(sub_model_selector)
      ## alert(sub_model_selector == "")
      ## alert(sub_model_selector == " ")
      ## alert(is.na(sub_model_selector)
      all_reefs <- TRUE
      file_str_fig_path <- "www/figures/"
      file_str_fig_body <- paste0( 
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
        ## ifelse(data_type == "fish", paste0("_", response_selector), ""),
        response_selector,
        "_",
        sub_model_selector)
      ## Raw summary figures
      if (reefs_selector != "All Reefs") {
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
      } else if (all_reefs) {
        output[[paste0(tab_id, '_raw_fig')]] <- NULL
        output[[paste0(tab_id, '_raw_fig_cap')]] <- renderText( {
          "Nothing to display when All Reefs selected"})
        output[[paste0(tab_id, '_raw_group_fig')]] <- NULL 
      }

      ## Partial plots summaries
      if (reefs_selector != "All Reefs") {
        ## alert(file_str_fig_body)
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
      } else if (all_reefs) {
        output[[paste0(tab_id, '_fig')]] <- NULL
        output[[paste0(tab_id, '_group_fig')]] <- NULL
      }

      ## Get the nested model tibble - this must be placed here before others
      if (reefs_selector != "All Reefs") {
        model_file <- paste0("www/data/modelled/",
                             data_type, "_", "reef", "_", reefs_selector, ".rds")
        if (file.exists(model_file)) {
          model_tbl <- readRDS(model_file) |>
            mutate(sub_model = ifelse(is.na(sub_model), " ", sub_model))
            ## mutate(sub_model =  replace_na(" "))
          ## alert(dim(model_tbl))
          ## alert(dim(model_tbl |> filter(sub_model == " ")))
          ## alert(model_tbl |> slice(1) |> as.data.frame())
          family_type <- model_tbl |>
            separate(splits, into = c("reef_zone", "depth", "shelf"), sep = "_") |> 
            filter(VARIABLE == group_selector,
                   reef_zone == zone_selector,
                   depth == depth_selector,
                   model_type == response_selector,
                   sub_model == sub_model_selector,
                   selected) |>
            pull(family_type)
          if (length(family_type>1)) family_type <- family_type[1]  ## this is a temporary measure while transitioning to all fish having sub_model
          ## alert(family_type)
          ## temp <- model_tbl |>
          ##   separate(splits, into = c("reef_zone", "depth", "shelf"), sep = "_") |> 
          ##   filter(VARIABLE == group_selector,
          ##          reef_zone == zone_selector,
          ##          depth == depth_selector,
          ##          model_type == response_selector,
          ##          selected) |>
          ##   pull(reef_zone)
          ## alert(model_file)
          ## ## alert(paste("sub_model = ", sub_model_selector))
          ## alert(paste("family type=", family_type))
          ## ## alert(colnames(model_tbl))
          ## temp <-model_tbl |>
          ##   separate(splits, into = c("reef_zone", "depth", "shelf"), sep = "_") |> 
          ##   filter(VARIABLE == group_selector,
          ##          reef_zone == zone_selector,
          ##          depth == depth_selector,
          ##          model_type == response_selector,
          ##          ## sub_model == sub_model_selector,
          ##          selected) |>
          ##   slice(1) |>
          ##   dplyr::select(family_type, reef_zone, depth, model_type, sub_model) |> 
          ##   as.data.frame()
          ## alert(temp)
          ## ## alert(head(temp))
          ## ## alert(head(model_tbl |> dplyr::select(reef_zone, sub_model)))
        } else {
         family_type <- ""
        }
        
        file_str_data_path <- "www/data/modelled/"
        file_str_data_body <- paste0( 
          data_type,
          "_reef_",
          reefs_selector,
          "_",
          group_selector,
          "_",
          family_type,
          "_",
          zone_selector,
          "_",
          depth_selector,
          "_",
          shelf_selector,
          "_",
          response_selector,
          "_",
          sub_model_selector,
          "_"
        )
          ## "_ _")
        ## Raw data
        nm5 <- paste0(file_str_data_path, file_str_data_body, "raw_data", ".rds")
        ## nm5 <- paste0("www/data/modelled/",
        ##               data_type,
        ##               "_reef_",
        ##               reefs_selector,
        ##               "_",
        ##               group_selector,
        ##               "_",
        ##               family_type,
        ##               "_",
        ##               zone_selector,
        ##               "_",
        ##               depth_selector,
        ##               "_",
        ##               shelf_selector,
        ##               "_",
        ##               response_selector,
        ##               "_ _",
        ##               "raw_data",
        ##               ".rds") 
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
            raw_bits <- data5 |>
              dplyr::select(Sector, Shelf, NRM_region) |>
              mutate(AIMS_REEF_NAME = reefs_selector,
                     GROUP = group_selector,
                     DEPTH = depth_selector,
                     FAMILY = reefs_tab_lookup[[tab_name]]$family) |>
                     ## FAMILY = ifelse(exists(family_type),
                     ##             family_type,
                     ##             reefs_tab_lookup[[tab_name]]$family )) |>
              distinct() 
          }
        }
      } else if (all_reefs) {
        ## alert(colnames(get_candidates(tab_name, data_type, scale = "reef") |> 
        ##   filter(selected_flag == 1)))
        ## write.csv(get_candidates(tab_name, data_type, scale = "reef") |> 
        ##   filter(selected_flag == 1), file = "~/data/AA.csv")
        add_data5 <- get_candidates(tab_name, data_type, scale = "reef") |> 
          filter(selected_flag == 1) |>
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type, data_scale, domain_name,
                                   ## group_selector,
                                   group,
                                   family_type,
                                   reef_zone, depth,
                                   ## zone_selector, depth_selector,
                                   ## shelf_selector, response_selector, " ",
                                   ## shelf_selector, response_selector, sub_model_selector,
                                   shelf, response_selector, sub_model,
                                   "raw_data.rds", sep = "_"))) |>
          mutate(raw = map(.x = nm,
                           .f = ~ {
                             if (file.exists(.x)) {
                               readRDS(.x)
                             } else NULL
                           })) |>
          dplyr::select(nm, raw) |>
          unnest("raw")

        ## write.csv(add_data5, file = "~/data/AB.csv")
        ## alert(colnames(add_data5))
        ## alert(tail(add_data5))
        ## alert(tail(add_data5$nm))
        output[[paste0(tab_id, "_raw_data_tbl")]] <- NULL
        nm_5 <- find_common_pattern(add_data5$nm) 
        ## alert(nm_5)
        ## alert(length(nm_5))
          ## output[[paste0(tab_id, "_raw_data_tbl")]] <- reactable::renderReactable({
          ##   ## reactable(add_data5)
          ##   make_table(add_data5, type = "other")
          ## })
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
      ## alert(file_str_data_path)
      ## alert(file_str_data_body)
      if (reefs_selector != "All Reefs") {
        nm <- paste0(file_str_data_path, file_str_data_body, "year_sum", ".rds")
        ## alert(nm)
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm)) {
            data <- readRDS(file = nm)
            data <- data |>
              mutate(AIMS_REEF_NAME = reefs_selector) |> 
              left_join(raw_bits, by = "AIMS_REEF_NAME")
            ## ## get the model metadata from the models database table
            ## add_data <- get_candidates(tab_name, data_type, scale = "reef") |> 
            ##   filter(selected_flag == 1) |>
            ##   filter(
            ##     domain_name == reefs_selector,
            ##     group == group_selector,
            ##     reef_zone == zone_selector,
            ##     depth == depth_selector,
            ##     shelf == shelf_selector,
            ##     model_type == response_selector,
            ##     sub_model == sub_model_selector)
            ## data <- data |>
            ##   mutate(FAMILY = add_data |>
            ##            pull(family_type))
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
      } else if (all_reefs) {
        add_data <- get_candidates(tab_name, data_type, scale = "reef") |> 
          filter(selected_flag == 1) |>
          ## filter(group == group_selector,
          ##        reef_zone == zone_selector,
          ##        depth == depth_selector,
          ##        shelf == shelf_selector,
          ##        model_type == response_selector,
          ##        sub_model == sub_model_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type, data_scale, domain_name,
                                   group,
                                   family_type, reef_zone, depth,
                                   shelf, model_type, sub_model,
                                   "year_sum.rds", sep = "_"))) |> 
          dplyr::select(data_type, data_scale, domain_name,
                        group, family_type, reef_zone, depth,
                        shelf, model_type, sub_model, nm
                        )
        output[[paste0(tab_id, "_annual_tbl")]] <- NULL
        ## output[[paste0(tab_id, "_annual_tbl")]] <- reactable::renderReactable({
        ##   ## reactable(add_data3)
        ##   make_table(add_data_2 |>
        ##              dplyr::select(-nm), type = "other")
        ## })
        nm_2 <- find_common_pattern(add_data$nm) 

        output[[paste0(tab_id, "_annual_download_data")]] <- downloadHandler(
          filename = function() {
            paste0(gsub("sum.rds", "annual_summary.csv",
                        gsub("__.*", "_", basename(nm_2))))
          },
          content = function(file) {
            add_data_2 <- add_data |> 
              mutate(dat =  map(.x = nm,
                                .f = ~ {
                                  if (!file.exists(.x)) return(NULL)
                                  readRDS(.x)
                                })) |>
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
                                .f = ~ {
                                  if (!file.exists(.x)) return(NULL)
                                  readRDS(.x)
                                  }
                                  )) |>
              unnest(dat)  |>
              dplyr::select(-nm)
            write_csv(add_data_3, file)
          }
        )
      }

      ## Annual group summaries
      if (reefs_selector != "All Reefs") {
        nm2 <- paste0(file_str_data_path, file_str_data_body, "year_group_sum", ".rds")
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm2)) {
            data2 <- readRDS(file = nm2)
            data2 <- data2 |>
              mutate(AIMS_REEF_NAME = reefs_selector) |> 
              left_join(raw_bits |> dplyr::select(-GROUP), by = "AIMS_REEF_NAME")
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
                ## mutate(AIMS_REEF_NAME = reefs_selector,
                ##        DATA_TYPE = data_type,
                ##        )
                write_csv(data, file)
              }
            )
          }
        }
      } else if (all_reefs) {
        output[[paste0(tab_id, "_annual_group_tbl")]] <- NULL 
        add_data2 <- get_candidates(tab_name, data_type, scale = "reef") |> 
          filter(selected_flag == 1) |>
          ## filter(group == group_selector,
          ##        reef_zone == zone_selector,
          ##        depth == depth_selector,
          ##        shelf == shelf_selector,
          ##        model_type == response_selector,
          ##        sub_model == sub_model_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type, data_scale, domain_name,
                                   group,
                                   family_type, reef_zone, depth,
                                   shelf, model_type, sub_model,
                                   "year_group_sum.rds", sep = "_"))) |> 
          dplyr::select(data_type, data_scale, domain_name,
                        group, family_type, reef_zone, depth,
                        shelf, model_type, sub_model, nm
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
      if (reefs_selector != "All Reefs") {
        nm3a <- paste0(file_str_data_path, file_str_data_body, "all_yearcomp_sum", ".rds")
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
      } else if (all_reefs) {
        add_data_3 <- get_candidates(tab_name, data_type, scale = "reef") |> 
          filter(selected_flag == 1) |>
          ## filter(group == group_selector,
          ##        reef_zone == zone_selector,
          ##        depth == depth_selector,
          ##        shelf == shelf_selector,
          ##        model_type == response_selector,
          ##        sub_model == sub_model_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type, data_scale, domain_name,
                                   group,
                                   family_type, reef_zone, depth,
                                   shelf, model_type, sub_model,
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
                                .f = ~ {
                                  if (!file.exists(.x)) return(NULL)
                                  readRDS(.x)
                                  }
                                  )) |>
              unnest(dat)  |>
              dplyr::select(-nm)
            write_csv(add_data_3b, file)
          }
        )
      }


      ## Annual comparison summaries (Comparison to most recent year)
      if (reefs_selector != "All Reefs") {
        nm3 <- paste0(file_str_data_path, file_str_data_body, "yearcomp_sum", ".rds")
        ## nm3 <- paste0("www/data/modelled/",
        ##               data_type,
        ##               "_reef_",
        ##               reefs_selector,
        ##               "_",
        ##               group_selector,
        ##               "_",
        ##               family_type,
        ##               "_",
        ##               zone_selector,
        ##               "_",
        ##               depth_selector,
        ##               "_",
        ##               shelf_selector,
        ##               "_",
        ##               response_selector,
        ##               "_ _",
        ##               "yearcomp_sum",
        ##               ".rds") 
        if (length(input[[paste0(tab_id, "_group_selector")]]) > 0) {
          if(file.exists(nm3)) {
            data3 <- readRDS(file = nm3)
            data3 <- data3 |>
              mutate(AIMS_REEF_NAME = reefs_selector) |> 
              left_join(raw_bits, by = "AIMS_REEF_NAME")
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
      } else if (all_reefs) {
        add_data_3_1 <- get_candidates(tab_name, data_type, scale = "reef") |> 
          filter(selected_flag == 1) |>
          ## filter(group == group_selector,
          ##        reef_zone == zone_selector,
          ##        depth == depth_selector,
          ##        shelf == shelf_selector,
          ##        model_type == response_selector,
          ##        sub_model == sub_model_selector) |> 
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type, data_scale, domain_name,
                                   group,
                                   family_type, reef_zone, depth,
                                   shelf, model_type, sub_model,
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
                                .f = ~ {
                                  if (!file.exists(.x)) return(NULL)
                                  readRDS(.x)
                                  }
                                  )) |>
              unnest(dat)  |>
              dplyr::select(-nm)
            write_csv(add_data_3_1b, file)
          }
        )
      }


      ## Raw aggregations summaries
      if (reefs_selector != "All Reefs") {
        nm4 <- paste0(file_str_data_path, file_str_data_body, "raw_sums", ".rds")
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
              paste0(gsub("raw_sums.rds", "raw_sums.csv",
                          gsub("__.*", "_", basename(nm4))))
            },
            content = function(file) {
              write.csv(data4, file)
            }
          )
        }
      } else if (all_reefs) {
        add_data4 <- get_candidates(tab_name, data_type, scale = "reef") |> 
          filter(selected_flag == 1) |>
          mutate(nm = paste0("www/data/modelled/",
                             paste(data_type, data_scale, domain_name,
                                   group_selector,
                                   family_type, zone_selector, depth_selector,
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
  }
})

download_posterior <- function(button_id, full_name, rds_name, csv_name, dat) {
  output[[button_id]] <- downloadHandler(
    filename = function() {
      ## Use the selected dataset as the suggested file name
      paste0(gsub(rds_name, csv_name,
                  gsub("__.*", "_", basename(full_name))))
    },
    content = function(file) {
      ## Write the dataset to the `file` that will be downloaded
      write_csv(dat, file)
    }
  )
}

download_summary <- function(button_id, full_name, rds_name, csv_name, dat) {
          output[[button_id]] <- downloadHandler(
            filename = function() {
              ## Use the selected dataset as the suggested file name
              ## paste0("annual_comp.csv")
              paste0(gsub("__.*", "_", basename(full_name)), csv_name)
            },
            content = function(file) {
              ## Write the dataset to the `file` that will be downloaded
              write.csv(dat, file)
            }
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

