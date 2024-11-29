## Extract data summary table: 
##data/pt_sum.csv (created by make_dashboard_table, get_data_summary)
## config_$models (the models that have been fit)

dashboard_tab_lookup <- list(
  "Photo-transects" =  list(
    data_type = "photo-transect",
    outputId = "dashboard_pt",
    sql = "pt.sql",
    "Extract data" = list(
      routine = "sql",
      outputId = "sql",
      default_tbl = "summary"
    ),
    "Prepare Sectors" = list(
      routine = "sector",
      outputId = "sector",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare NRMs" = list(
      routine = "nrm",
      outputId = "nrm",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare Reefs" = list(
      routine = "reef",
      outputId = "reef",
      default_tbl = "summary",
      choices = NULL
    )
  ),
  "Manta tow" =  list(
    data_type = "manta",
    outputId = "dashboard_manta",
    sql = "manta.sql",
    "Extract data" = list(
      routine = "sql",
      outputId = "sql",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare Sectors" = list(
      routine = "sector",
      outputId = "sector",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare NRMs" = list(
      routine = "nrm",
      outputId = "nrm",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare Reefs" = list(
      routine = "reef",
      outputId = "reef",
      default_tbl = "summary",
      choices = NULL
    )
  ),
  "Juveniles" =  list(
    data_type = "juveniles",
    outputId = "dashboard_juveniles",
    sql = "juv.sql",
    "Extract data" = list(
      routine = "sql",
      outputId = "sql",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare Sectors" = list(
      routine = "sector",
      outputId = "sector",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare NRMs" = list(
      routine = "nrm",
      outputId = "nrm",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare Reefs" = list(
      routine = "reef",
      outputId = "reef",
      default_tbl = "summary",
      choices = NULL
    )
  ),
  "Fish" =  list(
    data_type = "fish",
    outputId = "dashboard_fish",
    sql = "fish.sql",
    "Extract data" = list(
      routine = "sql",
      outputId = "sql",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare Sectors" = list(
      routine = "sector",
      outputId = "sector",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare NRMs" = list(
      routine = "nrm",
      outputId = "nrm",
      default_tbl = "summary",
      choices = NULL
    ),
    "Prepare Reefs" = list(
      routine = "reef",
      outputId = "reef",
      default_tbl = "summary",
      choices = NULL
    )
  )
)


## Triggered events ==================================================

## Change in data_type tabs (Photo-transects, Manta tow, Juveniles,
observeEvent(input$dashboard_panel, {     ## when change panels
  tab_name <- input$dashboard_panel
  lookup <- dashboard_tab_lookup[[tab_name]]
  tab_id <- lookup$outputId
  output[[paste0(tab_id, "_panel")]] <- renderUI({
    fluidRow( 
      column(width = 12,
             ## div(
             ##   id = "overlay",
             ##   style = "position: fixed;
             ## top: 0; left: 0; width: 100%; height: 100%;
             ## background-color: rgba(0, 0, 0, 0.7); 
             ## color: white; text-align: center; font-size: 24px;
             ## display: none; z-index:9999;",
             ## div(style = "position: relative; top: 50%; transform: translateY(-50%);", 
             ##     icon("spinner", class = "fa-spin fa-3x"), 
             ##     br(), 
             ##     "Processing, please wait..."
             ##     )
             ## ),
             div(
               div(
                 tabsetPanel(
                   id = "dashboard_sql_panel",
                   tabPanel(
                     title = "Extract data",
                     column(width = 7,
                            "The following table displays the first few rows of the extracted data", br(),
                            box(width = 12,
                                title = span("Current data", 
                                radioButtons("sql_tbl_choice", label = NULL,inline = TRUE,
                                             choiceNames = list("data", "summary"),
                                             choiceValues = list("data", "summary"),
                                             selected = "summary"
                                             ),
                                actionButton(inputId = "refresh_summary_tbl",
                                             label = "",
                                             icon = icon("rotate-right")),
                                downloadButton("download_raw_data", "Download data as csv",
                                               style = "margin-left:auto; padding-top:0px; padding-bottom:0px; position:absolute;right:40px;",
                                               class = "btn-enabled"),
                                ),
                                solidHeader = TRUE,
                                status = "info",
                                collapsible = TRUE,
                                reactableOutput(outputId = paste0(tab_id, "_sql_tbl")),
                                ),
                            ),
                     column(width = 5,
                            "Step 1: Run sql to extract data",
                            br(),
                            box(
                              title =
                                    span(
                                      icon("info", style = "margin-right: 10px;"),
                                      paste0("SQL (~/dashboard/data/", lookup$sql, ")"),
                                      actionButton("run_sql", "Run",
                                                   style = "margin-left:auto; padding-top:0px; padding-bottom:0px; position:absolute;right:10px;",
                                                   icon =  icon("play"))
                                    ),
                              width = 12,
                              solidHeader = TRUE,
                              status = "info",
                              style = "font-family:monospace;",
                              htmltools::includeMarkdown(paste0("data/", lookup$sql)),
                              ),
                            ## tags$a(href="data/pt1.csv", "Download CSV", download=NA),
                            br(),
                            "Step 2: Run post-export processing",
                            br(),
                            box(
                              title =
                                    span(
                                      icon("info", style = "margin-right: 10px;"),
                                      "Post-export R script",
                                      actionButton("run_process", "Run",
                                                   style = "margin-left:auto; padding-top:0px; padding-bottom:0px; position:absolute;right:10px;",
                                                   class = "btn-enabled"),
                                    ),
                              width = 12,
                              solidHeader = TRUE,
                              status = "info",
                              "This step will run an external R script that will perform a small amount of post-processing to ensure that the extracted data is compatible with the format and structure of the extracts produced by the production dashboard.  I should probably just wrap this step up into the back end of the sql extraction since without it, the rest of the processing cannot take place.",
                              ## htmltools::includeMarkdown(paste0("data/", lookup$sql)),
                              ),
                            ),
                     ),
                   tabPanel(
                     title = "Prepare Sectors",
                     column(width = 7,
                            box(width = 12,
                                title = span("Current data", 
                                radioButtons("sector_tbl_choice", label = NULL,inline = TRUE,
                                             choiceNames = list("summary"),
                                             choiceValues = list("summary"),
                                             selected = "summary"
                                             ),
                                actionButton(inputId = "refresh_summary_tbl",
                                             label = "",
                                             icon = icon("rotate-right")),
                                             ),
                                solidHeader = TRUE,
                                status = "info",
                                collapsible = TRUE,
                                reactableOutput(outputId = paste0(tab_id, "_sector_tbl")),
                                ),
                            ),
                     column(width = 5,
                            ## "Step 1: Prepare all reefs for modelling",
                            ## br(),
                            box(
                              title =
                                    span(
                                      icon("info", style = "margin-right: 10px;"),
                                      "Prepare sectors for modelling",
                                      actionButton("run_prepare_sector", "Run",
                                                   style = "margin-left:auto; padding-top:0px; padding-bottom:0px; position:absolute;right:10px;",
                                                   icon =  icon("play"))
                                    ),
                              width = 12,
                              solidHeader = TRUE,
                              status = "info",
                              "This will place each Sector in its on folder structure that mimics that employed by the production version of the dashboard"
                              ## htmltools::includeMarkdown(paste0("data/", lookup$sql)),
                              ),
                            box(
                              title =
                                    span(
                                      icon("info", style = "margin-right: 10px;"),
                                      "Fit sector models",
                                      actionButton("run_fit_sector", "Run",
                                                   style = "margin-left:auto; padding-top:0px; padding-bottom:0px; position:absolute;right:10px;",
                                                   icon =  icon("play"))
                                    ),
                              width = 12,
                              solidHeader = TRUE,
                              status = "info",
                              "This will run the analysis scripts for each individual Sector.",
                              br(),
                                selectInput(paste0(lookup$data_type, "_sector_model_choice"), label = "Sectors to model",
                                             selected = "all",
                                            multiple = TRUE,
                                             choices = "all"
                                             ),
                              ## htmltools::includeMarkdown(paste0("data/", lookup$sql)),
                              ),
                            ),
                   ),
                   tabPanel(
                     title = "Prepare NRMs",
                     column(width = 7,
                            box(width = 12,
                                title = span("Current data", 
                                radioButtons("nrm_tbl_choice", label = NULL,inline = TRUE,
                                             choiceNames = list("summary"),
                                             choiceValues = list("summary"),
                                             selected = "summary"
                                             ),
                                actionButton(inputId = "refresh_summary_tbl",
                                             label = "",
                                             icon = icon("rotate-right")),
                                             ),
                                solidHeader = TRUE,
                                status = "info",
                                collapsible = TRUE,
                                reactableOutput(outputId = paste0(tab_id, "_nrm_tbl")),
                                ),
                            ),
                     column(width = 5,
                            ## "Step 1: Prepare all reefs for modelling",
                            ## br(),
                            box(
                              title =
                                    span(
                                      icon("info", style = "margin-right: 10px;"),
                                      "Prepare NRMs for modelling",
                                      actionButton("run_prepare_nrm", "Run",
                                                   style = "margin-left:auto; padding-top:0px; padding-bottom:0px; position:absolute;right:10px;",
                                                   icon =  icon("play"))
                                    ),
                              width = 12,
                              solidHeader = TRUE,
                              status = "info",
                              "This will place each NRM in its on folder structure that mimics that employed by the production version of the dashboard"
                              ## htmltools::includeMarkdown(paste0("data/", lookup$sql)),
                              ),
                            box(
                              title =
                                    span(
                                      icon("info", style = "margin-right: 10px;"),
                                      "Fit nrm models",
                                      actionButton("run_fit_nrm", "Run",
                                                   style = "margin-left:auto; padding-top:0px; padding-bottom:0px; position:absolute;right:10px;",
                                                   icon =  icon("play"))
                                    ),
                              width = 12,
                              solidHeader = TRUE,
                              status = "info",
                              "This will run the analysis scripts for each individual NRM.",
                              br(),
                                selectInput(paste0(lookup$data_type, "_nrm_model_choice"), label = "NRMs to model",
                                             selected = "all",
                                            multiple = TRUE,
                                             choices = "all"
                                             ),
                              ## htmltools::includeMarkdown(paste0("data/", lookup$sql)),
                              ),
                            ),
                   ),
                   tabPanel(
                     title = "Prepare Reefs",
                     column(width = 7,
                            box(width = 12,
                                title = span("Current data", 
                                radioButtons("reef_tbl_choice", label = NULL,inline = TRUE,
                                             choiceNames = list("summary"),
                                             choiceValues = list("summary"),
                                             selected = "summary"
                                             ),
                                actionButton(inputId = "refresh_summary_tbl",
                                             label = "",
                                             icon = icon("rotate-right")),
                                             ),
                                solidHeader = TRUE,
                                status = "info",
                                collapsible = TRUE,
                                reactableOutput(outputId = paste0(tab_id, "_reef_tbl")),
                                ),
                            ),
                     column(width = 5,
                            ## "Step 1: Prepare all reefs for modelling",
                            ## br(),
                            box(
                              title =
                                    span(
                                      icon("info", style = "margin-right: 10px;"),
                                      "Prepare reefs for modelling",
                                      actionButton("run_prepare_reefs", "Run",
                                                   style = "margin-left:auto; padding-top:0px; padding-bottom:0px; position:absolute;right:10px;",
                                                   icon =  icon("play"))
                                    ),
                              width = 12,
                              solidHeader = TRUE,
                              status = "info",
                              "This will place each reef in its on folder structure that mimics that employed by the production version of the dashboard"
                              ## htmltools::includeMarkdown(paste0("data/", lookup$sql)),
                              ),
                            box(
                              title =
                                    span(
                                      icon("info", style = "margin-right: 10px;"),
                                      "Fit reef models",
                                      actionButton("run_fit_reefs", "Run",
                                                   style = "margin-left:auto; padding-top:0px; padding-bottom:0px; position:absolute;right:10px;",
                                                   icon =  icon("play"))
                                    ),
                              width = 12,
                              solidHeader = TRUE,
                              status = "info",
                              "This will run the analysis scripts for each individual reef.",
                              br(),
                                selectInput(paste0(lookup$data_type, "_reef_model_choice"), label = "Reefs to model",
                                             selected = "all",
                                            multiple = TRUE,
                                             choices = "all"
                                             ),
                              ## htmltools::includeMarkdown(paste0("data/", lookup$sql)),
                              ),
                            ),
                   ),
                   ),
                 ),
               div(id = paste0(lookup$data_type, "overlay_div"),
                 style = "margin-top: auto; margin-bottom:0px;",
                 box(
                   title = span(icon("info", style = "margin-right: 10px; text-align:left;"),
                                "Status",
                                div(id = "wait_message",
                                    icon("spinner", class = "fa-spin"),
                                    "Processing, please wait..."
                                )),
                   width = 7,
                   solidHeader = TRUE,
                   status = "info",
                   verbatimTextOutput("log_out")  ## note, this has a style defined in styles.R
                 )
               )
             )
             )
    )
  })
  shinyjs::disable("run_process")

  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  ## tbl <- get_table_data("summary", data_meta)
  ## tbl <- get_db_table_data(input$sql_tbl_choice, data_meta)
  ## alert(tbl)
  ## if (!is.null(tbl)) {
  ##   make_dashboard_table(tbl, ids) 
  ## }

})

## Change in routines tabs (Extract data, Prepare Sectors, Prepare NRMs, Prepare Reefs)
observeEvent(c(input$dashboard_sql_panel), {  ## when change sub panels
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  ## build_summary_data(data_meta)
  ## db <- readRDS(config_$db_file)
  ## rds_file <- gsub(".csv", ".rds", db[[data_meta$data_type]]$summary_file)
  ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
  ## if (!is.null(tbl)) {
  ##   make_dashboard_table(tbl, ids) 
  ## }
      tbl <- get_db_table_data(input$sql_tbl_choice, data_meta)

      if (!is.null(tbl)) {
        make_dashboard_table(tbl, ids) 
      }

  if (1 == 1) {
    
    if (data_meta$data_scale == "reef") {
      candidates <- get_candidates_to_select_from(data_meta, "AIMS_REEF_NAME")
      ## if (file.exists(rds_file)) {
        ## data <- readRDS(file = rds_file) 
        ## dashboard_tab_lookup[[ids$tab_name]][[ids$sub_tab_name]]$choices <-
        ##   sort(unique(data$reef))
        ## assign("dashboard_tab_lookup", dashboard_tab_lookup, envir = .GlobalEnv)
        updateSelectInput(inputId = paste0(data_meta$data_type, "_reef_model_choice"),
                          ## choices = c("all", sort(unique(data$reef)) 
                          choices = c("all", sort(unique(candidates)) 
                                      ))
      ## }
    }
    if (data_meta$data_scale == "nrm") {
      candidates <- get_candidates_to_select_from(data_meta, "NRM_REGION")
      ## if (file.exists(rds_file)) {
        ## data <- readRDS(file = rds_file) 
        ## dashboard_tab_lookup[[ids$tab_name]][[ids$sub_tab_name]]$choices <-
        ##   sort(unique(data$nrm))
        ## assign("dashboard_tab_lookup", dashboard_tab_lookup, envir = .GlobalEnv)
        updateSelectInput(inputId = paste0(data_meta$data_type, "_nrm_model_choice"),
                          ## choices = c("all", sort(unique(data$nrm))
                          choices = c("all", sort(unique(candidates))
                                      ))
      ## }
    }
    if (data_meta$data_scale == "sector") {
      candidates <- get_candidates_to_select_from(data_meta, "A_SECTOR")
      ## if (file.exists(rds_file)) {
        ## data <- readRDS(file = rds_file) 
        ## dashboard_tab_lookup[[ids$tab_name]][[ids$sub_tab_name]]$choices <-
        ##   sort(unique(data$sector))
        ## assign("dashboard_tab_lookup", dashboard_tab_lookup, envir = .GlobalEnv)
        updateSelectInput(inputId = paste0(data_meta$data_type, "_sector_model_choice"),
                          choices = c("all", sort(unique(candidates))
                                      ))
      ## }
    }
  }
  tbl <- get_db_table_data(input$sql_tbl_choice, data_meta)
  if (!is.null(tbl)) {
    make_dashboard_table(tbl, ids) 
  }
}
)

## Refresh table btn
observeEvent(c(input$refresh_summary_tbl), {
  if (input$refresh_summary_tbl == 0) return() ## dont run this on initialisaton
  ## tab_name <- input$dashboard_panel
  ## lookup <- dashboard_tab_lookup[[tab_name]]
  ## tab_id <- lookup$outputId
  ## sub_tab_name <- input$dashboard_sql_panel
  ## sub_tab_id <- lookup[[sub_tab_name]]$outputId
  ## sql_file <- lookup$sql
  ## csv_file <- paste0(config_$data_path, str_replace(lookup$sql, ".sql", ".csv"))
  ## ## alert(csv_file)
  ## get_data_summary(csv_file)
  ## alert("after here?")
  ## make_dashboard_table(input[[paste0(sub_tab_id, "_tbl_choice")]],
  ##                      csv_file, tab_id, sub_tab_id) 
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  

  tbl <- get_db_table_data(input$sql_tbl_choice, data_meta)
  if (!is.null(tbl)) {
    make_dashboard_table(tbl, ids) 
  }
  ## ## update_summary_data(data_meta)
  ## db <- readRDS(config_$db_file)
  ## csv_file <- db[[data_meta$data_type]]$data_file
  ## get_data_summary(data_meta, csv_file, db[[data_meta$data_type]]$summary_file)

  ## summary_file <- db[[data_meta$data_type]]$summary_file
  ## alert(summary_file)
  ## join_extracted_summary_to_model_meta(summary_file, data_meta)
  ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
  ## if (!is.null(tbl)) {
  ##   make_dashboard_table(tbl, ids) 
  ## }
},
ignoreInit = TRUE
)

## Extract data panel ------------------------------------------------

## Change in type of data to present in Extract data table
observeEvent(c(input$sql_tbl_choice), {
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
  tbl <- get_db_table_data(input$sql_tbl_choice, data_meta)
  if (!is.null(tbl)) {
    make_dashboard_table(tbl, ids) 
  }
}
)

## Run the SQL querie
observeEvent(input$run_sql, {
  ## get the id's of the data_type and routine tabs
  ## list2env(get_dashboard_tab_ids(), env = .GlobalEnv)  
  ## ## get the names of various files that can be derived from the sql file name
  ## list2env(get_dashboard_file_names(lookup), env = .GlobalEnv)  
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  ## prepare the ui for an external process
  ## alert("here")
  cat(paste("\nExtracting ",ids$tab_name, " from Oracle\n======================================\n"),
      file = config_$dashboard_log, append = TRUE)
  process_ui_start(data_meta, id = "run_sql")

  ## file.copy(from = config_$dashboard_log,
  ##           to = gsub(".log", ".old", config_$dashboard_log),
  ##           overwrite = TRUE)
  db <- readRDS(config_$db_file)
  ## alert(ids)
  sql_file <- ids$lookup$sql
  ## alert(db[[data_meta$data_type]])
  csv_file <- db[[data_meta$data_type]]$data_file
  process <- processx::process$new("java", 
                                   args = c("-jar",
                                            "../dev/dbExport.jar",
                                            paste0("data/", sql_file),
                                            csv_file,
                                            ## paste0(config_$data_path, csv_file),
                                            "reef",
                                            "reefmon"),
                                   stdout = config_$dashboard_log
                                   )
  timer_observer <- observe({
    invalidateLater(1000)
    if(isolate(process$is_alive()) == FALSE) {
      file.append(file1 = gsub(".log", ".old", config_$dashboard_log),
                  file2 = config_$dashboard_log)
      process_ui_end(data_meta, "run_sql")

      cat("\nGenerating summary table from export\n======================================\n",
          file = config_$dashboard_log, append = TRUE)

      create_db_table_from_extract(config_$db_path, data_meta$data_type, csv_file)
      create_db_summary(config_$db_path, data_meta$data_type,ids$sub_tab_id, csv_file) 
      tbl <- get_db_table_data(input$sql_tbl_choice, data_meta)

      if (!is.null(tbl)) {
        make_dashboard_table(tbl, ids) 
      }

      ## out <- system(sprintf("sqlite3 %s 'DROP TABLE IF EXISTS %s';",
      ##                       config_$db_path, data_meta$data_type),
      ##               wait = TRUE, intern = TRUE)
      ## out <- system(sprintf("sqlite3 %s '.mode csv' '.headers on' '.import %s %s';",
      ##                       config_$db_path, csv_file, data_meta$data_type),
      ##               wait = TRUE, intern = TRUE)
      ## ## out <- system(sprintf("sqlite3 %s 'DROP TABLE IF EXISTS ", data_meta$data_type, "; ',
      ## ##               '.mode csv' '.headers on' '.import %s %s';",
      ## ##                       config_$db_path, csv_file, data_meta$data_type),
      ## ##               wait = TRUE, intern = TRUE)

      ## db_tbl <- paste0(data_meta$data_type, "_sum")
      ## con <- dbConnect(RSQLite::SQLite(), config_$db_path)
      ## tbl(con, data_meta$data_type) |>
      ##   mutate(SURVEY_DATE1 = sql("datetime(SURVEY_DATE, 'localtime')")) |> 
      ##   group_by(NRM_REGION) |>
      ##   summarise(SURVEY_DATE1 = max(SURVEY_DATE1)) |> 
      ##   compute(db_tbl, temporary = FALSE, overwrite = TRUE)
      ## dbDisconnect(con)

      ## get_data_summary(data_meta, csv_file, db[[data_meta$data_type]]$summary_file)
      ## join_extracted_summary_to_model_meta(db[[data_meta$data_type]]$summary_file, data_meta)
      ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
      ## if (!is.null(tbl)) {
      ##   make_dashboard_table(tbl, ids) 
      ## }
      timer_observer$destroy()
    }
  }) 
  }
)

## Run the post-extract script
observeEvent(input$run_process, {
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  data_type <- data_meta$data_type
  db <- readRDS(config_$db_file)
  csv_file <- db[[data_meta$data_type]]$data_file
  ## prepare the ui for an external process
  process_ui_start(data_meta, id = "run_process")
  process <- processx::process$new("Rscript", 
                                   args = c("../dev/R/process_db_extract.R",
                                            paste0("--method=", data_type),
                                            "--purpose=post-process",
                                            paste0("--csv_file=",csv_file)),
                                   stdout = config_$dashboard_log
                                   )
  timer_observer <- observe({
    invalidateLater(1000)
    if(isolate(process$is_alive()) == FALSE) {
      file.append(file1 = gsub(".log", ".old", config_$dashboard_log),
                  file2 = config_$dashboard_log)
      process_ui_end(data_meta, "run_process")
      timer_observer$destroy()
    }
  }) 
}
)

## Prepare Reefs panel -----------------------------------------------

## Change in type of data to present in Prepare Reefs table
## The following is has no purpose
observeEvent(c(input$reef_tbl_choice), {
  tab_name <- input$dashboard_panel
  lookup <- dashboard_tab_lookup[[tab_name]]
  tab_id <- lookup$outputId
  sub_tab_name <- input$dashboard_sql_panel
  sub_tab_id <- lookup[[sub_tab_name]]$outputId
  ## alert(sub_tab_id)
  sql_file <- lookup$sql
  csv_file <- paste0(config_$data_path, str_replace(lookup$sql, ".sql", ".csv"))
  ## alert(csv_file)
  ## make_dashboard_table(input$reef_tbl_choice, csv_file, tab_id, sub_tab_id) 
}
)

## Run the prepare reefs for modelling script
observeEvent(input$run_prepare_reefs, {
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  data_type <- data_meta$data_type
  db <- readRDS(config_$db_file)
  rds_file <- gsub(".csv", ".rds", db[[data_meta$data_type]]$data_file)
  ## get the id's of the data_type and routine tabs
  ## list2env(get_dashboard_tab_ids(), env = .GlobalEnv)  
  ## get the names of various files that can be derived from the sql file name
  ## list2env(get_dashboard_file_names(lookup), env = .GlobalEnv)  
  ## prepare the ui for an external process
  process_ui_start(data_meta, id = "run_prepare_reefs")
  process <- processx::process$new("Rscript", 
                                   args = c("../dev/R/process_db_extract.R",
                                            paste0("--method=", data_type),
                                            "--purpose=make_reefs",
                                            paste0("--rds_file=", rds_file)),
                                   stdout = config_$dashboard_log
                                   )
  timer_observer <- observe({
    invalidateLater(1000)
    if(isolate(process$is_alive()) == FALSE) {
      ## build_summary_data(data_meta)
      ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
      ## if (!is.null(tbl)) {
      ##   make_dashboard_table(tbl, ids) 
      ## }

      ## rds_file <- gsub(".csv", ".rds", db[[data_meta$data_type]]$summary_file)
      ## ## alert(rds_file)
      ## ## alert(file.exists(rds_file))
      ## if (file.exists(rds_file)) {
      ##   data <- readRDS(file = rds_file) 
      ##   dashboard_tab_lookup[[ids$tab_name]][[ids$sub_tab_name]]$choices <-
      ##     sort(unique(data$reef))
      ##   assign("dashboard_tab_lookup", dashboard_tab_lookup, envir = .GlobalEnv)
      ##   ## alert(sort(unique(data$reef)))
      ##   updateSelectInput(inputId = paste0(data_meta$data_type, "_reef_model_choice"),
      ##                     choices = c("all", sort(unique(data$reef)) 
      ##                                 ))
      ## }
      file.append(file1 = gsub(".log", ".old", config_$dashboard_log),
                  file2 = config_$dashboard_log)
      process_ui_end(data_meta, "run_prepare_reefs")
      timer_observer$destroy()
    }
  }) 
}
)


## Run the fit reef models script
observeEvent(input$run_fit_reefs, {
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  data_type <- data_meta$data_type
  reefs <- input[[paste0(data_type, "_reef_model_choice")]]
  if ("all" %in% reefs) {
    reefs <- get_candidates_to_select_from(data_meta, "AIMS_REEF_NAME")
  }
  process_ui_start(data_meta, id = "run_fit_reef")
  process <- processx::process$new("Rscript", 
                                   args = c("../dev/R/run_models.R",
                                            paste0("--method=", data_type),
                                            paste0("--scale=reef"),
                                            paste0("--domain=", reefs),
                                            paste0("--log=", config_$dashboard_log)),
                                   stdout = config_$dashboard_log
                                   ## stderr =  config_$dashboard_log
                                   )
    timer_observer <- observe({
      invalidateLater(1000)
      if(isolate(process$is_alive()) == FALSE) {
        ## alert("Made it")
        config_$models <- get_config_models()
        ## put those models into database
        con <- dbConnect(RSQLite::SQLite(), config_$db_path)
        copy_to(con, config_$models, name = "models", temporary = FALSE, overwrite = TRUE)
        dbDisconnect(con)
        tbl <- get_db_table_data(input$sql_tbl_choice, data_meta)
        if (!is.null(tbl)) {
          make_dashboard_table(tbl, ids) 
        }

        process_ui_end(data_meta, "run_fit_reefs")
        timer_observer$destroy()
      }
    }) 
}
)


update_summary_data <- function(data_meta) {
  ## read in overall summary
  db <- readRDS(config_$db_file)
  summary_file <- db[[data_meta$data_type]]$summary_file
  data <- readRDS(file = summary_file)
  ## alert(data_meta)
  scale_summary_file <- db[[data_meta$data_type]][[paste0(data_meta$data_scale, "_summary_file")]]
  ## alert(scale_summary_file)
  ## alert(summary_file)
  if (!file.exists(scale_summary_file)) {
    data <- data |>
      group_by(!!sym(data_meta$data_scale)) |>
      summarise(survey_date = max(survey_date),
                extraction_date = max(extraction_date))
    saveRDS(data, file = scale_summary_file)
  } else {
    data_sub <- readRDS(file = scale_summary_file)
    join_extracted_summary_to_model_meta(scale_summary_file, data_meta)
  }
}

## Prepare NRMs panel ------------------------------------------------

## Run the prepare NRMs for modelling script
observeEvent(input$run_prepare_nrm, {
  ## ## get the id's of the data_type and routine tabs
  ## list2env(get_dashboard_tab_ids(), env = .GlobalEnv)  
  ## ## get the names of various files that can be derived from the sql file name
  ## list2env(get_dashboard_file_names(lookup), env = .GlobalEnv)  
  ## ## prepare the ui for an external process
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  data_type <- data_meta$data_type
  db <- readRDS(config_$db_file)
  rds_file <- gsub(".csv", ".rds", db[[data_meta$data_type]]$data_file)
  process_ui_start(data_meta, id = "run_prepare_nrm")
    process <- processx::process$new("Rscript", 
                                     args = c("../dev/R/process_db_extract.R",
                                              paste0("--method=", data_type),
                                              "--purpose=make_nrm",
                                              paste0("--rds_file=", rds_file)),
                                     stdout = config_$dashboard_log
                                     ## stderr =  config_$dashboard_log
                                     )
    timer_observer <- observe({
      invalidateLater(1000)
      if(isolate(process$is_alive()) == FALSE) {
        ## build_summary_data(data_meta)
        ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
        ## if (!is.null(tbl)) {
        ##   make_dashboard_table(tbl, ids) 
        ## }
        file.append(file1 = gsub(".log", ".old", config_$dashboard_log),
                    file2 = config_$dashboard_log)
        process_ui_end(data_meta, "run_prepare_nrm")
        timer_observer$destroy()
      }
    }) 
    ## config_$models <- get_config_models()
    ## assign("config_", config_, envir = .GlobalEnv)
}
)

## Run the fit NRM models script
observeEvent(input$run_fit_nrm, {
  ## ## get the id's of the data_type and routine tabs
  ## list2env(get_dashboard_tab_ids(), env = .GlobalEnv)  
  ## ## get the names of various files that can be derived from the sql file name
  ## list2env(get_dashboard_file_names(lookup), env = .GlobalEnv)  
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  data_type <- data_meta$data_type
  nrms <- input[[paste0(data_type, "_nrm_model_choice")]]
  if ("all" %in% nrms) {
    nrms <- get_candidates_to_select_from(data_meta, "NRM_REGION")
    ## dashboard_tab_lookup <- get("dashboard_tab_lookup", envir = .GlobalEnv)
    ## nrms <- dashboard_tab_lookup[[ids$tab_name]][[ids$sub_tab_name]]$choices
  }

  ## alert(class(reefs))
  ## names(process) <- nrms
  ## for (nrm in nrms) {
  ## rf <- reefs
    ## alert(rf)
    ## prepare the ui for an external process
  process_ui_start(data_meta, id = "run_fit_nrm")
  process <- processx::process$new("Rscript", 
                                   args = c("../dev/R/run_models.R",
                                            paste0("--method=", data_type),
                                            paste0("--scale=nrm"),
                                            paste0("--domain=", nrms),
                                            paste0("--log=", config_$dashboard_log)),
                                   stdout = config_$dashboard_log
                                   ## stderr =  config_$dashboard_log
                                   )

    ## process[[rf]] <- processx::process$new("docker", 
    ##                                  args = c(
    ##                                    "run",
    ##                                    "-i",
    ##                                    "--rm",
    ##                                    "-v", "/etc/localtime:/etc/localtime",
    ##                                    "-v", "/etc/timezone:/etc/timezone",
    ##                                    "-v", "/home/mlogan/dev:/home/Project",
    ##                                    "-v", "/home/mlogan/data:/data",
    ##                                    "ltmp-monitoring-model:latest",
    ##                                    "Rscript",
    ##                                    "/home/Project/R/00_main.R",
    ##                                    ## shQuote(paste0("--path='/data/photo-transect/2021-01-14/process/ALL/2024/ALL/reef/",reef,"/raw/reef_data.zip'")),  #generalise this
    ##                                    shQuote(paste0("--path='/data/",data_type, "/2021-01-14/process/ALL/2024/",rf,"/nrm/",rf,"/raw/reef_data.zip'")),  #generalise this
    ##                                    paste0("--method=", data_type),  #generalise this
    ##                                    shQuote(paste0("--domain=", rf)),
    ##                                    "--scale=nrm",  #generalise this
    ##                                    "--status=true",
    ##                                    "--refresh_data=false"
    ##                                  ),
    ##                                  stdout = config_$dashboard_log
    ##                                  )
    ## alert("loop finished")
    timer_observer <- observe({
      invalidateLater(1000)
      if(isolate(process$is_alive()) == FALSE) {
        ## alert("here")
        config_$models <- get_config_models()
        con <- dbConnect(RSQLite::SQLite(), config_$db_path)
        copy_to(con, config_$models, name = "models", temporary = FALSE, overwrite = TRUE)
        dbDisconnect(con)
        tbl <- get_db_table_data(input$sql_tbl_choice, data_meta)
        if (!is.null(tbl)) {
          make_dashboard_table(tbl, ids) 
        }
        ## assign("config_", config_, envir = .GlobalEnv)
        ## update_summary_data(data_meta)
        ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
        ## if (!is.null(tbl)) {
        ##   make_dashboard_table(tbl, ids) 
        ## }
        ## #also update the extract data tab summary table
        ## summary_file <- db[[data_meta$data_type]]$summary_file
        ## join_extracted_summary_to_model_meta(summary_file, data_meta)
        ## ## join_extracted_summary_to_model_meta(str_replace(csv_file,  ".csv", "_sum.csv")) 
        ## ## make_dashboard_table(input[[paste0(sub_tab_id, "_tbl_choice")]],
        ## ##                      csv_file, tab_id, sub_tab_id) 
        ## db <- readRDS(config_$db_file)
        ## summary_file <- db[[data_meta$data_type]]$summary_file
        ## join_extracted_summary_to_model_meta(summary_file, data_meta)
        process_ui_end(data_meta, "run_fit_nrm")
        timer_observer$destroy()
      }
    }) 
    ## process[[rf]]$wait()
    ## process[[rf]]$kill()
  ## }
}
)

## Prepare SECTORs panel ------------------------------------------------

## Run the prepare SECTORs for modelling script
observeEvent(input$run_prepare_sector, {
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  data_type <- data_meta$data_type
  db <- readRDS(config_$db_file)
  rds_file <- gsub(".csv", ".rds", db[[data_meta$data_type]]$data_file)
  ## ## get the id's of the data_type and routine tabs
  ## list2env(get_dashboard_tab_ids(), env = .GlobalEnv)  
  ## ## get the names of various files that can be derived from the sql file name
  ## list2env(get_dashboard_file_names(lookup), env = .GlobalEnv)  
  ## ## prepare the ui for an external process
  process_ui_start(data_meta, id = "run_prepare_sector")
    process <- processx::process$new("Rscript", 
                                     args = c("../dev/R/process_db_extract.R",
                                              paste0("--method=", data_type),
                                              "--purpose=make_sectors",
                                              paste0("--rds_file=", rds_file)),
                                     stdout = config_$dashboard_log
                                     ## stderr =  config_$dashboard_log
                                     )
    timer_observer <- observe({
      invalidateLater(1000)
      if(isolate(process$is_alive()) == FALSE) {
        ## build_summary_data(data_meta)
        ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
        ## if (!is.null(tbl)) {
        ##   make_dashboard_table(tbl, ids) 
        ## }
        file.append(file1 = gsub(".log", ".old", config_$dashboard_log),
                    file2 = config_$dashboard_log)
        process_ui_end(data_meta, "run_prepare_sector")
        timer_observer$destroy()
      }
    }) 
  ## config_$models <- get_config_models()
  ## assign("config_", config_, envir = .GlobalEnv)
}
)

## Run the fit SECTOR models script
observeEvent(input$run_fit_sector, {
  ids <- get_dashboard_tab_ids()  
  data_meta <- get_dashboard_data_type(ids)  
  data_type <- data_meta$data_type
  ## ## get the id's of the data_type and routine tabs
  ## list2env(get_dashboard_tab_ids(), env = .GlobalEnv)  
  ## ## get the names of various files that can be derived from the sql file name
  ## list2env(get_dashboard_file_names(lookup), env = .GlobalEnv)  
  sectors <- input[[paste0(data_type, "_sector_model_choice")]]
  if ("all" %in% sectors) {
    sectors <- get_candidates_to_select_from(data_meta, "A_SECTOR")
    ## dashboard_tab_lookup <- get("dashboard_tab_lookup", envir = .GlobalEnv)
    ## sectors <- dashboard_tab_lookup[[ids$tab_name]][[ids$sub_tab_name]]$choices
  }
  
  process_ui_start(data_meta, id = "run_fit_sector")
  process <- processx::process$new("Rscript", 
                                   args = c("../dev/R/run_models.R",
                                            paste0("--method=", data_type),
                                            paste0("--scale=sectors"),
                                            paste0("--domain=", sectors),
                                            paste0("--log=", config_$dashboard_log)),
                                   stdout = config_$dashboard_log
                                   ## stderr =  config_$dashboard_log
                                   )
    timer_observer <- observe({
      invalidateLater(1000)
      if(isolate(process$is_alive()) == FALSE) {
        config_$models <- get_config_models()
        con <- dbConnect(RSQLite::SQLite(), config_$db_path)
        copy_to(con, config_$models, name = "models", temporary = FALSE, overwrite = TRUE)
        dbDisconnect(con)
        tbl <- get_db_table_data(input$sql_tbl_choice, data_meta)
        if (!is.null(tbl)) {
          make_dashboard_table(tbl, ids) 
        }
        ## build_summary_data(data_meta)
        ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
        ## if (!is.null(tbl)) {
        ##   make_dashboard_table(tbl, ids) 
        ## }
          ## config_$models <- get_config_models()
          ## assign("config_", config_, envir = .GlobalEnv)
          ## update_summary_data(data_meta)
          ## tbl <- get_table_data(input$sql_tbl_choice, data_meta)
          ## if (!is.null(tbl)) {
          ##   make_dashboard_table(tbl, ids) 
          ## }
          ## db <- readRDS(config_$db_file)
          ## summary_file <- db[[data_meta$data_type]]$summary_file
          ## join_extracted_summary_to_model_meta(summary_file, data_meta)
        process_ui_end(data_meta, "run_fit_sector")
        timer_observer$destroy()
      }
    }) 


  
  if (1 == 2) {
    
    process <- vector("list", length(sectors))
    names(process) <- sectors
    for (rf in sectors) {
      ## rf <- reefs
      ## alert(rf)
      ## prepare the ui for an external process
      process_ui_start(data_meta, id = "run_fit_sector")
      process[[rf]] <- processx::process$new("docker", 
                                             args = c(
                                               "run",
                                               "-i",
                                               "--rm",
                                               "-v", "/etc/localtime:/etc/localtime",
                                               "-v", "/etc/timezone:/etc/timezone",
                                               "-v", "/home/mlogan/dev:/home/Project",
                                               "-v", "/home/mlogan/data:/data",
                                               "ltmp-monitoring-model:latest",
                                               "Rscript",
                                               "/home/Project/R/00_main.R",
                                               ## shQuote(paste0("--path='/data/photo-transect/2021-01-14/process/ALL/2024/ALL/reef/",reef,"/raw/reef_data.zip'")),  #generalise this
                                               shQuote(paste0("--path='/data/",data_type, "/2021-01-14/process/ALL/2024/",rf,"/Sectors/",rf,"/raw/reef_data.zip'")),  #generalise this
                                               paste0("--method=", data_type),  #generalise this
                                               shQuote(paste0("--domain=", rf)),
                                               "--scale=Sectors",  #generalise this
                                               "--status=true",
                                               "--refresh_data=false"
                                             ),
                                             stdout = config_$dashboard_log
                                             )
      ## alert("loop finished")
      timer_observer <- observe({
        invalidateLater(1000)
        if(isolate(process[[rf]]$is_alive()) == FALSE) {
          process_ui_end(data_meta, "run_fit_sector")
          config_$models <- get_config_models()
          assign("config_", config_, envir = .GlobalEnv)
          update_summary_data(data_meta)
          tbl <- get_table_data(input$sql_tbl_choice, data_meta)
          if (!is.null(tbl)) {
            make_dashboard_table(tbl, ids) 
          }
          db <- readRDS(config_$db_file)
          summary_file <- db[[data_meta$data_type]]$summary_file
          join_extracted_summary_to_model_meta(summary_file, data_meta)
          ## join_extracted_summary_to_model_meta(str_replace(csv_file,  ".csv", "_sum.csv")) 
          ## make_dashboard_table(input[[paste0(sub_tab_id, "_tbl_choice")]],
          ##                      csv_file, tab_id, sub_tab_id) 
          timer_observer$destroy()
        }
      }) 
      ## process[[rf]]$wait()
      ## process[[rf]]$kill()
    }

  }
}
)


## Functions =========================================================

output$download_raw_data <- downloadHandler(
  filename = function() {
    ## Use the selected dataset as the suggested file name
    paste0("pt_data", ".csv")
  },
  content = function(file) {
    ## Write the dataset to the `file` that will be downloaded
    file.copy("../data/pt.csv", file)
  },
  contentType = "text/csv"
)

log_file_reactive <- reactivePoll(1000, session,
  # This function returns the time that log_file was last modified
  checkFunc = function() {
    ## llog_file <- paste0("../data/", "dashboard.log")
      if (file.exists(config_$dashboard_log))
           file.info(config_$dashboard_log)$mtime[1]
         else
          ""
         },
  # This function returns the content of the status terminal output
  valueFunc = function() {
      if (file.exists(config_$dashboard_log))
        readLines(config_$dashboard_log, warn = FALSE)
      else ""
  }
)

output$log_out <- renderText( {
  log_file_reactive()
  if (file.exists(config_$dashboard_log)) {
    model_log_content <- readLines(config_$dashboard_log,
                                   warn = FALSE## ,
                                   ## n = 9
                                   )
    model_log_content <- paste(model_log_content, collapse = "\n")
    model_log_content
  } else {
    ""
  }
}
)

get_data_summary <- function(data_meta, data_file, summary_file) {
  ## list2env(get_dashboard_tab_ids(), env = .GlobalEnv)  
  ## list2env(get_dashboard_file_names(lookup), env = .GlobalEnv)  
  ## alert(paste0("data_file:", data_file))
  ## alert(data_type)
  ## write_csv(config_$models |> filter(data_scale == "nrm") |> head(), file = "../data/temp.csv")

  process_ui_start(data_meta, id = "refresh_summary_tbl")
  if (data_meta$data_type == "photo-transect") {
    out2 <- system(paste0(
      "cut -d, -f17,6,28,9 < ", data_file, " | uniq |
    Rscript -e \"library(dplyr); library(readr); input <- read_csv(stdin());
    input  <- input |> mutate(SURVEY_DATE = as.POSIXct(SURVEY_DATE, format='%d-%b-%Y %H:%M:%S')) |>
    group_by(NRM_REGION, A_SECTOR, AIMS_REEF_NAME) |>
    summarise(SURVEY_DATE=max(SURVEY_DATE));
    write.csv(input, row.names=FALSE)\"
    "
    ),
    intern = TRUE)
  }
  if (data_meta$data_type == "manta") {
    out2 <- system(paste0(
      "cut -d, -f5,6,10,15 < ", data_file, " | uniq |
    Rscript -e \"library(dplyr); library(readr); input <- read_csv(stdin());
    input  <- input |> mutate(SURVEY_DATE = as.POSIXct(SURVEY_DATE, format='%d-%b-%Y %H:%M:%S')) |>
    group_by(NRM_REGION, A_SECTOR, AIMS_REEF_NAME) |>
    summarise(SURVEY_DATE=max(SURVEY_DATE));
    write.csv(input, row.names=FALSE)\"
    "
    ),
    intern = TRUE)
  }
  cat(out2, file =  config_$dashboard_log, sep = "\n", append = FALSE)
  cat("Done!\n",
      file = config_$dashboard_log, append = TRUE)
  ## alert(head(out2))
  file.append(file1 = gsub(".log", ".old", config_$dashboard_log),
              file2 = config_$dashboard_log)
  process_ui_end(data_meta, "refresh_summary_tbl")
  data_info <- file.mtime(data_file)
  ## data_sum <- str_replace(data_file, ".csv", "_sum.csv")
  ## alert(out2)
  data <- as.data.frame(out2) |>
    slice(-1) |> 
    separate(everything(),
             into = c("nrm", "sector", "reef", "survey_date"), sep = ",") |>
    mutate(## reef = str_replace_all(reef, '"', ""),
           across(everything(), ~ str_replace_all(.x, '"', "")),
           survey_date = as.POSIXct(survey_date, "%Y-%m-%d %H:%M:%S"),
           extraction_date = format(data_info, "%Y-%m-%d %H:%M:%S"))
  saveRDS(data, file = summary_file)
 ## alert(data |> pull(survey_date) |> head())
  ##write_csv(data, file = data_sum)
        ## config_$models <- get_config_models()
  ## alert(config_$models)
  ## alert(paste("sub_tab_id:", sub_tab_id))
  ## if (sub_tab_id != "sql") 
  ##   data_sum <- str_replace(data_sum, "_sum", paste0("_", sub_tab_id, "_sum"))
  ## alert(paste0("data_sum:", data_sum))

  ## join_extracted_summary_to_model_meta(data, data_sum, data_type, sub_tab_id)
  
  ## ## if (exists("config_$models")) {
  ##   ## data_model <- config_$models |>
  ##   ##   filter(data_scale == "reef") |> 
  ##   ##   dplyr::select(reef = domain_name, model_date)
  ##   data <- data |>
  ##     dplyr::select(-any_of("model_date")) |> 
  ##     ## Join to reef-level model
  ##     left_join(config_$models |>
  ##               filter(data_type == data_type, data_scale == "reef") |> 
  ##               dplyr::select(reef = domain_name, reef_model_date = model_date) |>
  ##               distinct()
  ##               ) |>
  ##     distinct() |>
  ##     ## Join to nrm-level model
  ##     left_join(config_$models |>
  ##               filter(data_type == data_type, data_scale == "nrm") |> 
  ##               dplyr::select(nrm = domain_name, nrm_model_date = model_date) |>
  ##               distinct()
  ##               ) |>
  ##     ## Join to sector-level model
  ##     left_join(config_$models |>
  ##               filter(data_type == data_type, data_scale == "sector") |> 
  ##               dplyr::select(sector = domain_name, sector_model_date = model_date) |>
  ##               distinct()
  ##               ) |>
  ##     distinct() 
  ## ## }
  ## write_csv(data, file = data_sum)
}

## join_extracted_summary_to_model_meta <- function(data, data_sum, data_type, sub_tab_id) {
join_extracted_summary_to_model_meta <- function(summary_file, data_meta) {
  ## alert("join code run")
  ## alert(data_sum)
  ## alert("start join")
  ## alert(paste0("file exists:", file.exists(data_sum)))
  ## alert(names(read_csv(data_sum)))
  ## alert(names(config_$models))
  ## alert(paste0("data_sum:", data_sum))
  ## alert(paste0("sub_tab_id:", sub_tab_id))
  ## data <- read_csv(data_sum)
  data <- readRDS(summary_file)
  
  data <- data |>
    ## dplyr::select(-any_of("model_date")) |> 
    dplyr::select(-ends_with("model_date"))
  ## top_level <- ifelse(any(sub_tab_id %in% c("reef", "nrm", "sector")), FALSE, TRUE)
  top_level <- ifelse(any(data_meta$data_scale %in% c("reef", "nrm", "sector")), FALSE, TRUE)
  ## alert(data_meta)
  ## alert(summary_file)
  ## alert(top_level)
  ## if (data_meta$data_scale != "sql")
    ## data_sum <- str_replace(data_sum, "_sum", paste0("_", sub_tab_id, "_sum"))
    
  ## alert(str_detect(data_sum, "sector_"))
  if (data_meta$data_scale == "sector" | top_level) {
   data <- data |> 
      ## Join to sector-level model
      left_join(config_$models |>
                filter(data_type == data_meta$data_type, data_scale == "Sectors") |> 
                dplyr::select(sector = domain_name, sector_model_date = model_date) |>
                distinct()
                ) |>
      distinct() 
  }
  if (data_meta$data_scale == "nrm" | top_level) {
   data <- data |> 
      left_join(config_$models |>
                filter(data_type == data_meta$data_type, data_scale == "nrm") |> 
                dplyr::select(nrm = domain_name, nrm_model_date = model_date) |>
                distinct()
                ) |>
      distinct() 
  }
  ## alert(paste0("data_sum:", data_sum))
  if (data_meta$data_scale == "reef" | top_level) {
   data <- data |> 
      left_join(config_$models |>
                filter(data_type == data_meta$data_type, data_scale == "reef") |> 
                dplyr::select(reef = domain_name, reef_model_date = model_date) |>
                distinct()
                ) |>
      distinct() 
  }
  saveRDS(data, file = summary_file)
}



## observeEvent(c(input$tbl_choice), {
##   tab_name <- input$dashboard_panel
##   lookup <- dashboard_tab_lookup[[tab_name]]
##   tab_id <- lookup$outputId
##   sub_tab_name <- input$dashboard_sql_panel
##   sub_tab_id <- lookup[[sub_tab_name]]$outputId
##   ## alert(sub_tab_id)
##   sql_file <- lookup$sql
##   csv_file <- paste0(config_$data_path, str_replace(lookup$sql, ".sql", ".csv"))
##   alert(csv_file)
##   make_dashboard_table(input$tbl_choice, csv_file, tab_id, sub_tab_id) 
## }
## )

build_summary_data <- function(data_meta) {
  ## read in overall summary
  ## alert("in build")
  ## alert(data_meta$data_type)
  db <- readRDS(config_$db_file)
  ## alert(names(db))
  ## alert(db[[data_meta$data_type]])
  summary_file <- db[[data_meta$data_type]]$summary_file
  ## alert(summary_file)
  if (file.exists(summary_file)) {
    data <- readRDS(file = summary_file)
    if(data_meta$data_scale == "sql") {
      scale_summary_file <- summary_file
      data_meta$data_scale <- "reef"
    } else {
      scale_summary_file <- db[[data_meta$data_type]][[paste0(data_meta$data_scale, "_summary_file")]]
    }
    ## alert(data_meta$data_scale)
    if (!file.exists(scale_summary_file)) {
      data <- data |>
        group_by(!!sym(data_meta$data_scale)) |>
        summarise(survey_date = max(survey_date),
                  extraction_date = max(extraction_date))
      saveRDS(data, file = scale_summary_file)
    } else {
      data_sub <- readRDS(file = scale_summary_file)
      data_sub <- data_sub |>
        full_join(data |>
                  group_by(!!sym(data_meta$data_scale)) |>
                  summarise(survey_date = max(survey_date),
                            extraction_date = max(extraction_date))
                  )
      saveRDS(data_sub, file = scale_summary_file)
    }
  }
}
make_dashboard_table <- function(data, ids) {
  ## alert(paste0("data::", data))
## make_dashboard_table <- function(tbl_choice, csv_file, tab_id, sub_tab_id) {
  ## tab_name <- input$dashboard_panel
  ## lookup <- dashboard_tab_lookup[[tab_name]]
  ## tab_id <- lookup$outputId
  ## file_exists <- FALSE
  ## ## alert(sub_tab_id)
  ## if (tbl_choice == "data" & file.exists(csv_file)) {
  ##   if (file.exists(csv_file)) {
  ##     data <- read_csv(file = csv_file, n_max = 20)
  ##     file_exists <- TRUE
  ##   }
  ## } else if (tbl_choice == "summary") {
  ##   csv_file <- str_replace(csv_file, ".csv", "_sum.csv")
  ##   if (file.exists(csv_file)) {
  ##     ## alert(paste0("csv_file:", csv_file))
  ##     data <- read_csv(file = csv_file) 
  ##     file_exists <- TRUE
  ##     if (sub_tab_id == "reef") {
  ##       data_sum <- str_replace(csv_file, "_sum.csv", "_reef_sum.csv")
  ##       data <- read_csv(file = data_sum) 
  ##       data <- data |>
  ##         dplyr::select(-starts_with(c("nrm", "sector"))) 
  ##     }
  ##     if (sub_tab_id == "nrm") {
  ##       data_sum <- str_replace(csv_file, "_sum.csv", "_nrm_sum.csv")
  ##       data <- read_csv(file = data_sum) 
  ##       ## alert(data_sum)
  ##       data <- data |>
  ##         dplyr::select(-starts_with(c("reef", "sector"))) |> 
  ##         group_by(nrm) |>
  ##         summarise(across(ends_with("date"), max)) |>
  ##         ungroup()
  ##       ## write_csv(data, file = data_sum)
  ##     }
  ##     if (sub_tab_id == "sector") {
  ##       data_sum <- str_replace(csv_file, "_sum.csv", "_sector_sum.csv")
  ##       data <- read_csv(file = data_sum) 
  ##       ## alert(paste("data_sum", data_sum))
  ##       data <- data |>
  ##         dplyr::select(-starts_with(c("reef", "nrm"))) |> 
  ##         group_by(sector) |>
  ##         summarise(across(ends_with("date"), max)) |>
  ##         ungroup()
  ##       ## write_csv(data, file = data_sum)
  ##     }
  ##   }
  ## }  
  ## ## alert(paste0("names:", names(data)))
  ## if (file_exists) {
    colstyle <- function(survey_field, extract_field = NULL) {
      colDef(
        format = colFormat(datetime = TRUE),
                    style = function(value, index) {
                      if (is.na(value)) {
                          list(background = "red")
                      } else {
                        if (value < data[[survey_field]][index]) {
                          list(background = "red")
                        } else if (!is.null(extract_field)) {
                          if (value < data[[extract_field]][index])
                            list(background = "orange")
                          else 
                            list(background = "lightgreen")
                        } else 
                          list(background = "lightgreen")
                      }
                    })
    }
    output[[paste0(ids$tab_id, "_", ids$sub_tab_id, "_tbl")]] <- reactable::renderReactable({
      data |> reactable(
                columns = list(
                  survey_date = colDef(format = colFormat(datetime = TRUE)),
                  extraction_date = colstyle(survey_field = "survey_date"),
                  nrm_model_date = colstyle(survey_field = "survey_date",
                                            extract_field = "extraction_date"),
                  sector_model_date = colstyle(survey_field = "survey_date",
                                               extract_field = "extraction_date"),
                  reef_model_date = colstyle(survey_field = "survey_date",
                                             extract_field = "extraction_date")
                ),
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
    })
  ## }
}
## Return either the data/summary data or FALSE
## This function does not compile the data, it only reads it in
get_table_data <- function(tbl_choice, data_meta) {
  db <- readRDS(config_$db_file)
  ## alert(config_$db_file)
  ## alert(data_meta)
  file_exists <- FALSE 
  data <- NULL
  if (tbl_choice == "data") {
    data_file <- db[[data_meta$data_type]]$data_file 
    ## alert(data_file)
    if (file.exists(data_file)) {
      data <- read_csv(file = data_file, n_max = 20)
      ## alert(data)
      file_exists <- TRUE
    }
  } else if (tbl_choice == "summary") {
    ## alert(data_meta)
    if (data_meta$data_scale == "sql") { ## the Extract data tab
      summary_file <- db[[data_meta$data_type]]$summary_file 
      if (file.exists(summary_file)) {
        data <- readRDS(file = summary_file)
        file_exists <- TRUE
      }
    } else {  ## one of the Prepare tabs
      summary_file <- db[[data_meta$data_type]][[paste0(data_meta$data_scale, "_summary_file")]] 
      if (file.exists(summary_file)) {
        data <- readRDS(file = summary_file)
        file_exists <- TRUE
      }
    }
  }
  return(data)
}
get_dashboard_tab_ids <- function() {
  tab_name <- input$dashboard_panel
  lookup <- dashboard_tab_lookup[[tab_name]]
  tab_id <- lookup$outputId
  ## alert(names(input))
  if(!is.null(input$dashboard_sql_panel)) {
    sub_tab_name <- input$dashboard_sql_panel
    sub_tab_id <- lookup[[sub_tab_name]]$outputId
  } else {
    sub_tab_name <- "sql"
    sub_tab_id <- "sql"
  }
  list(tab_name = tab_name,
       lookup = lookup,
       tab_id =  tab_id,
       sub_tab_name =  sub_tab_name,
       sub_tab_id = sub_tab_id
       )
}
get_dashboard_data_type <- function(ids) {
  data_type <- ids$lookup$data_type
  data_scale <- ids$sub_tab_id
  list(data_type = data_type,
       data_scale = data_scale)
}

get_dashboard_file_names <- function(lookup) {
  sql_file <- lookup$sql
  csv_file <- paste0(config_$data_path, str_replace(lookup$sql, ".sql", ".csv"))
  csv_sum <- str_replace(csv_file, ".csv", "_sum.csv")
  data_type <- dashboard_tab_lookup[[tab_name]]$data_type
  rds_file <- paste0(config_$data_path, str_replace(lookup$sql, ".sql", ".rds"))
  list(sql_file = sql_file,
       csv_file = csv_file,
       csv_sum = csv_sum,
       rds_file = rds_file,
       data_type = data_type)
}
process_ui_start <- function(data_meta, id) {
  ## alert("in")
  ## alert(as.character(sys.call(-1)[[1]]))
  shinyjs::disable(id)
  shinyjs::addClass(id = id, class = "btn-disabled")
  shinyjs::addClass(id = paste0(data_meta$data_type, "overlay_div"), class = "overlay")
  show("wait_message")
  ## Temporarily copy the running dashboard.log to a backup That way,
  ## when the system call is run (which will always overwrite
  ## dashboard.log, the log so far is presurved and can be added to so
  ## as to accumulate the log.  The basic process is:
  ## - make a copy of the current dashboard.log
  ## - run system (processx) call which will flush dashboard.log
  ## - the last action of the system call should append the backup and
  ##   dashboard.log
  ## - once the system call is complete, the backup log will be renamed
  ##   as dashboard.log
  file.copy(from = config_$dashboard_log,
            to = gsub(".log", ".old", config_$dashboard_log),
            overwrite = TRUE)
}
process_ui_end <- function(data_meta, id) {
  ## Once the system call is complete, take the backup log and make it
  ## the actual log
  file.copy(from = gsub(".log", ".old", config_$dashboard_log),
            to = config_$dashboard_log,
            overwrite = TRUE)
  shinyjs::enable(id)
  shinyjs::removeClass(id = id, class = "btn-disabled")
  shinyjs::addClass(id = id, class = "btn-success")
  shinyjs::removeClass(id = paste0(data_meta$data_type, "overlay_div"), class = "overlay")
  hide("wait_message")
}


create_db_table_from_extract <- function(db_path, data_type, csv_file) {
  out <- system(sprintf("sqlite3 %s 'DROP TABLE IF EXISTS %s';",
                        db_path, data_type),
                wait = TRUE, intern = TRUE)
  out <- system(sprintf("sqlite3 %s '.mode csv' '.headers on' '.import %s %s';",
                        db_path, csv_file, data_type),
                wait = TRUE, intern = TRUE)

  ## out <- system(sprintf("sqlite3 %s 'ALTER TABLE %s ADD COLUMN extraction_date TEXT; UPDATE %s SET extraction_date = DATETIME(\"now\");'",
  ##                       db_path, data_type, data_type),
  ##               wait = TRUE, intern = TRUE)
  ## data_info <- format(file.mtime(csv_file), "%Y-%m-%d %H:%M:%S")
  ## con <- dbConnect(RSQLite::SQLite(), db_path)
  ## tbl(con, data_type) |>
  ##   mutate(extraction_date = "fish") |> 
  ##   compute(data_type, temporary = FALSE, overwrite = TRUE)
  ## dbDisconnect(con)
}

create_db_summary <- function(db_path, data_type, data_scale, csv_file) {
  ## Reef level
  data_info <- format(file.mtime(csv_file), "%Y-%m-%d %H:%M:%S")
  db_sum_tbl <- paste0(data_type, "_sum")
  con <- dbConnect(RSQLite::SQLite(), db_path)
  tbl(con, data_type) |>
    mutate(SURVEY_DATE = sql("datetime(SURVEY_DATE, 'localtime')")) |> 
    group_by(NRM_REGION, A_SECTOR, AIMS_REEF_NAME) |>
    summarise(survey_date = max(SURVEY_DATE)) |> 
    mutate(extraction_date = data_info) |> 
    compute(db_sum_tbl, temporary = FALSE, overwrite = TRUE)
  ## NRM level
  db_sum_tbl <- paste0(data_type, "_sector_sum")
  tbl(con, data_type) |>
    mutate(SURVEY_DATE = sql("datetime(SURVEY_DATE, 'localtime')")) |> 
    group_by(A_SECTOR) |>
    summarise(survey_date = max(SURVEY_DATE)) |> 
    mutate(extraction_date = data_info) |> 
    compute(db_sum_tbl, temporary = FALSE, overwrite = TRUE)
  ## Sector level
  db_sum_tbl <- paste0(data_type, "_nrm", "_sum")
  tbl(con, data_type) |>
    mutate(SURVEY_DATE = sql("datetime(SURVEY_DATE, 'localtime')")) |> 
    group_by(NRM_REGION) |>
    summarise(survey_date = max(SURVEY_DATE)) |> 
    mutate(extraction_date = data_info) |> 
    compute(db_sum_tbl, temporary = FALSE, overwrite = TRUE)
  dbDisconnect(con)
}


get_db_table_data <- function(tbl_choice, data_meta) {
  ## alert(data_meta)
  file_exists <- FALSE 
  data <- NULL
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  tbls <- dbListTables(con) 
  if (is.null(tbl_choice)) tbl_choice <- "summary"
  if (tbl_choice == "data" &
      data_meta$data_type %in% tbls) {
    db_tbl <- paste0(data_meta$data_type)
    ## con <- dbConnect(RSQLite::SQLite(), config_$db_path)
    data <- tbl(con, db_tbl) |>
      head(20) |>
      collect()
  } else if (tbl_choice == "summary") {
    if (data_meta$data_scale %in% c("reef", "sql") &
        paste0(data_meta$data_type, "_sum") %in% tbls) { ## the Extract data tab
      if (data_meta$data_scale == "sql") {
        db_tbl <- paste0(data_meta$data_type, "_sum")
        data <- tbl(con, db_tbl) |>
          left_join(tbl(con, "models") |>
                    filter(data_type == data_meta$data_type,
                           data_scale == "Sectors") |> 
                    select(A_SECTOR = domain_name, sector_model_date = model_date),
                    by = "A_SECTOR") |> 
          left_join(tbl(con, "models") |>
                    filter(data_type == data_meta$data_type,
                           data_scale == "nrm") |> 
                    select(NRM_REGION = domain_name, nrm_model_date = model_date),
                    by = "NRM_REGION") |> 
          left_join(tbl(con, "models") |>
                    filter(data_type == data_meta$data_type,
                           data_scale == "reef") |> 
                    select(AIMS_REEF_NAME = domain_name, reef_model_date = model_date),
                    by = "AIMS_REEF_NAME") |> 
          collect() |>
          distinct()
      } else {
        db_tbl <- paste0(data_meta$data_type, "_sum")
        data <- tbl(con, db_tbl) |>
          left_join(tbl(con, "models") |>
                    filter(data_type == data_meta$data_type,
                           data_scale == "reef") |> 
                    select(AIMS_REEF_NAME = domain_name, reef_model_date = model_date),
                    by = "AIMS_REEF_NAME") |> 
          collect() |>
          distinct()
      }
      ## mutate(extraction_date1 = as.POSIXct(extraction_date, format = "%Y-%m-%d %H:%M:%S"))
      ## dbDisconnect(con)
    } else if (paste0(data_meta$data_type, "_", data_meta$data_scale, "_sum") %in% tbls) {  ## one of the Prepare tabs
      db_tbl <- paste0(data_meta$data_type, "_", data_meta$data_scale, "_sum")
      ## con <- dbConnect(RSQLite::SQLite(), config_$db_path)
      if (data_meta$data_scale == "reef") {
        data <- tbl(con, db_tbl) |>
          left_join(tbl(con, "models") |>
                    filter(data_type == data_meta$data_type,
                           data_scale == "reef") |> 
                    select(AIMS_REEF_NAME = domain_name, reef_model_date = model_date),
                    by = "AIMS_REEF_NAME") |> 
          distinct() |> 
          collect() 
      }
      if (data_meta$data_scale == "nrm") {
        data <- tbl(con, db_tbl) |>
          left_join(tbl(con, "models") |>
                    filter(data_type == data_meta$data_type,
                           data_scale == "nrm") |> 
                    select(NRM_REGION = domain_name, nrm_model_date = model_date),
                    by = "NRM_REGION") |> 
          distinct() |> 
          collect()
      }
      if (data_meta$data_scale == "sector") {
        data <- tbl(con, db_tbl) |>
          left_join(tbl(con, "models") |>
                    filter(data_type == data_meta$data_type,
                           data_scale == "Sectors") |> 
                    select(A_SECTOR = domain_name, sector_model_date = model_date),
                    by = "A_SECTOR") |> 
          distinct() |> 
          collect()
      }
      ## dbDisconnect(con)
    }
  }
  dbDisconnect(con)
  return(data)
}

get_candidates_to_select_from <- function(data_meta, field) {
  if (data_meta$data_scale == "reef" )
    db_tbl <- paste0(data_meta$data_type, "_sum")
  else
    db_tbl <- paste0(data_meta$data_type, "_", data_meta$data_scale, "_sum")
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  tbls <- dbListTables(con) 
  if (db_tbl %in% tbls) {
  candidates <- tbl(con, db_tbl) |>
    select(!!sym(field)) |>
    distinct() |> 
    collect() |>
    pull(!!sym(field)) 
  } else
    candidates <- NULL
  dbDisconnect(con)
  return(candidates)
}
