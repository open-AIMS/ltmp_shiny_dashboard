config_ <- list(
  data_path = "../data"
  )
fls <- list.files(config_$data_path, recursive = TRUE)
## exclude those that start with spatial in their paths
fls <- str_subset(fls, "spatial.*", negate = TRUE)
## exclude those that dont have process in their paths
fls <- str_subset(fls, "process")
## exclude those that dont have raw in their paths
fls <- str_subset(fls, "raw")
## exclude those that dont end with a zip file
fls <- str_subset(fls, "zip$")
config_$meta <- tibble(path = fls) |>
  separate(path, into = c("data_type", "date", "X1",
                          "X2", "YEAR", "X3", "data_scale",
                          "domain_name", "X4", "file"),
           sep = "/", remove = FALSE) |>
  dplyr::select(-matches("X[0-9]"))
## config_$meta |> as.data.frame() |> head()
## config_$meta |> dim()
## config_$meta |> filter(data_scale == "nrm")
## config_$meta |> filter(data_scale == "reef")

## models
config_$model_path <- "../dev/data/modelled"
config_$data_path <- "../data/"
config_$dashboard_log <- "../data/dashboard.log"
unlink(config_$dashboard_log)
unlink(gsub(".log", ".old", config_$dashboard_log))
config_$db_file <- paste0(config_$data_path, "db.rda")


config_$lock_file <- paste0(config_$data_path, ".lockfile")

if(!file.exists(config_$db_file)) {
  db <- list("photo-transect" = list(
               data_file = paste0(config_$data_path, "photo-transect.csv"),
               summary_file = paste0(config_$data_path, "photo-transect_sum.rds"),
               nrm_summary_file = paste0(config_$data_path, "photo-transect_nrm_sum.rds"),
               sector_summary_file = paste0(config_$data_path, "photo-transect_sector_sum.rds"),
               reef_summary_file = paste0(config_$data_path, "photo-transect_reef_sum.rds")
             ),
             "manta" = list(
               data_file = paste0(config_$data_path, "manta.csv"),
               summary_file = paste0(config_$data_path, "manta_sum.rds"),
               nrm_summary_file = paste0(config_$data_path, "manta_nrm_sum.rds"),
               sector_summary_file = paste0(config_$data_path, "manta_sector_sum.rds"),
               reef_summary_file = paste0(config_$data_path, "manta_reef_sum.rds")
             ),
             "juveniles" = list(
               data_file = paste0(config_$data_path, "juvenile.csv"),
               summary_file = paste0(config_$data_path, "juvenile_sum.rds"),
               nrm_summary_file = paste0(config_$data_path, "juvenile_nrm_sum.rds"),
               sector_summary_file = paste0(config_$data_path, "juvenile_sector_sum.rds"),
               reef_summary_file = paste0(config_$data_path, "juvenile_reef_sum.rds")
             ),
             "fish" = list(
               data_file = paste0(config_$data_path, "fish.csv"),
               summary_file = paste0(config_$data_path, "fish_sum.rds"),
               nrm_summary_file = paste0(config_$data_path, "fish_nrm_sum.rds"),
               sector_summary_file = paste0(config_$data_path, "fish_sector_sum.rds"),
               reef_summary_file = paste0(config_$data_path, "fish_reef_sum.rds")
             )
             )
  saveRDS(db, file = config_$db_file)
}
  
config_$db_path <- paste0(config_$data_path, "dashboard.sqlite")
## What if the database does not exist
con <- dbConnect(RSQLite::SQLite(), config_$db_path)
## ## sql_create_table <- "
## ## CREATE TABLE manta (
## ##     P_CODE TEXT,
## ##     SURVEY_DATE TEXT,
## ##     value REAL
## ## );"
## ## dbExecute(con, sql_create_table)
dbDisconnect(con)

## system(sprintf("sqlite3 %s '.mode csv' '.import %s %s'", db_path, csv_file, table_name))
## system("sqlite3 dashboard.sqlite 'PRAGMA table_info(pt);'")
## system("sqlite3 dashboard.sqlite 'select * from pt_sum where limit 10;'")


ansi2html <- function(ansi){
  HTML(sprintf(
    "<pre style = 'line-height: 11.2pt;'>%s</pre>",
    gsub("\n", "<br/>", as.character(sgr_to_html(ansi)))
  ))
}


## trawl through model path and extract model metadata
get_config_models <- function() {
  fls <- list.files(config_$model_path,
                    recursive = TRUE, full.names = TRUE)
  ## write.csv(fls, file = "../data/temp1.csv")
  fls_wch <- str_detect(basename(fls), "^[^_]*_[^_]*_[^_]*\\.rds$")
  ## write.csv(fls_wch, file = "../data/temp2.csv")
  models <- tibble(path = unique(fls[fls_wch])) |> 
    mutate(mod = map(.x = path,
                     .f = ~ {
                       ss <- readRDS(.x)$label |>
                                       unlist() |>
                                       unique()
                       mtime <- file.mtime(.x)
                       tibble(str = ss) |>
                         separate(str, into = c("data_type", "data_scale",
                                                "domain_name", "group",
                                                "reef_zone", "depth",
                                                "shelf"),
                                  sep = "_") |>
                         mutate(model_date = format(mtime, "%Y-%m-%d %H:%M:%S")) |> 
                         as.data.frame()
                     })) |>
    unnest(c(mod))
  ## saveRDS(models, file = paste0(config_$data_path, "models.rds"))
  write_csv(models, file = paste0(config_$data_path, "models.csv")) 
  models
}

config_$models <- get_config_models() 

assign("config_", config_, envir = .GlobalEnv)

toggle_buttons <- function(on = NULL, off = NULL, success = NULL) {
 if (!is.null(on)) {
   for (i in on) {
     shinyjs::enable(i)
     shinyjs::removeClass(id = i, class = "btn-disabled")
     shinyjs::addClass(id = i, class = "btn-default")
     updateActionButton(inputId = i, icon = icon("play"))
   }
 }
 if (!is.null(success)) {
   for (i in success) {
      shinyjs::enable(i)
      shinyjs::removeClass(id = i, class = "btn-default")
      shinyjs::addClass(id = i, class = "btn-success")
     updateActionButton(inputId = i, icon = icon("circle-check"))
   }
 }
}
## toggle_buttons <- function(bttn1, bttn2 = NULL) {
  ## st <- get_stage_status(lst, stage = 1)
  ## if (st == "success") {
    ## if (!is.null(bttn2)) {
    ##   shinyjs::enable(bttn2)
    ##   shinyjs::removeClass(id = bttn2, class = "btn-disabled")
    ##   shinyjs::addClass(id = bttn2, class = "btn-default")
    ## }
    ## shinyjs::removeClass(id = bttn1, class = "bttn-default")
    ## shinyjs::addClass(id = bttn1, class = "btn-success")
    ## updateActionButton(inputId = bttn1, icon = icon("circle-check"))
  ## } else if (st == "failure") {
  ##   shinyjs::removeClass(id = bttn1, class = "bttn-primary")
  ##   shinyjs::addClass(id = bttn1, class = "bttn-danger")
  ##   updateActionButton(inputId = bttn1, icon = icon("circle-xmark"))
  ## } else if (st == "warning") {
  ##   shinyjs::removeClass(id = bttn1, class = "bttn-primary")
  ##   shinyjs::addClass(id = bttn1, class = "bttn-warning")
  ##   updateActionButton(inputId = bttn1, icon = icon("circle-exclamation"))
  ## }
## }
