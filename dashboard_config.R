config_ <- list(
  data_path = "../data/",
  model_path = "../dev/data/modelled/",
  dashboard_log = "../data/dashboard.log",
  locked = FALSE
) 
config_$db_file <- paste0(config_$data_path, "db.rda")
config_$lock_file = paste0(config_$data_path, ".lockfile")
config_$db_path = paste0(config_$data_path, "dashboard.sqlite")

unlink(config_$dashboard_log)
unlink(gsub(".log", ".old", config_$dashboard_log))


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
## config_$model_path <- "../dev/data/modelled"
## config_$data_path <- "../data/"
## config_$dashboard_log <- "../data/dashboard.log"
## unlink(config_$dashboard_log)
## unlink(gsub(".log", ".old", config_$dashboard_log))
## config_$db_file <- paste0(config_$data_path, "db.rda")
## config_$locked <- FALSE

## config_$lock_file <- paste0(config_$data_path, ".lockfile")

  


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

  


## config_$db_path <- paste0(config_$data_path, "dashboard.sqlite")
## What if the database does not exist
con <- dbConnect(RSQLite::SQLite(), config_$db_path)
## ## sql_create_table <- "
## ## CREATE TABLE manta (
## ##     P_CODE TEXT,
## ##     SURVEY_DATE TEXT,
## ##     value REAL
## ## );"
## ## dbExecute(con, sql_create_table)
## message("here")
## message(DBI::dbIsValid(con))
## ## data <- tbl(con, "models") |>
## ## collect() ##|>
## data <- tryCatch( {
##   tbl(con, "models") |>
##     collect()
## }, error = function(x) {
##   message("error occured: ", e$message)
##   NULL
## })
## message("finished")
dbDisconnect(con)
## ensure that the "shiny" group has rw
system(paste("chmod ug+rw", config_$db_path))



## system(sprintf("sqlite3 %s '.mode csv' '.import %s %s'", db_path, csv_file, table_name))
## system("sqlite3 dashboard.sqlite 'PRAGMA table_info(pt);'")
## system("sqlite3 dashboard.sqlite 'select * from pt_sum where limit 10;'")


ansi2html <- function(ansi){
  HTML(sprintf(
    "<pre style = 'line-height: 11.2pt;'>%s</pre>",
    gsub("\n", "<br/>", as.character(sgr_to_html(ansi)))
  ))
}

get_db_model_data<- function(method = NULL, scale = NULL, domain = NULL) {
  con <- dbConnect(RSQLite::SQLite(), config_$db_path)
  data <- 1
  data <- tbl(con, "models") |>
    collect() ##|> 
    ## dplyr::select(data_type, data_scale, domain_name, matches(".*_hash"))
    ## dplyr::select(data_type, data_scale, domain_name, contains("_hash"))
  if (!is.null(method)) {
    data <- data |> 
    filter(data_type == method,
           data_scale == scale,
           domain_name %in% domain)
  }
  dbDisconnect(con)
  return(data)
}

## trawl through model path and extract model metadata
## then join onto this, the model info in the database (if it exists)
## 1. trawl through model path and extract model metadata
## 2. determine whether the models database exists
## 2. if it does exist
##    2.1. read from the database
##    2.2. join in metadata
## 3. replace model database
get_config_models <- function() {
  ## sink(file = "/home/mlogan/data/AAAA.txt")
  ## Start by gleaning the info by scanning through the models folders
  fls <- list.files(config_$model_path,
                    recursive = TRUE, full.names = TRUE)
  ## write.csv(data.frame(config_$model_path), file = "../data/temp1.csv")
  fls_wch <- str_detect(basename(fls), "^[^_]*_[^_]*_[^_]*\\.rds$")
  ## write.csv(fls_wch, file = "../data/temp2.csv")
  if (length(fls_wch) > 0) {
    models <- tibble(path = unique(fls[fls_wch])) |> 
      mutate(mod = map(.x = path,
                       .f = ~ {
                         ss <- readRDS(.x)$label |>
                                         unlist() |>
                                         unique()
                         ## hash <- digest(.x, algo = "sha256", file = TRUE)
                         mtime <- file.mtime(.x)
                         tibble(str = ss) |>
                           separate(str, into = c("data_type", "data_scale",
                                                  "domain_name", "group",
                                                  "family_type",
                                                  "reef_zone", "depth",
                                                  "shelf"),
                                    sep = "_") |>
                           mutate(model_date = as.POSIXct(format(mtime, "%Y-%m-%d %H:%M:%S")),
                                  model_path = .x,
                                  reef_model_data_hash = "",
                                  sector_model_data_hash = "",
                                  nrm_model_data_hash = "") |> 
                           as.data.frame()
                       })) |>
      unnest(c(mod)) |>
      ## dplyr::select(-path) |> 
      mutate(data_scale = ifelse(data_scale == "Sectors", "sector", data_scale))
    con <- dbConnect(RSQLite::SQLite(), config_$db_path)
    tbls <- dbListTables(con) 
    dbDisconnect(con)
    ## alert(models |> filter(domain_name == "Reef 14-133"))
    if ("models" %in% tbls & 1 == 1) {
      models_db <- get_db_model_data()
      ## sink(file = "/home/mlogan/data/AAAA.txt")
      message('=models=================\n')
      cat(file = stderr(), paste(head(models), "\n"))
      message(paste0("dim models: ", dim(models), "\n"))
      message('=models db==============\n')
      message(head(as.data.frame(models_db)))
      message(paste0("dim models db: ", dim(models_db), "\n"))
      ## cat('========================\n')
      models <- models_db |>
        full_join(models |>
                  dplyr::select(-matches(".*\\_hash$|^path$|^model\\_.*")),
                  by = c("data_type" = "data_type",
                         "data_scale" = "data_scale",
                         "domain_name" = "domain_name",
                         "group" = "group",
                         "family_type" = "family_type",
                         "reef_zone" = "reef_zone",
                         "depth" = "depth",
                         "shelf")) 
      message(paste0("Dim: ", dim(models), "\n"))
      ## print(head(as.data.frame(models)))
      ## cat("asdfkj\n")
      ## sink()
    }
    ## write_csv(models, file = paste0(config_$data_path, "models.csv")) 
      ## sink()
    ## alert(models |> filter(domain_name == "Reef 14-133"))
    return(models)
  } else {
    ## need to put something in here incase no models have been run ever
    ## return(tibble(model_path = "", data_type = "", data_scale = "", domain_name = "",
    ##               group = "", "family_type" = "", reef_zone = "", depth = "", shelf = "", model_date = "",
    ##               sector_model_data_hash = "", nrm_model_data_hash = "",
    ##               path = "",
    ##               reef_model_data_hash = ""))
  }
}

## Potentially put this back
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

## if (!is.null(config_$models)) {
##   con <- dbConnect(RSQLite::SQLite(), config_$db_path)
##   copy_to(con, config_$models, name = "models", temporary = FALSE, overwrite = TRUE)
##   dbDisconnect(con)
## }

