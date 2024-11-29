cat(file = stderr(), ls(), "\n")
nrm_tab <- tabItem(
  tabName = "nrm",
  fluidRow(
    column(width = 2, h2("NRM regions", style = "margin-left: 15px;")),
    ## column(width = 2,
    ##        selectInput("nrm_selector",
    ##                    "Select NRM:",
    ##                    choices = config_$models |>
    ##                      filter(data_scale == "nrm",
    ##                             data_type == "photo-transect") |>
    ##                      pull(domain_name) |>
    ##                      unique()),
    ##                    ## choices = c("Burdekin", "Wet Tropics")),
    ##        )
    ),
  tabsetPanel(
    id = "nrm_panel",
    tabPanel(
      title = "Photo-transects",
      icon = icon("image"),
      ## id = "nrm_pt_tab",
      uiOutput("nrm_pt_panel")
    ),
    tabPanel(
      title = "Manta tow",
      icon = icon("person-swimming"),
      ## id = "nrm_manta_tab",
      uiOutput("nrm_manta_panel")
    ),
    tabPanel(
      title = "Juveniles",
      icon = icon("baby"),
      ## id = "nrm_juveniles_tab",
      uiOutput("nrm_juveniles_panel")
    ),
    tabPanel(
      title = "Fish",
      icon = icon("fish"),
      ## id = "nrm_fish_tab",
      uiOutput("nrm_fish_panel")
    )
  )
)
