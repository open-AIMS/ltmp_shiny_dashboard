sectors_tab <- tabItem(
  tabName = "sectors",
  fluidRow(
    column(width = 2, h2("Sectors", style = "margin-left: 15px;")),
    ## column(width = 2,
    ##        selectInput("sector_selector",
    ##                    "Select SECTOR:",
    ##                    choices = config_$models |>
    ##                      filter(data_scale == "sector",
    ##                             data_type == "photo-transect") |>
    ##                      pull(domain_name) |>
    ##                      unique()),
    ##                    ## choices = c("Burdekin", "Wet Tropics")),
    ##        )
    ),
  tabsetPanel(
    id = "sector_panel",
    tabPanel(
      title = "Photo-transects",
      icon = icon("image"),
      ## id = "sector_pt_tab",
      uiOutput("sector_pt_panel")
    ),
    tabPanel(
      title = "Manta tow",
      icon = icon("person-swimming"),
      ## id = "sector_manta_tab",
      uiOutput("sector_manta_panel")
    ),
    tabPanel(
      title = "Juveniles",
      icon = icon("baby"),
      ## id = "sector_juveniles_tab",
      uiOutput("sector_juveniles_panel")
    ),
    tabPanel(
      title = "Fish",
      icon = icon("fish"),
      ## id = "sector_fish_tab",
      uiOutput("sector_fish_panel")
    )
  )
)
