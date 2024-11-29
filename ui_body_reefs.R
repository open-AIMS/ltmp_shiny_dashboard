reefs_tab <- tabItem(
  tabName = "reefs",
  fluidRow(
    column(width = 2, h2("Reefs", style = "margin-left: 15px;")),
    ## column(width = 2,
    ##        style = "display:flex",
    ##        selectInput("reefs_selector",
    ##                    "Select Reef:",
    ##                    choices = config_$models |>
    ##                      filter(data_scale == "reef",
    ##                             data_type == "photo-transect") |>
    ##                      pull(domain_name) |>
    ##                      unique() |>
    ##                    sort()),
    ##        actionButton(inputId = "run_reef_refresh",
    ##                     class = "refresh_button",
    ##                     label = "",
    ##                     icon = icon("rotate-right")),
    ##        ),
    ## column(width = 2, actionButton("run_reef_refresh", "refresh"))
    ),
  tabsetPanel(
    id = "reefs_panel",
    tabPanel(
      title = "Photo-transects",
      icon = icon("image"),
      ## id = "reefs_pt_tab"
      uiOutput("reefs_pt_panel")
    ),
    tabPanel(
      title = "Manta tow",
      icon = icon("person-swimming"),
      ## id = "reefs_manta_tab"
      uiOutput("reefs_manta_panel")
    ),
    tabPanel(
      title = "Juveniles",
      icon = icon("baby"),
      ## id = "reefs_juveniles_tab"
      uiOutput("reefs_juveniles_panel")
    ),
    tabPanel(
      title = "Fish",
      icon = icon("fish"),
      ## id = "reefs_fish_tab"
      uiOutput("reefs_fish_panel")
    )
  )
)
