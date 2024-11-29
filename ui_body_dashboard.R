dashboard_tab <- tabItem(
  tabName = "dashboard",
  fluidRow(
    h2("Dashboard", style = "margin-left: 15px;"),
  ),
  tabsetPanel(
    id = "dashboard_panel",
    tabPanel(
      title = "Photo-transects",
      icon = icon("image"),
      ## id = "reefs_pt_tab"
      uiOutput("dashboard_pt_panel")
    ),
    tabPanel(
      title = "Manta tow",
      icon = icon("person-swimming"),
      ## id = "reefs_manta_tab"
      uiOutput("dashboard_manta_panel")
    ),
    tabPanel(
      title = "Juveniles",
      icon = icon("baby"),
      ## id = "reefs_juveniles_tab"
      uiOutput("dashboard_juveniles_panel")
    ),
    tabPanel(
      title = "Fish",
      icon = icon("fish"),
      ## id = "reefs_fish_tab"
      uiOutput("dashboard_fish_panel")
    )
  )
)
