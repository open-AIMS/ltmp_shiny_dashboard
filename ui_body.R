
source("ui_body_dashboard.R")
source("ui_body_reefs.R")
source("ui_body_sectors.R")
source("ui_body_nrm.R")
source("ui_body_zones.R")
source("styles.R")


body <- dashboardBody(
  tag_styles,
  ## callout_style,
  tabItems(
  ##   ## landing_tab,
    dashboard_tab,
    reefs_tab,
    sectors_tab,
    nrm_tab,
    zones_tab
  )
)
