shinyUI(dashboardPage(
    source("R/ui/header.R",local=T)$value,  # Load headerPanel
    source("R/ui/sidebar.R",local=T)$value, # Load sidebarPanel
    source("R/ui/main.R",local=T)$value     # Load mainPanel
))
