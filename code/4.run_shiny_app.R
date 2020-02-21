library(here)
source(here::here("server.R"))
source(here::here("ui.R"))

shinyApp(ui, server)
