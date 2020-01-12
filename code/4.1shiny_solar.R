# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "shiny", "sf", "ggplot2")

### install if necessary
lapply(packages, 
       function(x)
       {
         if(!(x %in% installed.packages()) | x %in% old.packages())
         {
           install.packages(x)  
         }
       })

lapply(packages, require, character.only = T)

# **********************************************************************************************
#### D E F I N I T I O N S ####
#rm(list=ls(all=TRUE))


# ***********************************************************************************************
# load data
map_and_data_solar_current_county <- readRDS(here("data", "processed", "map_and_data_solar_current_county.rds"))
map_and_data_solar_current_state <- readRDS(here("data", "processed", "map_and_data_solar_current_state.rds"))

# create server
server <- function(input, output) {
  
  dataset <- reactive({
    get(paste0("map_and_data_solar_current_", input$geo_level))
  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes(geometry = geometry)) +
      geom_sf(aes(fill = get(input$out_var))) +
      scale_fill_gradient(low = "#B6FF52", high = "#3B5518", name = "Quantity") +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())

    print(p)
    
  })
  
}

# create ui
dataset <- reactive({
  get(paste0("map_and_data_solar_current_", input$geo_level))
})

ui <- fluidPage(
  
  titlePanel("German Power Plants Explorer"),
  
  sidebarPanel(
    selectInput('geo_level', 'Geographical Level', c("State" = "state", "Community" = "county")),
    selectInput('out_var', 'Output Variable', c("Number of solar plants" = "n", "Sum of solar plants' power" = "sum"))
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)

shinyApp(ui, server)