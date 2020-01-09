# load data
map_and_data_solar_current_plz <- readRDS(here("data", "processed", "map_and_data_solar_current_plz.rds"))
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
            axis.ticks = element_blank())

    print(p)
    
  })
  
}

# create ui
dataset <- reactive({
  get(paste0("map_and_data_solar_current_", input$geo_level))
})

ui <- fluidPage(
  
  titlePanel("German Power System Explorer"),
  
  sidebarPanel(
    selectInput('geo_level', 'Geographical Level', c("State" = "state", "Community" = "plz")),
    selectInput('out_var', 'Output Variable', c("Number of solar plants" = "n", "Sum of solar plants' power" = "sum"))
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)

shinyApp(ui, server)