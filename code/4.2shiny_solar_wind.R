# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "shiny", "sf", "ggplot2", "dplyr")

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
#options(scipen = 999) # no scientific numbering
subfolder <- zip_list(here("data", "raw", "shape_germany_bundesland_landkreis.zip"))$filename[1]
subfolder <- gsub("/", "", subfolder, fixed = TRUE)
file_ger_shape <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_sta.shp")
file_ger_shape_state <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_lan.shp")
file_ger_shape_county <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_krs.shp")


# ***********************************************************************************************
#### load data ####
data_solar_wind_current <- readRDS(here("data", "processed", "data_solar_wind_current.rds"))

ger_shape <- st_read(file_ger_shape, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)
ger_shape_state <- st_read(file_ger_shape_state, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)
ger_shape_county <- st_read(file_ger_shape_county, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)


# some shapes need multiple entries of geometry
# these duplicated entries will multiply the amount of facilities and power -> needs correction if merged too early
# federal level: duplicate found
ger_shape$GEN[duplicated(ger_shape$GEN)]

# state-level: no duplicates
ger_shape_state$GEN[duplicated(ger_shape_state$GEN)]

# county-level: some duplicates (with different geometry information: adding later to one county)
ger_shape_county$GEN[duplicated(ger_shape_county$GEN)]


## prepare merge
names(ger_shape_state)[names(ger_shape_state) == "GEN"] <- "Bundesland"
names(ger_shape_county)[names(ger_shape_county) == "GEN"] <- "Landkreis"


# ***********************************************************************************************
#### aggregate data on county level ####
# rename NAs in county for wind to "Off-Shore" (checked before, is true)
data_solar_wind_current$Landkreis <- as.character(data_solar_wind_current$Landkreis)
entries <- which(is.na(data_solar_wind_current$Landkreis))
data_solar_wind_current$Landkreis[entries] <- "Off-Shore"

# consolidate
data_solar_wind_current_county <- data_solar_wind_current %>%
  select(Plz, Ort, Bundesland, Landkreis, Gemeinde, Nettonennleistung, EinheitenTyp) %>%
  group_by(EinheitenTyp, Landkreis) %>%
  summarize(n = length(Landkreis), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung),
            Einheitentyp = first(EinheitenTyp))


# merge shape file after aggregation
map_and_data_solar_wind_current_county <- inner_join(ger_shape_county, data_solar_wind_current_county)



# ***********************************************************************************************
#### aggregate data on state level ####
data_solar_wind_current_state <- data_solar_wind_current %>%
  group_by(EinheitenTyp, Bundesland) %>%
  summarize(n = length(Bundesland), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung),
            Einheitentyp = first(EinheitenTyp))

# merge shape file after aggregation
map_and_data_solar_wind_current_state <- inner_join(ger_shape_state, data_solar_wind_current_state)



# ***********************************************************************************************
#### create server ####
server <- function(input, output) {
  
  dataset <- reactive({
    get(paste0("map_and_data_solar_wind_current_", input$geo_level))
  })
  
  energy_source <- reactive({
    dataset() %>% filter(EinheitenTyp == input$source)
  })
  
  coloring <- reactive({
    if (input$source == "Solareinheit"){"Greens"}else{
      if (input$source == "Windeinheit"){"Blues"}
    }
  })
  
  title_legend <- reactive({
    if (input$out_var == "n"){"Number of power plants"}else{
      if (input$out_var == "sum"){"Power in kw(p)"}
    }
  })
  
  output$plot1 <- renderPlot({
    
    p <- ggplot(energy_source(), aes(geometry = geometry)) +
      geom_sf(aes(fill = get(input$out_var))) +
      scale_fill_gradient(low = "#B6FF52", high = "#3B5518", name = "Quantity") +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())

    print(p)
    
  })
  
  output$plot2 <- renderPlot({
    
    p <- tm_shape(energy_source()) +
      tm_polygons(input$out_var, palette = coloring(), n = 10, title = title_legend()) +
      tm_layout(legend.outside = TRUE) +
      tm_layout(frame = FALSE)
    
    print(p)
    
  })
  
}

# ***********************************************************************************************
#### create ui ####
dataset <- reactive({
  get(paste0("map_and_data_solar_current_", input$geo_level))
})

energy_source <- reactive({
  dataset() %>% filter(EinheitenTyp == input$source)
})

coloring <- reactive({
  if (input$source == "Solareinheit"){"Greens"}else{
    if (input$source == "Windeinheit"){"Blues"}
  }
})

title_legend <- reactive({
  if (input$out_var == "n"){"Number of power plants"}else{
    if (input$out_var == "sum"){"Power in kw(p)"}
  }
})


ui <- fluidPage(
  
  titlePanel("German Power Plants Explorer"),
  
  sidebarPanel(
    selectInput('source', 'Energy Source', c("Solar Energy" = "Solareinheit", "Wind Energy" = "Windeinheit")),
    selectInput('geo_level', 'Geographical Level', c("State" = "state", "County" = "county")),
    selectInput('out_var', 'Output Variable', c("Number of power plants" = "n", "Sum of power plants' power" = "sum"))
  ),
  
  mainPanel(
    plotOutput('plot2')
  )
)

shinyApp(ui, server)