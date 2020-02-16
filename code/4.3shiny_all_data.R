# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "shiny", "zip", "sf", "tmap", "tmaptools", "ggplot2", "dplyr", "shinydashboard", "leaflet")

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
options(scipen = 999) # no scientific numbering
subfolder <- zip_list(here("data", "raw", "shape_germany_bundesland_landkreis.zip"))$filename[1]
subfolder <- gsub("/", "", subfolder, fixed = TRUE)
file_ger_shape <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_sta.shp")
file_ger_shape_state <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_lan.shp")
file_ger_shape_county <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_krs.shp")


# ***********************************************************************************************
#### load data ####
# read all data files
data_state <- read.csv2(here("data", "processed", paste0("data_state", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_state_yearly <- read.csv2(here("data", "processed", paste0("data_state_yearly", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_state_yearly_extended <- read.csv2(here("data", "processed", paste0("data_state_yearly_extended", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)

data_county <- read.csv2(here("data", "processed", paste0("data_county", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_county_yearly <- read.csv2(here("data", "processed", paste0("data_county_yearly", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_county_yearly_extended <- read.csv2(here("data", "processed", paste0("data_county_yearly_extended", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)

data_offshore <- read.csv2(here("data", "processed", paste0("data_offshore", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)


# trim data before 2000 for the new plots

# read all shape files
ger_shape <- st_read(file_ger_shape, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)
ger_shape_state <- st_read(file_ger_shape_state, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)
ger_shape_county <- st_read(file_ger_shape_county, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)


# some shapes need multiple entries of geometry
# these duplicated entries will multiply the amount of facilities and power -> needs correction if merged too early
# federal level: duplicate found
#ger_shape$GEN[duplicated(ger_shape$GEN)]

# state-level: no duplicates
#ger_shape_state$GEN[duplicated(ger_shape_state$GEN)]

# county-level: some duplicates (with different geometry information: adding later to one county)
#ger_shape_county$GEN[duplicated(ger_shape_county$GEN)]
# numbers of counties
#nrow(ger_shape_county) - length(ger_shape_county$GEN[duplicated(ger_shape_county$GEN)])

## prepare merge
names(ger_shape_state)[names(ger_shape_state) == "GEN"] <- "Bundesland"
names(ger_shape_county)[names(ger_shape_county) == "GEN"] <- "Landkreis"

names(ger_shape_state)[names(ger_shape_state) == "RS"] <- "ags"
names(ger_shape_county)[names(ger_shape_county) == "RS"] <- "ags"

ger_shape_county$ags <- as.numeric(ger_shape_county$ags)
ger_shape_state$ags <- as.numeric(ger_shape_state$ags)

# ***********************************************************************************************
#### merge shape file after aggregation ####
## general maps for data exploration
map_and_data_state <- inner_join(ger_shape_state, data_state, by = c("ags" = "ags_federal_state"))
map_and_data_county <- inner_join(ger_shape_county, data_county, by = c("ags" = "ags_county"))
# Lage "Windkraft auf See" dropped
names(map_and_data_state)[names(map_and_data_state) == "Bundesland"] <- "regionid"
names(map_and_data_county)[names(map_and_data_county) == "name"] <- "regionid"

saveRDS(map_and_data_state, file = here("data", "processed","map_and_data_state.rds"))
saveRDS(map_and_data_county, file = here("data", "processed","map_and_data_county.rds"))
# missing ags_federal_state dropped
table(data_state$ags_federal_state, useNA = "always")
data_state[is.na(data_state$ags_federal_state),]
# related to offshore (always if ags_federal_state is missing)


## ...


# ***********************************************************************************************
#### create server ####
server <- function(input, output) {
  
  dataset <- reactive({
    get(paste0("map_and_data_", input$geo_level))
  })
  
  Energy <- reactive({
    dataset() %>% filter(EinheitenTyp == input$source)
  })
  
  Period <- reactive({
    df <- get(paste0("data_", input$geo_level, "_yearly"))
    df %>% filter(EinheitenTyp == input$source) %>%
      filter(start_year >= input$years[1]) %>%
      filter(start_year <= input$years[2]) 
  })
  
  dataset_region <- reactive({
    columns <- c("Solareinheit", "Windeinheit", "Biomasse", "Wasser", "Braunkohle", "Steinkohle", "Gas", "Mineralölprodukte", "Stromspeichereinheit", "Geothermie")
    
    selected_regionid <- input$map_shape_click$id
    
    # Turn "." back into "-" in the state names. e.g. Nordrhein.Westfalen to Nordrhein-Westfalen
    selected_regionid <- sub(".","-",selected_regionid, fixed = TRUE)
    
    dataset <- dataset() %>% 
      filter(regionid == selected_regionid) %>% 
      filter(EinheitenTyp %in% columns)
    
    # Highlight the chosen energy type in the bar chart
    dataset$EinheitenTyp[dataset$EinheitenTyp == input$source] <- paste0(input$source, "*")
    dataset
  })
  
  coloring <- reactive({
    if (input$source == "Solareinheit"){"YlOrRd"}else{
      if (input$source == "Windeinheit"){"Blues"}else{
        if(input$source == "Braunkohle"){"Greys"}else{
          if(input$source == "Biomasse"){"Greens"}else{
            if(input$source == "Wasser"){"Blues"}else{
              if(input$source == "Geothermie"){"Oranges"}else{
                if(input$source == "Steinkohle"){"Greys"}else{
                  if(input$source == "Gas"){"Greys"}else{
                    if(input$source == "Mineralölprodukte"){"PuBuGn"}else{
                      if(input$source == "Stromspeichereinheit"){"YlOrBr"}
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  
  title_legend <- reactive({
    if (input$out_var == "n"){"Number of Power Plants"}else{
      if (input$out_var == "sum"){"Power in kw(p)"} else{
        if (input$out_var == "mean"){"Power in kw(p)"}
      }
    }
  })
  
  label_energy <- reactive({
    if (input$source == "Solareinheit"){"Solar"}else{
      if (input$source == "Windeinheit"){"Wind"}else{
        if (input$source == "Biomasse"){"Biomass"}else{
          if (input$source == "Wasser"){"Water"}else{
            if (input$source == "Braunkohle"){"Brown Coal"}else{
              if (input$source == "Steinkohle"){"Black Coal"}else{
                if (input$source == "Gas"){"Gas"}else{
                  if (input$source == "Mineralölprodukte"){"Mineral Oil"}else{
                    if (input$source == "Stromspeichereinheit"){"Battery"}else{
                      if (input$source == "Geothermie"){"Geothermal"}else{
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  })
  
  
  output$title_bar_chart <- renderText({
    regionid <- input$map_shape_click$id
    if(is.null(regionid)){paste("Please select a region in the map")}else{
      if (input$out_var == "n"){paste("Total number of power plants in", regionid)}else{
        if (input$out_var == "sum"){paste("Total power production in", regionid)}else{
          if (input$out_var == "mean"){paste("Average power production per plant in", regionid)}
        }
      }
    }
  })
  

  output$plot1 <- renderPlot({
    
    p1 <- ggplot(Energy(), aes(geometry = geometry)) +
      geom_sf(aes(fill = get(input$out_var))) +
      scale_fill_gradient(low = "#B6FF52", high = "#3B5518", name = "Quantity") +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank())
    
    print(p1)
    
  })
  
  output$map <- renderLeaflet({
    
    p2 <- tm_shape(Energy()) +
      tm_polygons(input$out_var, palette = coloring(), n = input$scale, title = title_legend(), id = "regionid", popup.vars = c("Value:" = input$out_var)) +
      tm_layout(legend.outside = TRUE) +
      tm_layout(frame = FALSE)
    
    tmap_leaflet(p2)
    
  })
  
  output$plot_energy_bar_chart <- renderPlot({
    if (!is.null(input$map_shape_click)) {
      ggplot(dataset_region(), aes(x = reorder(EinheitenTyp, get(input$out_var)), y = get(input$out_var))) +
        labs(y = title_legend(), x = "Energy Source") +
        geom_bar(stat="identity", fill="steelblue") + theme_minimal(base_size = 16) + 
        theme(axis.text.y = element_text(size=12, face="bold")) +
        scale_x_discrete(labels=c("Solareinheit" = "Solar",  "Windeinheit" = "Wind", 
                                  "Biomasse" = "Biomass", "Wasser" = "Water",
                                  "Braunkohle" = "Brown Coal", "Steinkohle" = "Black Coal",
                                  "Gas" = "Gas" , "Mineralölprodukte" = "Mineral Oil",
                                  "Stromspeichereinheit" = "Battery", "Geothermie" = "Geothermal", 
                                  "Solareinheit*" = "Solar*",  "Windeinheit*" = "Wind*", 
                                  "Biomasse*" = "Biomass*", "Wasser*" = "Water*",
                                  "Braunkohle*" = "Brown Coal*", "Steinkohle*" = "Black Coal*",
                                  "Gas*" = "Gas*" , "Mineralölprodukte*" = "Mineral Oil*",
                                  "Stromspeichereinheit*" = "Battery*", "Geothermie*" = "Geothermal*")) +
        coord_flip() 
      # scale_x_discrete(labels=c("Gas"=expression(bold(Gas))))
    }
  }, bg="transparent")
  
  output$plot_change_over_time <- renderPlot({
    if (!is.null(input$map_shape_click)) {
      Period() %>%
      #data_state_yearly_after_2000 %>%
        #filter(EinheitenTyp == input$source) %>%
        ggplot(., aes(x=start_year, y=get(input$out_var), fill=EinheitenTyp))+
        geom_line(size=1.5) + geom_bar(stat="identity") +
        xlab('Year') + ylab(title_legend()) +
        theme_minimal(base_size = 16) +
        theme(axis.text.x=element_text(angle=90, hjust=1),
              axis.title=element_text(size=18,face="bold"),
              legend.title = element_blank()) +
        theme(legend.position="bottom") +
        scale_fill_discrete(labels = label_energy())
      
    }
  })
}

# ***********************************************************************************************
#### create ui ####
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Storyline", tabName = "storyline", icon = icon("dashboard")),
      menuItem("Data", tabName = "data_explorer", icon = icon("th")),
      menuItem("Reference", tabName = "reference", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "storyline",
              h2("Do rich regions use more renewable energy in Germany? (for example)"),
              div(class = "text",
                  p("Because we do not have a story yet,", tags$b("the content is left blank intentionally.")),
                  p("And I just put these sentences", tags$em("to test the codes."))
              )
      ),
      
      # Second tab content
      tabItem(tabName = "data_explorer",
              fluidRow(
                column(width = 4,
                       fluidRow(
                         column(width = 12,
                                h3("German Power Plants Explorer", align = "center"),
                                #titlePanel("German Power Plants Explorer"),
                                sidebarPanel(width = 12,
                                             selectInput('source', 'Energy Source', c("Solar Energy" = "Solareinheit", "Wind Energy" = "Windeinheit", 
                                                                                      "Biomass Energy" = "Biomasse", "Water Energy" = "Wasser",
                                                                                      "Brown Coal Energy" = "Braunkohle", "Black Coal Energy" = "Steinkohle",
                                                                                      "Gas Energy" = "Gas", "Mineral Oil Energy" = "Mineralölprodukte",
                                                                                      "Battery" = "Stromspeichereinheit", "Geothermal Energy" = "Geothermie")),
                                             selectInput('geo_level', 'Geographical Level', c("State" = "state", "County" = "county")),
                                             selectInput('out_var', 'Output Variable', c("Total number of power plants" = "n", 
                                                                                         "Total power production" = "sum", 
                                                                                         "Average power production per plant" = "mean" )),
                                             sliderInput("scale", "Rough Number of Legend Classes",
                                                         min = 2, max = 10, value = 6),
                                             sliderInput("years", "Period of interest for yearly change",
                                                         min = 1970, max = 2019, value = c(2000, 2019)),
                                ),
                         )
                       )
                       
                ),
                column(width = 8,
                       fluidRow(
                         column(width = 12,
                                h3("Germany in geographical zones"),
                                leafletOutput('map')
                         )
                       ),
                       fluidRow(
                         column(width = 12,
                                h3("Yearly change: ", textOutput("title_bar_chart")),
                                plotOutput('plot_change_over_time'),
                                h3("Comparison of energy sources for this region"),
                                plotOutput('plot_energy_bar_chart')
                         )
                       )
                )
              )
      ),
      
      # Third tab content
      tabItem(tabName = "reference",
              h2("Reference"),
              div(class = "list",
                  tags$ul(
                    tags$li(tags$b("Data"), ": German power plants raw data is downloaded from official register Marktstammdatenregister through following link:", tags$a(href="https://www.bundesnetzagentur.de/SharedDocs/Downloads/DE/Sachgebiete/Energie/Unternehmen_Institutionen/ErneuerbareEnergien/ZahlenDatenInformationen/VOeFF_Registerdaten/DatenAb310119.zip", "https://www.bundesnetzagentur.de/SharedDocs/Downloads/DE/Sachgebiete/Energie/", tags$br("Unternehmen_Institutionen/ErneuerbareEnergien/ZahlenDatenInformationen/VOeFF_Registerdaten/DatenAb310119.zip"))),
                    p("The register includes the potential production of power plants in Germany including renewables like wind, solar and biomass as well as coal and nuclear. The source provides more than 100 variables for various purposes."),
                    tags$li(tags$b("License"), ": What should we write about data license? Should we translate some relevant sections of the data usage policy of Bundesnetzagentur?")
                  )
              )
      )
    )
  )
)

shinyApp(ui, server)
