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
#### load data and prepare data####
# read all data files
data_state <- read.csv2(here("data", "processed", paste0("data_state", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_state_combined_all_sources <- read.csv2(here("data", "processed", paste0("data_state_combined_all_sources", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_state_yearly <- read.csv2(here("data", "processed", paste0("data_state_yearly", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_state_yearly_combined_all_sources <- read.csv2(here("data", "processed", paste0("data_state_yearly_combined_all_sources", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_state_yearly_extended <- read.csv2(here("data", "processed", paste0("data_state_yearly_extended", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)

data_county <- read.csv2(here("data", "processed", paste0("data_county", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_county_combined_all_sources <- read.csv2(here("data", "processed", paste0("data_county_combined_all_sources", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_county_yearly <- read.csv2(here("data", "processed", paste0("data_county_yearly", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_county_yearly_combined_all_sources <- read.csv2(here("data", "processed", paste0("data_county_yearly_combined_all_sources", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
data_county_yearly_extended <- read.csv2(here("data", "processed", paste0("data_county_yearly_extended", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)

data_offshore <- read.csv2(here("data", "processed", paste0("data_offshore", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)


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
map_data_state_combined_all_sources <- inner_join(ger_shape_state, data_state_combined_all_sources, by = c("ags" = "ags_federal_state"))
map_and_data_county <- inner_join(ger_shape_county, data_county, by = c("ags" = "ags_county"))
map_data_county_combined_all_sources <- inner_join(ger_shape_county, data_county_combined_all_sources, by = c("ags" = "ags_county"))

# yearly datasets
map_and_data_state_yearly <- inner_join(ger_shape_state, data_state_yearly, by = c("ags" = "ags_federal_state"))
map_data_state_yearly_combined_all_sources <- inner_join(ger_shape_state, data_state_yearly_combined_all_sources, by = c("ags" = "ags_federal_state"))

map_and_data_county_yearly <- inner_join(ger_shape_county, data_county_yearly, by = c("ags" = "ags_county"))
map_data_county_yearly_combined_all_sources <- inner_join(ger_shape_county, data_county_yearly_combined_all_sources, by = c("ags" = "ags_county"))


# Lage "Windkraft auf See" dropped

# create region IDs
names(map_and_data_state)[names(map_and_data_state) == "Bundesland"] <- "regionid"
names(map_data_state_combined_all_sources)[names(map_data_state_combined_all_sources) == "Bundesland"] <- "regionid"
names(map_and_data_county)[names(map_and_data_county) == "name"] <- "regionid"
names(map_data_county_combined_all_sources)[names(map_data_county_combined_all_sources) == "name"] <- "regionid"

names(map_and_data_state_yearly)[names(map_and_data_state_yearly) == "Bundesland"] <- "regionid"
names(map_data_state_yearly_combined_all_sources)[names(map_data_state_yearly_combined_all_sources) == "Bundesland"] <- "regionid"
names(map_and_data_county_yearly)[names(map_and_data_county_yearly) == "name"] <- "regionid"
names(map_data_county_yearly_combined_all_sources)[names(map_data_county_yearly_combined_all_sources) == "Landkreis"] <- "regionid"


# missing ags_federal_state dropped
table(data_state$ags_federal_state, useNA = "always")
data_state[is.na(data_state$ags_federal_state),]
map_data_state_combined_all_sources[is.na(map_data_state_combined_all_sources$ags_federal_state),]
map_data_state_yearly_combined_all_sources[is.na(map_data_state_yearly_combined_all_sources$ags_federal_state),]

# related to offshore (always if ags_federal_state is missing)

## ...


# ***********************************************************************************************
#### create server ####
server <- function(input, output) {
  
  dataset <- reactive({
    get(paste0("map_and_data_", input$geo_level))
  })

  dataset_combined <- reactive({
    get(paste0("map_data_", input$geo_level, "_combined_all_sources"))
  })
  
  dataset_yearly <- reactive({
    get(paste0("map_and_data_", input$geo_level, "_yearly"))
  })
  
  
  dataset_yearly_combined <- reactive({
    get(paste0("map_data_", input$geo_level, "_yearly_combined_all_sources"))
  })
  
  
  Energy <- reactive({
    if (input$source == "All") {
      dataset_combined()
    } else{
      dataset() %>% filter(EinheitenTyp == input$source)
    }
  })
   
  data_to_download <- reactive({
    if (input$source == "All") {
      dataset()
    } else{
      dataset() %>% filter(EinheitenTyp == input$source)
    }
  })
  
  # ---------- Period --------------------
  Period <- reactive({
    selected_regionid <- input$map_shape_click$id
    
    # Turn the generated "." back into "-" in the state names. e.g. Nordrhein.Westfalen to Nordrhein-Westfalen
    selected_regionid <- gsub(".","-",selected_regionid, fixed = TRUE)
    
    if(is.null(selected_regionid)){
      df <- data_state_yearly_combined_all_sources %>% # since combined later, not important if state or county level
        filter(start_year >= input$years[1]) %>%
        filter(start_year <= input$years[2])
      df
    }else{
      if (input$source == "All"){
        dataset_yearly_combined() %>%
          filter(start_year >= input$years[1]) %>%
          filter(start_year <= input$years[2]) %>%
          filter(regionid == selected_regionid)
      } else{
        dataset_yearly() %>%
          filter(EinheitenTyp == input$source) %>%
          filter(start_year >= input$years[1]) %>%
          filter(start_year <= input$years[2]) %>%
          filter(regionid == selected_regionid)
      }
    }
    
    
    
  })
  
  
  
  # ---------- dataset_region --------------------
  dataset_region <- reactive({
    columns <- c("Solareinheit", "Windeinheit", "Biomasse", "Wasser", "Braunkohle", "Steinkohle", "Gas", "Mineralölprodukte", "Stromspeichereinheit", "Geothermie")
    
    selected_regionid <- input$map_shape_click$id
    
    # Turn the generated "." back into "-" in the state names. e.g. Nordrhein.Westfalen to Nordrhein-Westfalen
    selected_regionid <- gsub(".","-",selected_regionid, fixed = TRUE)

    dataset <- dataset() %>% 
      filter(regionid == selected_regionid) %>% 
      filter(EinheitenTyp %in% columns)
    
    # Highlight the chosen energy type in the bar chart
    dataset$EinheitenTyp[dataset$EinheitenTyp == input$source] <- paste0(input$source, "*")
    dataset
  })
  
  order_coloring <- reactive({
    colors_bar_graph <- data.frame(EinheitenTyp = character(),
                                   colors = character(),
                                   stringsAsFactors = F)
    colors_bar_graph[1:10, ] <- NA
    colors_bar_graph$EinheitenTyp[1:10] <- c("Solareinheit",
                                             "Windeinheit",
                                             "Braunkohle",
                                             "Biomasse",
                                             "Wasser",
                                             "Geothermie",
                                             "Steinkohle",
                                             "Gas",
                                             "Mineralölprodukte",
                                             "Stromspeichereinheit")
    colors_bar_graph$colors <- c("#FFCC33",
                                 "#99CCFF",
                                 "#663300",
                                 "#336633",
                                 "#003366",
                                 "#AA5500", 
                                 "#000000", 
                                 "#999999", 
                                 "#000000", 
                                 "#FFCC00")
    
    colors_merged <- dataset_region() %>%
      reorder(EinheitenTyp, get(input$out_var)) %>%
      inner_join(colors_bar_graph, by = "EinheitenTyp")
    
    colors_merged$colors
    
  })
  
  coloring <- reactive({
    if (input$source == "Solareinheit"){"YlOrRd"}else{
      if (input$source == "Windeinheit"){"Blues"}else{
        if(input$source == "Braunkohle"){"YlOrBr"}else{
          if(input$source == "Biomasse"){"Greens"}else{
            if(input$source == "Wasser"){"Blues"}else{
              if(input$source == "Geothermie"){"Oranges"}else{
                if(input$source == "Steinkohle"){"Greys"}else{
                  if(input$source == "Gas"){"Greys"}else{
                    if(input$source == "Mineralölprodukte"){"Greys"}else{
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
  
  coloring_yearly_graph <- reactive({
    if (input$source == "All"){"#FF6600"}else{
      if (input$source == "Solareinheit"){"#FFCC33"}else{
        if (input$source == "Windeinheit"){"#99CCFF"}else{
          if(input$source == "Braunkohle"){"#663300"}else{
            if(input$source == "Biomasse"){"#336633"}else{
              if(input$source == "Wasser"){"#003366"}else{
                if(input$source == "Geothermie"){"#AA5500"}else{
                  if(input$source == "Steinkohle"){"#000000"}else{
                    if(input$source == "Gas"){"#999999"}else{
                      if(input$source == "Mineralölprodukte"){"#000000"}else{
                        if(input$source == "Stromspeichereinheit"){"#FFCC00"}
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
  
  
  output$title_change_over_time <- renderText({
    selected_regionid <- input$map_shape_click$id
    
    # Turn the generated "." back into "-" in the state names. e.g. Nordrhein.Westfalen to Nordrhein-Westfalen
    selected_regionid <- gsub(".","-", selected_regionid, fixed = TRUE)
    
    # prepare a check for region id to be in
    if (input$geo_level == "state"){
      if (input$source == "All"){
        check <- map_data_state_yearly_combined_all_sources$regionid
      }else{
        check <- map_and_data_state_yearly$regionid
      }
    }else{
      if (input$source == "All"){
        check <- map_data_county_yearly_combined_all_sources$regionid
      }else{
        check <- map_and_data_county_yearly$regionid
      }
    }
    
    if(is.null(selected_regionid)){paste("Please select a region in the map")}else{
      if (!(any(selected_regionid %in% check))){paste("Please select a region in the map")}else{
        if (input$out_var == "n"){paste("Total number of power plants in", selected_regionid)}else{
          if (input$out_var == "sum"){paste("Total power production in", selected_regionid)}else{
            if (input$out_var == "mean"){paste("Average power production per plant in", selected_regionid)}
          }
        }
      }
      
    }
  })
  
  output$map <- renderLeaflet({
    
    p2 <- tm_shape(Energy()) +
      tm_polygons(input$out_var, palette = coloring(), n = input$scale, title = title_legend(), 
                  id = "regionid", popup.vars = c("Value:" = input$out_var)) +
      tm_layout(legend.outside = TRUE) +
      tm_layout(frame = FALSE)
    
    tmap_leaflet(p2)
    
  })
  
  output$plot_energy_bar_chart <- renderPlot({
    if (!is.null(input$map_shape_click)) {
      ggplot(dataset_region(), aes(x = reorder(EinheitenTyp, get(input$out_var)), y = get(input$out_var), fill = EinheitenTyp)) +
        labs(y = title_legend(), x = "Energy Source") +
        geom_bar(stat="identity", fill = coloring_yearly_graph()) + theme_minimal(base_size = 16) + 
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
  
  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      # data_state_solar.csv
      paste0("data_",input$geo_level,"_",input$source, ".csv")
    },
    content = function(file) {
      data_to_download <- data_to_download()
      # geometry data is too big and messsies up with the csv file. can't drop it so this is the solution
      data_to_download$geometry <- NULL
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  output$plot_change_over_time <- renderPlot({
    if (!is.null(input$map_shape_click)) {
      
      ggplot(Period(), aes(x=start_year, y=get(input$out_var), fill=EinheitenTyp))+
        #geom_line(size=1.5) + 
        geom_bar(stat="identity", fill = coloring_yearly_graph()) +
        theme_minimal(base_size = 16) +
        xlab('Year') + ylab(title_legend()) +
        theme(axis.text.x=element_text(angle=90, hjust=1),
              axis.title=element_text(size=18,face="bold"),
              legend.title = element_blank()) +
        theme(legend.position="bottom") +
        scale_fill_discrete(labels = label_energy())
    }else{
      if(FALSE)# work but not fully developed: What about other out_var and other inputs
      {
        df <- data_state_yearly_combined_all_sources %>% # since combined later, not important if state or county level
          filter(start_year >= input$years[1]) %>%
          filter(start_year <= input$years[2])
        
        ggplot(df, aes(x=start_year, y=get(input$out_var)))+
          geom_bar(stat="identity", fill = "steelblue") +
          theme_minimal(base_size = 16) +
          xlab('Year') + ylab(title_legend()) +
          theme(axis.text.x=element_text(angle=90, hjust=1),
                axis.title=element_text(size=18,face="bold"),
                legend.title = element_blank()) +
          ggtitle("National development for all energy sources")
      }
    }
  }, bg="transparent")
}

# ***********************************************************************************************
#### create ui ####
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Explorer", tabName = "data_explorer", icon = icon("th")),
      menuItem("Storyline", tabName = "storyline", icon = icon("dashboard")),
      menuItem("Reference", tabName = "reference", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "data_explorer",
              fluidRow(
                column(width = 4,
                       fluidRow(
                         column(width = 12,
                                h3("German Power Plants Explorer", align = "center"),
                                #titlePanel("German Power Plants Explorer"),
                                sidebarPanel(width = 12,
                                             selectInput('source', 'Energy Source', c("All" = "All", "Solar Energy" = "Solareinheit", "Wind Energy" = "Windeinheit", 
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
                                             # Button
                                             downloadButton("downloadData", "Download Data")
                                )
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
                                h3("Yearly change (flow variable): ", textOutput("title_change_over_time")),
                                plotOutput('plot_change_over_time'),
                                h3("Comparison of energy sources for this region (stock variable)"),
                                plotOutput('plot_energy_bar_chart')
                         )
                       )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "storyline",
              h2("Would you like to participate in adventurous stories?\nJoin our R-Force!"),
              div(class = "text",
                  #p("Because we do not have a story yet,", tags$b("the content is left blank intentionally.")),
                  #p("And I just put these sentences", tags$em("to test the codes."))
                  p("We have connected the data on energy production in Germany to publicly available regional data at state and county level. So far we have some ideas which stories we could tell and are very open to your suggestions and your support!"),
                  p("Do rich regions in Germany have more solar panels?"),
                  p("Are voting results related to protests against wind power plants?"),
                  p("Which questions would you like to explore?")
              )
      ),
      
      # Third tab content
      tabItem(tabName = "reference",
              h2("Reference"),
              div(class = "list",
                  tags$ul(
                    tags$li(tags$b("Data"), ": German power plants raw data is downloaded from official register", tags$a(href="https://www.bundesnetzagentur.de/SharedDocs/Downloads/DE/Sachgebiete/Energie/Unternehmen_Institutionen/ErneuerbareEnergien/ZahlenDatenInformationen/VOeFF_Registerdaten/DatenAb310119.zip", "Marktstammdatenregister"),"."),
                    p("The source provides more than 100 variables for various purposes and we have only used a few of them."),
                    tags$li(tags$b("License"), ": Term of use of MaStR data is subject to", tags$a(href="http://www.gesetze-im-internet.de/mastrv/index.html", "Ordinance on the central electronic directory of energy management data"), "under", tags$a(href="http://www.gesetze-im-internet.de/mastrv/__15.html","Section 15 Public Accessibility of the Data"), "and", tags$a(href="http://www.gesetze-im-internet.de/mastrv/__20.html", "Section 20 Terms of Use"),".")
                  )
              )
      )
    )
  )
)

shinyApp(ui, server)
