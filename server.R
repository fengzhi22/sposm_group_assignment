library("utils")
library("here")
library("shiny")
library("zip")
library("sf")
library("tmap")
library("tmaptools")
library("ggplot2")
library("dplyr")
library("shinydashboard")
library("leaflet")

map_data_state_combined_all_sources <- readRDS(here::here("data","processed","map_data_state_combined_all_sources.rds"))
map_data_state_yearly_combined_all_sources <- readRDS(here::here("data","processed","map_data_state_yearly_combined_all_sources.rds"))
map_data_county_combined_all_sources <- readRDS(here::here("data","processed","map_data_county_combined_all_sources.rds"))
map_data_county_yearly_combined_all_sources <- readRDS(here::here("data","processed","map_data_county_yearly_combined_all_sources.rds"))
map_data_county <- readRDS(here::here("data","processed","map_data_county.rds"))
map_data_state <- readRDS(here::here("data","processed","map_data_state.rds"))
map_data_county_yearly <- readRDS(here::here("data","processed","map_data_county_yearly.rds"))
map_data_state_yearly <- readRDS(here::here("data","processed","map_data_state_yearly.rds"))

data_state_yearly <- read.csv2(here::here("data","processed","data_state_yearly.csv"), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
new_data_state <- subset(data_state_yearly, start_year >= "2000")


# ---------------------------------- First Story on Solar and Income ------------------------------
## load
data_state_solar_income_2015 <- read.csv2(here::here("data", "processed", paste0("data_state_solar_income_2015", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)
## load
data_county_solar_income_2015 <- read.csv2(here::here("data", "processed", paste0("data_county_solar_income_2015", ".csv")), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)


#### create server ####
server <- function(input, output) {
  
  dataset <- reactive({
    get(paste0("map_data_", input$geo_level))
  })
  
  dataset_combined <- reactive({
    get(paste0("map_data_", input$geo_level, "_combined_all_sources"))
  })
  
  dataset_yearly <- reactive({
    get(paste0("map_data_", input$geo_level, "_yearly"))
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
        check <- map_data_state_yearly$regionid
      }
    }else{
      if (input$source == "All"){
        check <- map_data_county_yearly_combined_all_sources$regionid
      }else{
        check <- map_data_county_yearly$regionid
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
  
  
  output$plot_state_income_n <- renderPlot({
    ggplot(data_state_solar_income_2015, aes(x=income_per_tax_person, y=solar_plants_per_1000_tax_person)) +
      expand_limits(x = c(25000, 45000), y = 0) +
      geom_point() +
      geom_smooth(method = lm, fullrange = T, se = F) +
      #stat_smooth(method = lm) +
      ggtitle("Relation of Income and Solar Plants at state level") +
      ylab("Solar plants per 1000 tax persons") +
      xlab("Income per tax person in Euros per year") +
      geom_text(aes(label=name), vjust = 1.2) +
      theme(panel.background = element_rect(fill = "transparent"), 
            plot.background = element_rect(fill = "transparent", color = NA))
  }, bg="transparent")
  
  output$plot_state_income_mean <- renderPlot({
    ggplot(data_state_solar_income_2015, aes(x=income_per_tax_person, y=mean)) +
      expand_limits(x = c(25000, 45000), y = 0) +
      geom_point() +
      geom_smooth(method = lm, fullrange = T, se = F) +
      #stat_smooth(method = lm) +
      ggtitle("Relation of Income and Solar Plants at state level") +
      ylab("Mean power of solar plants in Watt") +
      xlab("Income per tax person in Euros per year") +
      geom_text(aes(label=name), vjust = 1.2) +
      theme(panel.background = element_rect(fill = "transparent"), 
            plot.background = element_rect(fill = "transparent", color = NA))
    
  }, bg="transparent")
  
  output$plot_county_income_n <- renderPlot({
    ggplot(data_county_solar_income_2015, aes(x=income_per_tax_person, y=solar_plants_per_1000_tax_person)) +
      expand_limits(x = c(25000, 70000), y = 0) +
      geom_point() +
      #geom_smooth(fullrange = T, se = F) +
      #stat_smooth(method = lm) +
      ggtitle("Relation of Income and Solar Plants at county level") +
      ylab("Solar plants per 1000 tax persons") +
      xlab("Income per tax person in Euros per year") +
      #geom_text(aes(label=name), vjust = 1.2) +
      theme(panel.background = element_rect(fill = "transparent"), 
            plot.background = element_rect(fill = "transparent", color = NA))
    
  }, bg="transparent")
  
  output$plot_county_income_mean <- renderPlot({
    ggplot(data_county_solar_income_2015, aes(x=income_per_tax_person, y=mean)) +
      expand_limits(x = c(25000, 70000), y = 0) +
      geom_point() +
      geom_smooth(method = lm, fullrange = T, se = F) +
      #stat_smooth(method = lm) +
      ggtitle("Relation of Income and Solar Plants at county level") +
      ylab("Mean power of solar plants in Watt") +
      xlab("Income per tax person in Euros per year") +
      #geom_text(aes(label=name), vjust = 1.2) +
      theme(panel.background = element_rect(fill = "transparent"), 
            plot.background = element_rect(fill = "transparent", color = NA))
  }, bg="transparent")
}
