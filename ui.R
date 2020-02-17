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
              #br(),
              div(class = "text",
                  p("We have connected the data on energy production in Germany to publicly available regional data at state and county level. So far we have some ideas which stories we could tell and are very open to your suggestions and your support!"),
                  p(tags$b("Do rich regions in Germany have more solar power plants?")),
                  p(tags$b("Do rich regions in Germany have larger solar power plants?"))
                  #p("Are voting results related to protests against wind power plants?"),
                  #p("Which questions would you like to explore?")
              ),
              br(),
              h3("Income and Solar Plants at state level"),
              h4("Average yearly income and total amount of solar power plants"),
              br(),
              plotOutput("plot_state_income_n"),
              br(),
              br(),
              br(),
              
              h4("Average yearly income and mean power of plant in Watt"),
              br(),
              plotOutput("plot_state_income_mean"),
              br(),
              br(),
              
              h3("Income and Solar Plants at county level"),
              h4("Average yearly income and total amount of solar power plants"),
              br(),
              plotOutput("plot_county_income_n"),
              br(),
              br(),
              br(),
              
              h4("Average yearly income and mean power of plant in Watt"),
              br(),
              plotOutput("plot_county_income_mean")
              
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


