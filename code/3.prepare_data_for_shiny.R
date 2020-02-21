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
map_data_state <- left_join(ger_shape_state, data_state, by = c("ags" = "ags_federal_state"))
map_data_state_combined_all_sources <- left_join(ger_shape_state, data_state_combined_all_sources, by = c("ags" = "ags_federal_state"))
map_data_county <- left_join(ger_shape_county, data_county, by = c("ags" = "ags_county"))
map_data_county_combined_all_sources <- left_join(ger_shape_county, data_county_combined_all_sources, by = c("ags" = "ags_county"))

# yearly datasets
map_data_state_yearly <- left_join(ger_shape_state, data_state_yearly, by = c("ags" = "ags_federal_state"))
map_data_state_yearly_combined_all_sources <- left_join(ger_shape_state, data_state_yearly_combined_all_sources, by = c("ags" = "ags_federal_state"))

map_data_county_yearly <- left_join(ger_shape_county, data_county_yearly, by = c("ags" = "ags_county"))
map_data_county_yearly_combined_all_sources <- left_join(ger_shape_county, data_county_yearly_combined_all_sources, by = c("ags" = "ags_county"))


# Lage "Windkraft auf See" dropped

# create region IDs
names(map_data_state)[names(map_data_state) == "Bundesland"] <- "regionid"
names(map_data_state_combined_all_sources)[names(map_data_state_combined_all_sources) == "Bundesland"] <- "regionid"
names(map_data_county)[names(map_data_county) == "name"] <- "regionid"
names(map_data_county_combined_all_sources)[names(map_data_county_combined_all_sources) == "name"] <- "regionid"

names(map_data_state_yearly)[names(map_data_state_yearly) == "Bundesland"] <- "regionid"
names(map_data_state_yearly_combined_all_sources)[names(map_data_state_yearly_combined_all_sources) == "Bundesland"] <- "regionid"
names(map_data_county_yearly)[names(map_data_county_yearly) == "name"] <- "regionid"
names(map_data_county_yearly_combined_all_sources)[names(map_data_county_yearly_combined_all_sources) == "Landkreis"] <- "regionid"


# missing ags_federal_state dropped
table(data_state$ags_federal_state, useNA = "always")
data_state[is.na(data_state$ags_federal_state),]
map_data_state_combined_all_sources[is.na(map_data_state_combined_all_sources$ags_federal_state),]
map_data_state_yearly_combined_all_sources[is.na(map_data_state_yearly_combined_all_sources$ags_federal_state),]

saveRDS(map_data_state_combined_all_sources, file = here::here("data", "processed","map_data_state_combined_all_sources.rds"))
saveRDS(map_data_state_combined_all_sources, file =  here::here("data", "processed","map_data_state_combined_all_sources.rds"))
saveRDS(map_data_county_combined_all_sources, file =  here::here("data", "processed","map_data_county_combined_all_sources.rds"))
saveRDS(map_data_state_yearly_combined_all_sources, file =  here::here("data", "processed","map_data_state_yearly_combined_all_sources.rds"))
saveRDS(map_data_county_yearly_combined_all_sources, file =  here::here("data", "processed","map_data_county_yearly_combined_all_sources.rds"))
saveRDS(map_data_state, file = here::here("data", "processed","map_data_state.rds"))
saveRDS(map_data_county, file =  here::here("data", "processed","map_data_county.rds"))
saveRDS(map_data_state_yearly, file =  here::here("data", "processed","map_data_state_yearly.rds"))
saveRDS(map_data_county_yearly, file =  here::here("data", "processed","map_data_county_yearly.rds"))

# related to offshore (always if ags_federal_state is missing)

## ...
