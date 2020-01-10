# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "dplyr", "sf", "tmap", "tmaptools", "leaflet")

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

file_to_load <- "enh.csv"

subfolder <- zip_list(here("data", "raw", "shape_germany_bundesland_landkreis.zip"))$filename[1]
subfolder <- gsub("/", "", subfolder, fixed = TRUE)
file_ger_shape <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_sta.shp")
file_ger_shape_state <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_lan.shp")
file_ger_shape_county <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_krs.shp")


# ***********************************************************************************************
#### load data ####
enh <- read.csv2(here("data", "raw", file_to_load), row.names = NULL, encoding = "UTF-8")
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
#### selection [TBD] ####

### ATTENTION!!! [TBD]
# there are several different types of energy production
table(map_and_data$EinheitenTyp)

map_and_data_solar <- map_and_data_state %>%
  filter(EinheitenTyp == "Solareinheit")

# some of them are not in use anymore
map_and_data_solar_current <- map_and_data_solar %>%
  filter(is.na(EndgueltigeStilllegungDatum))

# save for later purposes
saveRDS(map_and_data_solar_current, here("data", "processed", "map_and_data_solar_current.rds")) 


# ***********************************************************************************************
#### aggregate data on county level ####
map_and_data_solar_current$EinheitenTyp <- as.character(map_and_data_solar_current$EinheitenTyp)
table(map_and_data_solar_current$EinheitenTyp)

map_and_data_solar_current_county <- map_and_data_solar_current %>%
  select(Plz, Ort, Bundesland, Landkreis, Gemeinde, Nettonennleistung, EinheitenTyp) %>%
  group_by(Landkreis) %>%
  summarize(n = length(Landkreis), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung),
            Einheitentyp = first(EinheitenTyp))

# check if number of counties is the same as in the shape file
length(map_and_data_solar_current_county$Landkreis) == (nrow(ger_shape_county) - length(ger_shape_county$Landkreis[duplicated(ger_shape_county$Landkreis)]))
# which counties are not in the shape file
map_and_data_solar_current_county$Landkreis[which(!map_and_data_solar_current_county$Landkreis %in% ger_shape_county$Landkreis)]
# -> [TBD] Why do these counties have no shape information? Do they belong to another county? ...


# merge shape file after aggregation
map_and_data_solar_current_county <- inner_join(map_and_data_solar_current_county, ger_shape_county)


# save data
saveRDS(map_and_data_solar_current_county, here("data", "processed", "map_and_data_solar_current_county.rds")) 



# ***********************************************************************************************
#### aggregate data further on state level ####
map_and_data_solar_current_state <- map_and_data_solar_current %>%
  group_by(Bundesland) %>%
  summarize(n = length(Bundesland), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung),
            Einheitentyp = first(EinheitenTyp))

# merge shape file after aggregation
map_and_data_solar_current_state <- inner_join(map_and_data_solar_current_state, ger_shape_state)

# save
saveRDS(map_and_data_solar_current_state, here("data", "processed", "map_and_data_solar_current_state.rds")) 



# ***********************************************************************************************
file.edit(here("code", "3.show_map.R"))
