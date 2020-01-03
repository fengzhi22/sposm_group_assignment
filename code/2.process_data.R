# **********************************************************************************************
#### D E F I N I T I O N S ####
file_to_load <- "enh.csv"
file_ger_shape_plz <- here("data", "raw", "shape_plz", "plz-gebiete.shp")
file_ger_shape_state <- here("data", "raw", "shape_state", "plz-1stellig.shp")

# ***********************************************************************************************
#### installing, loading libraries ####
packages <- c("here", "dplyr", "sf", "tmap", "tmaptools", "leaflet")

### install if necessary
lapply(packages, 
       function(x)
       {
           if(!(x %in% installed.packages()))
           {
               install.packages(x)  
           }
       })

lapply(packages, require, character.only = T)


# ***********************************************************************************************
#### load data ####
enh <- read.csv2(here("data", "raw", file_to_load), row.names = NULL, encoding = "UTF-8")
ger_shape_plz <- st_read(file_ger_shape_plz, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)
ger_shape_state <- st_read(file_ger_shape_state, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)

# prepare merge
names(ger_shape_plz)[names(ger_shape_plz) == "plz"] <- "Plz"
ger_shape_plz$Plz <- as.integer(ger_shape_plz$Plz)

# ***********************************************************************************************
#### match shape file on Plz ####
map_and_data <- inner_join(enh, ger_shape_plz)

which(is.na(map_and_data$geometry))
# --> match complete


# ***********************************************************************************************
#### selection [TBD] ####

### ATTENTION!!! [TBD]
# there are several different types of energy production
table(map_and_data$EinheitenTyp)

map_and_data_solar <- map_and_data %>%
  filter(EinheitenTyp == "Solareinheit")

# some of them are not in use anymore
map_and_data_solar_current <- map_and_data_solar %>%
  filter(is.na(EndgueltigeStilllegungDatum))


# ***********************************************************************************************
#### aggregate data on Plz ####
map_and_data_solar_current_plz <- map_and_data_solar_current %>%
  select(Plz, Ort, Bundesland, Gemeinde, Nettonennleistung, note, geometry) %>%
  group_by(Plz) %>%
  summarize(n = length(Plz), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung),
            Bundesland = first(Bundesland))

# problems with geometry: inner_join again after aggregation
map_and_data_solar_current_plz <- inner_join(map_and_data_solar_current_plz, ger_shape_plz)

table(map_and_data_solar_current_plz$Bundesland)


# ***********************************************************************************************
#### aggregate data further on sate level ####
map_and_data_solar_current_plz_ <- map_and_data_solar_current_plz %>%
  mutate(Plz1 = substr(Plz, 1, 1))

map_and_data_solar_current_state <- map_and_data_solar_current_plz_ %>%
  group_by(Plz1) %>%
  summarize(n = sum(n), mean = mean(mean), sum = sum(sum))

names(ger_shape_state)[names(ger_shape_state) == "plz"] <- "Plz1"

map_and_data_solar_current_state <- inner_join(map_and_data_solar_current_state, ger_shape_state)

# ***********************************************************************************************
#### save data that was a "long" time calculated ####
saveRDS(map_and_data_solar_current_plz, here("data", "processed", "map_and_data_solar_current_plz.rds")) 
saveRDS(map_and_data_solar_current_state, here("data", "processed", "map_and_data_solar_current_state.rds")) 

# ***********************************************************************************************
file.edit(here("code", "3.show_map.R"))
