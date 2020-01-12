# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "sf", "tmap", "tmaptools", "leaflet", "ggplot2", "dplyr")

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
data_solar_wind_current$EinheitenTyp <- as.character(data_solar_wind_current$EinheitenTyp)
table(data_solar_wind_current$EinheitenTyp)


# check if number of counties is the same as in the shape file
length(data_solar_wind_current_county$Landkreis) == (nrow(ger_shape_county) - length(ger_shape_county$Landkreis[duplicated(ger_shape_county$Landkreis)]))
# which counties are not in the shape file
data_solar_wind_current_county$Landkreis[which(!data_solar_wind_current_county$Landkreis %in% ger_shape_county$Landkreis)]
# -> [TBD] Why do these counties have no shape information? Do they belong to another county? ...
# -> quick-fix: merge using the shape file as original

# have a look at NA counties: Are these off-shore wind-turbines?
entries <- which(is.na(data_solar_wind_current$Landkreis))
length(entries)
table(data_solar_wind_current$EinheitenTyp[entries])
table(data_solar_wind_current$Lage[entries])
# -> yes, it's offshore: [TBD] How to deal with them?

# name them "Off-Shore"
data_solar_wind_current$Landkreis <- as.character(data_solar_wind_current$Landkreis)
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
#### save ####
saveRDS(map_and_data_solar_wind_current_county, here("data", "processed", "map_and_data_solar_wind_current_county.rds"))
saveRDS(map_and_data_solar_wind_current_state ,here("data", "processed", "map_and_data_solar_wind_current_state.rds"))
# [TBD] loading RDS files and using it with tmap raised an error
# --> That's why there might be problems with tmap in Shiny, 
# if not loading and assembling the shape file and data within Shiny



# ***********************************************************************************************
#### create map using ggplot ####

selection_source_county <- map_and_data_solar_wind_current_county %>%
  filter(EinheitenTyp == "Solareinheit")

selection_source_state <- map_and_data_solar_wind_current_state %>%
  filter(EinheitenTyp == "Solareinheit")


## Landkreis: Nettonennleistung
# lightgreen #C6FF72
# darkgreen #2B5508


## Landkreis: amount n of solar plants
map1_n <- ggplot(selection_source_county, aes(geometry = geometry)) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(low = "#C6FF72", high = "#2B5508") +
  ggtitle("Number of solar plants\nat community level (Landkreis)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
map1_n

ggsave(here("graphs","map1_n.pdf"))


map1_netto <-  ggplot(selection_source_county, aes(geometry = geometry)) +
  geom_sf(aes(fill = sum)) +
  scale_fill_gradient(low = "#C6FF72", high = "#2B5508") +
  ggtitle("Sum of solar plants' power at community level (Landkreis)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
map1_netto

ggsave(here("graphs","map1_netto.pdf"))

# *********************************************

### second map on state level
# number of solar plants at state level
map2_n <- ggplot(selection_source_state, aes(geometry = geometry)) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(low = "#C6FF72", high = "#2B5508") +
  ggtitle("Number of solar plants at 'state' level (Bundesland)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
map2_n

ggsave(here("graphs","map2_n.pdf"))


# power in total
map2_netto <- ggplot(selection_source_state, aes(geometry = geometry)) +
  geom_sf(aes(fill = sum)) +
  scale_fill_gradient(low = "#C6FF72", high = "#2B5508") +
  ggtitle("Sum of solar plants' power at 'state' level (Bundesland)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5))
map2_netto

ggsave(here("graphs","map2_netto.pdf"))




# ***********************************************************************************************
#### create map using tmap ####
# another example
#data("World")
#tm_shape(World) +
#  tm_polygons("HPI")

## see for different color palettes
#tmaptools::palette_explorer()

### state
tm_shape(selection_source_state) +
  tm_polygons("n", palette = "Greens", n = 10, title = "Number") +
  #tm_text("n") +
  tm_symbols(n = 10, scale = 1, alpha = 0.9,
             size = "sum",
             col = "sum",
             title.size = "Power",
             title.col = "Power",
             palette = "Blues") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'state' level\n(Bundesland)") + 
  tm_layout(frame = FALSE)

# [TBD]: What is this warning about?
sf::st_is_valid()


## reduced
tmob_solar_state_number <- tm_shape(selection_source_state) +
  tm_polygons("n", palette = "Greens", n = 10, title = "Number") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'state' level\n(Bundesland)")
tmob_solar_state_number
tmap_save(tmob_solar_state_number, here("graphs","tmap_solar_state_number.pdf"))


tmob_solar_state_power <- tm_shape(selection_source_state) +
  tm_polygons("sum", palette = "Greens", n = 10, title = "Power in kw(p)") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'state' level\n(Bundesland)")
tmob_solar_state_power
tmap_save(tmob_solar_state_power, here("graphs","tmap_solar_state_power.pdf"))


### county
tmob_solar_county_number <- tm_shape(selection_source_county) +
  tm_polygons("n", palette = "Greens", n = 10, title = "Number") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'county' level\n(Landkreis)")
tmob_solar_county_number
tmap_save(tmob_solar_county_number, here("graphs","tmap_solar_county_number.pdf"))


tmob_solar_county_power <- tm_shape(selection_source_county) +
  tm_polygons("sum", palette = "Greens", n = 10, title = "Power in kw(p)") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'county' level\n(Landkreis)")
tmob_solar_county_power
tmap_save(tmob_solar_county_power, here("graphs","tmap_solar_county_power.pdf"))



# ***********************************************************************************************
#### wind plots ####
selection_source_county <- map_and_data_solar_wind_current_county %>%
  filter(EinheitenTyp == "Windeinheit")

selection_source_state <- map_and_data_solar_wind_current_state %>%
  filter(EinheitenTyp == "Windeinheit")

# graphics
tmob_wind_state_number <- tm_shape(selection_source_state) +
  tm_polygons("n", palette = "Blues", n = 10, title = "Number") +
  tm_layout(legend.outside = TRUE,
            title = "Wind power plants\nat 'state' level\n(Bundesland)")
tmob_wind_state_number
tmap_save(tmob_wind_state_number, here("graphs","tmap_wind_state_number.pdf"))


tmob_wind_state_power <- tm_shape(selection_source_state) +
  tm_polygons("sum", palette = "Blues", n = 10, title = "Power in kw") +
  tm_layout(legend.outside = TRUE,
            title = "Wind power plants\nat 'state' level\n(Bundesland)")
tmob_wind_state_power
tmap_save(tmob_wind_state_power, here("graphs","tmap_wind_state_power.pdf"))


### county
tmob_wind_county_number <- tm_shape(selection_source_county) +
  tm_polygons("n", palette = "Blues", n = 10, title = "Number") +
  tm_layout(legend.outside = TRUE,
            title = "Wind power plants\nat 'county' level\n(Landkreis)")
tmob_wind_county_number
tmap_save(tmob_wind_county_number, here("graphs","tmap_wind_county_number.pdf"))


tmob_wind_county_power <- tm_shape(selection_source_county) +
  tm_polygons("sum", palette = "Blues", n = 10, title = "Power in kw") +
  tm_layout(legend.outside = TRUE,
            title = "Wind power plants\nat 'county' level\n(Landkreis)")
tmob_wind_county_power
tmap_save(tmob_wind_county_power, here("graphs","tmap_wind_county_power.pdf"))






# ***********************************************************************************************
file.edit(here("code", "4.2shiny_solar_wind.R"))
