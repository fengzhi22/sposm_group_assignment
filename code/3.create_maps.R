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
data_solar_current <- readRDS(here("data", "processed", "data_solar_current.rds"))

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
data_solar_current$EinheitenTyp <- as.character(data_solar_current$EinheitenTyp)
table(data_solar_current$EinheitenTyp)

# consolidate
data_solar_current_county <- data_solar_current %>%
  select(Plz, Ort, Bundesland, Landkreis, Gemeinde, Nettonennleistung, EinheitenTyp) %>%
  group_by(Landkreis) %>%
  summarize(n = length(Landkreis), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung),
            Einheitentyp = first(EinheitenTyp))

# check if number of counties is the same as in the shape file
length(data_solar_current_county$Landkreis) == (nrow(ger_shape_county) - length(ger_shape_county$Landkreis[duplicated(ger_shape_county$Landkreis)]))
# which counties are not in the shape file
data_solar_current_county$Landkreis[which(!data_solar_current_county$Landkreis %in% ger_shape_county$Landkreis)]
# -> [TBD] Why do these counties have no shape information? Do they belong to another county? ...
# -> quick-fix: merge using the shape file as original

# merge shape file after aggregation
map_and_data_solar_current_county <- inner_join(ger_shape_county, data_solar_current_county)



# ***********************************************************************************************
#### aggregate data on state level ####
data_solar_current_state <- data_solar_current %>%
  group_by(Bundesland) %>%
  summarize(n = length(Bundesland), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung),
            Einheitentyp = first(EinheitenTyp))

# merge shape file after aggregation
map_and_data_solar_current_state <- inner_join(ger_shape_state, data_solar_current_state)



# ***********************************************************************************************
#### create map using ggplot ####
## Landkreis: Nettonennleistung
map1_netto <- ggplot(map_and_data_solar_current_county, aes(geometry = geometry)) +
  geom_sf(aes(fill = sum)) +
  scale_fill_gradient(low = "#C6FF72", high = "#2B5508") +
  ggtitle("Sum of solar plants' power at community level (Landkreis)")
map1_netto

ggsave(here("graphs","map1_netto.pdf"))

# lightgreen #C6FF72
# darkgreen #2B5508

## Landkreis: amount n of solar plants
map1_n <- ggplot(map_and_data_solar_current_county, aes(geometry = geometry)) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(low = "#C6FF72", high = "#2B5508") +
  ggtitle("Number of solar plants at community level (Landkreis)")
map1_n
# --> takes some time
ggsave(here("graphs","map1_n.pdf"))
# *********************************************

### second map on state level
# power in total
map2_netto <- ggplot(map_and_data_solar_current_state, aes(geometry = geometry)) +
  geom_sf(aes(fill = sum)) +
  scale_fill_gradient(low = "#C6FF72", high = "#2B5508") +
  ggtitle("Sum of solar plants' power at 'state' level (Bundesland)")
map2_netto

ggsave(here("graphs","map2_netto.pdf"))


# number of solar plants at state level
map2_n <- ggplot(map_and_data_solar_current_state, aes(geometry = geometry)) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(low = "#C6FF72", high = "#2B5508") +
  ggtitle("Number of solar plants at 'state' level (Bundesland)")
map2_n

ggsave(here("graphs","map2_n.pdf"))



# ***********************************************************************************************
#### create map using tmap ####
# another example
#data("World")
#tm_shape(World) +
#  tm_polygons("HPI")

## see for different color palettes
#tmaptools::palette_explorer()

### state
tm_shape(map_and_data_solar_current_state) +
  tm_polygons("n", palette = "Greens", n = 10, title = "Number") +
  #tm_text("n") +
  tm_symbols(n = 10, scale = 1, alpha = 0.9,
             size = "sum",
             col = "sum",
             title.size = "Power",
             title.col = "Power",
             palette = "Blues") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'state' level\n(Bundesland)")

# [TBD]: What is this warning about?
sf::st_is_valid()


## reduced
tm_shape(map_and_data_solar_current_state) +
  tm_polygons("n", palette = "Greens", n = 10, title = "Number") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'state' level\n(Bundesland)")
tmap_save(here("graphs","map2_n.pdf"))

tm_shape(map_and_data_solar_current_state) +
  tm_polygons("sum", palette = "Greens", n = 10, title = "Power") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'state' level\n(Bundesland)")




### county
tm_shape(map_and_data_solar_current_county) +
  tm_polygons("sum", palette = "Greens", n = 10, title = "Power") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'county' level\n(Landkreis)")



tm_shape(map_and_data_solar_current_county) +
  tm_polygons("n", palette = "Greens", n = 10, title = "Number") +
  tm_layout(legend.outside = TRUE,
            title = "Solar plants\nat 'county' level\n(Landkreis)")




# ***********************************************************************************************
# ***********************************************************************************************
file.edit(here("code", "4.shiny.R"))
