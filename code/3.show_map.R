# **********************************************************************************************
#### D E F I N I T I O N S ####

file_to_load <- "enh.csv"
file_ger_shape_plz <- here("data", "raw", "shape_plz", "plz-gebiete.shp")
file_ger_shape_state <- here("data", "raw", "shape_state", "plz-1stellig.shp")

# ***********************************************************************************************
#### installing, loading libraries ####
packages <- c("here", "dplyr", "sf", "tmap", "tmaptools", "leaflet", "ggplot2")

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
map_and_data_solar_current_plz <- readRDS(here("data", "processed", "map_and_data_solar_current_plz.rds"))
map_and_data_solar_current_state <- readRDS(here("data", "processed", "map_and_data_solar_current_state.rds"))



# ***********************************************************************************************
#### show map ####
## Plz: Nettonennleistung
map1_netto <- ggplot(map_and_data_solar_current_plz, aes(geometry = geometry)) +
  geom_sf(aes(fill = sum)) +
  scale_fill_gradient(low = "#B6FF52", high = "#3B5518") +
  ggtitle("Sum of solar plants' power at community level (Postcode 5-digit)")
map1_netto
# --> takes some time
ggsave(here("graphs","map1_netto.pdf"))

# lightgreen #B6FF52
# darkgreen #3B5518

## Plz: amount n of solar plants
map1_n <- ggplot(map_and_data_solar_current_plz, aes(geometry = geometry)) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(low = "#B6FF52", high = "#3B5518") +
  ggtitle("Number of solar plants at community level (Postcode 5-digit)")
map1_n
# --> takes some time
ggsave(here("graphs","map1_n.pdf"))
# *********************************************

### second map on state level
# power in total
map2_netto <- ggplot(map_and_data_solar_current_state, aes(geometry = geometry)) +
  geom_sf(aes(fill = sum)) +
  scale_fill_gradient(low = "#B6FF52", high = "#3B5518") +
  ggtitle("Sum of solar plants' power at 'state' level (Postcode 1-digit)")
map2_netto

ggsave(here("graphs","map2_netto.pdf"))


# number of solar plants at state level
map2_n <- ggplot(map_and_data_solar_current_state, aes(geometry = geometry)) +
  geom_sf(aes(fill = n)) +
  scale_fill_gradient(low = "#B6FF52", high = "#3B5518") +
  ggtitle("Number of solar plants at 'state' level (Postcode 1-digit)")
map2_n

ggsave(here("graphs","map2_n.pdf"))

# ***********************************************************************************************
### try tmap [TBD]
#tm_shape(map_and_data_solar_current_plz) +