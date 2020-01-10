# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "dplyr", "sf", "tmap", "tmaptools", "leaflet", "ggplot2")

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


# ***********************************************************************************************
#### load data ####
map_and_data_solar_current_county <- readRDS(here("data", "processed", "map_and_data_solar_current_county.rds"))
map_and_data_solar_current_state <- readRDS(here("data", "processed", "map_and_data_solar_current_state.rds"))



# ***********************************************************************************************
#### show map ####
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
### try tmap [TBD]
#tm_shape(map_and_data_solar_current_plz) +

# ***********************************************************************************************
# ***********************************************************************************************
file.edit(here("code", "4.shiny.R"))
