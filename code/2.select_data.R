# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "zip", "dplyr", "sf")

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


# ***********************************************************************************************
#### load data ####
enh <- read.csv2(here("data", "raw", file_to_load), row.names = NULL, encoding = "UTF-8")



# ***********************************************************************************************
#### selection [TBD] ####

### ATTENTION!!! [TBD]
# there are several different types of energy production
table(enh$EinheitenTyp)

data_solar_wind <- enh %>%
  filter(EinheitenTyp %in% c("Solareinheit", "Windeinheit"))

table(data_solar_wind$EinheitenTyp)

# some of them might not be in use anymore
data_solar_wind_current <- data_solar_wind %>%
  filter(is.na(EndgueltigeStilllegungDatum))

# save for later purposes
saveRDS(data_solar_wind_current, here("data", "processed", "data_solar_wind_current.rds")) 



# ***********************************************************************************************
file.edit(here("code", "3.create_maps.R"))
