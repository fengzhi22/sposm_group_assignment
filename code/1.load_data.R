# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "httr", "readxl", "ggplot2")

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
# source to download
url <- "https://www.bundesnetzagentur.de/SharedDocs/Downloads/DE/Sachgebiete/Energie/Unternehmen_Institutionen/ErneuerbareEnergien/ZahlenDatenInformationen/VOeFF_Registerdaten/DatenAb310119.zip;jsessionid=54BBA736A5A4D71D90BD397228DBE629?__blob=publicationFile&v=7"
file_to_load <- "DatenAb310119.zip"

url_shape_ger <- "https://daten.gdz.bkg.bund.de/produkte/vg/vg2500/aktuell/vg2500_01-01.gk3.shape.zip"
file_ger_shape <- "shape_germany_bundesland_landkreis.zip"

#url_shape_plz <- "https://www.suche-postleitzahl.org/download_files/public/plz-gebiete.shp.zip"
#file_ger_shape_plz <- "shape_germany_plz.zip"

#url_shape_state <- "https://www.suche-postleitzahl.org/download_files/public/plz-1stellig.shp.zip"
#file_ger_shape_state <- "shape_germany_state.zip"

# more shape files:
# https://gdz.bkg.bund.de/index.php/default/digitale-geodaten/verwaltungsgebiete/verwaltungsgebiete-1-2-500-000-stand-01-01-vg2500.html
# https://www.suche-postleitzahl.org/downloads
# https://mapcruzin.com/free-germany-arcgis-maps-shapefiles.htm

# MaStR explanations of variables
url_variables_expl <- "https://www.bundesnetzagentur.de/SharedDocs/Downloads/DE/Sachgebiete/Energie/Unternehmen_Institutionen/DatenaustauschUndMonitoring/MaStR/MaStR%20-%20Erl%C3%A4uterungstexte%20Einheiten_180921.xlsx?__blob=publicationFile&v=4"


# ***********************************************************************************************
#### load data ####

input <- here("data", "raw")

# store data if not yet existing
if(!file.exists(here("data", "raw", file_to_load)))
{
  download.file(url, here("data", "raw", file_to_load))
}

if(!file.exists(here("data", "raw", file_ger_shape)))
{
  download.file(url_shape_ger, here("data", "raw", file_ger_shape))
}


#if(!file.exists(here("data", "raw", file_ger_shape_plz)))
#{
#  download.file(url_shape_plz, here("data", "raw", file_ger_shape_plz))
#}

#if(!file.exists(here("data", "raw", file_ger_shape_state)))
#{
#  download.file(url_shape_state, here("data", "raw", file_ger_shape_state))
#}

# save information on MaStR variables
if(!file.exists(here("resources", file_ger_shape_state)))
{
  download.file(url_variables_expl, here("resources", "MaStR_unit_explanations.xlsx"))
}


# ***********************************************************************************************
#### de-zip ####
zipF <- here("data", 'raw', file_to_load)
outDir <- here("data", 'raw')
unzip(zipF, exdir = outDir)
# If unzipping raises problems as we have experienced, you might download the file manually
# and save it in your git repository under data/raw/
# https://www.bundesnetzagentur.de/SharedDocs/Downloads/DE/Sachgebiete/Energie/Unternehmen_Institutionen/ErneuerbareEnergien/ZahlenDatenInformationen/VOeFF_Registerdaten/DatenAb310119.zip

xlsx_to_load <- zip_list(zipF)$filename


zipF2 <- here("data", 'raw', file_ger_shape)
outDir <-here("data", 'raw', "shape_ger")
unzip(zipF2, exdir = outDir)


#zipF2 <- here("data", 'raw', file_ger_shape_plz)
#outDir <-here("data", 'raw', "shape_plz")
#unzip(zipF2, exdir = outDir)

#zipF3 <- here("data", 'raw', file_ger_shape_state)
#outDir <-here("data", 'raw', "shape_state")
#unzip(zipF3, exdir = outDir)

# ***********************************************************************************************
#### read excel file ####
tabelle_enh <- read_excel(here("data", "raw", xlsx_to_load), sheet = "Tabelle_ENH")


# ***********************************************************************************************
#### prepare relevant data ####
vars <- c("ENH_MastrID", "ENH_MastrNummer", "ENH_Systemstatus", "ENH_Betriebsstatus", "ENH_EinheitenTyp", 
           "ENH_InbetriebnahmeDatum", "ENH_EndgueltigeStilllegungDatum", "ENH_Lage", "ENH_Plz", "ENH_Ort",
          "ENH_Bundesland", "ENH_Landkreis", "ENH_Gemeinde", "ENH_Breitengrad", "ENH_Laengengrad",
          "ENH_Bruttoleistung", "ENH_Nettonennleistung", "ENH_Nutzungsbereich", "ENH_Energietraeger",
          "ENH_EinspeisungsArt")

# take just if country is Germany (just some other cases)
# "ENH_land" = "Deutschland"
enh <- tabelle_enh[tabelle_enh$ENH_Land == "Deutschland", vars]
names(enh) <- gsub("ENH_", "",names(enh), fixed = TRUE)

## save as csv for faster loading next time
write.csv2(enh, file = here("data", "raw", "enh.csv"), row.names = FALSE, fileEncoding="UTF-8")



# ***********************************************************************************************
#### load next code file ####
file.edit(here("code", "2.select_data.R"))
