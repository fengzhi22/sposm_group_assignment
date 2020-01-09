# **********************************************************************************************
#### D E F I N I T I O N S ####
# source to download
url <- "https://www.bundesnetzagentur.de/SharedDocs/Downloads/DE/Sachgebiete/Energie/Unternehmen_Institutionen/ErneuerbareEnergien/ZahlenDatenInformationen/VOeFF_Registerdaten/DatenAb310119.zip;jsessionid=8E141CCB4412E0706DDA7F950F37B970?__blob=publicationFile&v=7"
file_to_load <- "DatenAb310119.zip"

url_shape_plz <- "https://www.suche-postleitzahl.org/download_files/public/plz-gebiete.shp.zip"
file_ger_shape_plz <- "shape_germany_plz.zip"

url_shape_state <- "https://www.suche-postleitzahl.org/download_files/public/plz-1stellig.shp.zip"
file_ger_shape_state <- "shape_germany_state.zip"

# more shape files:
# https://www.suche-postleitzahl.org/downloads
# https://mapcruzin.com/free-germany-arcgis-maps-shapefiles.htm

# MaStR explanations of variables
url_variables_expl <- "https://www.bundesnetzagentur.de/SharedDocs/Downloads/DE/Sachgebiete/Energie/Unternehmen_Institutionen/DatenaustauschUndMonitoring/MaStR/MaStR%20-%20Erl%C3%A4uterungstexte%20Einheiten_180921.xlsx?__blob=publicationFile&v=4"


# ***********************************************************************************************
#### installing, loading libraries ####
packages <- c("here", "httr", "zip", "readxl", "shiny", "ggplot2")

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

input <- here("data", "raw")

# store data if not yet existing
if(!file.exists(here("data", "raw", file_to_load)))
{
  download.file(url, here("data", "raw", file_to_load))
}


if(!file.exists(here("data", "raw", file_ger_shape_plz)))
{
    download.file(url_shape_plz, here("data", "raw", file_ger_shape_plz))
}

if(!file.exists(here("data", "raw", file_ger_shape_state)))
{
    download.file(url_shape_state, here("data", "raw", file_ger_shape_state))
}

# save information on MaStR variables
download.file(url_variables_expl, here("resources", "MaStR_unit_explanations.xlsx"))



# ***********************************************************************************************
#### de-zip ####
zipF <- here("data", 'raw', file_to_load)
outDir <- here("data", 'raw')
unzip(zipF, exdir = outDir)

xlsx_to_load <- zip_list(zipF)$filename


zipF2 <- here("data", 'raw', file_ger_shape_plz)
outDir <-here("data", 'raw', "shape_plz")
unzip(zipF2, exdir = outDir)

zipF3 <- here("data", 'raw', file_ger_shape_state)
outDir <-here("data", 'raw', "shape_state")
unzip(zipF3, exdir = outDir)

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
# -> don't do it: is larger than xlsx!?


# ***********************************************************************************************
#### load next code file ####
file.edit(here("code", "2.process_data.R"))
