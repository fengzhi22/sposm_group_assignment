# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "httr", "readxl", "zip")

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
file_energy <- "DatenAb310119.zip"

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
if(!file.exists(here("data", "raw", file_energy)))
{
  download.file(url, here("data", "raw", file_energy), mode="wb")
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
if(!file.exists(here("resources", "MaStR_unit_explanations.xlsx")))
{
  download.file(url_variables_expl, here("resources", "MaStR_unit_explanations.xlsx"), mode="wb")
}


# ***********************************************************************************************
#### de-zip ####
zipF <- here("data", 'raw', file_energy)
outDir <- here("data", 'raw')
unzip(zipF, exdir = outDir)
# If unzipping raises problems as we have experienced, you might download the file manually
# and save it in your git repository under data/raw/
# https://www.bundesnetzagentur.de/SharedDocs/Downloads/DE/Sachgebiete/Energie/Unternehmen_Institutionen/ErneuerbareEnergien/ZahlenDatenInformationen/VOeFF_Registerdaten/DatenAb310119.zip

xlsx_energy <- zip_list(zipF)$filename


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
tabelle_enh <- read_excel(here("data", "raw", xlsx_energy), sheet = "Tabelle_ENH")
# -> takes some minutes


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
#### prepare years of start and end of facilities ####
names(tabelle_enh) <- gsub("ENH_", "", names(tabelle_enh), fixed = TRUE)
names(tabelle_enh)

vars <- c("MastrID", "MastrNummer", "Systemstatus", "Betriebsstatus", "EinheitenTyp", 
          "GeplantesInbetriebnahmeDatum", "InbetriebnahmeDatum", "WiederInbetriebnahmeDatum",
          "VoruebergehendeStilllegungBeginn", "EndgueltigeStilllegungDatum", 
          "Lage", "Plz", "Ort",
          "Bundesland", "Landkreis", "Gemeinde", "Gemeindeschluessel",
          "Breitengrad", "Laengengrad", 
          "Leistungsbegrenzung", "Bruttoleistung", "Nettonennleistung", 
          "Nutzungsbereich", "Energietraeger", "EinspeisungsArt")

enh2 <- tabelle_enh[tabelle_enh$Land == "Deutschland", vars]

# get start and end year of observation
enh2$InbetriebnahmeDatum <- as.Date(enh2$InbetriebnahmeDatum)
enh2$GeplantesInbetriebnahmeDatum <- as.Date(enh2$GeplantesInbetriebnahmeDatum)
enh2$VoruebergehendeStilllegungBeginn <- as.Date(enh2$VoruebergehendeStilllegungBeginn)
enh2$WiederInbetriebnahmeDatum <- as.Date(enh2$WiederInbetriebnahmeDatum)
enh2$EndgueltigeStilllegungDatum <- as.Date(enh2$EndgueltigeStilllegungDatum)

# get years
enh2$start_year <- format(enh2$InbetriebnahmeDatum, "%Y")
enh2$start_pause <- format(enh2$VoruebergehendeStilllegungBeginn, "%Y")
enh2$end_pause <- format(enh2$WiederInbetriebnahmeDatum, "%Y")
enh2$end_year <- format(enh2$EndgueltigeStilllegungDatum, "%Y")

# check if missing start_years are those that are planned to on-grid
# how many NAs?
length(enh2$GeplantesInbetriebnahmeDatum[is.na(enh2$start_year)])
table(enh2$GeplantesInbetriebnahmeDatum[is.na(enh2$start_year)])
# -> some of the future, some might be delayed in switching on grid
# -> abstract from this issue and simply take these years
enh2$start_year[is.na(enh2$start_year)] <- format(enh2$GeplantesInbetriebnahmeDatum[is.na(enh2$start_year)], "%Y")

# how many NAs left now?
length(enh2$start_year[is.na(enh2$start_year)])
# which?
enh2[is.na(enh2$start_year), c("start_pause", "end_pause", "end_year", "EinheitenTyp")]
# just some cases: ignore them

enh2 <- enh2[!is.na(enh2$start_year), ]

## how many units are temporary closed or decommissioned?
# temporary closed
length(enh2$EinheitenTyp[!is.na(enh2$start_pause)])
# and not back
length(enh2$EinheitenTyp[!is.na(enh2$start_pause) & is.na(enh2$end_pause)])
# started when?
table(enh2$start_pause[!is.na(enh2$start_pause)])

# decommissioned
length(enh2$EinheitenTyp[!is.na(enh2$end_year)])
# none?
length(enh2$EinheitenTyp[!is.na(enh2$EndgueltigeStilllegungDatum)])
# indeed: none

# deactivated facilities?
table(enh2$Betriebsstatus)
enh2[enh2$Betriebsstatus == "Dauerhaft stillgelegt", c("start_year", "EndgueltigeStilllegungDatum")]
# -> no date existing: these are a problem!
enh2[enh2$Betriebsstatus == "Dauerhaft stillgelegt" & is.na(enh2$EndgueltigeStilllegungDatum), c("start_year", "EndgueltigeStilllegungDatum")]
# -> delete these cases
enh2 <- enh2[!(enh2$Betriebsstatus == "Dauerhaft stillgelegt" & is.na(enh2$EndgueltigeStilllegungDatum)), ]

table(enh2[enh2$Betriebsstatus == "Dauerhaft stillgelegt", c("VoruebergehendeStilllegungBeginn")])
enh2[enh2$Betriebsstatus == "Dauerhaft stillgelegt", c("EinheitenTyp")]
table(enh2[enh2$Betriebsstatus == "Dauerhaft stillgelegt", c("GeplantesInbetriebnahmeDatum")])
enh2 <- enh2[enh2$Betriebsstatus == "Dauerhaft stillgelegt",]

# delete those that are planned
enh2 <- enh2[enh2$Betriebsstatus != "In Planung",]
table(enh2$Betriebsstatus)

# get rid of two much variables before saving
drops <- c("Systemstatus", "InbetriebnahmeDatum", "GeplantesInbetriebnahmeDatum", "WiederInbetriebnahmeDatum",
           "VoruebergehendeStilllegungBeginn", "EndgueltigeStilllegungDatum")
enh2 <- enh2[, !(names(enh2) %in% drops)]
names(enh2)

## save as csv for faster loading next time
write.csv2(enh2, file = here("data", "raw", "enh2.csv"), row.names = FALSE, fileEncoding="UTF-8")


# ***********************************************************************************************
#### load AGS (Amtlicher Gemeindeschlüssel) ####
url_ags <- "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GVAuszugQ/AuszugGV4QAktuell.xlsx?__blob=publicationFile"
file_ags <- "AuszugGV4QAktuell.xlsx"

if(!file.exists(here("data", "raw", file_ags)))
{
  download.file(url_ags, here("data", "raw", file_ags), mode="wb")
}

# ***********************************************************************************************
#### load next code file ####
file.edit(here("code", "2.select_data.R"))
