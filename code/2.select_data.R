# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here", "zip", "dplyr", "readxl", "sf", "curl", "devtools")

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

### from other sources + preparation of depending packages
packages_destatiscleanr <- c("assertthat", "glue", "R6", "Rcpp", "BH", "hms", "clipr", "cli", "fansi", "pillar", "vctrs", "backports", "digest")

### install if necessary
lapply(packages_destatiscleanr, 
       function(x)
       {
         if(!(x %in% installed.packages()) | x %in% old.packages())
         {
           install.packages(x)  
         }
       })

lapply(packages_destatiscleanr, require, character.only = T)

#devtools::install_github("cutterkom/destatiscleanr")
library("destatiscleanr")

# **********************************************************************************************
#### D E F I N I T I O N S ####
#rm(list=ls(all=TRUE))
options(scipen = 999) # no scientific numbering

file_to_load <- "enh.csv"
file_to_load2 <- "enh2.csv"

subfolder <- zip_list(here("data", "raw", "shape_germany_bundesland_landkreis.zip"))$filename[1]
subfolder <- gsub("/", "", subfolder, fixed = TRUE)
file_ger_shape <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_sta.shp")
file_ger_shape_state <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_lan.shp")
file_ger_shape_county <- here("data", "raw", "shape_ger", subfolder, "vg2500", "vg2500_krs.shp")


# ***********************************************************************************************
#### load data ####
enh <- read.csv2(here("data", "raw", file_to_load), row.names = NULL, encoding = "UTF-8")
enh2 <- read.csv2(here("data", "raw", file_to_load2), row.names = NULL, encoding = "UTF-8", stringsAsFactors = FALSE)

ger_shape <- st_read(file_ger_shape, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)
ger_shape_state <- st_read(file_ger_shape_state, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)
ger_shape_county <- st_read(file_ger_shape_county, options = "ENCODING=UTF-8", stringsAsFactors = FALSE)


# ***********************************************************************************************
#### load AGS (Amtlicher Gemeindeschlüssel) ####
#ags <- destatiscleanr(here("data", "raw", paste0("AuszugGV4QAktuell", ".xlsx")))
# -> just works for csv files
ags_xlsx <- "AuszugGV4QAktuell.xlsx"
ags <- read_excel(here("data", "raw", ags_xlsx), sheet = 2, skip = 6, col_names = FALSE)
names(ags) <- c("satzart",
                "textkennzeichen",
                "ags_federal_state",
                "ags_district",
                "ags_county",
                "ags_municipality_group",
                "ags_municipality",
                "name",
                "area",
                "pop_total",
                "pop_male",
                "pop_fem",
                "pop_per_sqkm",
                "plz",
                "longitude",
                "latitude",
                "travel_area_key",
                "travel_area_name",
                "urbanization_key",
                "urbanization_name")

# delete comments from the end
ags <- ags[!is.na(ags$ags_federal_state), ]

# ags of federal states
ags_federal_states <- ags[is.na(ags$ags_district), c("ags_federal_state", "name")]
names(ags_federal_states) <- c("ags", "name")
ags_federal_states
write.csv2(ags_federal_states, file = here("data", "processed", "ags_federal_states.csv"), row.names = FALSE, fileEncoding="UTF-8")
#test <- read.csv(here("data", "raw", "ags_federal_states.csv"), encoding = "UTF-8")


# ags of counties
ags_counties <- ags[is.na(ags$ags_municipality_group) & is.na(ags$ags_municipality) & !is.na(ags$ags_county), c("ags_federal_state", "ags_district", "ags_county", "name")]
ags_counties$ags_county <- paste0(ags_counties$ags_federal_state,
                                  ags_counties$ags_district,
                                  ags_counties$ags_county)
ags_counties$ags_county <- gsub("NA", "", ags_counties$ags_county)

ags_counties$ags_district <- paste0(ags_counties$ags_federal_state,
                                  ags_counties$ags_district)
ags_counties$ags_district <- gsub("NA", "", ags_counties$ags_district)


write.csv2(ags_counties, file = here("data", "processed", "ags_counties.csv"), row.names = FALSE, fileEncoding="UTF-8")

# *******************************************
### create correspondence table
ags_correspondence_table <- ags[,1:8]
# municipality
ags_correspondence_table$ags_municipality <- paste0(ags_correspondence_table$ags_federal_state,
                                                    ags_correspondence_table$ags_district,
                                                    ags_correspondence_table$ags_county,
                                                    ags_correspondence_table$ags_municipality)
ags_correspondence_table$ags_municipality <- gsub("NA", "",ags_correspondence_table$ags_municipality)

# county
ags_correspondence_table$ags_county <- paste0(ags_correspondence_table$ags_federal_state,
                                              ags_correspondence_table$ags_district,
                                              ags_correspondence_table$ags_county)
ags_correspondence_table$ags_county <- gsub("NA", "",ags_correspondence_table$ags_county)

# district
ags_correspondence_table$ags_district <- paste0(ags_correspondence_table$ags_federal_state,
                                                ags_correspondence_table$ags_district)
ags_correspondence_table$ags_district <- gsub("NA", "",ags_correspondence_table$ags_district)

#ags_correspondence_table$ags_municipality <- as.numeric(ags_correspondence_table$ags_municipality)
write.csv2(ags_correspondence_table, file = here("data", "processed", "ags_correspondence_table.csv"), row.names = FALSE, fileEncoding="UTF-8")


# ***********************************************************************************************
#### merge enh2 to ags ####
names(enh2)[names(enh2) %in% "Gemeindeschluessel"] <- "ags_municipality"

# transform ags in enh2 (leading zeros are missing and needed for the couty cutoff)
enh2$ags_municipality <- as.character(enh2$ags_municipality)
to_change <- which(nchar(enh2$ags_municipality) == 7)
enh2$ags_municipality[to_change] <- gsub("^", "0", enh2$ags_municipality[to_change])

enh2$ags_county <- substr(enh2$ags_municipality, start = 1, stop = 5)

# changes of the county organisation for federal state Niedersachsen
# https://www.statistik.niedersachsen.de/download/61481
unique(enh2$ags_county[enh2$Landkreis == "Göttingen" & !is.na(enh2$Landkreis)])
# -> change 03152 to 03159 (03152 does not exist in 2019 anymore)
enh2$ags_county[enh2$Landkreis == "Göttingen" & !is.na(enh2$Landkreis)] <- "03159"
unique(enh2$ags_county[enh2$Landkreis == "Osterode am Harz" & !is.na(enh2$Landkreis)])
# -> change 03156 to 03159 (03156 does not exist in 2019 anymore)
enh2$ags_county[enh2$Landkreis == "Osterode am Harz" & !is.na(enh2$Landkreis)] <- "03159"

# merge via ags_county
#enh3 <- left_join(enh2, ags_correspondence_table[, c(3,5,7,8)], by = "ags_municipality")
enh3 <- left_join(enh2, ags_counties[, c(1,3,4)], by = "ags_county")
# missmatches?
missings <- which(is.na(enh3$ags_federal_state))
length(missings)

# what types of missings?
table(enh3$EinheitenTyp[missings])
table(enh3$Lage[missings])
# -> missings because of off-shore wind: create new Landkreis: offshore
length(which(enh3$Lage == "Windkraft auf See"))
to_change <- which(enh3$Lage == "Windkraft auf See")
unique(enh3$Landkreis[enh3$Lage == "Windkraft auf See" & !is.na(enh3$Lage)])
enh3$Landkreis <- as.character(enh3$Landkreis)
enh3$Landkreis[enh3$Lage == "Windkraft auf See"] <- "offshore"
unique(enh3$Bundesland[enh3$Lage == "Windkraft auf See" & !is.na(enh3$Lage)])
# -> do not change federal state to offshore


## important question to check:
# Do all counties and federal states with the same name have the same ags?
length(unique(enh3$Landkreis))
length(unique(enh3$ags_county))
# -> no
differences <- enh3 %>%
  group_by(ags_county, Landkreis) %>%
  summarize(n_county = length(ags_county), n_landkreis = length(Landkreis))
# which differ?
differences[differences$n_county != differences$n_landkreis, ]
# none?
duplicates <- differences$Landkreis[duplicated(differences$Landkreis)]
duplicates


differences2 <- enh3 %>%
  group_by(ags_county, Landkreis, name) %>%
  summarize(n_county = length(ags_county), n_landkreis = length(Landkreis))
# which names differ?
differences2[differences2$Landkreis != differences2$name, ]
# 
duplicates <- differences2$Landkreis[duplicated(differences2$Landkreis)]
duplicates
to_check <- differences2[differences2$Landkreis %in% duplicates, ]
# -> means for the procedure
# 1. consolidating on ags_county level
# 2. merging further regional statistic information on that level
# 3. when showing maps, use ags_county but display the "name" in hovering mode

# which are not in the shape file
missing <- which(!(unique(enh3$ags_county[!is.na(enh3$ags_county)]) %in% ger_shape_county$RS))
missing
# -> none are missing: that means that all can become connected to the shape file (very good!)
# but some ags_county might have multiple names for Landkreis!?
length(unique(ags_counties$ags_county))
length(unique(ags_counties$name))

length(unique(enh3$ags_county[!is.na(enh3$ags_county)]))
length(unique(ger_shape_county$RS))
length(unique(ger_shape_county$GEN))

# exist in shape?
table(enh3$Landkreis[!(enh3$ags_county %in% ger_shape_county$RS)])
# just offshore has no shape information

# do all shape counties have information on energy in this counties as well?
table(ger_shape_county$GEN[!(ger_shape_county$RS %in% enh3$ags_county)])
# -> Yes! This is great news :D
# prerequisite so that all other regional data can be matched via ags_county information



# ***********************************************************************************************
#### data cleaning ####
# due some inconsistencies in the source data
# relating to the relation of Bundesland to ags,
# respectively of county to ags,
# we drop Bundesland and county here.
# The names re-appear with the shape-file later
drops <- c("Bundesland", "Landkreis")
enh3 <- enh3[, !(names(enh3) %in% drops)]

# split Verbrennung into Braunkohle, Steinkohle, Gas and Mineralölprodukte
enh3$EinheitenTyp[enh3$Energietraeger == "Braunkohle"] <- "Braunkohle"
enh3$EinheitenTyp[enh3$Energietraeger == "Steinkohle"] <- "Steinkohle"
enh3$EinheitenTyp[enh3$Energietraeger == "Erdgas"] <- "Gas"
enh3$EinheitenTyp[enh3$Energietraeger == "andere Gase"] <- "Gas"
enh3$EinheitenTyp[enh3$Energietraeger == "Mineralölprodukte"] <- "Mineralölprodukte"
View(enh3[enh3$EinheitenTyp == "Verbrennung", ])
length(which(enh3$EinheitenTyp == "Verbrennung"))



# ***********************************************************************************************
#### aggregate data on state level by year ####
data_state_yearly <- enh3 %>%
  group_by(start_year, EinheitenTyp, ags_federal_state) %>%
  summarize(n = length(ags_federal_state), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung)) %>%
  ungroup()


table(data_state_yearly$start_year)
# there are solar panels that seem to be veeeeery old before 1950!?
# -> How can that be?
#[TBD]

write.csv2(data_state_yearly, file = here("data", "processed", "data_state_yearly.csv"), row.names = FALSE, fileEncoding="UTF-8")


# ***********************************************************************************************
#### aggregate data on state level ####
data_state <- enh3 %>%
  group_by(EinheitenTyp, ags_federal_state) %>%
  summarize(n = length(ags_federal_state), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung)) %>%
  ungroup()

write.csv2(data_state, file = here("data", "processed", "data_state.csv"), row.names = FALSE, fileEncoding="UTF-8")


# ***********************************************************************************************
#### aggregate data on county level by year ####
# consolidate
data_county_yearly <- enh3 %>%
  group_by(start_year, EinheitenTyp, name, ags_county) %>%
  summarize(n = length(ags_county), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung)) %>%
  ungroup()

# save as csv
write.csv2(data_county_yearly, file = here("data", "processed", "data_county_yearly.csv"), row.names = FALSE, fileEncoding="UTF-8")


# ***********************************************************************************************
#### aggregate data on county level ####
# consolidate
data_county <- enh3 %>%
  group_by(EinheitenTyp, name, ags_county) %>%
  summarize(n = length(ags_county), mean = mean(Nettonennleistung), sum = sum(Nettonennleistung)) %>%
  ungroup()

# save as csv
write.csv2(data_county, file = here("data", "processed", "data_county.csv"), row.names = FALSE, fileEncoding="UTF-8")



# ***********************************************************************************************
#### granular data set with offshore windpower ####
data_offshore <- enh3[enh3$Lage == "Windkraft auf See" & !is.na(enh3$Lage), ]

write.csv2(data_offshore, file = here("data", "processed", "data_offshore.csv"), row.names = FALSE, fileEncoding="UTF-8")




# ***********************************************************************************************
#### regional statistic: package  ####
# devtools::install_github("cutterkom/destatiscleanr")

## download csv_files from:
# https://www.regionalstatistik.de/
# some data needs an active login to download: 
# You can do that for free but it has to be done by hand


# load data using destatiscleanr
county_income <- destatiscleanr(here("data", "raw", paste0("73111-01-01-4_income", ".csv")))

state_elections <- destatiscleanr(here("data", "raw", paste0("14111-01-03-4_bundestagswahlen_1994-2017_kreisebene", ".csv")))



### organize data
## income
names(county_income) <- c("year", "ags", "name", "pop_obliged_to_income_taxes",
                          "total_income_in_1000_euro", "total_taxes_on_income_in_1000_euro")

drops <- c("name")
county_income <- county_income[, !(names(county_income) %in% drops)]

table(county_income$year)
# just some years have information


## state elections
names(state_elections) <- c("date", "ags", "name", "voters_eligible", "voter_turnout",
                            "votes_valid", "votes_cdu_csu", "votes_spd", "votes_greens",
                            "votes_fdp", "votes_the_left", "votes_afd", "votes_other_parties")

drops <- c("name")
state_elections <- state_elections[, !(names(state_elections) %in% drops)]

state_elections$date <- as.Date(state_elections$date, format = "%d.%m.%Y")
state_elections$year <- format(state_elections$date, "%Y")
state_elections$year <- as.numeric(state_elections$year)
# delete date
state_elections <- state_elections[, -1]

table(state_elections$year)
# just some years have information


# ****************************
### merging regional data
## state yearly
# income
data_state_yearly_extended <- data_state_yearly %>%
  arrange(start_year, ags_federal_state) %>%
  left_join(county_income, by = c("start_year" = "year", "ags_federal_state" = "ags"))
  
nrow(data_state_yearly_extended[!is.na(data_state_yearly_extended$total_income_in_1000_euro),])
# just some years have information

## state yearly
# state elections
data_state_yearly_extended <- data_state_yearly_extended %>%
  left_join(state_elections, by = c("start_year" = "year", "ags_federal_state" = "ags"))

# save
write.csv2(data_state_yearly_extended, file = here("data", "processed", "data_state_yearly_extended.csv"), row.names = FALSE, fileEncoding="UTF-8")



## county yearly
# income
data_county_yearly_extended <- data_county_yearly %>%
  arrange(start_year, ags_county) %>%
  left_join(county_income, by = c("start_year" = "year", "ags_county" = "ags"))

nrow(data_county_yearly_extended[!is.na(data_county_yearly_extended$total_income_in_1000_euro),])
# just some years have information

## state yearly
# state elections
data_county_yearly_extended <- data_county_yearly_extended %>%
  left_join(state_elections, by = c("start_year" = "year", "ags_county" = "ags"))

# save
write.csv2(data_county_yearly_extended, file = here("data", "processed", "data_county_yearly_extended.csv"), row.names = FALSE, fileEncoding="UTF-8")



# ***********************************************************************************************
file.edit(here("code", "4.3shiny_all_data.R"))
