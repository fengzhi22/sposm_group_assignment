# ***********************************************************************************************
#### tmap example ####
data("World")
tm_shape(World)



# ***********************************************************************************************
#### set directory at top level of the project ####
# identify folder of running or sourced script
thisFolder <- function() {
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (any(grepl("RStudio", cmdArgs))) {
    # Rscript
    return(setwd(dirname(rstudioapi::getActiveDocumentContext()$path)))
  } else {
    # 'source'd via R console
    return(setwd(dirname(sys.frame(1)$ofile)))
  }
}
thisFolder()


if (grepl("/code$", getwd()))
{
  setwd(gsub("/code$", "", getwd()))
}
getwd()

# set working directory to root folder of project
unlink(".here")
set_here()
here()

# --> here works like shit if you do not start the R-session new


# ***********************************************************************************************
#### create Rproj (if not existing) ####
#proj_file <- "sposm_group_assignment.Rproj" # not used

# --> don't do it: raises erros in usage (when combined with GDrive?)
if(FALSE)
{
    if (!file.exists(proj_file))
    {
        fileConn <- file(proj_file)
        writeLines(c("Version: 1.0", "", "RestoreWorkspace: No", "SaveWorkspace: No", 
                     "AlwaysSaveHistory: Default", "", "EnableCodeIndexing: Yes", 
                     "UseSpacesForTab: Yes", "NumSpacesForTab: 4", "Encoding: UTF-8", 
                     "", "RnwWeave: knitr", "LaTeX: pdfLaTeX")
                   , fileConn)
        close(fileConn)
    }
}



# ***********************************************************************************************
#### open R-project ####
# --> don't do it: raises erros in usage (when combined with GDrive?)
#openFile('sposm_group_assignment.Rproj')





# ***********************************************************************************************
#### test shape files ####
#C:/Users/Sebastian/Google Drive/_Downloads/vg250_neu

test <- st_read("C:/Users/Sebastian/Google Drive/_Downloads/vg250_neu", options = "ENCODING=UTF-8", stringsAsFactors = FALSE)




# ***********************************************************************************************
#### messing with regional data and other sources ####

### regional statistic
# use Wiesbaden to do that?
# https://github.com/sumtxt/wiesbaden

# You need an account at 
# https://www.regionalstatistik.de/
# to use the data

# destatis_user <-c(user="your-username",password="your-password",db="database-shortname")

# GET("https://www.regionalstatistik.de", authenticate(destatis_user[1], destatis_user[2], "basic"))
## authenticate against Windows credentails
page <- GET("https://www.regionalstatistik.de", authenticate(":", ":", "ntlm"))
# -> does not work!?

test_login(genesis=c(db='regio'))
# -> does not work!?


#download_csv()
test <- retrieve_datalist(tableseries = "14111", genesis=c(db="regio"))
#retrieve_data()
# -> does not work!?

test2 <- retrieve_data(tablename="14111KJ002", genesis=c(db="regio") )
# -> does not work!?

# e.g. state elections 1.9.2019
#download_csv(here("data", "raw", "14311-01"))
# -> does not work: loads some html table

# 
#download_csv(here("data", "raw", "14342-01-03-4"))



### ags
# devtools::install_github("sumtxt/ags")
#library("ags")
# -> Not helpful

# choroplethr package
#ags <- data("ChoroplethPostalCodesAndAGS_Germany")
# -> does not work, data not existent


## Destatis download by hand
# https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GV100ADQ/GV100AD3101.html
# use package raster to load ascii file

# ****
url_ags <- "https://www.destatis.de/DE/Themen/Laender-Regionen/Regionales/Gemeindeverzeichnis/Administrativ/Archiv/GV100ADQ/GV100AD3101.zip?__blob=publicationFile"
file_ags <- "GV100AD3101.zip"

if(!file.exists(here("data", "raw", file_ags)))
{
  download.file(url_ags, here("data", "raw", file_ags))
}

zipF <- here("data", 'raw', file_ags)
outDir <- here("data", 'raw')
unzip(zipF, exdir = outDir)

#ags <- raster(here("data", 'raw', "GV100AD_310120.ASC"))
# -> does not work

#ags <- read.csv(here("data", 'raw', "GV100AD_310120.ASC"), header = FALSE, skip = 1)
# -> fucking messy


df <- data_state_yearly
df$EinheitenTyp <- "All"
df %>% group_by(start_year, EinheitenTyp, ags_federal_state) %>%
  summarize(n = sum(n), mean = mean(mean), sum = sum(sum)) %>%
  ungroup() -> test


Period <- reactive({
  df <- get(paste0("data_", input$geo_level, "_yearly"))
  if (input$source == "All"){
    df$EinheitenTyp <- "All"
    if(input$geo_level == "state"){
      df %>% group_by(start_year, EinheitenTyp, ags_federal_state) %>%
        summarize(n = sum(n), mean = mean(mean), sum = sum(sum)) %>%
        ungroup() %>%
        filter(start_year >= input$years[1]) %>%
        filter(start_year <= input$years[2]) %>%
        filter(ags_federal_state == input$map_shape_click$id) -> df
    }else{
      df %>% group_by(start_year, EinheitenTyp, ags_county) %>%
        summarize(n = sum(n), mean = mean(mean), sum = sum(sum)) %>%
        ungroup() %>%
        filter(start_year >= input$years[1]) %>%
        filter(start_year <= input$years[2]) %>%
        filter(ags_county == input$map_shape_click$id) -> df
    }
  }else{
    if(input$geo_level == "state"){
      df %>% filter(EinheitenTyp == input$source) %>%
        filter(start_year >= input$years[1]) %>%
        filter(start_year <= input$years[2]) %>%
        filter(ags_federal_state == input$map_shape_click$id) -> df
    }else{
      df %>% filter(EinheitenTyp == input$source) %>%
        filter(start_year >= input$years[1]) %>%
        filter(start_year <= input$years[2]) %>%
        filter(ags_county == input$map_shape_click$id) -> df
    }
  }
})    



#### test 2
Period <- reactive({
  df <- get(paste0("data_", input$geo_level, "_yearly"))
  
  selected_regionid <- input$map_shape_click$id
  
  # Turn the generated "." back into "-" in the state names. e.g. Nordrhein.Westfalen to Nordrhein-Westfalen
  selected_regionid <- sub(".","-",selected_regionid, fixed = TRUE)
  
  if(input$geo_level == "state"){
    df %>% filter(EinheitenTyp == input$source) %>%
      filter(start_year >= input$years[1]) %>%
      filter(start_year <= input$years[2]) %>%
      filter(ags_federal_state == selected_regionid)
  }#else{
  #  df %>% filter(EinheitenTyp == input$source) %>%
  #    filter(start_year >= input$years[1]) %>%
  #   filter(start_year <= input$years[2]) %>%
  #  filter(ags_county == input$map_shape_click$id) -> df
  #}
})    

