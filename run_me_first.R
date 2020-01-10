# ***********************************************************************************************
#### installing, loading libraries ####
library("utils")

packages <- c("here",  "PBSmodelling")

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
authors <- c("Le Binh", "Yagmur Dalman", "Zhu Fengzhi", "Sebastian Geschonke")


# ***********************************************************************************************
#### set directory at top level of the project ####
set_here()
here()


# ***********************************************************************************************
#### write .gitignore
if (!file.exists(".gitignore"))
{
    fileConn <- file(".gitignore")
    writeLines(c("# History files",
                 ".Rhistory",
                 "",
                 "# Session files",
                 ".RData",
                 "",
                 "# RStudio files",
                 ".Rproj.user/",
                 "",
                 "# folders with probably a lot of content",
                 "/data/*",
                 "",
                 "",
                 "# inspirational R-code for later purposes",
                 "/code/else.R",
                 "# OAuth2 token, see https://github.com/hadley/httr/releases/tag/v0.3",
                 ".httr-oauth",
                 "",
                 "# knitr and R markdown default cache directories",
                 "/*_cache/",
                 "/cache/",
                 "",
                 "# Temporary files created by R markdown",
                 "*.utf8.md",
                 "*.knit.md",
                 "",
                 "# Shiny token, see https://shiny.rstudio.com/articles/shinyapps.html",
                 "rsconnect/"), fileConn)
    close(fileConn)
}

# ***********************************************************************************************
#### write LICENSE ####
year <- format(Sys.Date(), "%Y")

if (!file.exists("LICENSE"))
{
    fileConn <- file("LICENSE")
    
    writeLines(c(
    "MIT License",
    "",
    paste0("Copyright (c) ", year),
    authors,
    "",
    "Permission is hereby granted, free of charge, to any person obtaining a copy",
    'of this software and associated documentation files (the "Software"), to deal',
    "in the Software without restriction, including without limitation the rights",
    "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell",
    "copies of the Software, and to permit persons to whom the Software is",
    "furnished to do so, subject to the following conditions:",
    "",
    "The above copyright notice and this permission notice shall be included in all",
    "copies or substantial portions of the Software.",
    "",
    'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR',
    "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,",
    "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE",
    "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER",
    "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
    "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE",
    "SOFTWARE."
    ), fileConn)
}
close(fileConn)



# ***********************************************************************************************
#### create folder structure ####
folders <- c(here('analysis'),
             here('cache'), 
             here('code'),
             here('data'), here('data', 'raw'), here('data', 'processed'),
             here('graphs'),
             here('output'),
             here('resources')
             )

for (i in folders)
{
  if(!dir.exists(i))
  {
      dir.create(i)
  }
    
}



# ***********************************************************************************************
#### open next code file ####
file.edit(here("code", "1.load_data.R"))

