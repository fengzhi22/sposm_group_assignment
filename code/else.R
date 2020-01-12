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
