# ------------------- LOAD LIBRARIES ------------------------#
packages <- c("qgraph", "shiny", "ggplot2", "plyr", "RCurl", "XML", "reshape", "devtools", "ggvis")
if (length(setdiff(packages, installed.packages())) > 0)
  install.packages(setdiff(packages, installed.packages()))

if (length(setdiff("shinydashboard", installed.packages())) > 0)
  devtools::install_github("rstudio/shinydashboard")

# devtools required for shinydashboard
library(devtools)
# shinydashboard package required for the layout
library(shinydashboard)
# require(Matrix)
require(qgraph)
require(shiny)
# require(rCharts)
# require(XLConnect)
# require(gdata)
require(RCurl)                # Required for obtaining data from Google spreadsheets 
# require(repmis)             # Required for obtaining data from Dropbox
require(reshape)
require(ggplot2)
#require(rCharts)
#require(sfsmisc)
require(ggvis)
require(plyr)
require(XML)
#require(reshape2)

g_m <- list(m=data.frame(), n=data.frame())  # dataframe containing uploaded adjacency matrix 
g_ss <- list(m=data.frame(), n=data.frame()) # dataframe containing uploaded start states
g_sf <- list(m=data.frame(), n=data.frame()) # dataframe containing uploaded squashing functions
g_io <- list(m=data.frame(), n=data.frame()) # dataframe containing uploaded inputs/outputs
g_t <- reactiveValues(q=data.frame())        # temp queue
g_r <- reactiveValues(q=data.frame(),        # queue of scenarios
                      r=list(),              # list containing fcm results for each iteration
                      s=data.frame())        # dataframe containing summary of fcm results
g_error <- ''                                # Error message generated when uploading data

initialize <- function(){
  g_m <<- list(m=data.frame(), n=data.frame())  # dataframe containing uploaded adjacency matrix 
  g_ss <<- list(m=data.frame(), n=data.frame()) # dataframe containing uploaded start states
  g_sf <<- list(m=data.frame(), n=data.frame()) # dataframe containing uploaded squashing functions
  g_io <<- list(m=data.frame(), n=data.frame()) # dataframe containing uploaded inputs/outputs
  g_t$q <<- data.frame()                        # temp queue
  g_r$q <<- data.frame()                        # queue of scenarios
  g_r$r <<- list()                              # list containing fcm results for each iteration
  g_r$s <<- data.frame()                        # dataframe containing summary of fcm results
  g_error <<- ''                                # Error message generated when uploading data
}

# devtools::install_github(c('rstudio/ggvis', 'rstudio/shiny'))
g_squash <- c('binary', 'tanh', 'sigmoid', 'all')
g_sep <- c('<M>', '<IO>', '<SS>', '<SF>')
g_ftypes <- list(MentalModeler = 'mmp', csv = 'csv')
g_flocations <- list(Local='local', 'Google Spreadsheet'='google', Dropbox = 'dropbox')
initialize()