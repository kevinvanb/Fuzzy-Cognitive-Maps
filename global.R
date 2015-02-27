# ------------------- LOAD LIBRARIES ------------------------#
packages <- c("qgraph", "shiny", "ggplot2", "plyr", "RCurl", 
              "XML", "reshape", "devtools", "ggvis")
if (length(setdiff(packages, installed.packages())) > 0)
  install.packages(setdiff(packages, installed.packages()))

if (length(setdiff("shinydashboard", installed.packages())) > 0)
   devtools::install_github("rstudio/shinydashboard")

# devtools required for shinydashboard
library(devtools)
# Required for creating the web page
library(shiny)
# shinydashboard package required for the layout
library(shinydashboard)
# Required for plotting the network 
library(qgraph)
# Required for obtaining data from Google spreadsheets 
library(RCurl)
library(reshape2)
# require(ggplot2)
# Required for line plots
library(ggvis)
# Required for manipulating the data
library(plyr)
# Required for reading the Mentalmodeler file
library(XML)

# ------------------- SETUp GLOBAL VALUES ------------------------#
# Default squashing function list
g_squash <- c("binary", "tanh", "sigmoid", "All Above")
# Squashing functions as equations
g_squash_f <- c(binary = "ifelse(x < 0.5, 0, 1)", tanh = "tanh(x)", sigmoid = "1/(1 + exp(-x))")
g_sep <- c('<M>', '<IO>', '<SS>', '<SF>')
g_ftypes <- list(MentalModeler = 'mmp', csv = 'csv')
g_flocations <- list(Local='local', 'Google Spreadsheet'='google', Dropbox = 'dropbox')            


# g_data <- list()
# g_reactive <- reactiveValues(temp_queue = data.frame(), queue = data.frame(),
#                              res_iter = list(), res_summary = data.frame())


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
#   g_data <- list(amat_data = data.frame(), amat_names = data.frame(),
#                  sstate_data = data.frame(), sstate_names = data.frame(),
#                  squash_data = data.frame(), squash_names = data.frame(), 
#                  inout_data = data.frame(), inout_names = data.frame())
#   g_reactive$temp_queue <- data.frame()
#   g_reactive$queue <- data.frame() 
#   g_reactive$res_iter <- data.frame()
#   g_reactive$res_summary <- data.frame()
  
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

initialize()