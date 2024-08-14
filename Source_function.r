library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(dplyr)
library(stringr)
library(png)
library(DT)
library(rintrojs)
library(qdapTools)
library(bslib)
library(httr)
library(zip)
library(readxl)

Sys.setlocale('LC_ALL', 'C')

#####################
#####Reading data####
#####################
dav <- "https://cloud.uol.de/remote.php/webdav/ListPapers"
username <- "********"
password <- "******************"


links <- read.csv("links.csv", header = TRUE, sep = ",")

