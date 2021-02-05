## Installing required packages if they do not already exist.
# required_packages <- c("shiny","shiny.semantic","leaflet","leaflet.extras2",
#                        "dplyr","readr","geosphere","shinycssloaders")
# new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)
# 
# rm(required_packages,new_packages)

## Loading global packages
library(shiny)
library(shiny.semantic)
library(leaflet)
library(leaflet.extras2)
