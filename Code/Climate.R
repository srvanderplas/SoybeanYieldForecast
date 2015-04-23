# Code to plot climate data
# in response to email from Sotirios on April 22, 2015

# R libraries -------------------------------------------------------------------
library(ggplot2) # plotting
library(lubridate) # handling date/time data
library(stringr) # string manipulation
library(dplyr) # summarising, organizing, and manipulating data subsets
library(reshape2) # rearranging data
library(readr) # reading data into R
library(Hmisc) # capitalize function
# -------------------------------------------------------------------------------

# Workspace setup ---------------------------------------------------------------
# Data should be in ./Data
# Code should be in ./Code
# Ensure we're in the proper working directory:
wd <- str_split(getwd(), "/")[[1]][-1] # list of all folders in the path

# if SoybeanYieldForecast is not in the path, stop with an error
if(!"SoybeanYieldForecast"%in%wd){
  stop(paste0("SoybeanYieldForecast (the project root directory) must be in the path: \n", getwd()))
}

# otherwise, check to make sure it's the last directory in the path
# (so that all file paths will be accurate from this point on)
if(which(wd=="SoybeanYieldForecast")!=length(wd)){
  # If the project root is not the last folder in the path,
  # then transverse the path until the project root is in the path
  setwd(paste(rep("../", length(wd)-which(wd=="SoybeanYieldForecast")), collapse=""))
}
rm(wd)

# -------------------------------------------------------------------------------


# Unit conversion --------------------------------------------------------------
# Convert rain from mm to inches. [inches =  0.0393701 * mm]
# Convert max temperature from oC to F. [F = 32 + C*9/5]
# Convert min temperature from oC to F. [F = 32 + C*9/5]

mmToIn <- function(x){  0.0393701*x }
cToF <- function(x) { 32 + x*9/5 }

# -------------------------------------------------------------------------------

# File name, format and explanation ---------------------------------------------
# Climate.csv
# the format will not change – it is the final, but every 7-10 days we will add some more rows, not columns.
# The met files contains daily historical data (1980-2014 on max and min temperature, radiation and rain from 2 locations, Ames and Sutherlands).
# Also it has actual data from 2015, from January 1 to june 25, 2015 approximately, for each location.
# -------------------------------------------------------------------------------

# Variables --------------------------------------------------------------------
# max-ames-h    = maximum temperature in oC from Ames using historical data (1980-2014)
# max-suth-h    = maximum temperature in oC from Sutherland using historical data (1980-2014)
# max-ames      =  maximum temperature in oC from Ames using actual data from 2015
# max-suth      = maximum temperature in oC from Sutherland using actual data from 2015
#
# min-ames-h    = minimum temperature in oC from Ames using historical data (1980-2014)
# min-suth-h    = minimum temperature in oC from Sutherland using historical data (1980-2014)
# min-ames      =  minimum temperature in oC from Ames using actual data from 2015
# min-suth      = minimum temperature in oC from Sutherland using actual data from 2015
#
# rad-ames-h    = radiation in Mj/m2 from Ames using historical data (1980-2014)
# rad-suth-h    = radiation in Mj/m2 from Sutherland using historical data (1980-2014)
# rad-ames      =  radiation in Mj/m2 from Ames using actual data from 2015
# rad-suth      = radiation in Mj/m2 from Sutherland using actual data from 2015
#
# rain-ames-h   = rain in mm from Ames using historical data (1980-2014)
# rain-suth-h   = rain in mm from Sutherland using historical data (1980-2014)
# rain-ames     =  rain in mm from Ames using actual data from 2015
# rain-suth     = rain in mm from Sutherland using actual data from 2015
# -------------------------------------------------------------------------------

# Columns explanation -----------------------------------------------------------
# - Index = number of simulations (34 years * 365 calendar days within a year; not needed; skip this)
# - Day = day of the year (1 to 366 or 365 depends).
#      Note that the “day” is repeated several times within the file because this is how APSIM outputs data and do not want to spend time deleting extra columns.
#      Just use column “B” to refer to historical years (1980-2014) and column “F” for the actual year (2015). #      In column “B” there are several cycles from 1 to 365, the first one refers to year 1980 and the last one to year 2014
# -------------------------------------------------------------------------------

## Data Setup
