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
# Read in climate data ----------------------------------------------------------
climateData <- read_csv("Data/Climate.csv")
# set column "B" to be "Day" and "F" to indicate 2015
names(climateData)[c(2, 6)] <- c("Day", "ThisYear")
# Delete extra "day" columns
idx <- which(names(climateData)=="day")
climateData <- climateData[,-idx]
# Set years
climateData$Year <- year(ymd("1980-01-01") + days(1:nrow(climateData)-1))
rm(idx)

# Vectorize string substitution pattern/replacement -----------------------------
apply_subs <- function(x, pattern, replacement){
  if(length(pattern)!=length(replacement)){
    stop("pattern and replacement must have the same length")
  }
  if(!is.character(pattern)){
    pattern <- as.character(pattern)
  }
  if(!is.character(replacement)){
    replacement <- as.character(replacement)
  }

  if(length(pattern)==1){
    return(str_replace_all(x, pattern, replacement))
  } else {
    return(str_replace_all(apply_subs(x, pattern[-1], replacement[-1]), pattern[1], replacement[1]))
  }
}

# Function to fix climate variables ---------------------------------------------
fixClimateVar <- function(climateData, var="temp", projected=NULL, locSubs = data_frame(from="suth", to="sutherland")){
  # Check var is one of "temp", "rad", "rain", "max", "min"
  if(!var%in%c("temp", "rad", "rain", "max", "min")){
    stop("Argument var must be one of \"temp\", \"rad\", \"rain\", \"max\", \"min\"")
  }

  if(var=="temp"){ # Deal with "temperature" case separately
    dfMax <- fixClimateVar(climateData, var="max", projected=projected)
    dfMin <- fixClimateVar(climateData, var="min", projected=projected)
    res <- merge(dfMax, dfMin) %>% arrange(!CurrentYear, Location, Index, Day)
    return(res)
  }

  valueName <- switch(var,
                      max = "MaxTemp",
                      min = "MinTemp",
                      rad = "Radiation",
                      rain = "Rain")

  # get historical information
  dvars <- names(climateData)[which(grepl(var, names(climateData)) & grepl("-h", names(climateData)))]

  histData <- climateData[,c("Index", "Day", "Year", dvars)] %>%
    # Convert to long form
    melt(id.vars=c("Index", "Day", "Year"), value.name=valueName, variable.name="Location")

  # Indicate data is historical
  histData$CurrentYear <- FALSE
  # Strip -h from variable name
  histData$Location <- str_replace_all(histData$Location, "-h", "")
  # Remove NA values
  histData <- filter(histData, !is.na(histData[,valueName]))


  # get current information
  dvars <- names(climateData)[which(grepl(var, names(climateData)) & !grepl("-h", names(climateData)))]

  curData <- climateData[,c("Index", "Day", "Year", dvars)] %>%
    # Convert to long form
    melt(id.vars=c("Index", "Day", "Year"), value.name=valueName, variable.name="Location")

  # Indicate data is for the current year
  curData$CurrentYear <- TRUE
  # Remove NA values
  curData <- filter(curData, !is.na(curData[,valueName]))

  # If projected days aren't defined, define them based on last 14 days of current data
  if(is.null(projected)){
    projected <- rev(sort(unique(curData$Day)))[1:14]
  }

  # Ensure column names are the same before rbinding everything together
  if(sum(!names(histData)%in%names(curData))!=0){
    stop("Names in Historical data do not match names in Current year's data - most probable cause is data file malformation")
  }

  res <- rbind(histData, curData)

  # Data is projected if it is from the current year and
  #   the Day is contained in the variable projected
  res$Projected <- res$Day%in%projected & res$CurrentYear

  # Ensure locSubs has the right number of columns
  if(ncol(locSubs)!=2){
    stop("locSubs argument must have two columns: from, to")
  }

  # Parse Location
  res$Location <- str_replace(res$Location, paste0(var, "-"), "") %>%
    apply_subs(locSubs[,1], locSubs[,2]) %>%
    capitalize()

  if(var%in%c("min", "max")){
    # Convert Celsius to Fahrenheit
    res[,valueName] <- cToF(res[,valueName])
  }

  if(var=="rain"){
    res[,valueName] <- mmToIn(res[,valueName])
  }

  res <- res %>% arrange(!CurrentYear, Location, Index, Day)
  return(res)
}

# Create variable datasets then combine ----------------------------------------

temperature <- fixClimateVar(climateData, "temp")
radiation <- fixClimateVar(climateData, "rad")
precipitation<- fixClimateVar(climateData, "rain")

climate <- merge(temperature, radiation)
climate <- merge(climate, precipitation) %>% arrange(!CurrentYear, Location, Index, Day)

# Housekeeping ------------------------------------------------------------------
# Calculate years
climate$Year[climate$CurrentYear] <- 2015

# Test if all years with 366 days are leap years
if(sum(!leap_year(unique(climate$Year[climate$Day==366])))>0)
  stop("Error: Non leap year with 366 days. Check Year calculation and/or spreadsheet format!")

## ------------------------------------------------------------------------------


### Graphs

# Figure 1 ----------------------------------------------------------------------
# Figure 1: cumulative rain from April 1st to October 31st (see attached picture)
# - Use the historical data to calculate median, 5 and 95% probabilities and shade
# - Projected are always the last 14 rows from “max-ames”, “max-suth”, “min-ames”, ….
# - Actual are the data in the e.g. “max-ames” minus the last 14 rows
# - Calculate sums per month and % deviation from long term average. Tell R to output this in a table so we can copy and paste the information to the final destination.
# - Make a nice legend  and axes titles with units
# - Note the x-axis in the graph should always start from April 1st and end Oct 31st.
# - Start the accumulation of rain from April 1st and skip the extra data that I have in the csv.

# Is date in correct range?
climate$Date <- as.Date(sprintf("%s-01-01", climate$Year))
yday(climate$Date) <- climate$Day
climate$AprOct <- abs(month(climate$Date)-7)<4
climate$plotDate <- climate$Date
year(climate$plotDate) <- 2015

# Cumulative Rainfall
Rainfall <- climate %>%
  arrange(Date) %>%
  filter(AprOct) %>%
  group_by(Location, Year, CurrentYear) %>%
  mutate(CumRain = cumsum(Rain))

# Month Averages
monthRain <- Rainfall %>%
  mutate(month = month(Date, label=T)) %>%
  group_by(Location, Year, month) %>%
  summarise(CumRain=CumRain[which.max(Date)], CurrentYear=unique(CurrentYear)) %>%
  group_by(Location, month) %>%
  mutate(AvgCumRain = mean(CumRain[!CurrentYear]))

monthRainDev <- monthRain %>%
  filter(Year==2015) %>%
  mutate(PctDevCumRain = (mean(CumRain[Year==2015])-AvgCumRain)/AvgCumRain*100)

monthRainDevTable <- dcast(monthRainDev, Location ~ month, value.var="PctDevCumRain")

# Calculate quantiles
rain.bands <- filter(Rainfall, !CurrentYear) %>%
  group_by(Location, plotDate) %>%
  summarise(lb=quantile(CumRain, .05),
            ub=quantile(CumRain, .95))
rain.bands$Type <- "Historical\nAverage"

# 2015 data
AvgRainfall <- filter(Rainfall, !Projected) %>%
  group_by(Location, plotDate, CurrentYear) %>%
  summarise(CumRain=median(CumRain)) %>%
  mutate(Type=c("Historical\nAverage", "2015")[CurrentYear+1])

ggplot() +
  geom_ribbon(data=rain.bands, aes(x=plotDate, ymin=lb, ymax=ub), alpha=0.5, fill="royalblue1") +
  geom_line(data=AvgRainfall, aes(x=plotDate, y=CumRain, color=Type)) +
  scale_color_manual("", values=c("2015" = "black", "Historical\nAverage"="royalblue4")) +
  facet_wrap(~Location, nrow=2) +
  ggtitle("Rainfall") +
  ylab("Cumulative Rainfall (in)") +
  xlab("Date")

library(knitr)
kable(monthRainDevTable)
#--------------------------------------------------------------------------------

