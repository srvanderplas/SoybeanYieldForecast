# Code to plot crop data: yield, biomass, and lai
# in response to email from Sotirios on April 26, 2015

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
# Convert kg/ha to lbs/acre

KgHaToLbAcre <- function(x){ 0.89*x }

# -------------------------------------------------------------------------------

# File name, format and explanation ---------------------------------------------
# Crop_Ames_Early.csv
# The csv format will not change, but I might add extra columns to the right.
# This is 90% file format I would say.
# -------------------------------------------------------------------------------

# Variables --------------------------------------------------------------------

# -------------------------------------------------------------------------------

# Columns explanation -----------------------------------------------------------

# -------------------------------------------------------------------------------

## Data Setup
# Read in climate data ----------------------------------------------------------
# Read in header, split on comma, and remove spaces
vars <- readLines("Data/Crop_Ames_Early.csv", n=1) %>%
  str_split(pattern = ",") %>%
  unlist() %>%
  str_replace_all(" ", "")

cropData <- read_csv("Data/Crop_Ames_Early2.csv")
# Set year from date/Index
cropData$Year <- year(ymd("1980-01-01") + days(1:nrow(cropData)-1))
# Set date from date/Index
cropData$Date <- ymd("1980-01-01") + days(1:nrow(cropData)-1)
# Get date of year from index information
cropData$plotDate <- cropData$Date
year(cropData$plotDate) <- 2015

# pull out 2015 data
cropData2015 <- cropData[,c(1, 6, which(str_detect(names(cropData), "2015")))]
# Remove rows with mainly NA values
cropData2015 <- filter(cropData2015, rowSums(is.na(cropData2015))<6)
# Convert to long form and alter variable names
cropData2015 <- melt(cropData2015, id.vars = c(1,2), variable.name="variable", value.name="value")
cropData2015$variable <- str_replace(cropData2015$variable, "2015$", "")
# Set type (for when data is merged back together)
cropData2015$Type <- "2015"
# Set date
cropData2015$plotDate <- ymd("2015-01-01") + days(cropData2015$day-1)
cropData2015$Date <- ymd("2015-01-01") + days(cropData2015$day-1)
cropData2015$Year <- year(cropData2015$Date)
# Indicate projected data
cropData2015$projected <- (1:nrow(cropData2015))<(nrow(cropData2015)-13)
cropData2015 <- cropData2015 %>% group_by(Type, Year, variable) %>% filter(value>0)


# pull out historical data with 2015 management
cropDataHistorical <- cropData[,c(1, 2, which(str_detect(names(cropData), "H$")))]
# Remove rows with mainly NA values
cropDataHistorical <- filter(cropDataHistorical, rowSums(is.na(cropDataHistorical))<6)
# Convert to long form and alter variable names
cropDataHistorical <- melt(cropDataHistorical, id.vars = c(1,2), variable.name="variable", value.name="value")
cropDataHistorical$variable <- str_replace(cropDataHistorical$variable, "H$", "")
# Set type (for when data is merged back together)
cropDataHistorical$Type <- "Historical climate data, 2015 management"
# Set date
cropDataHistorical$plotDate <- ymd("2015-01-01") + days(cropDataHistorical$day-1)
cropDataHistorical$Date <- ymd("1980-01-01") + days(cropDataHistorical$Index)
cropDataHistorical$Year <- year(cropDataHistorical$Date)
cropDataHistorical <- cropDataHistorical %>% group_by(Type, Year, variable) %>% filter(value>0)

# Measured data
var.types <- c("biomass", "lai", "NO3", "SW", "ST")
# Columns are ordered day | ___M | ___MStdError
# start by detecting StdError column, since it's the only column without repeated values
idx <- which(str_detect(names(cropData), "(biomass|lai|NO3|SW|ST)MStdError$"))
# corresponding day occurs 2 cols before StdError
measured.days <- cropData[,c(1, idx-2)]
# corresponding value occurs 1 col before StdError
measured.values <- cropData[,c(1, idx-1)]
# Standard error columns
measured.se <- cropData[,c(1, idx)]
# label days with corresponding variables, then tranform to long form
names(measured.days)[-1] <- str_sub(names(measured.values)[-1], 0, -2)
measured.days <- melt(measured.days, id.vars = 1, variable.name="variable", value.name="day")
# transform to long form and then remove "M" from variable names
measured.values <- melt(measured.values, id.vars = 1, variable.name = "variable", value.name = "value")
measured.values$variable <- str_replace(measured.values$variable, "M$", "")
# transform to long form and then remove "MStdError" from variable names
measured.se <- melt(measured.se, id.vars = 1, variable.name = "variable", value.name = "StdError")
measured.se$variable <- str_replace(measured.se$variable, "MStdError$", "")

cropDataMeasured <- left_join(measured.days, measured.values) %>% left_join(measured.se) %>% filter(!is.na(day))
# Set type (for when data is merged back together)
cropDataMeasured$Type <- "Measured 2015 Value"
# Set date
cropDataMeasured$plotDate <- ymd("2015-01-01") + days(cropDataMeasured$day-1)
cropDataMeasured$Date <- ymd("2015-01-01") + days(cropDataMeasured$day-1)
cropDataMeasured$Year <- year(cropDataMeasured$Date)

# Expected End of Season values
cropDataEndSeason <- cropData[,c(1, 2, which(str_detect(names(cropData), "HF$")))]
# Remove rows with mainly NA values
cropDataEndSeason <- filter(cropDataEndSeason, rowSums(is.na(cropDataEndSeason))<6)
# Convert to long form and alter variable names
cropDataEndSeason <- melt(cropDataEndSeason, id.vars = c(1,2), variable.name="variable", value.name="value")
cropDataEndSeason$variable <- str_replace(cropDataEndSeason$variable, "HF$", "")
# Set type (for when data is merged back together)
cropDataEndSeason$Type <- "Expected Value (to end of season)"
# Set date
cropDataEndSeason$plotDate <- ymd("2015-01-01") + days(cropDataEndSeason$day-1)
cropDataEndSeason$Date <- ymd("1980-01-01") + days(cropDataEndSeason$Index)
cropDataEndSeason$Year <- year(cropDataEndSeason$Date)
cropDataEndSeason <- cropDataEndSeason %>% group_by(Type, Year, variable) %>% filter(value>0)

# -------------------------------------------------------------------------------


# Biomass (see example in the pptx).
# X-axis = Date from April 20 to Oct 31, fixed.
# Y-axis = Above ground total biomass (lbs/acre)
# Units conversion: lbs/acre = 0.89 * kg/ha (the reported biomass values are in kg/ha)
# Y-Variables to show in the figure
# 1. BiomassH, column C versus column B, to appear as a shade and calculate also the long term average value and show it as a black line. In the legend call it biomass production with 2015 management and historical weather data
# 2. Biomass2015, column G versus column F, to appear as a thick solid line – leave out the last 14 rows! In the legend call it biomass production 2015
# 3. Biomass2015, column G versus column F, to appear as a thick solid line – plot only the last 14 rows! In the legend call it forecasted biomass production for the next 14 days
# 4. BiomassHF, column E versus column B, to appear as a shade. In the legend call it expected biomass production until the end of the season. This plot should start from the point when the Biomass2015 ends.
# 5. BiomassM, column U versus column T, to appear as points. In the legend call it measured biomass production.
# 6. BiomassMStdError, column X versus T, to appear as +/- error bars in the points.

ggplot() +
  geom_line(aes(x=plotDate, y=value, group=Year), data=filter(cropDataHistorical, variable=="biomass" & plotDate >= dmy("20-4-2015") & plotDate<=dmy("31-10-2015")))
