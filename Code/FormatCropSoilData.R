## Climate Data Setup

library(ggplot2) # plotting
library(lubridate) # handling date/time data
library(stringr) # string manipulation
library(dplyr) # summarising, organizing, and manipulating data subsets
library(reshape2) # rearranging data
library(readr) # reading data into R
library(Hmisc) # capitalize function

source("Code/UnitConversions.R")

# Function to fix yield and biomass
cummaxValue <- function(df){
  unmodified <- filter(df, !variable%in%c("biomass", "yield"))
  modified <- filter(df, variable%in%c("biomass", "yield")) %>%
    arrange(plotDate) %>%
    group_by(variable, Type, Year) %>%
    mutate(value = cummax(value)) %>%
    ungroup()

  return(bind_rows(unmodified, modified))
}


# Read in crop and soil data ----------------------------------------------------
CornAmesEarly<-read_csv("Data/Corn_Ames_Early.csv")
CornAmesEarly$location<-"ames"
CornAmesEarly$planting<-"early"
CornAmesEarly$crop<-"corn"

CornAmesLate<-read_csv("Data/Corn_Ames_Late.csv")
CornAmesLate$location<-"ames"
CornAmesLate$planting<-"late"
CornAmesLate$crop<-"corn"

CornSuthEarly<-read_csv("Data/Corn_Suth_Early.csv")
CornSuthEarly$location<-"suth"
CornSuthEarly$planting<-"early"
CornSuthEarly$crop<-"corn"
colnames(CornSuthEarly)<-colnames(CornAmesEarly)

CornSuthLate<-read_csv("Data/Corn_Suth_Late.csv")
CornSuthLate$location<-"suth"
CornSuthLate$planting<-"late"
CornSuthLate$crop<-"corn"
colnames(CornSuthLate)<-colnames(CornAmesEarly)

SoyAmesEarly<-read_csv("Data/Soy_Ames_Early.csv")
SoyAmesEarly$location<-"ames"
SoyAmesEarly$planting<-"early"
SoyAmesEarly$crop<-"soy"

SoyAmesLate<-read_csv("Data/Soy_Ames_Late.csv")
SoyAmesLate$location<-"ames"
SoyAmesLate$planting<-"late"
SoyAmesLate$crop<-"soy"

SoySuthEarly<-read_csv("Data/Soy_Suth_Early.csv")
SoySuthEarly$location<-"suth"
SoySuthEarly$planting<-"early"
SoySuthEarly$crop<-"soy"
colnames(SoySuthEarly)<-colnames(CornAmesEarly)

SoySuthLate<-read_csv("Data/Soy_Suth_Late.csv")
SoySuthLate$location<-"suth"
SoySuthLate$planting<-"late"
SoySuthLate$crop<-"soy"
colnames(SoySuthLate)<-colnames(CornAmesEarly)

cropData<-rbind(CornAmesEarly, CornAmesLate, CornSuthEarly, CornSuthLate, 
                SoyAmesEarly, SoyAmesLate, SoySuthEarly, SoySuthLate)

# remove spaces from the end of variable names
names(cropData) <- names(cropData) %>%
  str_replace("[ \\s]$", "")
# Set year from date/Index
cropData$Year <- year(ymd("1980-01-01") + days(1:nrow(cropData)-1))
# Set date from date/Index
cropData$Date <- ymd("1980-01-01") + days(1:nrow(cropData)-1)
# Get date of year from index information
cropData$plotDate <- cropData$Date
year(cropData$plotDate) <- 2015

# 2015 Data ---------------------------------------------------------------------
# pull out 2015 data
cropData2015 <- cropData[,c(1, 6,58, 59, 60, which(str_detect(names(cropData), "2015")))]
# Remove rows with mainly NA values
cropData2015 <- filter(cropData2015, rowSums(is.na(cropData2015))<4)
# Convert Units
cropData2015 <- convertUnits(cropData2015)
# Convert to long form and alter variable names
cropData2015 <- melt(cropData2015, id.vars = c(1,2, 3, 4, 5), variable.name="variable", value.name="value")
cropData2015$variable <- str_replace(cropData2015$variable, "2015$", "")
# Set type (for when data is merged back together)
cropData2015$Type <- "2015"
# Set date
cropData2015$plotDate <- ymd("2015-01-01") + days(cropData2015$day-1)
cropData2015$Date <- ymd("2015-01-01") + days(cropData2015$day-1)
cropData2015$Year <- year(cropData2015$Date)
# Indicate projected data
endDataDate <- unique(cropData2015$plotDate[cropData2015$day==(max(cropData2015$day)-13)])
cropData2015$projected <- cropData2015$plotDate>=endDataDate
cropDataExtra <- filter(cropData2015, plotDate==endDataDate)
cropData2015$Type[cropData2015$projected] <- "Forecasted"
cropDataExtra$projected <- FALSE
cropData2015 <- bind_rows(cropData2015, cropDataExtra)
cropData2015 <- cropData2015 %>%
  group_by(Type, variable) %>%
  arrange(variable, plotDate) %>%
  cummaxValue()
rm(cropDataExtra)
# End 2015 Data Processing  -----------------------------------------------------

# Historical Data (2015 Management) ---------------------------------------------
# pull out historical data with 2015 management
cropDataHistorical <- cropData[,c(1, 2, 58, 59, 60, which(str_detect(names(cropData), "H$")))]
# Remove rows with mainly NA values
cropDataHistorical <- filter(cropDataHistorical, rowSums(is.na(cropDataHistorical))<4)
# Convert Units
cropDataHistorical <- convertUnits(cropDataHistorical)
# Convert to long form and alter variable names
cropDataHistorical <- melt(cropDataHistorical, id.vars = c(1,2, 3, 4, 5), variable.name="variable", value.name="value")
cropDataHistorical$variable <- str_replace(cropDataHistorical$variable, "H$", "")
# Set type (for when data is merged back together)
cropDataHistorical$Type <- "Historical climate data, 2015 management"
# Set date
cropDataHistorical$plotDate <- ymd("2015-01-01") + days(cropDataHistorical$day-1)
cropDataHistorical$Date <- ymd("1980-01-01") + days(cropDataHistorical$Index)
cropDataHistorical$Year <- year(cropDataHistorical$Date)
cropDataHistorical <- cropDataHistorical %>%
  group_by(Type, Year, variable)%>%
  arrange(variable, Year, plotDate) %>%
  cummaxValue()
# End Processing Historical Data (2015 Management) ------------------------------

# Measured Data -----------------------------------------------------------------
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
# Convert Units
measured.values <- convertUnits(measured.values)
measured.se <- convertUnits(measured.se)
# label days with corresponding variables, then tranform to long form
names(measured.days)[-1] <- str_sub(names(measured.values)[-1], 0, -2)
measured.days <- melt(measured.days, id.vars = c(1,3,4,5), variable.name="variable", value.name="day")
# transform to long form and then remove "M" from variable names
measured.values <- melt(measured.values, id.vars = c(1,3,4,5), variable.name = "variable", value.name = "value")
measured.values$variable <- str_replace(measured.values$variable, "M$", "")
# transform to long form and then remove "MStdError" from variable names
measured.se <- melt(measured.se, id.vars = c(1, 3, 4, 5), variable.name = "variable", value.name = "StdError")
measured.se$variable <- str_replace(measured.se$variable, "MStdError$", "")

# Merge datasets together
cropDataMeasured <- suppressWarnings(left_join(measured.days, measured.values) %>% left_join(measured.se) %>% filter(!is.na(day)))
# Set type (for when data is merged back together)
cropDataMeasured$Type <- "2015"
cropDataMeasured$PointType <- "2015"
# Set date
cropDataMeasured$day<-as.integer(cropDataMeasured$day)
cropDataMeasured$plotDate <- ymd("2015-01-01") + days(cropDataMeasured$day-1)
cropDataMeasured$Date <- ymd("2015-01-01") + days(cropDataMeasured$day-1)
cropDataMeasured$Year <- year(cropDataMeasured$Date)
rm(measured.values, measured.se, measured.days, var.types, idx)
# End Processing Measured Data --------------------------------------------------

# Expected End of Season values -------------------------------------------------
cropDataEndSeason <- cropData[,c(1, 2, 58, 59, 60, which(str_detect(names(cropData), "HF$")))]
# Remove rows with mainly NA values
cropDataEndSeason <- filter(cropDataEndSeason, rowSums(is.na(cropDataEndSeason))<4)
# Convert Units
cropDataEndSeason <- convertUnits(cropDataEndSeason)
# Convert to long form and alter variable names
cropDataEndSeason <- melt(cropDataEndSeason, id.vars = c(1,2, 3, 4,5), variable.name="variable", value.name="value")
cropDataEndSeason$variable <- str_replace(cropDataEndSeason$variable, "HF$", "")
# Set type (for when data is merged back together)
cropDataEndSeason$Type <- "Expected Value (to end of season)"
# Set date
cropDataEndSeason$plotDate <- ymd("2015-01-01") + days(cropDataEndSeason$day-1)
cropDataEndSeason$Date <- ymd("1980-01-01") + days(cropDataEndSeason$Index)
cropDataEndSeason$Year <- year(cropDataEndSeason$Date)
cropDataEndSeason <- cropDataEndSeason %>%
  group_by(Type, Year, variable)%>%
  arrange(variable, Year, plotDate) %>%
  cummaxValue()
# End Processing Expected End of Season values ----------------------------------

# Soil Data ---------------------------------------------------------------------
soilData <- cropData[,c("SAT", "DUL", "LL")] %>%
  unique() %>% convertUnits()
soilData <- filter(soilData, rowSums(is.na(soilData))<3) %>%
  melt(variable.name="Type", value.name="value")
soilData$Type <- factor(soilData$Type, levels=c("SAT", "DUL", "LL"), labels=c("Saturated Water Content", "Field Capacity", "Permanent Wilting Point"))
# End Processing Soil Data ------------------------------------------------------
