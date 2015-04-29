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

source("Code/UnitConversions.R")

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
source("Code/FormatClimateData.R")

# Set default ggplot2 theme
theme_set(theme_bw())

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

# Projected
ProjRainfall <- filter(Rainfall, Projected)
LastRecordedRain <- with(Rainfall, Rainfall[(plotDate%in%min(ProjRainfall$plotDate-days(1)) & CurrentYear),])
ProjRainfall <- rbind(LastRecordedRain, ProjRainfall) %>%
  group_by(Location, plotDate, CurrentYear) %>%
  summarise(CumRain=median(CumRain)) %>%
  mutate(Type="Projected")

AvgRainfall <- rbind(AvgRainfall, ProjRainfall)

AvgRainfall$Type <- factor(AvgRainfall$Type, levels=c("Historical\nAverage", "2015", "Projected"))
rain.bands$Type <- factor(rain.bands$Type, levels=c("Historical\nAverage", "2015", "Projected"))
AvgRainfall$linetype <- c("1", "1", "11")[AvgRainfall$Type]

ggplot() +
  geom_ribbon(data=rain.bands, aes(x=plotDate, ymin=lb, ymax=ub), alpha=0.5, fill="lightskyblue3") +
  geom_line(data=AvgRainfall, aes(x=plotDate, y=CumRain, color=Type, linetype=Type, size=Type, group=Type)) +
  facet_wrap(~Location, nrow=2) +
  scale_color_manual("", values=c("lightskyblue4", "black", "black")) +
  scale_linetype_manual("", values=c("solid", "solid", "11")) +
  scale_size_manual("", values=c(1, 2, 2)) +
  ggtitle("Rainfall") +
  ylab("Cumulative Rainfall (in)") +
  xlab("Date")

# library(knitr)
# kable(monthRainDevTable)
#--------------------------------------------------------------------------------

# Figure 2 ----------------------------------------------------------------------
# Figure 2: radiation from April 1st to October 31st (see attached picture)
# - Use the historical data to calculate median, 5 and 95% probabilities and shade
# - Projected are always the last 14 rows from “rad-ames-h”, “rad-suth-h”
# - Actual are the data in the e.g. “rad-ames” minus the last 14 rows
# - Calculate sums per month and % deviation from long term average. Tell R to output this in a table so we can copy and paste the information to the final destination.
# - Make a nice legend  and axes titles with units
# - Note the x-axis in the graph should always start from April 1st and end Oct 31st.
# - Start the accumulation of rain from April 1st and skip the extra data that I have in the csv.

# Cumulative Radiation
Radiation <- climate %>%
  arrange(Date) %>%
  filter(AprOct) %>%
  group_by(Location, Year, CurrentYear) %>%
  mutate(CumRad = cumsum(Radiation))

# Month Averages
monthRad <- Radiation %>%
  mutate(month = month(Date, label=T)) %>%
  group_by(Location, Year, month) %>%
  summarise(CumRad=CumRad[which.max(Date)], CurrentYear=unique(CurrentYear)) %>%
  group_by(Location, month) %>%
  mutate(AvgCumRad = mean(CumRad[!CurrentYear]))

monthRadDev <- monthRad %>%
  filter(Year==2015) %>%
  mutate(PctDevCumRad = (mean(CumRad[Year==2015])-AvgCumRad)/AvgCumRad*100)

monthRadDevTable <- dcast(monthRadDev, Location ~ month, value.var="PctDevCumRad")

# Calculate quantiles
Rad.bands <- filter(Radiation, !CurrentYear) %>%
  group_by(Location, plotDate) %>%
  summarise(lb=quantile(CumRad, .05),
            ub=quantile(CumRad, .95))
Rad.bands$Type <- "Historical\nAverage"


# 2015 data
AvgRadiation <- filter(Radiation, !Projected) %>%
  group_by(Location, plotDate, CurrentYear) %>%
  summarise(CumRad=median(CumRad)) %>%
  mutate(Type=c("Historical\nAverage", "2015")[CurrentYear+1])

# Projected
ProjRadiation <- filter(Radiation, Projected)
LastRecordedRad <- subset(Radiation, plotDate==(min(ProjRadiation$plotDate-days(1))) & CurrentYear)
ProjRadiation <- rbind(LastRecordedRad, ProjRadiation) %>%
  group_by(Location, plotDate, CurrentYear) %>%
  summarise(CumRad=median(CumRad)) %>%
  mutate(Type="Projected")

AvgRadiation <- rbind(AvgRadiation, ProjRadiation)

AvgRadiation$Type <- factor(AvgRadiation$Type, levels=c("Historical\nAverage", "2015", "Projected"))
Rad.bands$Type <- factor(Rad.bands$Type, levels=c("Historical\nAverage", "2015", "Projected"))
AvgRadiation$linetype <- c("1", "1", "11")[AvgRadiation$Type]

ggplot() +
  geom_ribbon(data=Rad.bands, aes(x=plotDate, ymin=lb, ymax=ub), alpha=0.5, fill="orange3") +
  geom_line(data=AvgRadiation, aes(x=plotDate, y=CumRad, color=Type, linetype=Type, size=Type, group=Type)) +
  facet_wrap(~Location, nrow=2) +
  scale_color_manual("", values=c("orange4", "black", "black")) +
  scale_linetype_manual("", values=c("solid", "solid", "11")) +
  scale_size_manual("", values=c(1, 2, 2)) +
  ggtitle("Radiation") +
  ylab("Cumulative Radiation (MJ/m^2)") +
  xlab("Date")

# Non-cumulative Radiation ------------------------------------------------------
Radiation <- climate %>%
  arrange(Date) %>%
  filter(AprOct) %>%
  group_by(Location, Year, CurrentYear)

# Month Averages
monthRad <- Radiation %>%
  mutate(month = month(Date, label=T)) %>%
  group_by(Location, Year, month) %>%
  summarise(Rad=mean(Radiation), CurrentYear=unique(CurrentYear)) %>%
  group_by(Location, month) %>%
  mutate(AvgRad = mean(Rad[!CurrentYear]))

monthRadDev <- monthRad %>%
  filter(Year==2015) %>%
  mutate(PctDevRad = (mean(Rad[Year==2015])-AvgRad)/AvgRad*100)

monthRadDevTable <- dcast(monthRadDev, Location ~ month, value.var="PctDevRad")

# Calculate quantiles
rad.bands <- filter(Radiation, !CurrentYear) %>%
  group_by(Location, plotDate) %>%
  summarise(lb=quantile(Radiation, .05),
            ub=quantile(Radiation, .95))
rad.bands$Type <- "Historical\nAverage"

# 2015 data
AvgRadiation <- filter(Radiation, !Projected) %>%
  group_by(Location, plotDate, CurrentYear) %>%
  summarise(Rad=median(Radiation)) %>%
  mutate(Type=c("Historical\nAverage", "2015")[CurrentYear+1])

# This option has a loess smooth for the median and current year's data, with the current year's observations shown as points
ggplot() +
  geom_ribbon(data=rad.bands, aes(x=plotDate, ymin=lb, ymax=ub), alpha=0.5, fill="orange3") +
  stat_smooth(data=AvgRadiation, aes(x=plotDate, y=Rad, color=Type), se=F, method="loess", geom="line", span=.5) +
  geom_point(data=subset(AvgRadiation, CurrentYear), aes(x=plotDate, y=Rad, color=Type), shape=1) +
  scale_color_manual("", values=c("2015" = "black", "Historical\nAverage"="orange4")) +
  facet_wrap(~Location, nrow=2) +
  ggtitle("Radiation") +
  ylab("Radiation (Mj/m^2)") +
  xlab("Date")

# A compromise between the two: use only 1/8th of the data to smooth at any given point
ggplot() +
  geom_ribbon(data=rad.bands, aes(x=plotDate, ymin=lb, ymax=ub), alpha=0.5, fill="orange3") +
  stat_smooth(data=AvgRadiation, aes(x=plotDate, y=Rad, color=Type), se=F, method="loess", geom="line", span=1/8) +
  geom_point(data=subset(AvgRadiation, CurrentYear), aes(x=plotDate, y=Rad, color=Type), shape=1) +
  scale_color_manual("", values=c("2015" = "black", "Historical\nAverage"="orange4")) +
  facet_wrap(~Location, nrow=2) +
  ggtitle("Radiation") +
  ylab("Radiation (Mj/m^2)") +
  xlab("Date")


# This option shows only the data, connected with a very jagged line.
ggplot() +
  geom_ribbon(data=rad.bands, aes(x=plotDate, ymin=lb, ymax=ub), alpha=0.5, fill="orange3") +
  geom_line(data=AvgRadiation, aes(x=plotDate, y=Rad, color=Type)) +
  scale_color_manual("", values=c("2015" = "black", "Historical\nAverage"="orange4")) +
  facet_wrap(~Location, nrow=2) +
  ggtitle("Radiation") +
  ylab("Radiation (Mj/m^2)") +
  xlab("Date")

# library(knitr)
# kable(monthRadDevTable)
#--------------------------------------------------------------------------------


# Figure 3 ----------------------------------------------------------------------
# Figure 3: Temperature from April 1st to October 31st
# - Use the historical data to calculate median, 5 and 95% probabilities and shade
# - Projected are always the last 14 rows from “min-ames-h”, “min-suth-h”
# - Actual are the data in the e.g. “min-ames” minus the last 14 rows
# - Calculate sums per month and % deviation from long term average. Tell R to output this in a table so we can copy and paste the information to the final destination.
# - Make a nice legend  and axes titles with units
# - Note the x-axis in the graph should always start from April 1st and end Oct 31st.
# - Start the accumulation of rain from April 1st and skip the extra data that I have in the csv.

# Temperature
Temp <- climate %>%
  arrange(Date) %>%
  filter(AprOct) %>%
  group_by(Location, Year, CurrentYear)

# Month Averages
monthTemp <- Temp %>%
  mutate(month = month(Date, label=T)) %>%
  group_by(Location, Year, month) %>%
  summarise(MinTemp=mean(MinTemp), MaxTemp=mean(MaxTemp), CurrentYear=unique(CurrentYear)) %>%
  group_by(Location, month) %>%
  mutate(AvgMinTemp = mean(MinTemp[!CurrentYear]), AvgMaxTemp = mean(MaxTemp[!CurrentYear]))

monthTempDev <- monthTemp %>%
  filter(Year==2015) %>%
  mutate(PctDevMinTemp = (mean(MinTemp[Year==2015])-AvgMinTemp)/AvgMinTemp*100,
         PctDevMaxTemp = (mean(MaxTemp[Year==2015])-AvgMaxTemp)/AvgMaxTemp*100)

monthMinTempDevTable <- dcast(monthTempDev, Location ~ month, value.var="PctDevMinTemp")
monthMaxTempDevTable <- dcast(monthTempDev, Location ~ month, value.var="PctDevMaxTemp")

# Calculate quantiles
min.temp.bands <- filter(Temp, !CurrentYear) %>%
  group_by(Location, plotDate) %>%
  summarise(lb=quantile(MinTemp, .05),
            ub=quantile(MinTemp, .95))
min.temp.bands$Type <- "Historical\nAverage"

max.temp.bands <- filter(Temp, !CurrentYear) %>%
  group_by(Location, plotDate) %>%
  summarise(lb=quantile(MaxTemp, .05),
            ub=quantile(MaxTemp, .95))
max.temp.bands$Type <- "Historical\nAverage"


# 2015 data
AvgTemp <- filter(Temp, !Projected) %>%
  group_by(Location, plotDate, CurrentYear) %>%
  summarise(MinTemp=median(MinTemp), MaxTemp=median(MaxTemp)) %>%
  mutate(Type=c("Historical\nAverage", "2015")[CurrentYear+1])

# Projected
ProjTemp <- filter(Temp, Projected) %>%
  group_by(Location, plotDate, CurrentYear) %>%
  summarise(MinTemp=median(MinTemp), MaxTemp=median(MaxTemp)) %>%
  mutate(Type="Projected")

AvgTemp <- rbind(AvgTemp, ProjTemp)

AvgTemp$Type <- factor(AvgTemp$Type, levels=c("Historical\nAverage", "2015", "Projected"))
min.temp.bands$Type <- factor(min.temp.bands$Type, levels=c("Historical\nAverage", "2015", "Projected"))
max.temp.bands$Type <- factor(max.temp.bands$Type, levels=c("Historical\nAverage", "2015", "Projected"))
AvgTemp$linetype <- c("1", "1", "11")[AvgTemp$Type]

ggplot() +
  geom_ribbon(data=min.temp.bands, aes(x=plotDate, ymin=lb, ymax=ub), alpha=0.5, fill="mediumorchid3") +
  geom_line(data=AvgTemp, aes(x=plotDate, y=MinTemp, color=Type, linetype=Type, size=Type, group=Type)) +
  facet_wrap(~Location, nrow=2) +
  scale_color_manual("", values=c("mediumorchid4", "black", "black")) +
  scale_linetype_manual("", values=c("solid", "solid", "11")) +
  scale_size_manual("", values=c(1, 2, 2)) +
  ggtitle("Minimum Temperature") +
  ylab("Degrees (F)") +
  xlab("Date")


ggplot() +
  geom_ribbon(data=max.temp.bands, aes(x=plotDate, ymin=lb, ymax=ub), alpha=0.5, fill="firebrick1") +
  geom_line(data=AvgTemp, aes(x=plotDate, y=MaxTemp, color=Type, linetype=Type, size=Type, group=Type)) +
  facet_wrap(~Location, nrow=2) +
  scale_color_manual("", values=c("firebrick4", "black", "black")) +
  scale_linetype_manual("", values=c("solid", "solid", "11")) +
  scale_size_manual("", values=c(1, 2, 2)) +
  ggtitle("Maximum Temperature") +
  ylab("Degrees (F)") +
  xlab("Date")
#--------------------------------------------------------------------------------
