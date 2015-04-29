# Code to plot soil data: water, temperature, nitrogen,
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

source("Code/UnitConversions.R")

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

## Data Setup (same as Crops.R setup)
# Read in crop/soil data --------------------------------------------------------

source("Code/FormatCropSoilData.R")

# Calculate Bands ---------------------------------------------------------------

historicalBands <- cropDataHistorical %>%
  group_by(plotDate, variable, Type) %>%
  summarise(lb = quantile(value, .05),
            median = quantile(value, .50),
            ub = quantile(value, .95)) %>%
  filter(plotDate >= dmy("20-4-2015") & plotDate<=dmy("31-10-2015"))
historicalBands$FillType <- historicalBands$Type


endSeason <- cropDataEndSeason %>%
  group_by(plotDate, variable, Type) %>%
  summarise(LB = quantile(value, .05),
            median = quantile(value, .50),
            UB = quantile(value, .95)) %>%
  filter(plotDate >= endDataDate & plotDate<=dmy("31-10-2015"))
endSeason$FillType <- endSeason$Type

# -------------------------------------------------------------------------------

# Soil Water
# X-axis = Date from April 1st to Oct 31, fixed.
# Y-axis = Soil water (inches)
# Units conversion: inches =  0.0393701 * mm (the reported SW values are in mm)
# Y-Variables to show in the figure
# 1. SAT, column AF versus column B, to appear as a solid line.
#    In the legend call it Saturated water content
# 2. DUL, column AJ versus column B, to appear as a solid line.
#    In the legend call it Field capacity
# 3. LL, column AL versus column B, to appear as a solid line.
#    In the legend call it Permanent wilting point
# 4. SW2015, column AH versus column AG,
#    to appear as a thick solid line – leave out the last 14 rows!
#    In the legend call it soil water 2015
# 5. SW2015, column AH versus column AG,
#    to appear as a thick solid line – plot only the last 14 rows!
#    In the legend call it forecasted soil water for the next 14 days
# 6. SWM, column AT versus column AS,
#    to appear as points.
#    In the legend call it measured soil water
# 7. SWMStdError, column AU versus AS, to appear as +/- error bars in the points.

# Set default ggplot2 theme
theme_set(theme_bw())

soilData$color <- c("slateblue4", "brown", "red")

p <- ggplot()

for(i in 1:3){
  p <- p +
    geom_hline(aes(yintercept=value), color=soilData$color[i], size=1,
               data=soilData[i,]) +
    geom_text(aes(x=mdy("10-31-2015"), y=value, label=Type),
              vjust=-0.25, hjust=1, color=soilData$color[i],
              data=soilData[i,])
}

p +
  ylab("Soil Water (in)") +
  xlab(NULL) +
  xlim(mdy(c("04-01-2015", "10-31-2015"))) +
  # 2015 Data and projection
  geom_path(aes(x=plotDate, y=value,
                color=paste(Type, "Soil Water"),
                linetype=paste(Type, "Soil Water")), size=2,
            data=filter(cropData2015, variable=="SW")) +
  # 2015 Measured Data
  geom_point(aes(x=plotDate, y=value, shape="2015"), color="black",
             data=filter(cropDataMeasured, variable=="SW"), size=3) +
  # 2015 Measured SE
  geom_errorbar(aes(x=plotDate, ymin=value-StdError, ymax=value+StdError),
                color="black",
                data=filter(cropDataMeasured, variable=="SW")) +
  scale_shape_manual("Measured Data", values=1) +
  scale_linetype_manual(
    "Water Measurement",
    values=c(
      "2015 Soil Water" = "solid",
      "Forecasted Soil Water" = "11")) +
  scale_color_manual(
    "Water Measurement",
    values=c(
      "2015 Soil Water" = "steelblue",
      "2015 Measurement" = "black",
      "2015 Measurement Error" = "black",
      "Forecasted Soil Water" = "steelblue2")) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.position="bottom", legend.direction="vertical", legend.box="horizontal")


# -------------------------------------------------------------------------------

# Soil Temperature
# X-axis = Date from April 1st to Oct 31, fixed.
# Y-axis = Soil temperature (F)
# Units conversion: F = 32 + C*9/5 (the reported ST values are in oC)
# Y-Variables to show in the figure
# 1. ST2015, column AP versus column AG,
#    to appear as a thick solid line – leave out the last 14 rows!
#    In the legend call it soil temperature 2015
# 2. SW2015, column AP versus column AG,
#    to appear as a thick solid line – plot only the last 14 rows!
#    In the legend call it forecasted soil temperature for the next 14 days
# 3. STM, column BB versus column BA,
#    to appear as points.
#    In the legend call it measured soil temperature
# 4. STMStdError, column BE versus BA,
#    to appear as +/- error bars in the points.

# Set default ggplot2 theme
theme_set(theme_bw())


ggplot() +
  ylab("Soil Temperature (F)") +
  xlab(NULL) +
  xlim(mdy(c("04-01-2015", "10-31-2015"))) +
  # 2015 Data and projection
  geom_path(aes(x=plotDate, y=value,
                color=paste(Type, "Soil Temperature"),
                linetype=paste(Type, "Soil Temperature")), size=2,
            data=filter(cropData2015, variable=="ST")) +
  # 2015 Measured Data
  geom_point(aes(x=plotDate, y=value, shape="2015"), color="black",
             data=filter(cropDataMeasured, variable=="ST"), size=3) +
  # 2015 Measured SE
  geom_errorbar(aes(x=plotDate, ymin=value-StdError, ymax=value+StdError),
                color="black",
                data=filter(cropDataMeasured, variable=="ST")) +
  scale_shape_manual("Measured Data", values=1) +
  scale_linetype_manual(
    "Simulated Data",
    values=c(
      "2015 Soil Temperature" = "solid",
      "Forecasted Soil Temperature" = "11")) +
  scale_color_manual(
    "Simulated Data",
    values=c(
      "2015 Soil Temperature" = "violetred4",
      "2015 Measurement" = "black",
      "2015 Measurement Error" = "black",
      "Forecasted Soil Temperature" = "violetred2")) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.position="bottom", legend.direction="vertical", legend.box="horizontal")



# -------------------------------------------------------------------------------

# Soil Nitrogen
# X-axis = Date from April 1st to Oct 31, fixed.
# Y-axis = Soil Nitrogen in lbs/acres at 1 feet
# Units conversion: F = 32 + C*9/5 (the reported ST values are in oC)
# Y-Variables to show in the figure
# 1. ST2015, column AP versus column AG,
#    to appear as a thick solid line – leave out the last 14 rows!
#    In the legend call it soil temperature 2015
# 2. SW2015, column AP versus column AG,
#    to appear as a thick solid line – plot only the last 14 rows!
#    In the legend call it forecasted soil temperature for the next 14 days
# 3. STM, column BB versus column BA,
#    to appear as points.
#    In the legend call it measured soil temperature
# 4. STMStdError, column BE versus BA,
#    to appear as +/- error bars in the points.

# Set default ggplot2 theme
theme_set(theme_bw())


ggplot() +
  ylab("Soil Nitrogen (lbs/acre) at 1 foot") +
  xlab(NULL) +
  xlim(mdy(c("04-01-2015", "10-31-2015"))) +
  # 2015 Data and projection
  geom_path(aes(x=plotDate, y=value,
                color=paste(Type, "Soil Nitrogen"),
                linetype=paste(Type, "Soil Nitrogen")), size=2,
            data=filter(cropData2015, variable=="NO3")) +
  # 2015 Measured Data
  geom_point(aes(x=plotDate, y=value, shape="2015"), color="black",
             data=filter(cropDataMeasured, variable=="NO3"), size=3) +
  # 2015 Measured SE
  geom_errorbar(aes(x=plotDate, ymin=value-StdError, ymax=value+StdError),
                color="black",
                data=filter(cropDataMeasured, variable=="NO3")) +
  scale_shape_manual("Measured Data", values=1) +
  scale_linetype_manual(
    "Simulated Data",
    values=c(
      "2015 Soil Nitrogen" = "solid",
      "Forecasted Soil Nitrogen" = "11")) +
  scale_color_manual(
    "Simulated Data",
    values=c(
      "2015 Soil Nitrogen" = "violetred4",
      "2015 Measurement" = "black",
      "2015 Measurement Error" = "black",
      "Forecasted Soil Nitrogen" = "violetred2")) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.position="bottom", legend.direction="vertical", legend.box="horizontal")


