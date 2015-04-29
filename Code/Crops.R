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
# Read in crop/soil data --------------------------------------------------------

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
# Convert Biomass
cropData2015$biomass2015 <- KgHaToLbAcre(cropData2015$biomass2015)
# Convert to long form and alter variable names
cropData2015 <- melt(cropData2015, id.vars = c(1,2), variable.name="variable", value.name="value")
cropData2015$variable <- str_replace(cropData2015$variable, "2015$", "")
# Set type (for when data is merged back together)
cropData2015$Type <- "2015 Production"
# Set date
cropData2015$plotDate <- ymd("2015-01-01") + days(cropData2015$day-1)
cropData2015$Date <- ymd("2015-01-01") + days(cropData2015$day-1)
cropData2015$Year <- year(cropData2015$Date)
# Indicate projected data
endDataDate <- unique(cropData2015$plotDate[cropData2015$day==(max(cropData2015$day)-13)])
cropData2015$projected <- cropData2015$plotDate>=endDataDate
cropDataExtra <- filter(cropData2015, plotDate==endDataDate)
cropData2015$Type[cropData2015$projected] <- "Forecasted 2015 Production"
cropDataExtra$projected <- FALSE
cropData2015 <- bind_rows(cropData2015, cropDataExtra)
cropData2015 <- cropData2015 %>%
  group_by(Type, variable) %>%
  arrange(variable, plotDate)


# pull out historical data with 2015 management
cropDataHistorical <- cropData[,c(1, 2, which(str_detect(names(cropData), "H$")))]
# Remove rows with mainly NA values
cropDataHistorical <- filter(cropDataHistorical, rowSums(is.na(cropDataHistorical))<6)
# Convert Biomass
cropDataHistorical$biomassH <- KgHaToLbAcre(cropDataHistorical$biomassH)
# Convert to long form and alter variable names
cropDataHistorical <- melt(cropDataHistorical, id.vars = c(1,2), variable.name="variable", value.name="value")
cropDataHistorical$variable <- str_replace(cropDataHistorical$variable, "H$", "")
# Set type (for when data is merged back together)
cropDataHistorical$Type <- "Historical climate data, 2015 management"
# Set date
cropDataHistorical$plotDate <- ymd("2015-01-01") + days(cropDataHistorical$day-1)
cropDataHistorical$Date <- ymd("1980-01-01") + days(cropDataHistorical$Index)
cropDataHistorical$Year <- year(cropDataHistorical$Date)
cropDataHistorical <- cropDataHistorical %>%
  group_by(Type, Year, variable)

# Measured data
var.types <- c("biomass", "lai", "NO3", "SW", "ST")
# Columns are ordered day | ___M | ___MStdError
# start by detecting StdError column, since it's the only column without repeated values
idx <- which(str_detect(names(cropData), "(biomass|lai|NO3|SW|ST)MStdError$"))
# corresponding day occurs 2 cols before StdError
measured.days <- cropData[,c(1, idx-2)]
# corresponding value occurs 1 col before StdError
measured.values <- cropData[,c(1, idx-1)]
# Convert biomass
measured.values$biomassM <- KgHaToLbAcre(measured.values$biomassM)
# Standard error columns
measured.se <- cropData[,c(1, idx)]
# Convert biomass
measured.se$biomassMStdError <- KgHaToLbAcre(measured.se$biomassMStdError)
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
cropDataMeasured$Type <- "2015 Production"
cropDataMeasured$PointType <- "2015 Production"
# Set date
cropDataMeasured$plotDate <- ymd("2015-01-01") + days(cropDataMeasured$day-1)
cropDataMeasured$Date <- ymd("2015-01-01") + days(cropDataMeasured$day-1)
cropDataMeasured$Year <- year(cropDataMeasured$Date)

# Expected End of Season values
cropDataEndSeason <- cropData[,c(1, 2, which(str_detect(names(cropData), "HF$")))]
# Remove rows with mainly NA values
cropDataEndSeason <- filter(cropDataEndSeason, rowSums(is.na(cropDataEndSeason))<6)
# Convert Biomass
cropDataEndSeason$biomassHF <- KgHaToLbAcre(cropDataEndSeason$biomassHF)
# Convert to long form and alter variable names
cropDataEndSeason <- melt(cropDataEndSeason, id.vars = c(1,2), variable.name="variable", value.name="value")
cropDataEndSeason$variable <- str_replace(cropDataEndSeason$variable, "HF$", "")
# Set type (for when data is merged back together)
cropDataEndSeason$Type <- "Expected Value (to end of season)"
# Set date
cropDataEndSeason$plotDate <- ymd("2015-01-01") + days(cropDataEndSeason$day-1)
cropDataEndSeason$Date <- ymd("1980-01-01") + days(cropDataEndSeason$Index)
cropDataEndSeason$Year <- year(cropDataEndSeason$Date)
cropDataEndSeason <- cropDataEndSeason %>%
  group_by(Type, Year, variable)

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

# Set default ggplot2 theme
theme_set(theme_bw())


ggplot() +
  ylab("Biomass (lbs/acre)") +
  xlab(NULL) +
  xlim(mdy(c("04-20-2015", "10-31-2015"))) +
  # Historical Bands
  geom_ribbon(aes(x=plotDate, ymin=lb, ymax=ub, fill=FillType),
              data=filter(historicalBands, variable=="biomass")) +
  # End of Season Bands
  geom_ribbon(aes(x=plotDate, ymin=LB, ymax=UB, fill=FillType),
              data=filter(endSeason, variable=="biomass"), alpha=.25) +
  # Historical Median
  geom_line(aes(x=plotDate, y=median, color=Type, linetype=Type), size=1,
            data=filter(historicalBands, variable=="biomass")) +
  # 2015 Data and projection
  geom_line(aes(x=plotDate, y=value, color=Type, linetype=Type), size=2,
            data=filter(cropData2015, variable=="biomass")) +
  # 2015 Measured Data
  geom_point(aes(x=plotDate, y=value, color=Type, shape=PointType),
             data=filter(cropDataMeasured, variable=="biomass"), size=5) +
  # 2015 Measured SE
  geom_errorbar(aes(x=plotDate, ymin=value-StdError, ymax=value+StdError, color=Type),
                data=filter(cropDataMeasured, variable=="biomass")) +
  scale_shape_manual("Measured Data", values=1) +
  scale_linetype_manual(values=c(
    "2015 Production" = "solid",
    "Expected Value (to end of season)" = NA,
    "Forecasted 2015 Production" = "11",
    "Historical climate data, 2015 management" = "solid")) +
  scale_fill_manual("90% Prediction Intervals", values=c(
    "2015 Production" = "transparent",
    "Expected Value (to end of season)" = "grey30",
    "Forecasted 2015 Production" = "transparent",
    "Historical climate data, 2015 management" = "darkseagreen1")) +
  scale_color_manual(values=c(
    "2015 Production" = "black",
    "Expected Value (to end of season)" = NA,
    "Forecasted 2015 Production" = "grey30",
    "Historical climate data, 2015 management" = "darkseagreen4")) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.position="bottom")

# -------------------------------------------------------------------------------

# Leaf Area Index.
# X-axis = Date from April 20 to Oct 31, fixed.
# Y-axis = Leaf area index (m2 leaf/m2 soil)
# Y-Variables to show in the figure
# 1. laiH, column O versus column B, to appear as a shade and calculate also the long term average value and show it as a black line. In the legend call it LAI with 2015 management and historical weather data
# 2. lai2015, column S versus column F, to appear as a thick solid line – leave out the last 14 rows! In the legend call it LAI 2015
# 3. lai2015, column S versus column F, to appear as a thick solid line – plot only the last 14 rows! In the legend call it forecasted LAI for the next 14 days
# 4. laiHF, column Q versus column B, to appear as a shade. In the legend call it expected LAI until the end of the season. This plot should start from the point when the lai2015 ends.
# 5. laiM, column Z versus column T, to appear as points. In the legend call it measured LAI. # 6. laiMStdError, column AC versus T, to appear as +/- error bars in the points.

# Set default ggplot2 theme
theme_set(theme_bw())


ggplot() +
  ylab("Leaf Area Index (m^2 leaf / m^2 soil)") +
  xlab(NULL) +
  xlim(mdy(c("04-20-2015", "10-31-2015"))) +
  # Historical Bands
  geom_ribbon(aes(x=plotDate, ymin=lb, ymax=ub, fill=FillType),
              data=filter(historicalBands, variable=="lai")) +
  # End of Season Bands
  geom_ribbon(aes(x=plotDate, ymin=LB, ymax=UB, fill=FillType),
              data=filter(endSeason, variable=="lai"), alpha=.25) +
  # Historical Median
  geom_line(aes(x=plotDate, y=median, color=Type, linetype=Type), size=1,
            data=filter(historicalBands, variable=="lai")) +
  # 2015 Data and projection
  geom_line(aes(x=plotDate, y=value, color=Type, linetype=Type), size=2,
            data=filter(cropData2015, variable=="lai")) +
  # 2015 Measured Data
  geom_point(aes(x=plotDate, y=value, color=Type, shape=PointType),
             data=filter(cropDataMeasured, variable=="lai"), size=5) +
  # 2015 Measured SE
  geom_errorbar(aes(x=plotDate, ymin=value-StdError, ymax=value+StdError, color=Type),
                data=filter(cropDataMeasured, variable=="lai")) +
  scale_shape_manual("Measured Data", values=1) +
  scale_linetype_manual(values=c(
    "2015 Production" = "solid",
    "Expected Value (to end of season)" = NA,
    "Forecasted 2015 Production" = "11",
    "Historical climate data, 2015 management" = "solid")) +
  scale_fill_manual("90% Prediction Intervals", values=c(
    "2015 Production" = "transparent",
    "Expected Value (to end of season)" = "grey30",
    "Forecasted 2015 Production" = "transparent",
    "Historical climate data, 2015 management" = "darkseagreen1")) +
  scale_color_manual(values=c(
    "2015 Production" = "black",
    "Expected Value (to end of season)" = NA,
    "Forecasted 2015 Production" = "grey30",
    "Historical climate data, 2015 management" = "darkseagreen4")) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.position="bottom")

# -------------------------------------------------------------------------------

# Yield
# Leave for now
# X-axis = Date from April 20 to Oct 31, fixed.
# Y-axis =
# Y-Variables to show in the figure
# 1.

# Set default ggplot2 theme
theme_set(theme_bw())


ggplot() +
  ylab("Yield") +
  xlab(NULL) +
  xlim(mdy(c("04-20-2015", "10-31-2015"))) +
  # Historical Bands
  geom_ribbon(aes(x=plotDate, ymin=lb, ymax=ub, fill=FillType),
              data=filter(historicalBands, variable=="yield")) +
  # End of Season Bands
  geom_ribbon(aes(x=plotDate, ymin=LB, ymax=UB, fill=FillType),
              data=filter(endSeason, variable=="yield"), alpha=.25) +
  # Historical Median
  geom_line(aes(x=plotDate, y=median, color=Type, linetype=Type), size=1,
            data=filter(historicalBands, variable=="yield")) +
  # 2015 Data and projection
  geom_line(aes(x=plotDate, y=value, color=Type, linetype=Type), size=2,
            data=filter(cropData2015, variable=="yield")) +
#   # 2015 Measured Data
#   geom_point(aes(x=plotDate, y=value, color=Type, shape=PointType),
#              data=filter(cropDataMeasured, variable=="yield"), size=5) +
#   # 2015 Measured SE
#   geom_errorbar(aes(x=plotDate, ymin=value-StdError, ymax=value+StdError, color=Type),
#                 data=filter(cropDataMeasured, variable=="yield")) +
  # scale_shape_manual("Measured Data", values=1) +
  scale_linetype_manual(values=c(
    "2015 Production" = "solid",
    "Expected Value (to end of season)" = NA,
    "Forecasted 2015 Production" = "11",
    "Historical climate data, 2015 management" = "solid")) +
  scale_fill_manual("90% Prediction Intervals", values=c(
    "2015 Production" = "transparent",
    "Expected Value (to end of season)" = "grey30",
    "Forecasted 2015 Production" = "transparent",
    "Historical climate data, 2015 management" = "lightgoldenrod1")) +
  scale_color_manual(values=c(
    "2015 Production" = "black",
    "Expected Value (to end of season)" = NA,
    "Forecasted 2015 Production" = "grey30",
    "Historical climate data, 2015 management" = "lightgoldenrod4")) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.position="bottom")
