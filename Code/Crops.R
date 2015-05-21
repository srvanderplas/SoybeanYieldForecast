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

## Data Setup
# Read in crop/soil data --------------------------------------------------------

source("Code/FormatCropSoilData.R")

# Calculate Bands ---------------------------------------------------------------

cropDataHistorical<-na.omit(cropDataHistorical)
historicalBands <- cropDataHistorical %>%
  group_by(plotDate, variable, Type) %>%
  summarise(lb = quantile(value, .05),
            median = quantile(value, .50),
            ub = quantile(value, .95)) %>%
  filter(plotDate >= dmy("20-4-2015") & plotDate<=dmy("31-10-2015"))
historicalBands$FillType <- historicalBands$Type

cropDataEndSeason<-na.omit(cropDataEndSeason)
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


p <- ggplot() +
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
  geom_line(aes(x=plotDate, y=value, color=paste(Type, "Production"), linetype=paste(Type, "Production")), size=2,
            data=filter(cropData2015, variable=="biomass")) +
  # 2015 Measured Data
  geom_point(aes(x=plotDate, y=value, color=paste(Type, "Production"), shape=PointType),
             data=filter(cropDataMeasured, variable=="biomass"), size=5) +
  # 2015 Measured SE
  geom_errorbar(aes(x=plotDate, ymin=value-StdError, ymax=value+StdError,
                    color=paste(Type, "Production")),
                data=filter(cropDataMeasured, variable=="biomass")) +
  scale_shape_manual("Measured Data", values=1) +
  scale_linetype_manual(values=c(
    "2015 Production" = "solid",
    "Expected Value (to end of season)" = NA,
    "Forecasted Production" = "11",
    "Historical climate data, 2015 management" = "solid")) +
  scale_fill_manual("90% Prediction Intervals", values=c(
    "2015 Production" = "transparent",
    "Expected Value (to end of season)" = "grey30",
    "Forecasted Production" = "transparent",
    "Historical climate data, 2015 management" = "darkseagreen1")) +
  scale_color_manual(values=c(
    "2015 Production" = "black",
    "Expected Value (to end of season)" = NA,
    "Forecasted Production" = "grey30",
    "Historical climate data, 2015 management" = "darkseagreen4")) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  facet_wrap(~crop)+
  theme(legend.position="bottom", legend.direction="vertical", legend.box="horizontal")

plots<- cropData2015 %>%
        group_by(location, planting, crop) %>%
        do(plot = p %+% .)

pdf(file="same plots.pdf", height = 8, width = 8)
plots$plot
dev.off()

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
  geom_line(aes(x=plotDate, y=value, color=paste(Type, "Production"), linetype=paste(Type, "Production")), size=2,
            data=filter(cropData2015, variable=="lai")) +
  # 2015 Measured Data
  geom_point(aes(x=plotDate, y=value, color=paste(Type, "Production"), shape=PointType),
             data=filter(cropDataMeasured, variable=="lai"), size=5) +
  # 2015 Measured SE
  geom_errorbar(aes(x=plotDate, ymin=value-StdError, ymax=value+StdError,
                    color=paste(Type, "Production")),
                data=filter(cropDataMeasured, variable=="lai")) +
  scale_shape_manual("Measured Data", values=1) +
  scale_linetype_manual(values=c(
    "2015 Production" = "solid",
    "Expected Value (to end of season)" = NA,
    "Forecasted Production" = "11",
    "Historical climate data, 2015 management" = "solid")) +
  scale_fill_manual("90% Prediction Intervals", values=c(
    "2015 Production" = "transparent",
    "Expected Value (to end of season)" = "grey30",
    "Forecasted Production" = "transparent",
    "Historical climate data, 2015 management" = "darkseagreen1")) +
  scale_color_manual(values=c(
    "2015 Production" = "black",
    "Expected Value (to end of season)" = NA,
    "Forecasted Production" = "grey30",
    "Historical climate data, 2015 management" = "darkseagreen4")) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.position="bottom", legend.direction="vertical", legend.box="horizontal")


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
  geom_line(aes(x=plotDate, y=value, color=paste(Type, "Production"), linetype=paste(Type, "Production")), size=2,
            data=filter(cropData2015, variable=="yield")) +
#   # 2015 Measured Data
#   geom_point(aes(x=plotDate, y=value, color=paste(Type, "Production"), shape=PointType),
#              data=filter(cropDataMeasured, variable=="yield"), size=5) +
#   # 2015 Measured SE
#   geom_errorbar(aes(x=plotDate, ymin=value-StdError, ymax=value+StdError,
#                     color=paste(Type, "Production")),
#                 data=filter(cropDataMeasured, variable=="yield")) +
  # scale_shape_manual("Measured Data", values=1) +
  scale_linetype_manual(values=c(
    "2015 Production" = "solid",
    "Expected Value (to end of season)" = NA,
    "Forecasted Production" = "11",
    "Historical climate data, 2015 management" = "solid")) +
  scale_fill_manual("90% Prediction Intervals", values=c(
    "2015 Production" = "transparent",
    "Expected Value (to end of season)" = "grey30",
    "Forecasted Production" = "transparent",
    "Historical climate data, 2015 management" = "lightgoldenrod1")) +
  scale_color_manual(values=c(
    "2015 Production" = "black",
    "Expected Value (to end of season)" = NA,
    "Forecasted Production" = "grey30",
    "Historical climate data, 2015 management" = "lightgoldenrod4")) +
  guides(colour = guide_legend(override.aes = list(shape = NA))) +
  theme(legend.position="bottom", legend.direction="vertical", legend.box="horizontal")

