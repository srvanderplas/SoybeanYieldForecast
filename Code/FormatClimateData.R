## Climate Data Setup

source("Code/UnitConversions.R")

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
  # Debugging: variable definitions
  # var="max"
  # projected=NULL
  # locSubs=data_frame(from="suth", to="sutherland")

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
  histData <- histData[!is.na(histData[,valueName]),]


  # get current information
  dvars <- names(climateData)[which(grepl(var, names(climateData)) & !grepl("-h", names(climateData)))]

  curData <- climateData[,c("Index", "Day", "Year", dvars)] %>%
    # Convert to long form
    melt(id.vars=c("Index", "Day", "Year"), value.name=valueName, variable.name="Location")

  # Indicate data is for the current year
  curData$CurrentYear <- TRUE
  # Remove NA values
  curData <- curData[!is.na(curData[,valueName]),]

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
