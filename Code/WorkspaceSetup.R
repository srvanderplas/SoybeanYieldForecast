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
