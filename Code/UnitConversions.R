# Unit conversion --------------------------------------------------------------
# Convert kg/ha to lbs/acre
# Convert mm to inches [inches =  0.0393701 * mm]
# Convert temperature from oC to F. [F = 32 + C*9/5]

kgHaToLbAcre <- function(x){ 0.89*x }
mmToIn <- function(x){  0.0393701*x }
cToF <- function(x, shift=T) { ifelse(shift, 32, 0) + x*9/5 }

convertUnits <- function(df){
  vars <- names(df)
  kgha <- rowSums(sapply(c("biomass", "yield", "NO3"), function(i) grepl(vars, pattern=i)))>0
  mm <- rowSums(sapply(c("SW", "DUL", "LL", "SAT"), function(i) grepl(vars, pattern=i)))>0
  temp <- rowSums(sapply(c("ST"), function(i) grepl(vars, pattern=i)))>0

  is.se <- grepl(vars, pattern="StdError")

  for(i in vars[kgha]){
    df[,i] <- kgHaToLbAcre(df[,i])
  }
  for(i in vars[mm]){
    df[,i] <- mmToIn(df[,i])
  }
  for(i in vars[temp&!is.se]){
    df[,i] <- cToF(df[,i])
  }
  for(i in vars[temp&is.se]){
    df[,i] <- cToF(df[,i], shift=F)
  }
  return(df)
}

# -------------------------------------------------------------------------------
