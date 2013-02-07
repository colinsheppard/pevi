###################################################################################################
# objectives.R 
#
# Objective function file for optimization.  This file contains functions that will have 
# access to any data present at the end of a batch of model runs whose results are stored in a 
# data frame "results" which mirrors the data frame generated in run-experiment.R.
# The function must return a scalar numeric value.  These functions will be minimized.  All of the various
# objective functions are defined here.  Only one is used at a time and that objective is specified
# in optim-config.R
###################################################################################################

frac.stranded.by.delay <- function(results){
  return(mean(results$frac.stranded.by.delay,na.rm=T))
}

frac.drivers.delayed <- function(results){
  return(mean(results$frac.drivers.delayed,na.rm=T))
}

infrastructure.cost <- function(results){
  return(mean(results$infrastructure.cost,na.rm=T))
}

