###################################################################################################
# objectives.R 
#
# Objective function file for optimization.  This file contains functions that will have 
# access to any data present at the end of a model run (after the post-process script executes) 
# and must return a scalar numeric value.  These functions will be minimized.  All of the various
# objective functions are defined here.  Only one is used at a time and that objective is specified
# in optim-setup.R
###################################################################################################

###################################################################################################
# total.carbon
#
# returns the total amount of carbon emitted in the scenario
###################################################################################################
total.carbon <- function(){
  return(sum(carbon.emitted))
}

###################################################################################################
# percent.re.overall
#
# returns the negative of the total percentage of overall demand met by renewables in the modeled year
###################################################################################################
percent.re.overall <- function(){
  #if(exists('logger',mode='function')){ logger(paste("frac.re:",paste(frac.re,collapse='---'))) } 
  if(is.nan(frac.re$overall.frac.re)){
    return(Inf)
  }else{
    return(-frac.re$overall.frac.re*100)
  }
}

###################################################################################################
# percent.re.of.demand
#
# returns the negative of the percentage of electric demand met by renewables in the modeled year
###################################################################################################
percent.re.of.demand <- function(){
  #if(exists('logger',mode='function')){ logger(paste("% RE of Demand Objective should be",-total.percent.renewables.of.demand)) }
  return(-total.percent.renewables.of.demand)
}

###################################################################################################
# percent.re.of.supply
#
# returns the negative of the total percentage of generated electricity that came from renewables 
# in the modeled year
###################################################################################################
percent.re.of.supply <- function(){
  return(-total.percent.supply)
}

###################################################################################################
# levelized.cost
#
# minimize the weighted average, overall levelized costs for delivering energy 
###################################################################################################
levelized.cost <- function(){
  return(lev.cost$total)
}

###################################################################################################
# heat.frac.re
#
# minimize the negative of the fraction of heating from renewables 
###################################################################################################
heat.frac.re <- function(){
  return(-frac.re$heat.frac.re)
}

###################################################################################################
# trans.frac.re
#
# minimize the negative of the fraction of transportation from renewables 
###################################################################################################
trans.frac.re <- function(){
  return(-frac.re$trans.frac.re)
}

###################################################################################################
# marg.re.spillage
#
# minimize marginal re spillage measured as a fraction of the time production from 1 more MW of 
# capacity would be spilled 
###################################################################################################
marg.re.spillage <- function(){
  return(marginal.re.spillage)
}

###################################################################################################
# re.spillage
#
# minimize re.spillage measured as a fraction of total.demand
###################################################################################################
re.spillage <- function(){
  return(total.re.spillage)
}

###################################################################################################
# total.annual.cost
#
# minimize the weighted average, overall annualized costs for delivering energy 
###################################################################################################
total.annual.cost <- function(){
  return(ann.cost$total)
}

###################################################################################################
# total.annual.cost.with.carbon
#
# minimize the weighted average, overall annualized costs for delivering energy including the cost
# of carbon
###################################################################################################
total.annual.cost.with.carbon <- function(){
  return(ann.cost$total + carbon.cost$total)
}

###################################################################################################
# max.impact
#
# minimize the negative of the summed economic impact in terms of output.  we scale jobs to BAU
# and we scale output to BAU and sum the two together for the overall impact 
###################################################################################################
max.impact <- function(){
  jobs <- total.economic.impact[paste(names(econ.impact),'.jobs.annual',sep='')]/103.7
  outp <- total.economic.impact[paste(names(econ.impact),'.output.annual',sep='')]/19891813
  return( -sum(jobs[!is.nan(jobs)]) - sum(outp[!is.nan(outp)]))
}

###################################################################################################
# ackley.wind.wave
#
# for testing model, this returns the ackley function centered at 75,75
###################################################################################################
ackley.wind.wave <- function(){
  return(ackley(c(module.settings$supply.wind$func.params$capacity-75,module.settings$supply.wave$func.params$capacity-75)))
}

###################################################################################################
# ackley
#
# the true ackley function
###################################################################################################
ackley<- function(x){
  return(exp(1) + 20 
      - 20*exp(-0.2*sqrt(1/length(x)*sum(x^2))) 
      - exp(1/length(x)*sum(cos(2*pi*x)))
  )
}

####
# snippet for plotting the ackley function in 3D
####
#xdim <- seq(-30,30,by=0.1)
#ydim <- seq(-30,30,by=0.1)
#ack <- array(NA,c(length(xdim),length(ydim)))
#for(i in 1:length(xdim)){
  #for(j in 1:length(ydim)){
    #ack[i,j] <- ackley(c(xdim[i],ydim[j]))
  #}
#}
#contour(ack)
#filled.contour(ack)
