###################################################################################################
# constraints.R 
#
# Template constraint function file for optimization.  This file contains functions that will have 
# access to any data present at the end of a model run (after the post-process script executes) 
# and must return zero or infinity.  These are constraints on results of the model (as opposed to 
# constraints on the decision variables themselves which take the form of bounds).  
#
# Each function returns a result that is then summed with the objective definied in objectives.R.
# So the constraints should return either infinity or zero, to signal whether the model result was
# acceptable or not.
###################################################################################################

###################################################################################################
# electricity.demand.met
#
# the number of hours in the year that electricity demand is not fully met by supply must not 
# exceed meet.electricity.demand.threshold 
###################################################################################################
electricity.demand.met <- function(){
  #if(exists('logger',mode='function')){ logger(paste("Electricty demand met constraint",sum(!balance$demand.elec.load<balance$supply.tot+1e-5) < constraint.params$meet.electricity.demand.threshold)) }
  return(all.or.nothing(sum(!balance$demand.elec.load<balance$supply.tot+1e-5) < constraint.params$meet.electricity.demand.threshold))
}

###################################################################################################
# total.annual.cost.with.carbon.below.thresh
#
# the overall annualized costs for delivering energy must be less than the threshold
# max.total.annual.cost.with.carbon 
###################################################################################################
total.annual.cost.with.carbon.below.thresh <- function(){
  return(all.or.nothing(ann.cost$total+carbon.cost$total <= constraint.params$max.total.annual.cost.with.carbon))
}

###################################################################################################
# emissions.below.thresh
#
# the total carbon emissions are below the threshold max.emissions 
###################################################################################################
emissions.below.thresh <- function(){
  return(all.or.nothing(sum(carbon.emitted) <= constraint.params$max.emissions))
}

###################################################################################################
# lcoe.thresh
#
# firming and non-firming renewable generators have to come in under their respective threshes
###################################################################################################
lcoe.below.thresh <- function(){
  test <- T
  for(gen in c('wind','wave')){
    if(gen %in% names(lev.cost)){
      test <- test & lev.cost[gen] <= constraint.params$re.non.firming.lcoe.threshold
    }
  }
  for(gen in c('hydro','biomass')){
    if(gen %in% names(lev.cost)){
      test <- test & lev.cost[gen] <= constraint.params$re.firming.lcoe.threshold
    }
  }
  return(all.or.nothing(test))
}

###################################################################################################
# levelized.cost.below.thresh
#
# the weighted average, overall levelized costs for delivering energy be less than the threshold
# max.levelized.cost.threshold 
###################################################################################################
levelized.cost.below.thresh <- function(){
  return(all.or.nothing(lev.cost$total <= constraint.params$max.levelized.cost.threshold))
}

###################################################################################################
# x.percent.re.overall
#
# the % RE of demand must be x or greater where 'x' is set by the constraint param:
# percent.re.overall.threshold
###################################################################################################
x.percent.re.overall <- function(){
  #if(exists('logger',mode='function')){ logger(paste("X percent overall constraint",frac.re$overall.frac.re,constraint.params$percent.re.overall.threshold/100)) }
  return(all.or.nothing(frac.re$overall.frac.re >= constraint.params$percent.re.overall.threshold/100))
}

###################################################################################################
# seventy.five.percent.re.of.demand
#
# the % RE of demand must be 75 or greater
###################################################################################################
seventy.five.percent.re.of.demand <- function(){
  return(all.or.nothing(total.percent.renewables.of.demand>=75))
}

###################################################################################################
# seventy.five.percent.re.of.supply
#
# the % RE of supply must be 75 or greater
###################################################################################################
seventy.five.percent.re.of.supply <- function(){
  return(all.or.nothing(total.percent.supply>=75))
}

###################################################################################################
# all.or.nothing
# 
# helper function that returns 0 if TRUE and Inf of FALSE
###################################################################################################
all.or.nothing <- function(test){
  if(test){ return(0) }else{ return(Inf) }
}
