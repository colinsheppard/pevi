###################################################################################################
# constraints.R 
#
# Template constraint function file for optimization.  This file contains functions that will have 
# access to any data present in the results data frame at the end of a batch of model runs
# and must return zero or infinity.  These are constraints on results of the model (as opposed to 
# constraints on the decision variables themselves which take the form of bounds).  
#
# Each function returns a result that is then summed with the objective definied in objectives.R.
# So the constraints should return either infinity or zero, to signal whether the model result was
# acceptable or not.
###################################################################################################

###################################################################################################
# frac.drivers.delayed.below.thresh
#
# the fraction of drivers delayed cannot exceed the threshold give by max.frac.drivers.delayed
###################################################################################################
frac.drivers.delayed.below.thresh <- function(results){
  return(all.or.nothing(mean(results$frac.drivers.delayed,na.rm=T) < constraint.params$max.frac.drivers.delayed))
}

###################################################################################################
# all.or.nothing
# 
# helper function that returns 0 if TRUE and Inf of FALSE
###################################################################################################
all.or.nothing <- function(test){
  if(test){ return(0) }else{ return(Inf) }
}
