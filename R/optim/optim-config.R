###################################################################################################
# optim-config.R 
#
# Optimization used for exploring everything all together.
###################################################################################################

#optim.code <- 'min-cost-constrained-by-frac-delayed'
optim.code <- 'min-cost-constrained-by-frac-stranded'

###################################################################################################
# decision.vars       data.frame containing the names and bounds of the variables to alter in the 
#                     optimization, the variables are assumed to exist inside module.settings, the
#                     module name and parameter name are specified (e.g. 'supply.wind','capacity')
###################################################################################################
decision.vars <- data.frame(taz=c(1:52,1:52),level=c(rep(2,52),rep(3,52)),lbound=rep(0,104),ubound=c(rep(15,52),rep(10,52)))
decision.vars$name <-  c(paste("T",1:52,"-L2",sep=""),paste("T",1:52,"-L3",sep=""))

###################################################################################################
# objective.name
# the name of the function that will serve as the objective of the optimization.  the objective
# will be executed at the end of a batch of model runs and it must return a scalar value to be minimized.
###################################################################################################
#objective.name <- 'frac.drivers.delayed'
objective.name <- 'infrastructure.cost'

###################################################################################################
# constraint.names
# a list of the functions to execute as constraints to the model.  these functions are evaluated
# at the same time as and added to the objective function.  they take on a value of Inf or 0.
#
# available constraint names:
#   'frac.drivers.delayed.below.thresh'
#
# constraint.params           
# contains any parameters needed by constraints specified in constraint.names
###################################################################################################
constraint.names <- c(
  #'frac.drivers.delayed.below.thresh'
  #'num.stranded.below.thresh'
  'frac.stranded.below.thresh'
)
constraint.params <- list()
constraint.params[['max.frac.drivers.delayed']] <- 0.03
constraint.params[['max.frac.stranded']] <- 0.01
constraint.params[['max.num.stranded']] <- 1

###################################################################################################
# de.params           
# contains the parameters of the differential evolution optimization scheme
###################################################################################################
de.params <- list()
de.params[['np']] <- 48           # number of particles
de.params[['f']] <- 1.0          # 0-2, scales the difference vector, higher means faster rate of
                                  # convergence, lower means more robust
de.params[['cr']] <- 0.96         # 0-1, cross-over, indicates probability of using any 'gene' (or value for a 
                                  # decision variable) of the candidate particle rather than the previous particle
de.params[['max.iter']] <- 900   # max iterations

###################################################################################################
# stop.criteria       will act on an array representing the fitness of all the particles, it returns 
#                     true if the algorithm should stop
#
# stop.params           
# contains any parameters needed by the stop.criteria function
###################################################################################################
stop.params <- list()
stop.params[['diff.from.best.threshold']]     <- .02   # stopping threshold, when all ptx are 
                                                       # within this fraction of the best return T
