###################################################################################################
# optim-config.R 
#
# Optimization used for exploring everything all together.
###################################################################################################

hot.start <- T
num.cpu <- 6

#optim.code <- 'min-cost-constrained-by-frac-delayed'
#optim.code <- 'min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'min-frac-stranded-constrained-by-cost-50-50'
optim.code <- 'min-delay-cost-constrained-by-budget'

###################################################################################################
# decision.vars       data.frame containing the names and bounds of the variables to alter in the 
#                     optimization, the variables are assumed to exist inside module.settings, the
#                     module name and parameter name are specified (e.g. 'supply.wind','capacity')
###################################################################################################
#decision.vars <- data.frame(taz=rep(1:53,3),level=rep(1:3,each=53),lbound=rep(0,53*3),ubound.start=c(rep(3,53),rep(2,53),rep(1,53)),ubound.actual=c(rep(40,53),rep(20,53),rep(5,53)))
taz.with.ext <- c(1:53,-c(1,2,15,23,26,28,31,39,48,49,51,53))
decision.vars <- data.frame(taz=taz.with.ext,level=rep(1:3,each=65),lbound=rep(0,65*3),ubound.start=c(rep(3,65),rep(2,65),rep(1,65)),ubound.actual=c(rep(60,65),rep(10,65),rep(4,65)))
decision.vars$name <-  c(paste("T",taz.with.ext,"-L1",sep=""),paste("T",taz.with.ext,"-L2",sep=""),paste("T",taz.with.ext,"-L3",sep=""))

###################################################################################################
# objective.name
# the name of the function that will serve as the objective of the optimization.  the objective
# will be executed at the end of a batch of model runs and it must return a scalar value to be minimized.
###################################################################################################
#objective.name <- 'frac.drivers.delayed'
#objective.name <- 'infrastructure.cost'
#objective.name <- 'frac.stranded.by.delay'
objective.name <- 'delay.cost'

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
  #'frac.stranded.below.thresh'
  'total.cost.below.thresh'
)
constraint.params <- list()
constraint.params[['max.frac.drivers.delayed']] <- 0.03
constraint.params[['max.frac.stranded']] <- 0.01
constraint.params[['max.num.stranded']] <- 1
constraint.params[['max.cost']] <- 1e6
constraint.params[['include.external.chargers']] <- T

###################################################################################################
# de.params           
# contains the parameters of the differential evolution optimization scheme
###################################################################################################
de.params <- list()
de.params[['np']] <- 48          # number of particles
de.params[['f']] <- 1.0          # 0-2, scales the difference vector, higher means faster rate of
                                 # convergence, lower means more robust
de.params[['cr']] <- 0.96        # 0-1, cross-over, indicates probability of using any 'gene' (or value for a 
                                 # decision variable) of the candidate particle rather than the previous particle
de.params[['max.iter']] <- 6000  # max iterations

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
