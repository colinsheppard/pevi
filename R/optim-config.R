###################################################################################################
# all.R
#
# Optimization used for exploring everything all together.
###################################################################################################

###################################################################################################
# decision.vars       data.frame containing the names and bounds of the variables to alter in the 
#                     optimization, the variables are assumed to exist inside module.settings, the
#                     module name and parameter name are specified (e.g. 'supply.wind','capacity')
###################################################################################################
decision.vars <- data.frame(taz=c(1:52,1:52),level=c(rep(2,52),rep(3,52)),lbound=rep(0,104),ubound=c(rep(15,52),rep(10,52)))
decision.vars$name <-  c(paste("T",1:52,"-L2",sep=""),paste("T",1:52,"-L3",sep=""))
decision.vars <- data.frame(matrix(c(

###################################################################################################
# objective.name
# the name of the function that will serve as the objective of the optimization.  the objective
# will be executed at the end of  each model run (so it has all products and inputs of a model 
# run available to it) and it must return a scalar value to be minimized.
###################################################################################################
#objective.name <- 'percent.re.overall'
objective.name <- 'total.carbon'
#objective.name <- 'percent.re.of.demand'
#objective.name <- 'levelized.cost'
#objective.name <- 'total.annual.cost'
#objective.name <- 'total.annual.cost.with.carbon'

###################################################################################################
# constraint.names
# a list of the functions to execute as constraints to the model.  these functions are evaluated
# at the same time as and added to the objective function.  they take on a value of Inf or 0.
#
# available constraint names:
#   'electricity.demand.met'
#   'total.annual.cost.with.carbon'
#   'x.percent.re.overall'
#   'seventy.five.percent.re.of.demand'
#   'levelized.cost.below.thresh'
#   'lcoe.below.thresh'
#
# constraint.params           
# contains any parameters needed by constraints specified in constraint.names
###################################################################################################
constraint.names <- c(
  'electricity.demand.met',
  'total.annual.cost.with.carbon.below.thresh'
  #'lcoe.below.thresh'         # this lcoe tests individual generators againts threshes
  #'x.percent.re.overall'
  #'seventy.five.percent.re.of.demand'
  #'levelized.cost.below.thresh'   # this lcoe tests the overall weighted average cost
)
constraint.params <- list()
constraint.params[['meet.electricity.demand.threshold']] <- 8   # relevant to the constraint that 
                                                                # elec demand can only NOT be met for X hours a year 
constraint.params[['max.levelized.cost.threshold']] <- 70       # $/MWh relevant to the constraint that 
                                                                # levelized cost stay below a certain value
constraint.params[['percent.re.overall.threshold']] <- 75       # % of overall primary energy from RE (elec,heat,trans)
constraint.params[['max.total.annual.cost.with.carbon']] <- Inf # 
constraint.params[['re.firming.lcoe.threshold']] <- 123          # 
constraint.params[['re.non.firming.lcoe.threshold']] <- 113      # 

###################################################################################################
# de.params           
# contains the parameters of the differential evolution optimization scheme
###################################################################################################
de.params <- list()
de.params[['np']] <- 44           # number of particles
de.params[['f']] <- 0.58          # 0-2, scales the difference vector, higher means faster rate of
                                  # convergence, lower means more robust
de.params[['cr']] <- 0.96         # 0-1, cross-over, indicates probability of using any 'gene' (or value for a 
                                  # decision variable) of the candidate particle rather than the previous particle
de.params[['max.iter']] <- 5000   # max iterations

                                                              # within this fraction of the best return T
de.params <- list()
de.params[['np']] <- 23           # number of particles
de.params[['f']] <- 0.66          # 0-2, scales the difference vector, higher means faster rate of
                                  # convergence, lower means more robust
de.params[['cr']] <- 0.94         # 0-1, cross-over, indicates probability of using any 'gene' (or value for a 
                                  # decision variable) of the candidate particle rather than the previous particle
de.params[['max.iter']] <- 175    # max iterations

###################################################################################################
# stop.criteria       will act on an array representing the fitness of all the particles, it returns 
#                     true if the algorithm should stop
#
# stop.params           
# contains any parameters needed by the stop.criteria function
###################################################################################################
stop.params <- list()
stop.params[['diff.from.best.threshold']]     <- .005   # stopping threshold, when all ptx are 
                                                        # within this fraction of the best return T
stop.criteria <- function(fit,iter=gen.num){
  if(all(fit==Inf))return(F)
  # true if all deviations of fitnesses from the min are less than threshold OR if we've hit max iterations
  return(all((fit-min(fit))/abs(min(fit))<stop.params$diff.from.best.threshold) | iter>=de.params$max.iter)
}
stop.params <- list()
stop.params[['diff.from.best.threshold']]          <- .0005    # stopping threshold, when all ptx are 
                                                              # within this fraction of the best return T
###################################################################################################
# fitness.params      will act on an array representing the fitness of all the particles, it returns 
#                     true if the algorithm should stop
#
# stop.params           
# contains any parameters needed by the stop.criteria function
###################################################################################################
fitness.params <- list()
fitness.params[['summary.stat']]          <- 'frac.drivers.delayed'
fitness.params[['aggregation.function']]  <- 'mean'
