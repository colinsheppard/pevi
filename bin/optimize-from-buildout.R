#!/usr/local/bin/Rscript --no-save --no-restore
##############################################################################################################################################
# Script to conduct a DE optim based on the results of a buildout optimization
##############################################################################################################################################

##############################################################################################################################################
# LOAD LIBRARIES NEED BY THIS SCRIPT
load.libraries(c('optparse','yaml','RNetLogo','plyr','reshape','stringr'),quietly=T)

##############################################################################################################################################
# COMMAND LINE OPTIONS 
option_list <- list(
  make_option(c("-d", "--experimentdir"), type="character", default='.', help="Path to the directory containing the outputs of a buildout optimization [\"%default\"]"),
  make_option(c("-o", "--stopcrit"), type="double", default=0.05, help="Stopping criterion for DE Optim, fractional difference between min and max obj in the particles [%default]"),
  make_option(c("-c", "--constraint"), type="double", default=-1, help="Override cost constraint in dollars, negative number means use value in optim-config.R [%default]"),
  make_option(c("-u", "--numcpu"), type="integer", default=-1, help="Override num.cpu in .Rprofile, a negative integer means do not override [%default]"),
  make_option(c("-j", "--jremem"), type="integer", default=10, help="Memory allocation to JRE threads in GB [%default]"),
  make_option(c("-t", "--hotstart"),action="store_true", type="logical", default=F, help="Set hot.start to TRUE, overriding the value in optim-config.R [%default]"),
  make_option(c("-v", "--version"),type="character", default='2.1.2', help="Version number of PEVI to use [%default]")
)
if(interactive()){
  setwd(pp(pevi.shared,'data/outputs/optim-new/delhi-revised-base-seed10/'))
  args<-c('-c','2.25e6','-t')
  args <- parse_args(OptionParser(option_list = option_list,usage = "optimize-from-buildout.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "optimize-from-buildout.R [options]"),positional_arguments=F)
}

if(substr(args$experimentdir,1,1)!="/")args$experimentdir <- pp(getwd(),"/",args$experimentdir)
if(substr(args$experimentdir,nchar(args$experimentdir),nchar(args$experimentdir)) != "/")args$experimentdir <- pp(args$experimentdir,"/")


Sys.setenv(NOAWT=1)
options(java.parameters=pp("-Xmx",args$jremem,"g"))
load.libraries(c('snow','yaml','stringr','RNetLogo'))

# Paths
path.to.inputs <- paste(pevi.shared,'data/inputs/optim-de/',sep='')
path.to.outputs <- paste(pevi.shared,'data/outputs/optim-de/',sep='')
nl.path <- "/Applications/NetLogo\ 5.0.5"
model.path <- paste(pevi.home,"netlogo/PEVI-v",args$version,"-nolog.nlogo",sep='')

source(pp(args$experimentdir,'params.R'))
source(paste(pevi.home,"R/reporters-loggers.R",sep=''))
source(paste(pevi.home,"R/optim/optim-functions.R",sep='')) 

# The following config files are saught first in the experiment outputs directory, then the default location
if(file.exists(pp(args$experimentdir,'optim-config.R'))){
  source(pp(args$experimentdir,'optim-config.R'))
}else{
  my.cat('WARNING: cannot find optim-config.R in buildout outputs directory, using default pevi/R/optim/optim-config.R instead')
  source(paste(pevi.home,"R/optim/optim-config.R",sep=''))
}
if(file.exists(pp(args$experimentdir,'objectives.R'))){
  source(pp(args$experimentdir,'objectives.R'))
}else{
  my.cat('WARNING: cannot find objectives.R in buildout outputs directory, using default pevi/R/optim/objectives.R instead')
  source(paste(pevi.home,"R/optim/objectives.R",sep=''))
}
if(file.exists(pp(args$experimentdir,'constraints.R'))){
  source(pp(args$experimentdir,'constraints.R'))
}else{
  my.cat('WARNING: cannot find constraints.R in buildout outputs directory, using default pevi/R/optim/constraints.R instead')
  source(paste(pevi.home,"R/optim/constraints.R",sep=''))
}

# pull the infrastructure sited by the buildout algorithm
buildout.progress <- data.table(read.csv(pp(args$experimentdir,'buildout-progress.csv')))
seed <- tail(buildout.progress$seed,1)
pev.penetration <- tail(buildout.progress$penetration,1)
my.cat(pp("pen ",pev.penetration,' seed ',seed))
exp.name <- pp(optim.scenario,'-seed',seed)

# let commandline args override settings in params.R, optim-config.R, constraints.R
if(args$hotstart){
  hot.start <- T
}
if(args$numcpu>0){
  num.cpu <- args$numcpu
}
if(args$constraint>0){
  constraint.params[['max.cost']] <- args$constraint
}
optim.code <- exp.name

make.dir(pp(path.to.inputs,optim.code))
file.copy(pp(args$experimentdir,'params.txt'),pp(path.to.inputs,optim.code))
make.dir(paste(path.to.outputs,optim.code,sep=''))

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(pp(args$experimentdir,'vary.yaml'),file.info(pp(args$experimentdir,'vary.yaml'))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- paste(pevi.shared,"data/inputs/",vary[[file.param]],sep='')
}
# setup the data frame containing all combinations of those parameter values
vary.tab.original <- expand.grid(vary,stringsAsFactors=F)
# set the penetration of the driver input files
vary.tab <- vary.tab.original
vary.tab$`driver-input-file` <- str_replace(vary.tab$`driver-input-file`,"penXXX",paste("pen",pev.penetration*100,sep=""))
results <- data.frame(vary.tab,reporters)
results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))

# get a snow cluster started
if(!exists('cl')){
  my.cat(pp('starting ',num.cpu,' new clusters'))
  cl <- makeCluster(c(rep(list(list(host="localhost")),num.cpu)),type="SOCK")
  streval(pp('clusterEvalQ(cl,options(java.parameters="-Xmx',args$jremem,'g"))'))
  clusterEvalQ(cl,Sys.setenv(NOAWT=1))
  clusterEvalQ(cl,library('RNetLogo'))
  clusterExport(cl,c('init.netlogo','model.path','logfiles'))
  clusterEvalQ(cl,init.netlogo())
}

# initialize the particles (also called "agents" in DE)
n <- nrow(decision.vars)
np <- de.params$np
gen.num <- 1
all.ptx <- array(NA,c(np,n+1,de.params$max.iter+1))
colnames(all.ptx) <- c(decision.vars$name,'fitness')
all.ptx[,c('fitness'),gen.num] <- Inf

# initialize the list to store fitness values so we don't recompute if not necessary
fit.history <- list()

# we need tighter initial distribution on decision variables to get into the feasible region, but then loosen back up after
decision.vars$ubound <- decision.vars$ubound.start

build.increment.m <- melt(build.increment)
names(build.increment.m) <- c('level','num')
build.increment.m <- data.table(build.increment.m)
the.levels <- as.numeric(substr(build.increment.m$level,2,2))
build.increment.m[,':='(level=NULL)]
build.increment.m[,':='(level=the.levels)]
buildout.progress <- buildout.progress[cum.cost < args$constraint]
buildout.progress <- join.on(buildout.progress, build.increment.m,'winner.level','level')
buildout.progress.sum <- buildout.progress[,list(num=sum(num)),by=c('winner.taz','winner.level')]
decision.vars$order <- 1:nrow(decision.vars)
starting.ptx <- join.on(data.table(decision.vars),buildout.progress.sum,c('taz','level'),c('winner.taz','winner.level'))
starting.ptx[is.na(num),num:=0]
setkey(starting.ptx,order)

# initialize the particles
initialize.try <- 0
while(!hot.start & all(all.ptx[,c('fitness'),gen.num]==Inf)){
  my.cat('Finding initial distribution of particles')
  for(i in 1:10){
    cat('.')
    system('sleep 0.25')
  }
  # allow for a break of the loop by creation of a file titled "BREAK" in the dropbox run directory
  if(file.exists(paste(path.to.outputs,optim.code,"/BREAK",sep=''))){
    file.remove(paste(path.to.outputs,optim.code,"/BREAK",sep=''))
    break
  }

  for(i in 1:n){
    all.ptx[,i,gen.num] <- round(rnorm(np,starting.ptx$num[i] - initialize.try/2,decision.vars$ubound.start[i]/3))
    all.ptx[,i,gen.num][all.ptx[,i,gen.num]<0] <- 0
    all.ptx[,i,gen.num][all.ptx[,i,gen.num]>decision.vars$ubound.actual[i]] <- decision.vars$ubound.actual[i]
  }
  # evaluate the fitness of the initial particle population
  res <- evaluate.fitness(all.ptx[,1:n,gen.num])
  all.ptx[,c('fitness'),gen.num] <- res

  # save the fitness for future use
  for(i in 1:np){
    fit.history[[paste(all.ptx[i,1:n,gen.num],collapse=",")]] <- all.ptx[i,'fitness',gen.num]
  }
  initialize.try <- initialize.try + 1
}
# loosen the bounds back up
decision.vars$ubound <- decision.vars$ubound.actual

if(hot.start){
  cl.prev <- cl
  args.prev <- args
  load(paste(path.to.outputs,optim.code,"/0saved-state-pen",pev.penetration*100,".Rdata",sep=''))
  path.to.inputs <- paste(pevi.shared,'data/inputs/optim-de/',sep='')
  path.to.outputs <- paste(pevi.shared,'data/outputs/optim-de/',sep='')
  rm('cl')
  cl <- cl.prev
  rm('cl.prev')
  rm('args')
  arg <- args.prev
  rm('args.prev')
  old.max.iter <- de.params$max.iter
  if(file.exists(pp(args$experimentdir,'optim-config.R'))){
    source(pp(args$experimentdir,'optim-config.R'))
  }else{
    source(paste(pevi.home,"R/optim/optim-config.R",sep=''))
  }
  gen.num <- gen.num - 1
  all.ptx.prev <- all.ptx 
  all.ptx <- array(NA,c(np,n+1,de.params$max.iter+1))
  colnames(all.ptx) <- c(decision.vars$name,'fitness')
  all.ptx[,,1:(old.max.iter+1)] <- all.ptx.prev
  # hack to deal with broken history accounting
  #for(gen.num in 1:899){
    #for(i in 1:np){
      #fit.history[[paste(all.ptx[i,1:n,gen.num],collapse=",")]] <- all.ptx[i,'fitness',gen.num]
    #}
  #}
}else{
  save.image(paste(path.to.outputs,optim.code,"/0saved-state-pen",pev.penetration*100,".Rdata",sep=''))
}

# enter the loop
while(!stop.criteria(all.ptx[,'fitness',gen.num],gen.num)){

  print(paste("gen:",gen.num))
  source(paste(pevi.home,"R/optim/optim-functions.R",sep='')) # allows hot-swapping code

  # initialize the candidate particles from the list of working particles
  cand <- all.ptx[,,gen.num]
  cand[,'fitness'] <- NA # just to avoid confusion between the update of the particles and before eval of fitness

  # which dimensions are stuck on a single value
  stuck.dimensions <- c(apply(all.ptx[,1:n,gen.num],2,sum)>0 & apply(all.ptx[,1:n,gen.num],2,sd)<0.2,F)
  stuck.dimensions.rare <- c(apply(all.ptx[,1:n,gen.num],2,sum)==0,F)

  for(i in 1:np){
    # for each ptx, select 3 other mutually exclusive particles
    mutation.ptx <- sample((1:np)[-i],3)

    # decide which dimensions (decision variables) to alter in the creation of a candidate ptx, the logic of this is:
    #   - chose 1 dimension randomly to change
    #   - for the rest of the dimensions, test if a random draw between 0,1 is less than CR ('cross-over' parameter), 
    #     if it is, change this dimension 
    cr.dimensions <- c(1:n==sample(1:n,1) | runif(n)<de.params$cr,F)

    # for the chosen dimensions, create a candidate ptx using vector addition A + F(B-C) where F is the DE parameter and A,B,C
    # are the mutation particles selection above
    cand[i,cr.dimensions] <- all.ptx[mutation.ptx[1],cr.dimensions,gen.num] + 
                              de.params$f * (all.ptx[mutation.ptx[2],cr.dimensions,gen.num] - all.ptx[mutation.ptx[3],cr.dimensions,gen.num])

    # add some white noise to dimensions with 0 variation amongst the particles 
    cand[i,cr.dimensions & stuck.dimensions] <- cand[i,cr.dimensions & stuck.dimensions] + rnorm(sum(cr.dimensions & stuck.dimensions),0,0.5)
    cand[i,cr.dimensions & stuck.dimensions.rare] <- cand[i,cr.dimensions & stuck.dimensions.rare] + rnorm(sum(cr.dimensions & stuck.dimensions.rare),0,0.16667)
  }
  # keep the candidate particles inside the search space
  for(j in 1:n){
    cand[cand[,j]<decision.vars$lbound[j],j] <- decision.vars$lbound[j]
    cand[cand[,j]>decision.vars$ubound[j],j] <- decision.vars$ubound[j]
  }
  # finally, round the variables because this is a discrete problem
  cand <- round(cand)
  to.eval <- c()
  for(i in 1:np){
    if(is.null(fit.history[[paste(cand[i,1:n],collapse=",")]])){
      to.eval <- c(to.eval,i)
    }else{
      cand[i,'fitness'] <- fit.history[[paste(cand[i,1:n],collapse=",")]]
    }
  }
  
  # evaluate the fitness of the particles that are not stored in fit.history
  res <- evaluate.fitness(cand[to.eval,1:n])
  cand[to.eval,c('fitness')] <- res

  # save the fitness for future use
  for(i in 1:np){
    fit.history[[paste(cand[i,1:n],collapse=",")]] <- cand[i,'fitness']
  }

  # test which candidates are better than the previous ptx
  improved.ptx <- which(cand[,'fitness'] < all.ptx[,'fitness',gen.num])

  # new generation of particles is a mixture of the previous and the candidate ptx's that have shown improvement
  all.ptx[,,gen.num+1] <- all.ptx[,,gen.num]
  all.ptx[improved.ptx,,gen.num+1] <- cand[improved.ptx,]

  print(all.ptx[,'fitness',gen.num])
  notice.file <- paste(path.to.outputs,optim.code,"/",whoami,'-pen',pev.penetration*100,'-gen',gen.num,'-fit',roundC(mean(all.ptx[,'fitness',gen.num],na.rm=T),4),'-',format(Sys.time(),"%Y-%m-%d_%H%M%S"),'.csv',sep='')
  write.csv(all.ptx[,,gen.num],file=notice.file)

  gen.num <- gen.num + 1

  # save the state of the optmization in case of crash
  save.image(paste(path.to.outputs,optim.code,"/0saved-state-pen",pev.penetration*100,".Rdata",sep=''))

  # create a 5 second pause to allow for interruption
  for(i in 1:20){
    cat('.')
    system('sleep 0.25')
  }
  # allow for a break of the loop by creation of a file titled "BREAK" in the dropbox run directory
  if(file.exists(paste(path.to.outputs,optim.code,"/BREAK",sep=''))){
    file.remove(paste(path.to.outputs,optim.code,"/BREAK",sep=''))
    break
  }
}

# add noise to a previous set of ptx
#all.ptx[,1:n,gen.num] <- all.ptx[,1:n,gen.num] + round(runif(np*n,-3,3))
#for(j in 1:n){
  #all.ptx[all.ptx[,j,gen.num]<decision.vars$lbound[j],j,gen.num] <- decision.vars$lbound[j]
  #all.ptx[all.ptx[,j,gen.num]>decision.vars$ubound[j],j,gen.num] <- decision.vars$ubound[j]
#}
