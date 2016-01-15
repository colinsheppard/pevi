# Note on El Capitan, I had to use the following commands to get libs configured so that rJava and RNetLogo use Java 1.8
#
# sudo ln -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib
#
# sudo R CMD javareconf -n
#
# Then I installed rJava 0.9-8 from source available here: https://www.rforge.net/rJava/files/

Sys.setenv(NOAWT=1)
load.libraries(c('snow','yaml','stringr','RNetLogo'))

base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'

path.to.inputs <- paste(pevi.shared,'data/inputs/optim-de/',sep='')
path.to.outputs <- paste(pevi.shared,'data/outputs/optim-de/',sep='')
nl.path <- "/Applications/NetLogo\ 5.0.5"
model.path <- paste(pevi.home,"netlogo/PEVI-v2.1.2-nolog.nlogo",sep='')

source(paste(pevi.home,"R/optim/optim-functions.R",sep='')) # note this will in turn call optim-config, objectives, and constraints
source(paste(pevi.home,"R/optim/optim-config.R",sep=''))
source(paste(pevi.home,"R/optim/objectives.R",sep=''))
source(paste(pevi.home,"R/optim/constraints.R",sep=''))
source(paste(pevi.home,"R/reporters-loggers.R",sep=''))
make.dir(paste(path.to.inputs,optim.code,sep=''))
make.dir(paste(path.to.outputs,optim.code,sep=''))

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(paste(path.to.inputs,'vary.yaml',sep=''),file.info(paste(path.to.inputs,'vary.yaml',sep=''))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- paste(pevi.shared,"data/inputs/",vary[[file.param]],sep='')
}
# setup the data frame containing all combinations of those parameter values
vary.tab.original <- expand.grid(vary,stringsAsFactors=F)

pev.penetration <- 0.005


for(pev.penetration in c(0.005)){ #c(0.005,0.01,0.02,0.04)){
  print(paste("pen",pev.penetration))

  # set the penetration of the driver input files
  vary.tab <- vary.tab.original
  vary.tab$`driver-input-file` <- str_replace(vary.tab$`driver-input-file`,"penXXX",paste("pen",pev.penetration*100,sep=""))
  results <- data.frame(vary.tab,reporters)
  results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
  results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))

  # get a snow cluster started
  if(!exists('cl')){
    print('starting new cluster')
    cl <- makeCluster(c(rep(list(list(host="localhost")),num.cpu)),type="SOCK")
    clusterEvalQ(cl,options(java.parameters="-Xmx10g"))
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

  # initialize the particles
  while(!hot.start & all(all.ptx[,c('fitness'),gen.num]==Inf)){
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
      all.ptx[,i,gen.num] <- round(rnorm(np,0,decision.vars$ubound.start[i]/3))
      all.ptx[,i,gen.num][all.ptx[,i,gen.num]<0] <- 0
      #all.ptx[,i,gen.num] <- sample(seq(decision.vars$lbound[i],decision.vars$ubound[i]),np,replace=T)
    }
    # evaluate the fitness of the initial particle population
    res <- evaluate.fitness(all.ptx[,1:n,gen.num])
    all.ptx[,c('fitness'),gen.num] <- res

    # save the fitness for future use
    for(i in 1:np){
      fit.history[[paste(all.ptx[i,1:n,gen.num],collapse=",")]] <- all.ptx[i,'fitness',gen.num]
    }
  }
  # loosen the bounds back up
  decision.vars$ubound <- decision.vars$ubound.actual

  if(hot.start){
    cl.prev <- cl
    load(paste(path.to.outputs,optim.code,"/0saved-state-pen",pev.penetration*100,".Rdata",sep=''))
    rm('cl')
    cl <- cl.prev
    rm('cl.prev')
    old.max.iter <- de.params$max.iter
    source(paste(pevi.home,"R/optim/optim-config.R",sep=''))
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
}

# add noise to a previous set of ptx
#all.ptx[,1:n,gen.num] <- all.ptx[,1:n,gen.num] + round(runif(np*n,-3,3))
#for(j in 1:n){
  #all.ptx[all.ptx[,j,gen.num]<decision.vars$lbound[j],j,gen.num] <- decision.vars$lbound[j]
  #all.ptx[all.ptx[,j,gen.num]>decision.vars$ubound[j],j,gen.num] <- decision.vars$ubound[j]
#}
