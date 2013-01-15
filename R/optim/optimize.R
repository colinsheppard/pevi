library(colinmisc)
Sys.setenv(NOAWT=1)
load.libraries(c('snow','yaml','stringr','RNetLogo'))

base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/optim/',sep='')
path.to.outputs <- paste(base.path,'pev-shared/data/outputs/optim/',sep='')
nl.path <- "/Applications/NetLogo\ 5.0.3"
model.path <- paste(path.to.pevi,"netlogo/PEVI.nlogo",sep='')

source(paste(path.to.pevi,"R/optim/optim-functions.R",sep='')) # note this will in turn call optim-config, objectives, and constraints
source(paste(path.to.pevi,"R/reporters-loggers.R",sep=''))

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(paste(path.to.inputs,'vary.yaml',sep=''),file.info(paste(path.to.inputs,'vary.yaml',sep=''))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- paste(path.to.pevi,'netlogo/',vary[[file.param]],sep='')
}
# setup the data frame containing all combinations of those parameter values
vary.tab.original <- expand.grid(vary,stringsAsFactors=F)

pev.penetration <- 0.04
location <- 'colin-serc'
#location <- 'colin-home'
num.cpu <- 11
#num.cpu <- 7

for(pev.penetration in c(0.005,0.01,0.02,0.04)){
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
    #cl <- makeCluster(c(rep(list(list(host="localhost",outfile=paste(path.to.outputs,optim.code,"/cluster-out.txt",sep=''))),num.cpu)),type="SOCK")
    cl <- makeCluster(c(rep(list(list(host="localhost")),num.cpu)),type="SOCK")
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

  # initialize the particles
  while(all(all.ptx[,c('fitness'),gen.num]==Inf)){
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
      all.ptx[,i,gen.num] <- sample(seq(decision.vars$lbound[i],decision.vars$ubound[i]),np,replace=T)
    }
    # evaluate the fitness of the initial particle population
    res <- evaluate.fitness(all.ptx[,1:n,gen.num])
    all.ptx[,c('fitness'),gen.num] <- res

    # save the fitness for future use
    for(i in 1:np){
      fit.history[[paste(all.ptx[,1:n,gen.num],collapse=",")]] <- all.ptx[,'fitness',gen.num]
    }
  }
  save.image(paste(path.to.outputs,optim.code,"/0saved-state-pen",pev.penetration*100,".Rdata",sep=''))

  # enter the loop
  while(!stop.criteria(all.ptx[,'fitness',gen.num],gen.num)){

    print(paste("gen:",gen.num))
    source(paste(path.to.pevi,"R/optim/optim-functions.R",sep='')) # allows hot-swapping code

    # initialize the candidate particles from the list of working particles
    cand <- all.ptx[,,gen.num]
    cand[,'fitness'] <- NA # just to avoid confusion between the update of the particles and before eval of fitness

    for(i in 1:np){
      # for each ptx, select 3 other mutually exclusive particles
      mutation.ptx <- sample((1:np)[-i],3)

      # decide which dimensions (decision variables) to alter in the creation of a candidate ptx, the logic of this is:
      #   - chose 1 dimension randomly to change
      #   - for the rest of the dimensions, test if a random draw between 0,1 is less than CR ('cross-over' parameter), 
      #     if it is, change this dimension 
      cr.dimensions <- 1:n==sample(1:n,1) | runif(n)<de.params$cr

      # for the chosen dimensions, create a candidate ptx using vector addition A + F(B-C) where F is the DE parameter and A,B,C
      # are the mutation particles selection above
      cand[i,cr.dimensions] <- all.ptx[mutation.ptx[1],cr.dimensions,gen.num] + 
                                de.params$f * (all.ptx[mutation.ptx[2],cr.dimensions,gen.num] - all.ptx[mutation.ptx[3],cr.dimensions,gen.num])
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
      if(is.null(fit.history[[paste(cand[i,],collapse=",")]])){
        to.eval <- c(to.eval,i)
      }else{
        cand[i,'fitness'] <- fit.history[[paste(cand[i,],collapse=",")]]
      }
    }
    
    # evaluate the fitness of the particles that are not stored in fit.history
    res <- evaluate.fitness(cand[to.eval,1:n])
    cand[to.eval,c('fitness')] <- res

    # save the fitness for future use
    for(i in 1:np){
      fit.history[[paste(cand[i,],collapse=",")]] <- cand[i,'fitness']
    }

    # test which candidates are better than the previous ptx
    improved.ptx <- which(cand[,'fitness'] < all.ptx[,'fitness',gen.num])

    # new generation of particles is a mixture of the previous and the candidate ptx's that have shown improvement
    all.ptx[,,gen.num+1] <- all.ptx[,,gen.num]
    all.ptx[improved.ptx,,gen.num+1] <- cand[improved.ptx,]

    print(all.ptx[,'fitness',gen.num])
    notice.file <- paste(path.to.outputs,optim.code,"/",location,'-pen',pev.penetration*100,'-gen',gen.num,'-fit',roundC(mean(all.ptx[,'fitness',gen.num],na.rm=T),4),'-',format(Sys.time(),"%Y-%m-%d_%H%M%S"),'.csv',sep='')
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

