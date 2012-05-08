library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','snow'))

path.to.geatm <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
path.to.outputs <- '~/Dropbox/serc/pev-colin/data/scheduler-optim/'
path.to.ctpp <- '~/Dropbox/serc/pev-colin/data/CTPP/'
path.to.nhts <- '~/Dropbox/serc/pev-colin/data/NHTS/'
path.to.hevi <- '~/Dropbox/serc/pev-colin/hevi/'

#load(paste(path.to.nhts,"TripChaining/chntrp09.Rdata",sep=''))
#load(paste(path.to.nhts,"HHV2PUB.Rdata",sep=''))
#load(paste(path.to.nhts,"TripChaining/tour09.Rdata",sep=''))

load(paste(path.to.geatm,"od-aggregated.Rdata",sep=''))
load(file=paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))
source(paste(path.to.hevi,"R/optim-functions.R",sep=''))

de.params <- list()
de.params[['np']] <- 20           # number of particles
de.params[['f']] <- 0.66          # 0-2, scales the difference vector, higher means faster rate of
                                  # convergence, lower means more robust
de.params[['cr']] <- 0.94         # 0-1, cross-over, indicates probability of using any 'gene' (or value for a 
                                  # decision variable) of the candidate particle rather than the previous particle
de.params[['max.iter']] <- 300    # max iterations

stop.params <- list()
stop.params[['diff.from.best.threshold']]          <- .005    # stopping threshold, when all ptx are 
                                                              # within this fraction of the best return T
decision.vars <- data.frame(lbound=c(0.01,rep(1e-6,6)),ubound=c(10,rep(1,6)))

pev.penetration <- 0.01
location <- 'colin-serc'
num.cpu <- 22

for(pev.penetration in c(0.01,0.02,0.04,0.03,0.05,0.1,0.15,0.2,0.25)){
  print(paste("pen",pev.penetration))

  # get a snow cluster started
  if(!exists('cl')){
    print('starting new cluster')
    cl <- makeSOCKcluster(rep("localhost",num.cpu))
  }

  # initialize the particles (also called "agents" in DE)
  n <- nrow(decision.vars)
  np <- de.params$np
  gen.num <- 1
  all.ptx <- array(NA,c(np,n+1,de.params$max.iter+1))
  colnames(all.ptx) <- c('scale.dist.thresh',0:(n-2),'fitness')
  all.ptx[,c('fitness'),gen.num] <- Inf

  # initialize the particles
  while(all(all.ptx[,c('fitness'),gen.num]==Inf)){
    for(i in 1:10){
      cat('.')
      system('sleep 0.25')
    }
    for(i in 1:np){
      all.ptx[i,1:n,gen.num] <- runif(n,decision.vars$lbound,decision.vars$ubound)
    }
    # evaluate the fitness of the initial particle population
    res <- evaluate.fitness(all.ptx[,1:n,gen.num])
    all.ptx[,c('fitness'),gen.num] <- res
  }

  # enter the loop
  while(!stop.criteria(all.ptx[,'fitness',gen.num],gen.num)){
    print(paste("gen:",gen.num))

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
    
    # by design, every candidate must be different from the original particle in at least one dimension, so we must evaluate the fitness of the whole list
    # of particles
    res <- evaluate.fitness(cand[,1:n])
    cand[,c('fitness')] <- res


    # test which candidates are better than the previous ptx
    improved.ptx <- which(cand[,'fitness'] < all.ptx[,'fitness',gen.num])

    # new generation of particles is a mixture of the previous and the candidate ptx's that have shown improvement
    all.ptx[,,gen.num+1] <- all.ptx[,,gen.num]
    all.ptx[improved.ptx,,gen.num+1] <- cand[improved.ptx,]

    print(all.ptx[,'fitness',gen.num])

    gen.num <- gen.num + 1

    # save the state of the optmization in case of crash
    save.image(paste(path.to.outputs,"0saved-state-pen",pev.penetration*100,".Rdata",sep=''))

    # give myself notice 
    system(paste("touch ",path.to.outputs,location,'-pen',pev.penetration*100,'-gen',gen.num-1,'-fit',roundC(mean(all.ptx[,'fitness',gen.num]),4),'-',format(Sys.time(),"%Y-%m-%d_%H%M%S"),sep=''))
    
    # create a 5 second pause to allow for interruption
    for(i in 1:20){
      cat('.')
      system('sleep 0.25')
    }
    # allow for a break of the loop by creation of a file titled "BREAK" in the dropbox run directory
    if(file.exists(paste(path.to.outputs,"BREAK",sep='')))break
  }
}


