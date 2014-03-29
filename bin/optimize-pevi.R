#!/usr/bin/Rscript --no-save --no-restore

##############################################################################################################################################
# Script to conduct the buildout optimization
##############################################################################################################################################

##############################################################################################################################################
# LOAD LIBRARIES NEED BY THIS SCRIPT
load.libraries(c('optparse','yaml','RNetLogo','plyr','reshape','stringr'),quietly=T)

##############################################################################################################################################
# COMMAND LINE OPTIONS 
option_list <- list(
  make_option(c("-d", "--experimentdir"), type="character", default='.', help="Path to the directory containing the files needed to run the optimization (params.txt, vary.yaml, paths.yaml) [\"%default\"]"),
  make_option(c("-s", "--seed"), type="integer", default=-1, help="Override seeds in params.R with a single value, a negative integer means do not override [%default]"),
  make_option(c("-t", "--hotstart"),action="store_true", type="logical", default=F, help="Set hot.start to TRUE, overriding the value in params.R [%default]"),
  make_option(c("-c", "--correcttwo"),action="store_true", type="logical", default=F, help="Correct 2%, this will delete all 2% results and hot start from iter 1 [%default]")
)
if(interactive()){
  setwd(pp(pevi.shared,'data/inputs/optim-new/delhi-half-homeless/'))
  setwd(pp(pevi.shared,'data/inputs/optim-new/delhi-swap/'))
  args<-c('-s','21','-c')
  args <- parse_args(OptionParser(option_list = option_list,usage = "optimize-pevi.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "optimize-pevi.R [options]"),positional_arguments=F)
}

if(substr(args$experimentdir,1,1)!="/")args$experimentdir <- pp(getwd(),"/",args$experimentdir)
if(substr(args$experimentdir,nchar(args$experimentdir),nchar(args$experimentdir)) != "/")args$experimentdir <- pp(args$experimentdir,"/")

Sys.setenv(NOAWT=1)
#options(java.parameters="-Xmx1024m")
options(java.parameters="-Xmx2048m")

source(pp(args$experimentdir,'params.R'))

# let commandline args override settings in params.R
if(args$seed>=0){
  seeds <- args$seed
}
if(args$hotstart){
  hot.start <- T
}
correct.two <- args$correcttwo 

source(paste(pevi.home,"R/optim/buildout-functions.R",sep=''))
source(paste(pevi.home,"R/reporters-loggers.R",sep=''))

# in case hot start or nl.obj is not specified in params.R
hot.start <- ifelse(exists('hot.start'),hot.start,F)
nl.obj <- ifelse(exists('nl.obj'),nl.obj,'marginal-cost-to-reduce-delay')

# The params file will need to be set in R, so we can edit it mid-run. 
# Otherwise, batch setup is impossible.

# NL does not recognize tilde expansion. KEEP THIS IN MIND.
param.file <- pp(args$experimentdir,'params.txt')
param.file.data <- read.table(param.file,sep='\t')
param.file.data <- streval(pp('data.frame(',pp(apply(param.file.data,1,function(x){ pp(str_replace_all(x[1],'-','.'),'=',ifelse(length(grep('file|directory',x[1]))>0,pp('"',x[2],'"'),ifelse(x[2]%in%c('true','false'),x[2]=='true',x[2]))) }),collapse=","),')'))

taz.names <- NA
if(file.exists(pp(args$experimentdir,'taz-names.csv')))taz.names <- read.csv(pp(args$experimentdir,'taz-names.csv'))

# For the current optimization, we need to know the installation costs of the chargers. Assuming that we won't want to vary
# those costs within an optimization run, we read them in here. If we do want to vary those costs, this would need to be 
# moved to the driver-file for loop.

charger.info <- read.table(pp(pevi.shared,param.file.data$charger.type.input.file),row.names=NULL,header=T,sep='\t')
names(charger.info) <- c('level','charge.rate','energy.price','installed.cost')

# initialize infrastructure table, skip data, and the taz/charger combinations
init.charger.file <- pp(pevi.shared,param.file.data$charger.input.file)
init.charger.buildout <- read.table(init.charger.file,header=T,sep='\t')
levels.to.vary <- as.numeric(substr(names(build.increment)[build.increment>0],2,2))
taz.charger.combos <- expand.grid(subset(init.charger.buildout,X.TAZ>0)$X.TAZ,subset(charger.info,level %in% levels.to.vary)$level)
names(taz.charger.combos) <- c('taz','level')
taz.charger.combos$include <- T
taz.charger.combos$key <- pp(taz.charger.combos$taz,'-',taz.charger.combos$level)
taz.charger.combos$name <- NA
if(all(!is.na(taz.names)))taz.charger.combos$name <- taz.names$name[match(taz.charger.combos$taz,taz.names$taz)]
taz.charger.combos$obj <- NA
names(init.charger.buildout) <- c(';TAZ','L0','L1','L2','L3','L4')
# Now initialize the "index banks" that correspond to the various brackets of decision variable values, sorted by their objective,
# with which we base the choice upon to evaluate or not evaluate the objective during each iteration.
n.alternatives <- nrow(taz.charger.combos)
if(skip.rate > n.alternatives / 2)stop(pp('Error - it is inadvisable to have such a high skip.rate (',skip.rate,') with only ',n.alternatives,' decision variables'))
skip.bank.size <- round(n.alternatives/skip.rate)
ind.banks <- list()
ind.banks.seq <- list()
for(skip.i in 1:skip.rate){
  if(skip.i == skip.rate){
    # the last group should just go to n in order to handle a skip size that doesn't evenly divide n
    ind.banks[[skip.i]] <- ((skip.i - 1)*skip.bank.size + 1):n.alternatives
  }else{
    ind.banks[[skip.i]] <- ((skip.i - 1)*skip.bank.size + 1):(skip.i*skip.bank.size)
  }
  # here we create a logical vector corresponding to whether the decision variable should be evaluated in the iteration
  ind.banks.seq[[skip.i]] <- rep(1:skip.i,length.out=max.chargers.per.pen)==1
}

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(pp(args$experimentdir,'/vary.yaml',sep=''),file.info(pp(args$experimentdir,'/vary.yaml',sep=''))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- pp(path.to.vary.files,vary[[file.param]],sep='')
}

# setup the data frame containing all combinations of those parameter values
vary.tab.original <- expand.grid(vary,stringsAsFactors=F)
#  vary.tab.original$row <- 1:nrow(vary.tab.original)

if(correct.two){
  my.cat(pp("DELETING PEN 2% RESULTS"))
  # test to see if there are any 2% results to delete,
  # if yes, make backups and then delete
	
  for(seed in seeds){
    optim.code <- pp(optim.scenario,'-seed',seed)
    path.to.outputs <- paste(path.to.outputs.base,optim.code,'/',sep='')
    final.2pen.filepath <- pp(path.to.outputs,optim.code,'-pen2-final-infrastructure.txt')
    if(file.exists(final.2pen.filepath)){
      path.to.bak <- pp(path.to.outputs,'bad-2pen-backup/')
      make.dir(path.to.bak)
      file.copy(pp(path.to.outputs,'optimization-history.Rdata'),path.to.bak)
      file.copy(pp(path.to.outputs,'build-result-history.Rdata'),path.to.bak)
      file.copy(pp(path.to.outputs,'buildout-progress.csv'),path.to.bak)
      file.copy(pp(path.to.outputs,'charger-buildout-history.Rdata'),path.to.bak)
      file.copy(final.2pen.filepath,path.to.bak)
      unlink(final.2pen.filepath)
	    load(pp(path.to.outputs,'charger-buildout-history.Rdata'))
      charger.buildout.history <- subset(charger.buildout.history,penetration < 0.02)
	    save(charger.buildout.history,file=pp(path.to.outputs,'charger-buildout-history.Rdata'))
	    load(pp(path.to.outputs,'optimization-history.Rdata'))
      opt.history <- subset(opt.history,penetration < 0.02)
	    save(opt.history,file=pp(path.to.outputs,'optimization-history.Rdata'))
	    load(pp(path.to.outputs,'build-result-history.Rdata'))
      build.result.history <- subset(build.result.history,penetration < 0.02)
	    save(build.result.history,file=pp(path.to.outputs,'build-result-history.Rdata'))
    }
  }
  hot.start <- T
}
if(hot.start){
  found.seeds <- sort(as.numeric(unlist(lapply(str_split(list.files(path.to.outputs.base,pp(optim.scenario,'-seed',seeds,'$',collapse="|")),'seed'),function(l){ l[2] }))))
  start.seed <- tail(found.seeds,1)
  optim.code <- pp(optim.scenario,'-seed',start.seed)
  seed.inds <- which(seeds>=start.seed)
  path.to.outputs <- paste(path.to.outputs.base,optim.code,'/',sep='')
	load(pp(path.to.outputs,'charger-buildout-history.Rdata'))
  start.pen <- tail(charger.buildout.history$penetration,1)
  pen.inds <- which(pev.penetrations>=start.pen)
  start.iter <- tail(charger.buildout.history$iter,1)
  my.cat(pp("HOT START: seed ",start.seed,", pen ",start.pen,", iter ",start.iter))
}else{
  my.cat(pp("COLD START: seed ",seeds[1],", pen 0.005 iter 1"))
  seed.inds <- 1:(length(seeds))
  pen.inds <- 1:(length(pev.penetrations))
}

# configure cluster and get RNetLogo running
model.path <- paste(pevi.home,"netlogo/PEVI-nolog.nlogo",sep='')
if(!exists('cl')){
  print('starting new cluster')
  cl <- makeCluster(c(rep(list(list(host="localhost")),num.cpu)),type="SOCK")
  clusterEvalQ(cl,options(java.parameters="-Xmx2048m"))
  clusterEvalQ(cl,Sys.setenv(NOAWT=1))
  clusterEvalQ(cl,library('RNetLogo'))
  clusterExport(cl,c('init.netlogo','model.path','logfiles'))
  clusterEvalQ(cl,init.netlogo())
}

for(seed in seeds[seed.inds]){
#seed <- seeds[seed.inds][1]
  optim.code <- paste(optim.scenario,'-seed',seed,sep='')
  print(optim.code)

  # setup the paths
  path.to.inputs <- pp(args$experimentdir,optim.code,'/')
  make.dir(path.to.inputs)
  charger.file <- pp(path.to.inputs,'charger-file.txt')
  path.to.outputs <- paste(path.to.outputs.base,optim.code,'/',sep='')
  make.dir(path.to.outputs)
  file.copy(param.file,path.to.outputs)
  file.copy(pp(args$experimentdir,'params.R'),path.to.outputs)
  cat(pp(whoami,' starting ',format(Sys.time(), "%Y-%m-%d %H:%M:%S")),file=pp(path.to.outputs,'WHOAMI.txt'),append=T)

  if(hot.start){
    charger.buildout <- subset(charger.buildout.history,penetration==start.pen & iter==start.iter)
    charger.buildout <- charger.buildout[,-grep('penetration|iter',names(charger.buildout))]
		load(file=pp(path.to.outputs,'optimization-history.Rdata'))
    load(file=pp(path.to.outputs,'build-result-history.Rdata'))
  }else{
    #	Initialize the starting infrastructure and write the file to the inputs dir.
    charger.buildout <- init.charger.buildout

    charger.buildout.history <- data.frame()
    opt.history <- data.frame()
    build.result.history <- data.frame()
  }
  write.table(charger.buildout,charger.file,quote=FALSE,sep='\t',row.names=FALSE)

  for(pev.penetration in pev.penetrations[pen.inds]){
  #pev.penetration <- pev.penetrations[pen.inds][1]
    print(paste("pen",pev.penetration))
    
    if(hot.start){
      taz.charger.combos <- subset(opt.history,penetration==start.pen & iteration==start.iter)[,c('taz','level','include','key','name','obj','cv')]
      current.obj <- taz.charger.combos$obj[1]
      begin.build.i <- start.iter
    }else{
      current.obj <- Inf
      begin.build.i <- 1
      taz.charger.combos$include <- T
    }
    
    # Note that the expectation is that all pev penetrations beyond the first are even multiples of the first
    if(pev.penetration == pev.penetrations[1]){
      vary.tab <- vary.tab.original
    }else{
      pen.ratio <- pev.penetration/pev.penetrations[1]
    	new.vary <- vary
    	new.vary$'driver-input-file' <- new.vary$'driver-input-file'[1:round(length(vary$'driver-input-file')/pen.ratio)]
    	vary.tab <- expand.grid(new.vary,stringsAsFactors=F)
    }
    vary.tab$`driver-input-file` <- str_replace(vary.tab$`driver-input-file`,"penXXX",paste("pen",pev.penetration*100,sep=""))

    # Start for loop for overall penetration level optimization
		for(build.i in begin.build.i:max.chargers.per.pen){
		 #build.i <- begin.build.i
      # at this point, if we're in hot start, drop the history from the current iteration and turn hot start off
      if(hot.start){
        reference.charger.cost <- subset(opt.history,penetration==start.pen & iteration==build.i)$mean.charger.cost[1] 
        reference.delay.cost <- subset(opt.history,penetration==start.pen & iteration==build.i)$mean.delay.cost[1] 
        opt.history <- subset(opt.history,!(penetration==start.pen & iteration>=start.iter))
			  save(opt.history,file=pp(path.to.outputs,'optimization-history.Rdata'))
        charger.buildout.history <- subset(charger.buildout.history,!(penetration==start.pen & iter>=start.iter))
			  save(charger.buildout.history,file=pp(path.to.outputs,'charger-buildout-history.Rdata'))
        if(nrow(build.result.history)>0) build.result.history <- subset(build.result.history,!(penetration==start.pen & iteration>=start.iter))
			  save(build.result.history,file=pp(path.to.outputs,'build-result-history.Rdata'))
        write.table(subset(read.csv(pp(path.to.outputs,'buildout-progress.csv')),!(penetration==start.pen & iteration>=start.iter)),file=pp(path.to.outputs,'buildout-progress.csv'),sep=',',row.names=F)
        hot.start <- F
      }
			print(paste('build.i = ',build.i))

      if(build.i == 1){
        if(nl.obj == 'marginal-cost-to-reduce-delay'){
          print('evaluate baseline cost and delay')
          baseline.results <- evaluate.baseline()
          reference.charger.cost <- mean(baseline.results$total.charger.cost)
          reference.delay.cost <- mean(baseline.results$total.delay.cost)
        }else{
          reference.charger.cost <- 0
          reference.delay.cost <- 0
        }
      }
      
      #	Next is the loop through driver files. the snow parallelization happens here.
			build.result <- evaluate.fitness()

      # if running interactively and you want to check out the results:
      # ggplot(build.result,aes(x=factor(taz),y=obj)) + geom_point() + facet_wrap(~level) + stat_summary(fun.data = "mean_cl_boot", colour = "red")
			
      #	We've run every combination of tazs/chargers for each driver file. Now we asses which charger to place
      #	by averaging the objective function results across all replicates.
			result.means <- ddply(build.result,.(taz,level),function(df){
        if(nl.obj == 'marginal-cost-to-reduce-delay'){
          the.obj <- (mean(df$total.delay.cost) - reference.delay.cost)/(mean(df$total.charger.cost) - reference.charger.cost)
          data.frame(obj = the.obj,cv=sd(df$obj)/the.obj,key=pp(df$taz[1],'-',df$level[1]),mean.delay.cost=mean(df$total.delay.cost),mean.charger.cost=mean(df$total.charger.cost))
        }else{
          data.frame(obj = mean(df$obj),cv=sd(df$obj)/mean(df$obj),key=pp(df$taz[1],'-',df$level[1]),mean.delay.cost=0,mean.charger.cost=0)
        }
			})
      combos.in.results <- match(taz.charger.combos$key[taz.charger.combos$include],result.means$key)
      taz.charger.combos$obj[taz.charger.combos$include] <- result.means$obj[combos.in.results]
      taz.charger.combos$cv[taz.charger.combos$include] <- result.means$cv[combos.in.results]
      taz.charger.combos$mean.delay.cost[taz.charger.combos$include] <- result.means$mean.delay.cost[combos.in.results]
      taz.charger.combos$mean.charger.cost[taz.charger.combos$include] <- result.means$mean.charger.cost[combos.in.results]
      # sort the results and add key, IMPORTANT, everying below relies on this ordering
      taz.charger.combos <- taz.charger.combos[order(taz.charger.combos$obj),] 
      # ggplot(taz.charger.combos,aes(x=factor(taz),y=obj)) + geom_point() + facet_wrap(~level) 
      
      opt.iter <- taz.charger.combos
      opt.iter$penetration <- pev.penetration
      opt.iter$iteration <- build.i
      opt.history <- rbind(opt.history,opt.iter)
			save(opt.history,file=pp(path.to.outputs,'optimization-history.Rdata'))
      build.result$penetration <- pev.penetration
      build.result$iteration <- build.i
      build.result.history <- rbind(build.result.history,build.result)
			save(build.result.history,file=pp(path.to.outputs,'build-result-history.Rdata'))

      #	Winner determined by lowest objective function
			print(pp('winner taz = ',taz.charger.combos$taz[1],' level = ',taz.charger.combos$level[1]))

      reference.charger.cost <- taz.charger.combos$mean.charger.cost[1] 
      reference.delay.cost <- taz.charger.combos$mean.delay.cost[1] 
			
      if(nl.obj == 'marginal-cost-to-reduce-delay'){
        # If our objective value is 0 or greater, we're done.
        if(taz.charger.combos$obj[1] < 0){
          current.obj <- taz.charger.combos$obj[1]
        } else {
          current.obj <- Inf
          break
        }
      } else {
        # If our objective value has reached a minimum, we're done.
        if(current.obj > taz.charger.combos$obj[1]) {
          current.obj <- taz.charger.combos$obj[1]
        } else {
          current.obj <- Inf
          break
        }
      }
			
      #	Now update our infrastructure file for the next round
			charger.buildout[taz.charger.combos$taz[1],grep(taz.charger.combos$level[1],names(charger.buildout))] <- (charger.buildout[taz.charger.combos$taz[1],grep(taz.charger.combos$level[1],names(charger.buildout))] + build.increment[pp('l',taz.charger.combos$level[1])])
			write.table(charger.buildout,charger.file,quote=FALSE,sep='\t',row.names=FALSE)
			
			# Create an Rdata file with a data frame holding all buildout information thus far.
			charger.buildout.iter <- charger.buildout
			charger.buildout.iter$penetration <- pev.penetration
			charger.buildout.iter$iter <- build.i
			charger.buildout.history <- rbind(charger.buildout.history,charger.buildout.iter)
			save(charger.buildout.history,file=pp(path.to.outputs,'charger-buildout-history.Rdata'))

      # finally, a status update on the state of the optimization run
      write.table(data.frame(timestamp=format(Sys.time(), "%Y-%m-%d %H:%M:%S"),seed=seed,penetration=pev.penetration,iteration=build.i,n.alternatives.evaluated=sum(taz.charger.combos$include),obj=current.obj,winner.taz=taz.charger.combos$taz[1],winner.name=taz.charger.combos$name[1],winner.level=taz.charger.combos$level[1],t(taz.charger.combos$obj)),append=file.exists(pp(path.to.outputs,'buildout-progress.csv')),col.names=!file.exists(pp(path.to.outputs,'buildout-progress.csv')),file=pp(path.to.outputs,'buildout-progress.csv'),sep=',',row.names=F)

      # Finally update taz.charger.combos to include/exclude poorly performing alternatives
      for(bank.i in 1:length(ind.banks)){
        taz.charger.combos$include[ind.banks[[bank.i]]] <- ind.banks.seq[[bank.i]][build.i+1]
      }

      # break if requested
      if(file.exists(pp(path.to.outputs,'BREAK'))){
        unlink(pp(path.to.outputs,'BREAK'))
        system(pp('touch ',path.to.outputs,'STOPPED'))
        stop('BREAK file found, stopping')
      }
    } # end iteration loop
    
    # Write out our final infrastructure to a new file.
		final.output.filepath <- pp(path.to.outputs,optim.code,'-pen',pev.penetration*100,'-final-infrastructure.txt')
    write.table(charger.buildout,final.output.filepath,quote=FALSE,sep='\t',row.names=FALSE)    
    
  } # end penetration-level loop
  
  #	Reset the initial charger file.
	write.table(init.charger.buildout,charger.file,quote=FALSE,sep='\t',row.names=FALSE)    
    
  cat(pp(whoami,' ending ',format(Sys.time(), "%Y-%m-%d %H:%M:%S")),file=pp(path.to.outputs,'WHOAMI.txt'),append=T)
} # end seed loop

if(F){
  # Plots to analyze opt.history
  load(pp(pevi.shared,"/data/outputs/optim-new/development-seed1/optimization-history.Rdata"))
  opt.history <- data.table(opt.history,key=c('penetration','iteration','obj'))
  opt.history[,rank:=1:length(name),by=c('penetration','iteration')]

  ggplot(subset(opt.history,rank<50),aes(x=iteration,y=rank,label=key,colour=factor(taz))) + geom_text() + facet_wrap(~penetration) + labs(x="Iteration",y="Rank",title="Top 50 Alternatives by Iteration")

  # what's the distribution of change in rank from one iteration to the next
  setkey(opt.history,'penetration','key','iteration')
  opt.history[,rank:=as.integer(rank)]
  delta.rank <- opt.history[,list(delta=diff(rank)),by=c('penetration','key')]
  before.win <- ddply(opt.history,.(penetration,key),function(df){
    data.frame(rank=ifelse(1 %in% df$rank,df$rank[which(df$rank==1)-1],NA))
  })
  hist(before.win$rank)
}

