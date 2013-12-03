Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
library(colinmisc)
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape','stringr','snow'))

#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
base.path <- '/Users/Raskolnikovbot3001/Dropbox/serc/'

seed <- 19

#num.cpu <- 8
#num.cpu <- 12
num.cpu <- 2

#optim.code <- 'min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'min-cost-constrained-by-frac-stranded-50-50-seed9'
optim.code <- paste('linked2-50-50-seed',seed,sep='')
#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-25-75'
#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-75-25'
#optim.code <- 'thresh-1-linked-min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'thresh-2-linked-min-cost-constrained-by-frac-stranded-50-50'

link.pens <- str_detect(optim.code,"linked")  # should the infrastructure from lower pens be used as starting place for higher? otherwise,
                # infrastructure is reset to zero

path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/buildout/',optim.code,'/',sep='')
path.to.outputs <- paste(base.path,'pev-shared/data/outputs/buildout/',optim.code,'/',sep='')

source(paste(path.to.pevi,"R/optim/buildout-functions.R",sep=''))
source(paste(path.to.pevi,"R/reporters-loggers.R",sep=''))

make.dir(path.to.inputs)
if(!file.exists(paste(path.to.inputs,'params.txt',sep=''))){
  system(paste("cp ",path.to.inputs,'../linked2-50-50-seed1/params.txt ',path.to.inputs,sep=''))
}
make.dir(path.to.outputs)

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(paste(path.to.inputs,'../vary-linked2.yaml',sep=''),file.info(paste(path.to.inputs,'../vary-linked2.yaml',sep=''))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- paste(path.to.pevi,'netlogo/',vary[[file.param]],sep='')
}
naming <- yaml.load(readChar(paste(path.to.inputs,'../naming.yaml',sep=''),file.info(paste(path.to.inputs,'../naming.yaml',sep=''))$size))

# setup the data frame containing all combinations of those parameter values
vary.tab.original <- expand.grid(vary,stringsAsFactors=F)

# load the reporters and loggers needed to summarize runs and disable logging

if(!exists('cl')){
  print('starting new cluster')
  cl <- makeCluster(c(rep(list(list(host="localhost")),num.cpu)),type="SOCK")
  clusterEvalQ(cl,options(java.parameters="-Xmx2048m"))
  clusterEvalQ(cl,Sys.setenv(NOAWT=1))
  clusterEvalQ(cl,library('RNetLogo'))
}

# start NL
# nl.path <- "/Applications/NetLogo\ 5.0.3"
nl.path <- "/Applications/NetLogo\ 5.0.4"
tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
model.path <- paste(path.to.pevi,"netlogo/PEVI-nolog.nlogo",sep='')
NLLoadModel(model.path)

for(cmd in paste('set log-',logfiles,' false',sep='')){ NLCommand(cmd) }

#pev.penetration <- 0.01
for(pev.penetration in c(0.005,0.01,0.02,0.04)){
  print(paste("pen",pev.penetration))
  if(pev.penetration <= 0.0051){
    vary.tab <- vary.tab.original
  }else if(pev.penetration <= 0.0101){
    vary.tab <- data.frame(vary.tab.original[1:40,])
  }else if(pev.penetration <= 0.0201){
    vary.tab <- data.frame(vary.tab.original[1:20,])
  }else if(pev.penetration <= 0.0401){
    vary.tab <- data.frame(vary.tab.original[1:10,])
  }
  names(vary.tab) <- "driver-input-file"
  vary.tab$`driver-input-file` <- str_replace(vary.tab$`driver-input-file`,"penXXX",paste("pen",pev.penetration*100,sep=""))
  results <- data.frame(vary.tab,reporters)
  results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
  results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))

  if(!link.pens | pev.penetration==0.005){
    #build.result <- data.frame(cost=rep(NA,105),pain=rep(NA,105),chargers=0)  # No chargers
    build.result <- data.frame(cost=rep(NA,105),pain=rep(NA,105),chargers=c(rep(0,5),1,rep(0,16),1,rep(0,3),1,rep(0,105-27))) # Existing, 1 in EKA_Waterfront, 1 in EKA_NW101, 1 in ARC_Plaza
    begin.build.i <- 1
  }else{
    begin.build.i <- build.i
  }
  NLCommand('clear-all')
  NLCommand(paste('set starting-seed',seed))
  for(build.i in begin.build.i:250){
    print(paste("build iter:",build.i))
    if(build.i == begin.build.i){
    	print(paste('build.i = ',build.i,sep=''))
      write.charger.file(build.result$chargers[1:104])
      for(results.i in 1:nrow(results)){
      
#				 DO these need to be specified here, or can we do that from in NL?
      
#        NLCommand('clear-all-and-initialize')
#        NLCommand(paste('set parameter-file "',path.to.inputs,'params.txt"',sep=''))
#        NLCommand(paste('set model-directory "',path.to.pevi,'netlogo/"',sep=''))
#        NLCommand('read-parameter-file')
        for(param in names(vary.tab)){
          if(is.character(vary.tab[1,param])){
            NLCommand(paste('set ',param,' "',vary.tab[results.i,param],'"',sep=''))
          }else{
            NLCommand(paste('set ',param,' ',vary.tab[results.i,param],'',sep=''))
          }
        }
        
#				Setup charger-input file once, at beginning,. Add-charger and delete-charger handle the rest.        
        
#        NLCommand(paste('set charger-input-file "',path.to.inputs,'chargers-alt-0.txt"',sep=''))
        if(!is.na(seed)){      
					NLCommand('set fix-seed TRUE')       
        } else {
        	NLCommand('set fix-seed FALSE')
        }
        NLCommand('setup-in-batch-mode')
        NLCommand('go-until')
        results[results.i,names(reporters)] <- tryCatch(NLDoReport(1,"",reporter = paste("(sentence",paste(reporters,collapse=' '),")"),as.data.frame=T,df.col.names=names(reporters)),error=function(e){ NA })
      }
      build.result[105,c('cost','pain')] <- c(mean(as.numeric(results$infrastructure.cost)),mean(as.numeric(results$frac.stranded.by.delay)))
    }
    build.result <- evaluate.fitness(build.result)
    
    build.result$marg.cost.of.abatement <- (build.result$cost - build.result$cost[105])*1000/(build.result$pain[105] - build.result$pain)/100
    build.result$marg.cost.of.abatement[build.result$marg.cost.of.abatement<0] <- Inf
    if(all(Inf==build.result$marg.cost.of.abatement[1:104]))break
    winner.i <- which.min(build.result$marg.cost.of.abatement[1:104])
    if(winner.i <= 52) {
    	winner.taz <- winner.i
    	winner.level <- 2
    } else {
    	winner.taz <- winner.i - 52
    	winner.level <- 3
    }
    build.result$chargers[winner.i] <- build.result$chargers[winner.i] + 1  # NEW CHARGER GETS BUILT HERE
    NLCommand(paste('add-charger',winner.taz,winner.level))
    write.csv(build.result,file=paste(path.to.outputs,'buildout-pen',pev.penetration*100,'-iter',build.i,'-cost',build.result$cost[winner.i],'.csv',sep=''))
    build.result[105,] <- build.result[winner.i,]
    print(paste("winner: ",winner.i,sep=''))
    save.image(file=paste(path.to.outputs,'buildout-pen',pev.penetration*100,'.Rdata',sep=''))
  }
}
