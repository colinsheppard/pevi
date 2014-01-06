Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape','stringr','snow'))

seed <- 20

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

path.to.inputs <- pp(pevi.shared,'data/inputs/buildout/',optim.code,'/')
path.to.outputs <- pp(pevi.shared,'data/outputs/buildout/',optim.code,'/')

source(pp(pevi.home,"R/optim/buildout-functions.R"))
source(pp(pevi.home,"R/reporters-loggers.R"))

make.dir(path.to.inputs)
if(!file.exists(pp(path.to.inputs,'params.txt'))){
  system(pp("cp ",path.to.inputs,'../linked2-50-50-seed1/params.txt ',path.to.inputs))
}
make.dir(path.to.outputs)

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(pp(path.to.inputs,'../vary-linked2.yaml'),file.info(pp(path.to.inputs,'../vary-linked2.yaml'))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- pp(pevi.home,'netlogo/',vary[[file.param]])
}
naming <- yaml.load(readChar(pp(path.to.inputs,'../naming.yaml'),file.info(pp(path.to.inputs,'../naming.yaml'))$size))

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
tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
model.path <- pp(pevi.home,"netlogo/PEVI-nolog.nlogo")
NLLoadModel(model.path)

for(cmd in pp('set log-',logfiles,' false')){ NLCommand(cmd) }

#pev.penetration <- 0.01
for(pev.penetration in c(0.005,0.01,0.02,0.04)){
  print(pp("pen ",pev.penetration))
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
  vary.tab$`driver-input-file` <- str_replace(vary.tab$`driver-input-file`,"penXXX",pp("pen ",pev.penetration*100))
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
  NLCommand(pp('set starting-seed ',seed))
  for(build.i in begin.build.i:250){
    print(pp("build iter: ",build.i))
    if(build.i == begin.build.i){
    	print(pp('build.i = ',build.i))
      write.charger.file(build.result$chargers[1:104])
      for(results.i in 1:nrow(results)){
      
#				 DO these need to be specified here, or can we do that from in NL?
      
#        NLCommand('clear-all-and-initialize')
#        NLCommand(paste('set parameter-file "',path.to.inputs,'params.txt"',sep=''))
#        NLCommand(paste('set model-directory "',pevi.home,'netlogo/"',sep=''))
#        NLCommand('read-parameter-file')
        for(param in names(vary.tab)){
          if(is.character(vary.tab[1,param])){
            NLCommand(pp('set ',param,' "',vary.tab[results.i,param],'"'))
          }else{
            NLCommand(pp('set ',param,' ',vary.tab[results.i,param],''))
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
    NLCommand(pp('add-charger ',winner.taz,winner.level))
    write.csv(build.result,file=pp(path.to.outputs,'buildout-pen',pev.penetration*100,'-iter',build.i,'-cost',build.result$cost[winner.i],'.csv'))
    build.result[105,] <- build.result[winner.i,]
    print(pp("winner: ",winner.i))
    save.image(file=pp(path.to.outputs,'buildout-pen',pev.penetration*100,'.Rdata'))
  }
}
