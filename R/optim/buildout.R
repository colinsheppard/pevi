Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
library(colinmisc)
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape','stringr','snow'))

base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/Raskolnikovbot3001/Dropbox/'

optim.code <- 'min-cost-constrained-by-frac-stranded-50-50'

num.cpu <- 8
#num.cpu <- 11

path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/buildout/',optim.code,'/',sep='')
path.to.outputs <- paste(base.path,'pev-shared/data/outputs/buildout/',optim.code,'/',sep='')

source(paste(path.to.pevi,"R/optim/buildout-functions.R",sep=''))
source(paste(path.to.pevi,"R/reporters-loggers.R",sep=''))

make.dir(path.to.inputs)
make.dir(path.to.outputs)

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(paste(path.to.inputs,'../vary.yaml',sep=''),file.info(paste(path.to.inputs,'../vary.yaml',sep=''))$size))
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
nl.path <- "/Applications/NetLogo\ 5.0.3"
tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
model.path <- paste(path.to.pevi,"netlogo/PEVI-nolog.nlogo",sep='')
NLLoadModel(model.path)

for(cmd in paste('set log-',logfiles,' false',sep='')){ NLCommand(cmd) }

#pev.penetration <- 0.01
for(pev.penetration in c(0.005,0.01,0.02,0.04)){
  print(paste("pen",pev.penetration))
  vary.tab <- vary.tab.original
  vary.tab$`driver-input-file` <- str_replace(vary.tab$`driver-input-file`,"penXXX",paste("pen",pev.penetration*100,sep=""))
  results <- data.frame(vary.tab,reporters)
  results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
  results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
  results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
  results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))

  build.result <- data.frame(cost=rep(NA,105),pain=rep(NA,105),chargers=0)
  for(build.i in 1:250){
    print(paste("build iter:",build.i))
    if(build.i == 1){
      write.charger.file(build.result$chargers[1:104])
      for(results.i in 1:nrow(results)){
        NLCommand('clear-all-and-initialize')
        NLCommand(paste('set parameter-file "',path.to.inputs,'params.txt"',sep=''))
        NLCommand(paste('set model-directory "',path.to.pevi,'netlogo/"',sep=''))
        NLCommand('read-parameter-file')
        for(param in names(vary.tab)){
          if(is.character(vary.tab[1,param])){
            NLCommand(paste('set ',param,' "',vary.tab[results.i,param],'"',sep=''))
          }else{
            NLCommand(paste('set ',param,' ',vary.tab[results.i,param],'',sep=''))
          }
        }
        NLCommand(paste('set charger-input-file "',path.to.inputs,'chargers-alt-0.txt"',sep=''))
        NLCommand('setup')
        NLCommand('dynamic-scheduler:go-until schedule 500')
        results[results.i,names(reporters)] <- tryCatch(NLDoReport(1,"",reporter = paste("(sentence",paste(reporters,collapse=' '),")"),as.data.frame=T,df.col.names=names(reporters)),error=function(e){ NA })
      }
      build.result[105,c('cost','pain')] <- c(mean(as.numeric(results$infrastructure.cost)),mean(as.numeric(results$frac.stranded.by.delay)))
    }
    build.result <- evaluate.fitness(build.result)
    
    build.result$marg.cost.of.abatement <- (build.result$cost - build.result$cost[105])*1000/(build.result$pain[105] - build.result$pain)/100
    build.result$marg.cost.of.abatement[build.result$marg.cost.of.abatement<0] <- Inf
    if(all(Inf==build.result$marg.cost.of.abatement[1:104]))break
    winner.i <- which.min(build.result$marg.cost.of.abatement[1:104])
    build.result$chargers[winner.i] <- build.result$chargers[winner.i] + 1
    write.csv(build.result,file=paste(path.to.outputs,'buildout-pen',pev.penetration*100,'-iter',build.i,'-cost',build.result$cost[winner.i],'.csv',sep=''))
    build.result[105,] <- build.result[winner.i,]
    print(paste("winner: ",winner.i,sep=''))
    save.image(file=paste(path.to.outputs,'buildout-pen',pev.penetration*100,'.Rdata',sep=''))
  }
}
