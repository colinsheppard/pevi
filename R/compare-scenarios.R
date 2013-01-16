Sys.setenv(NOAWT=1)
library(colinmisc)
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape'))

#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/Raskolnikovbot3001/Dropbox/'

path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/compare/charge-safety-factor/',sep='')

to.log <- c('pain','charging','tazs')

# load the reporters and loggers needed to summarize runs and disable logging
source(paste(path.to.pevi,"R/reporters-loggers.R",sep=''))

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(paste(path.to.inputs,'vary.yaml',sep=''),file.info(paste(path.to.inputs,'vary.yaml',sep=''))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- paste(path.to.pevi,'netlogo/',vary[[file.param]],sep='')
}
naming <- yaml.load(readChar(paste(path.to.inputs,'naming.yaml',sep=''),file.info(paste(path.to.inputs,'naming.yaml',sep=''))$size))

# setup the data frame containing all combinations of those parameter values
vary.tab <- expand.grid(vary,stringsAsFactors=F)

results <- data.frame(vary.tab)
results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
if("charger.input.file" %in% names(vary)){
  results$infrastructure.scenario <- as.numeric(unlist(lapply(strsplit(as.character(results$charger.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) })))
  results$infrastructure.scenario.named <- results$infrastructure.scenario
  results$infrastructure.scenario.order <- results$infrastructure.scenario
  for(scen.i in as.numeric(names(naming$`charger-input-file`))){
    results$infrastructure.scenario.named[results$infrastructure.scenario == scen.i] <- naming$`charger-input-file`[[as.character(scen.i)]][[1]]
    results$infrastructure.scenario.order[results$infrastructure.scenario == scen.i] <- as.numeric(naming$`charger-input-file`[[as.character(scen.i)]][[2]])
  }
  results$infrastructure.scenario.named  <- reorder(factor(results$infrastructure.scenario.named),results$infrastructure.scenario.order)
}
if("vehicle.type.input.file" %in% names(vary)){
  results$vehicle.scenario <- as.numeric(unlist(lapply(strsplit(as.character(results$vehicle.type.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) })))
  results$vehicle.scenario.named <- results$vehicle.scenario
  results$vehicle.scenario.order <- results$vehicle.scenario
  for(scen.i in as.numeric(names(naming$`vehicle-type-input-file`))){
    results$vehicle.scenario.named[results$vehicle.scenario == scen.i] <- naming$`vehicle-type-input-file`[[as.character(scen.i)]][[1]]
    results$vehicle.scenario.order[results$vehicle.scenario == scen.i] <- as.numeric(naming$`vehicle-type-input-file`[[as.character(scen.i)]][[2]])
  }
}

# start NL
nl.path <- "/Applications/NetLogo\ 5.0.3"
tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
model.path <- paste(path.to.pevi,"netlogo/PEVI.nlogo",sep='')
NLLoadModel(model.path)

for(cmd in paste('set log-',logfiles,' false',sep='')){ NLCommand(cmd) }
for(cmd in paste('set log-',to.log,' true',sep='')){ NLCommand(cmd) }

logs <- list()

# for every combination of parameters, run the model and capture the summary statistics
for(results.i in 1:nrow(results)){
  if(results.i%%10 == 0){
    cat(paste(results.i,""))
    save(logs,file=paste(path.to.inputs,'logs.Rdata',sep=''))
  }
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
  NLCommand('setup')
  NLCommand('dynamic-scheduler:go-until schedule 30')
  if(results.i == 1){
    outputs.dir <- NLReport('outputs-directory')
    for(logger in to.log){
      tmp <- read.csv(paste(outputs.dir,logger,"-out.csv",sep=''),stringsAsFactors=F)
      logs[[logger]] <- data.frame(results[results.i,],tmp,row.names=1:nrow(tmp))
    }
  }else{
    for(logger in to.log){
      tmp <- read.csv(paste(outputs.dir,logger,"-out.csv",sep=''),stringsAsFactors=F)
      logs[[logger]] <- rbind(logs[[logger]],data.frame(results[results.i,],tmp,row.names=1:nrow(tmp)))
    }
  }
}
save(logs,file=paste(path.to.inputs,'logs.Rdata',sep=''))

NLQuit()

# ANALYZE PAIN
ggplot(subset(logs[['pain']],pain.type=="delay"),aes(x=time,y=state.of.charge,colour=vehicle.type))+geom_point()+facet_grid(charge.safety.factor~replicate)
ggplot(subset(logs[['pain']],pain.type=="denial" & replicate==1),aes(x=time,y=state.of.charge,colour=vehicle.type))+geom_point()+facet_grid(charge.safety.factor~replicate)

ggplot(subset(logs[['charging']],charger.level>0),aes(x=time,y=begin.soc,colour=factor(charger.level)))+geom_point()+facet_grid(charge.safety.factor~replicate)

# ANALYZE charger availability
ggplot(melt(subset(logs[['tazs']],replicate==1 & charge.safety.factor==1),id.vars=c("time","taz","replicate","charge.safety.factor"),measure.vars=c(paste("num.avail.L",1:3,sep=''))),aes(x=time,y=value,colour=variable))+geom_line()+facet_wrap(~taz)

ggplot(melt(subset(logs[['tazs']],replicate==1),id.vars=c("time","taz","replicate","charge.safety.factor"),measure.vars=c(paste("num.avail.L",1:3,sep=''))),aes(x=time,y=value,colour=variable))+
geom_line()+facet_grid(charge.safety.factor~taz)

