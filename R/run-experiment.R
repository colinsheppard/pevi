Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
library(colinmisc)
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape'))

base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/Raskolnikovbot3001/Dropbox/'

exp.name <- commandArgs(trailingOnly=T)[1]
#exp.name <- "charge-safety-factor"
#exp.name <- "chargers"
#exp.name <- "charger-search-distance"
#exp.name <- "probability-of-unneeded-charge"
#exp.name <- "wait-time-mean"
#exp.name <- "willing-to-roam-time-threshold"
#exp.name <- "time-opportunity-cost"
#exp.name <- "electric-fuel-consumption"
#exp.name <- "electric-fuel-consumption-mean"

path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/sensitivity/',exp.name,'/',sep='')

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(paste(path.to.inputs,'vary.yaml',sep=''),file.info(paste(path.to.inputs,'vary.yaml',sep=''))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- paste(path.to.pevi,'netlogo/',vary[[file.param]],sep='')
}
naming <- yaml.load(readChar(paste(path.to.inputs,'../naming.yaml',sep=''),file.info(paste(path.to.inputs,'../naming.yaml',sep=''))$size))

# setup the data frame containing all combinations of those parameter values
vary.tab <- expand.grid(vary,stringsAsFactors=F)

# load the reporters and loggers needed to summarize runs and disable logging
source(paste(path.to.pevi,"R/reporters-loggers.R",sep=''))

results <- data.frame(vary.tab,reporters)
results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
if("charger-input-file" %in% names(vary)){
  results$infrastructure.scenario <- as.numeric(unlist(lapply(strsplit(as.character(results$charger.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) })))
  results$infrastructure.scenario.named <- results$infrastructure.scenario
  results$infrastructure.scenario.order <- results$infrastructure.scenario
  for(scen.i in as.numeric(names(naming$`charger-input-file`))){
    results$infrastructure.scenario.named[results$infrastructure.scenario == scen.i] <- naming$`charger-input-file`[[as.character(scen.i)]][[1]]
    results$infrastructure.scenario.order[results$infrastructure.scenario == scen.i] <- as.numeric(naming$`charger-input-file`[[as.character(scen.i)]][[2]])
  }
  results$infrastructure.scenario.named  <- reorder(factor(results$infrastructure.scenario.named),results$infrastructure.scenario.order)
}
if("vehicle-type-input-file" %in% names(vary)){
  results$vehicle.scenario <- unlist(lapply(strsplit(as.character(results$vehicle.type.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) }))
  results$vehicle.scenario.named <- results$vehicle.scenario
  results$vehicle.scenario.order <- results$vehicle.scenario
  for(scen.i in names(naming$`vehicle-type-input-file`)){
    results$vehicle.scenario.named[results$vehicle.scenario == scen.i] <- naming$`vehicle-type-input-file`[[scen.i]][[1]]
    results$vehicle.scenario.order[results$vehicle.scenario == scen.i] <- as.numeric(naming$`vehicle-type-input-file`[[scen.i]][[2]])
  }
}

# start NL
nl.path <- "/Applications/NetLogo\ 5.0.3"
tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
model.path <- paste(path.to.pevi,"netlogo/PEVI.nlogo",sep='')
NLLoadModel(model.path)

for(cmd in paste('set log-',logfiles,' false',sep='')){ NLCommand(cmd) }

print(paste("n-runs:",nrow(results)))

# for every combination of parameters, run the model and capture the summary statistics
for(results.i in 1:nrow(results)){
  #print(results.i)
  if(results.i%%10 == 0){
    cat(paste(results.i,""))
    save(results,file=paste(path.to.inputs,'results.Rdata',sep=''))
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
  NLCommand('random-seed 1')
  NLCommand('setup')
  NLCommand('time:go-until 500')
  results[results.i,names(reporters)] <- tryCatch(NLDoReport(1,"",reporter = paste("(sentence",paste(reporters,collapse=' '),")"),as.data.frame=T,df.col.names=names(reporters)),error=function(e){ NA })
}

for(res in names(reporters)){
  results[,res] <- as.numeric(results[,res])
}
save(results,file=paste(path.to.inputs,'results.Rdata',sep=''))
