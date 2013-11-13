Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape','stringr'))

#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
base.path <- '/Users/Raskolnikovbot3001/Dropbox/serc/'

exp.name <- commandArgs(trailingOnly=T)[1]
#exp.name <- "linked-min-cost-constrained-by-frac-stranded-50-50"

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

results$infrastructure.scenario <- as.numeric(unlist(lapply(strsplit(as.character(results$charger.input.file),'-iter',fixed=T),function(x){ as.numeric(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
results$infrastructure.scenario.pen <- as.numeric(unlist(lapply(strsplit(as.character(results$charger.input.file),'-pen',fixed=T),function(x){ as.numeric(strsplit(x[2],".txt",fixed=T)[[1]][1]) })))

results <- subset(results,penetration==infrastructure.scenario.pen)

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
  NLCommand('profiler:start')
  NLCommand('clear-all-and-initialize')
  NLCommand(paste('set parameter-file "',path.to.inputs,'params.txt"',sep=''))
  NLCommand(paste('set model-directory "',path.to.pevi,'netlogo/"',sep=''))
  NLCommand('read-parameter-file')
  for(param in names(vary.tab)){
    param.dots <- str_replace_all(param,"-",".")
    if(is.character(vary.tab[1,param])){
      NLCommand(paste('set ',param,' "',results[results.i,param.dots],'"',sep=''))
    }else{
      NLCommand(paste('set ',param,' ',results[results.i,param.dots],'',sep=''))
    }
  }
  NLCommand('random-seed 2')
  NLCommand('setup')
  NLCommand('dynamic-scheduler:go-until schedule 500')
  results[results.i,names(reporters)] <- tryCatch(NLDoReport(1,"",reporter = paste("(sentence",paste(reporters,collapse=' '),")"),as.data.frame=T,df.col.names=names(reporters)),error=function(e){ NA })
  NLCommand('profiler:stop')
  NLCommand('print profiler:report')
}

for(res in names(reporters)){
  results[,res] <- as.numeric(results[,res])
}
save(results,file=paste(path.to.inputs,'results.Rdata',sep=''))
