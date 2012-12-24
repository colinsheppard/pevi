Sys.setenv(NOAWT=1)
library(colinmisc)
load.libraries(c('plyr','yaml','RNetLogo'))

path.to.pevi <- '/Users/critter/Dropbox/serc/pev-colin/pevi/'
path.to.inputs <- '/Users/critter/Dropbox/serc/pev-colin/pev-shared/data/inputs/sensitivity/charge-safety-factor/'

.jinit(parameters="-Xmx1024m")

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(paste(path.to.inputs,'vary.yaml',sep=''),file.info(paste(path.to.inputs,'vary.yaml',sep=''))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- paste(path.to.pevi,'netlogo/',vary[[file.param]],sep='')
}

# setup the data frame containing all combinations of those parameter values
vary.tab <- expand.grid(vary,stringsAsFactors=F)

# specify the reporters used to summarize an experiment and setup results data frame
reporters <- data.frame(num.drivers="(count drivers)",
  num.trips="(sum [ length itin-change-flag - sum itin-change-flag ] of drivers)",
  total.delay="(sum [ sum itin-delay-amount  ] of drivers)",
  mean.delay="(mean [ sum itin-delay-amount  ] of drivers)",
  frac.drivers.delayed="(count drivers with [ sum itin-delay-amount > 0 ] / count drivers)",
  num.unscheduled.trips="(sum [ sum itin-change-flag ] of drivers)",
  energy.charged="(sum [ energy-received ] of drivers)",
  driver.expenses="(sum [ expenses ] of drivers)",
  infrastructure.cost="(sum [ [installed-cost] of this-charger-type ] of chargers)",
  gasoline.used="(sum [ gasoline-used ] of drivers)",
  miles.driven="(sum [ miles-driven ] of drivers)",
  num.denials="(sum [ num-denials ] of drivers)",
  frac.denied="(count drivers with [num-denials > 0] / count drivers)",stringsAsFactors=F)

results <- data.frame(vary.tab,reporters)
results$infrastructure.scenario <- as.numeric(unlist(lapply(strsplit(as.character(results$charger.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) })))
results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
results$vehicle.scenario <- as.numeric(unlist(lapply(strsplit(as.character(results$vehicle.type.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) })))

# start NL
nl.path <- "/Applications/NetLogo\ 5.0.1"
NLStart(nl.path, gui=F)
model.path <- paste(path.to.pevi,"netlogo/PEVI.nlogo",sep='')
NLLoadModel(model.path)

NLCommand('set log-wait-time false','set log-charging false','set log-charge-time false','set log-seek-charger false')

# for every combination of parameters, run the model and capture the summary statistics
for(results.i in 1:nrow(results)){
  print(results.i)
  #if(results.i%%25 == 0)cat(paste(results.i,""))
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
  NLCommand('go')

  results[results.i,names(reporters)] <- NLDoReport(1,"",reporter = paste("(sentence",paste(reporters,collapse=' '),")"),as.data.frame=T,df.col.names=names(reporters))
}

NLQuit()

# bug, row 50 is crapping out somehow
