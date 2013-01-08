Sys.setenv(NOAWT=1)
library(colinmisc)
load.libraries(c('ggplot2','yaml','RNetLogo'))

#path.to.pevi <- '/Users/critter/Dropbox/serc/pev-colin/pevi/'
#path.to.inputs <- '/Users/critter/Dropbox/serc/pev-colin/pev-shared/data/inputs/sensitivity/charge-safety-factor/'
path.to.pevi <- '/Users/sheppardc/Dropbox/serc/pev-colin/pevi/'
path.to.inputs <- '/Users/sheppardc/Dropbox/serc/pev-colin/pev-shared/data/inputs/sensitivity/charge-safety-factor/'
path.to.inputs <- '/Users/sheppardc/Dropbox/serc/pev-colin/pev-shared/data/inputs/sensitivity/willing-to-roam-time-threshold/'

#.jinit(parameters="-Xmx1024m")

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(paste(path.to.inputs,'vary.yaml',sep=''),file.info(paste(path.to.inputs,'vary.yaml',sep=''))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- paste(path.to.pevi,'netlogo/',vary[[file.param]],sep='')
}
naming <- yaml.load(readChar(paste(path.to.inputs,'naming.yaml',sep=''),file.info(paste(path.to.inputs,'naming.yaml',sep=''))$size))

# setup the data frame containing all combinations of those parameter values
vary.tab <- expand.grid(vary,stringsAsFactors=F)

# specify the reporters used to summarize an experiment and setup results data frame
reporters <- data.frame(num.drivers="(count drivers)",
  num.trips="(sum [ length itin-change-flag - sum itin-change-flag ] of drivers)",
  total.delay="(sum [ sum itin-delay-amount  ] of drivers)",
  mean.delay="((sum [ sum itin-delay-amount  ] of drivers) / (sum [length (filter [? > 0] itin-delay-amount)] of drivers))",
  frac.drivers.delayed="(count drivers with [ sum itin-delay-amount > 0 ] / count drivers)",
  num.unscheduled.trips="(sum [ sum itin-change-flag ] of drivers)",
  energy.charged="(sum [ energy-received ] of drivers)",
  driver.expenses="(sum [ expenses ] of drivers)",
  infrastructure.cost="(sum [ [installed-cost] of this-charger-type ] of chargers)",
  gasoline.used="(sum [ gasoline-used ] of drivers)",
  miles.driven="(sum [ miles-driven ] of drivers)",
  num.denials="(sum [ num-denials ] of drivers)",
  num.stranded='(num-stranded)',
  mean.duty.factor="(mean-duty-factor)",
  frac.denied="(count drivers with [num-denials > 0] / count drivers)",stringsAsFactors=F)

# log files, these all get set to false so logging is deactivated
logfiles<-c("wait-time","charging","charge-time","seek-charger","seek-charger-result","need-to-charge","trip-journey-timeuntildepart","break-up-trip","break-up-trip-choice","charge-limiting-factor","drivers")

results <- data.frame(vary.tab,reporters)
results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
results$infrastructure.scenario <- as.numeric(unlist(lapply(strsplit(as.character(results$charger.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) })))
results$infrastructure.scenario.named <- results$infrastructure.scenario
for(scen.i in as.numeric(names(naming$`charger-input-file`))){
  results$infrastructure.scenario.named[results$infrastructure.scenario == scen.i] <- naming$`charger-input-file`[[as.character(scen.i)]]
}
results$vehicle.scenario <- as.numeric(unlist(lapply(strsplit(as.character(results$vehicle.type.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) })))
results$vehicle.scenario.named <- results$vehicle.scenario
for(scen.i in as.numeric(names(naming$`vehicle-type-input-file`))){
  results$vehicle.scenario.named[results$vehicle.scenario == scen.i] <- naming$`vehicle-type-input-file`[[as.character(scen.i)]]
}

# start NL
nl.path <- "/Applications/NetLogo\ 5.0.3"
NLStart(nl.path, gui=F)
model.path <- paste(path.to.pevi,"netlogo/PEVI.nlogo",sep='')
NLLoadModel(model.path)

for(cmd in paste('set log-',logfiles,' false',sep='')){ NLCommand(cmd) }

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
  NLCommand('setup')
  NLCommand('dynamic-scheduler:go-until schedule 500')
  results[results.i,names(reporters)] <- tryCatch(NLDoReport(1,"",reporter = paste("(sentence",paste(reporters,collapse=' '),")"),as.data.frame=T,df.col.names=names(reporters)),error=function(e){ NA })
}

for(res in names(reporters)){
  results[,res] <- as.numeric(results[,res])
}
save(results,file=paste(path.to.inputs,'results.Rdata',sep=''))

NLQuit()

# HOW MANY DRIVERS ARE DELAYED
ggplot(results,aes(x=infrastructure.scenario.named,y=frac.drivers.delayed))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~vehicle.scenario.named)

# UNSCHEDULED TRIPS / DRIVER
ggplot(results,aes(x=infrastructure.scenario.named,y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~vehicle.scenario.named)

# GASOLINE USED
ggplot(results,aes(x=infrastructure.scenario.named,y=gasoline.used))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~vehicle.scenario.named)

# DUTY FACTOR
ggplot(results,aes(x=infrastructure.scenario.named,y=mean.duty.factor))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~vehicle.scenario.named)

# CHARGE-SAFETY-FACTOR
ggplot(results,aes(x=factor(charge.safety.factor),y=total.delay))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggplot(results,aes(x=factor(charge.safety.factor),y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggplot(results,aes(x=factor(charge.safety.factor),y=num.stranded/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()

# WILLING TO ROAM TIME THRESHOLD 
ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=total.delay/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=num.stranded/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=mean.duty.factor))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
