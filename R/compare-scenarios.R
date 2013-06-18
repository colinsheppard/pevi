Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
library(colinmisc)
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape'))

base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/Raskolnikovbot3001/Dropbox/'

path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/compare/charge-safety-factor/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/compare/phev-only/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/compare/patterns/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/compare/animation/',sep='')

#to.log <- c('pain','charging','need-to-charge')
to.log <- c('pain','charging','tazs','trip')
#to.log <- c('pain','charging','trip')

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
if("driver.input.file" %in% names(vary)){
  results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
  results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
}
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
  cat(paste(results.i,""))
  if(results.i%%10 == 0){
    save(logs,file=paste(path.to.inputs,'logs.Rdata',sep=''))
  }
  NLCommand('clear-all-and-initialize')
  NLCommand('random-seed 1')
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
  if("tazs" %in% to.log)NLCommand('set log-taz-time-interval 15')
  NLCommand('set go-until-time 30')
  NLCommand('setup')

  NLCommand('time:go-until go-until-time')
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
  if(length(grep("animation",path.to.inputs))>0){
    for(logger in to.log){
      file.copy(paste(outputs.dir,logger,"-out.csv",sep=''),paste(outputs.dir,logger,"-out-",results.i,".csv",sep=''))
    }
  }
}
if(length(grep("animation",path.to.inputs))>0){
  for(logger in to.log){
    file.remove(paste(outputs.dir,logger,"-out.csv",sep=''))
  }
}
save(logs,file=paste(path.to.inputs,'logs.Rdata',sep=''))

# ANALYZE PAIN
ggplot(logs[['pain']],aes(x=time,y=state.of.charge,colour=pain.type,shape=vehicle.type))+geom_point()+facet_grid(charge.safety.factor~replicate)
ggplot(subset(logs[['pain']],pain.type=="delay"),aes(x=time,y=state.of.charge,colour=pain.type,shape=location))+geom_point()+facet_grid(charge.safety.factor~replicate)
ggplot(subset(logs[['pain']],pain.type=="delay"),aes(x=time,y=state.of.charge,colour=pain.type,label=location))+geom_text()+facet_grid(charge.safety.factor~replicate)
ggplot(subset(logs[['pain']],pain.type=="delay"),aes(x=time,y=state.of.charge,colour=vehicle.type,shape=vehicle.type,label=location))+geom_point()+geom_text(aes(y=state.of.charge+.03))+facet_grid(charge.safety.factor~replicate)

# CHARGING
ggplot(subset(logs[['charging']],charger.level>0),aes(x=time,y=begin.soc,colour=factor(charger.level)))+geom_point()+facet_grid(charge.safety.factor~replicate)

# ANALYZE charger availability
ggplot(melt(subset(logs[['tazs']],replicate==1 & charge.safety.factor==1),id.vars=c("time","taz","replicate","charge.safety.factor"),measure.vars=c(paste("num.avail.L",1:3,sep=''))),aes(x=time,y=value,colour=variable))+geom_line()+facet_wrap(~taz)

ggplot(melt(subset(logs[['tazs']],replicate==1),id.vars=c("time","taz","replicate","charge.safety.factor"),measure.vars=c(paste("num.avail.L",1:3,sep=''))),aes(x=time,y=value,colour=variable))+
geom_line()+facet_grid(charge.safety.factor~taz)

# NEED TO CHARGE
ggplot(subset(logs[['need-to-charge']],need.to.charge.=="true"),aes(x=time,y=soc,colour=calling.event,shape=vehicle.type))+geom_point()+facet_grid(charge.safety.factor~replicate)
ddply(logs[['need-to-charge']],.(calling.event,charge.safety.factor),function(df){ data.frame(num.need.to.charge=sum(df$need.to.charge.=="true")) })

# PATTERNS
# Frac events by Charger Type
ggplot(ddply(logs[['charging']],.(vehicle.type),function(df){ 
    data.frame(charger.level=ddply(df,.(charger.level),nrow)$charger.level,
      frac.charges=ddply(df,.(charger.level),nrow)$V1/nrow(df)) 
  }),
  aes(x=1,y=frac.charges))+geom_bar(stat="identity",aes(fill=factor(charger.level)))+ xlab('') + labs(fill='Level') + facet_wrap(~vehicle.type)

ggplot(ddply(logs[['charging']],.(probability.of.unneeded.charge,vehicle.type),function(df){ 
    data.frame(charger.level=ddply(df,.(charger.level),nrow)$charger.level,
      frac.charges=ddply(df,.(charger.level),nrow)$V1/nrow(df)) 
  }),
  aes(x=1,y=frac.charges))+
  geom_bar(stat="identity",aes(fill=factor(charger.level)))+
  facet_grid(probability.of.unneeded.charge~vehicle.type)+ xlab('') + labs(fill='Level')

# Miles per day by vehicle type
ggplot(ddply(ddply(logs[['trip']],.(vehicle.type,replicate),function(df){ sum(df$distance)/length(unique(df$driver)) }),.(vehicle.type),function(df){ data.frame(miles.per.day=mean(df$V1))}),
  aes(x=1,y=miles.per.day))+geom_bar(stat="identity")+ xlab('') + labs(fill='Level') + facet_wrap(~vehicle.type)

# Miles between charging
v.trips <- subset(logs[['trip']],vehicle.type=="volt")
v.ch <- subset(logs[['charging']],vehicle.type=="volt")

miles.between.charges <- ddply(v.ch,.(driver,replicate),function(df){
  df.row <- 1
  res <- sum(subset(v.trips,driver==df$driver[1] & replicate==df$replicate[1] & time <= df$time[df.row])$distance)
  while(df.row < nrow(df)){
    df.row <- df.row + 1
    res <- c(res,sum(subset(v.trips,driver==df$driver[1] & replicate==df$replicate[1] & time <= df$time[df.row] & time > df$time[df.row-1])$distance))
  }
  data.frame(miles=res)
})
ggplot(miles.between.charges,aes(x=miles))+geom_histogram()+geom_vline(data=data.frame(v=c(mean(miles.between.charges$miles),median(miles.between.charges$miles)),c=c('green','red')),aes(xintercept=v,colour=c))

# charges per driver per day
charges.per.driver <- ddply(logs[['charging']],.(replicate),function(df){ data.frame(charges.per.driver=nrow(df)/subset(logs[['summary']],replicate==df$replicate[1] & metric=="num.drivers")$value)})
charges.per.driver$leaf <- ddply(subset(logs[['charging']],vehicle.type=='leaf'),.(replicate),function(df){ nrow(df)/subset(logs[['summary']],replicate==df$replicate[1] & metric=="num.bevs")$value})$V1
charges.per.driver$volt <- ddply(subset(logs[['charging']],vehicle.type=='volt'),.(replicate),function(df){ nrow(df)/(subset(logs[['summary']],replicate==df$replicate[1] & metric=="num.drivers")$value - subset(logs[['summary']],replicate==df$replicate[1] & metric=="num.bevs")$value)})$V1
ggplot(charges.per.driver,aes(x=charges.per.driver))+geom_histogram(binwidth=0.1)

# Volt fraction miles on electricity
v.frac.elec <- ddply(v.trips,.(driver),function(df){ 
  data.frame(frac=(1 - (sum(df$gas.used)/0.027)/sum(df$distance)))
})
ggplot(v.frac.elec,aes(x=frac))+geom_histogram()+geom_vline(data=data.frame(v=c(mean(v.frac.elec$frac),median(v.frac.elec$frac)),c=c('green','red')),aes(xintercept=v,colour=c))

# State-of-charge at start
logs[['charging']]$at.home <- logs[['charging']]$charger.level==0
logs[['charging']]$at.home[logs[['charging']]$at.home] <- "Residential"
logs[['charging']]$at.home[logs[['charging']]$at.home=="FALSE"] <- "Public"
soc.at.beg <- ddply(logs[['charging']],.(at.home,vehicle.type),function(df){ data.frame(up.to=seq(0.1,1,by=.1),percent=hist(df$begin.soc,plot=F,breaks=seq(0,1,by=0.1))$counts/nrow(df)*100) })
ggplot(soc.at.beg,aes(x=up.to,y=percent,fill=factor(at.home)))+geom_bar(stat='identity',position="dodge")+facet_wrap(~vehicle.type)+labs(fill="Charger Type")

# Home charging power demand
caps <- c(6.6,2.4,6.6,30)
demand <- ddply(logs[['tazs']],.(replicate,time),function(df){ colSums(df[,paste('num.L',0:3,sep='')] - df[,paste('num.avail.L',0:3,sep='')]) * caps })
demand <- data.frame(logs[['tazs']][,c('replicate','time','taz')],t(apply(logs[['tazs']][,paste('num.L',0:3,sep='')] - logs[['tazs']][,paste('num.avail.L',0:3,sep='')],1,function(x){ x * caps })))
names(demand) <- c('replicate','time','taz','pow.L0','pow.L1','pow.L2','pow.L3')

demand.sum <- ddply(ddply(demand,.(time,replicate),function(df){ colSums(df[,c('pow.L0','pow.L1','pow.L2','pow.L3')]) }),
                    .(time),function(df){ rbind( data.frame(level=0,min=min(df$pow.L0),max=max(df$pow.L0),median=median(df$pow.L0),mean=mean(df$pow.L0)),
                                                        data.frame(level=1,min=min(df$pow.L1),max=max(df$pow.L1),median=median(df$pow.L1),mean=mean(df$pow.L1)),
                                                        data.frame(level=2,min=min(df$pow.L2),max=max(df$pow.L2),median=median(df$pow.L2),mean=mean(df$pow.L2)),
                                                        data.frame(level=3,min=min(df$pow.L3),max=max(df$pow.L3),median=median(df$pow.L3),mean=mean(df$pow.L3))) })
ggplot(melt(subset(demand.sum,level==0),id.vars=c('time','level')),aes(x=time,y=value,colour=variable))+geom_line()+facet_wrap(~level)
ggplot(melt(subset(demand.sum,level>0),id.vars=c('time','level')),aes(x=time,y=value,colour=variable))+geom_line()+facet_wrap(~level)
demand.sum <- ddply(demand,.(time,taz),function(df){ rbind( data.frame(level=0,min=min(df$pow.L0),max=max(df$pow.L0),median=median(df$pow.L0),mean=mean(df$pow.L0)),
                                                        data.frame(level=1,min=min(df$pow.L1),max=max(df$pow.L1),median=median(df$pow.L1),mean=mean(df$pow.L1)),
                                                        data.frame(level=2,min=min(df$pow.L2),max=max(df$pow.L2),median=median(df$pow.L2),mean=mean(df$pow.L2)),
                                                        data.frame(level=3,min=min(df$pow.L3),max=max(df$pow.L3),median=median(df$pow.L3),mean=mean(df$pow.L3))) })
ggplot(melt(subset(demand.sum,level==0),id.vars=c('time','level','taz')),aes(x=time,y=value,colour=variable))+geom_line()+facet_wrap(~taz)

# Charging length and energy distributions
ggplot(logs[['charging']],aes(x=duration))+geom_histogram(binwidth=1)+facet_wrap(~public)
ggplot(logs[['charging']],aes(x=energy))+geom_histogram(binwidth=0.1)


