Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx30g")
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape','stringr'))

#exp.name <- commandArgs(trailingOnly=T)[1]
#exp.name <- 'delhi-baseline-pain'
#exp.name <- 'delhi-smart-charging-demand'
exp.name <- 'delhi-smart-quasi-reopt'
path.to.inputs <- pp(pevi.shared,'data/inputs/compare/',exp.name,'/')

#to.log <- c()
#to.log <- 'pain'
#to.log <- c('pain','charging')
to.log <- c('pain','charging','trip')
#to.log <- c('pain','charging','tazs','trip') # use this for animations

# load the reporters and loggers needed to summarize runs and disable logging
source(paste(pevi.home,"R/reporters-loggers.R",sep=''))

# read the parameters and values to vary in the experiment
vary <- yaml.load(readChar(paste(path.to.inputs,'vary.yaml',sep=''),file.info(paste(path.to.inputs,'vary.yaml',sep=''))$size))
#vary <- yaml.load(readChar(paste(path.to.inputs,'vary-noL1.yaml',sep=''),file.info(paste(path.to.inputs,'vary-noL1.yaml',sep=''))$size))
for(file.param in names(vary)[grep("-file",names(vary))]){
  vary[[file.param]] <- str_replace_all(str_replace_all(str_replace_all(vary[[file.param]],"pevi.home",pevi.home),"pevi.shared",pevi.shared),"pevi.nondrop",pevi.nondrop)
}
naming <- yaml.load(readChar(paste(path.to.inputs,'naming.yaml',sep=''),file.info(paste(path.to.inputs,'naming.yaml',sep=''))$size))

# setup the data frame containing all combinations of those parameter values
vary.tab <- expand.grid(vary,stringsAsFactors=F)

results <- data.frame(vary.tab)
if("driver-input-file" %in% names(vary)){
  results$penetration <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
  results$replicate <- as.numeric(unlist(lapply(strsplit(as.character(results$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
  results$itin.scenario <- unlist(lapply(strsplit(as.character(results$driver.input.file),'/driver-schedule',fixed=T),function(x){ tail(unlist(strsplit(x[1],"/",fixed=T)[[1]]),1) }))
  results$itin.scenario.named <- results$itin.scenario
  results$itin.scenario.order <- results$itin.scenario
  for(scen.i in names(naming$`driver-input-file`)){
    results$itin.scenario.named[results$itin.scenario == scen.i] <- naming$`driver-input-file`[[scen.i]][[1]]
    results$itin.scenario.order[results$itin.scenario == scen.i] <- as.numeric(naming$`driver-input-file`[[as.character(scen.i)]][[2]])
  }
}
if("charger-input-file" %in% names(vary)){
  results$infrastructure.scenario <- unlist(lapply(strsplit(as.character(results$charger.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) }))
  results$infrastructure.scenario.named <- results$infrastructure.scenario
  results$infrastructure.scenario.order <- results$infrastructure.scenario
  if(all(is.na(results$infrastructure.scenario))){
    for(scen.i in names(naming$`charger-input-file`)){
      results$infrastructure.scenario[grep(scen.i,as.character(results$charger.input.file))] <- scen.i
      results$infrastructure.scenario.named[results$infrastructure.scenario == scen.i] <- naming$`charger-input-file`[[scen.i]][[1]]
      results$infrastructure.scenario.order[results$infrastructure.scenario == scen.i] <- as.numeric(naming$`charger-input-file`[[scen.i]][[2]])
    }
  }else{
    for(scen.i in as.numeric(names(naming$`charger-input-file`))){
      results$infrastructure.scenario.named[results$infrastructure.scenario == scen.i] <- naming$`charger-input-file`[[as.character(scen.i)]][[1]]
      results$infrastructure.scenario.order[results$infrastructure.scenario == scen.i] <- as.numeric(naming$`charger-input-file`[[as.character(scen.i)]][[2]])
    }
  }
  results$infrastructure.scenario.named  <- reorder(factor(results$infrastructure.scenario.named),results$infrastructure.scenario.order)
}
if("vehicle-type-input-file" %in% names(vary)){
  results$vehicle.scenario <- as.numeric(unlist(lapply(strsplit(as.character(results$vehicle.type.input.file),'-scen',fixed=T),function(x){ unlist(strsplit(x[2],".txt",fixed=T)) })))
  results$vehicle.scenario.named <- results$vehicle.scenario
  results$vehicle.scenario.order <- results$vehicle.scenario
  if(all(is.na(results$vehicle.scenario))){
    for(scen.i in names(naming$`vehicle-type-input-file`)){
      results$vehicle.scenario[grep(scen.i,as.character(results$vehicle.type.input.file))] <- scen.i
      results$vehicle.scenario.named[results$vehicle.scenario == scen.i] <- naming$`vehicle-type-input-file`[[scen.i]][[1]]
      results$vehicle.scenario.order[results$vehicle.scenario == scen.i] <- as.numeric(naming$`vehicle-type-input-file`[[scen.i]][[2]])
    }
  }else{
    for(scen.i in as.numeric(names(naming$`vehicle-type-input-file`))){
      results$vehicle.scenario.named[results$vehicle.scenario == scen.i] <- naming$`vehicle-type-input-file`[[as.character(scen.i)]][[1]]
      results$vehicle.scenario.order[results$vehicle.scenario == scen.i] <- as.numeric(naming$`vehicle-type-input-file`[[as.character(scen.i)]][[2]])
    }
  }
}
if(exp.name=='delhi-animation' | exp.name=='delhi-battery-swap' | exp.name=='consistent-vs-quadrupled'){
  results <- subset(results,(penetration==0.5 & infrastructure.scenario=='delhi-final-rec-pen-0.5') | 
                    (penetration==1 & infrastructure.scenario=='delhi-final-rec-pen-1') | 
                    (penetration==2 & infrastructure.scenario=='delhi-final-rec-pen-2') |
                    (penetration==0.5 & infrastructure.scenario=='delhi-final-with-swap-pen-0.5') | 
                    (penetration==1 & infrastructure.scenario=='delhi-final-with-swap-pen-1') | 
                    (penetration==2 & infrastructure.scenario=='delhi-final-with-swap-pen-2') &
                    ((num.simulation.days==2 & itin.scenario=='consistent') |
                    (num.simulation.days==4 & itin.scenario=='quadrupled'))
                    )
}

# start NL
tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
#model.path <- paste(pevi.home,"netlogo/PEVI.nlogo",sep='')
model.path <- paste(pevi.home,"netlogo/PEVI-v2.1.2.nlogo",sep='')
NLLoadModel(model.path)

for(cmd in paste('set log-',logfiles,' false',sep='')){ NLCommand(cmd) }
for(cmd in paste('set log-',to.log,' true',sep='')){ NLCommand(cmd) }

logs <- list()
logs[['results']] <- results
for(reporter in names(reporters)){
  logs[['results']][,reporter] <- NA
}

# for every combination of parameters, run the model and capture the summary statistics
for(results.i in 1:nrow(results)){
#results.i <- 1
  cat(paste(results.i,""))
  system('sleep 0.01')
  if(results.i%%10 == 0){
    save(logs,file=paste(path.to.inputs,'logs.Rdata',sep=''))
  }
  NLCommand('clear-all-and-initialize')
  NLCommand('random-seed 1')
  NLCommand(paste('set param-file-base "',pevi.shared,'"',sep=''))
  NLCommand(paste('set parameter-file "',path.to.inputs,'/params.txt"',sep=''))
  NLCommand(paste('set model-directory "',pevi.home,'netlogo/"',sep=''))
  NLCommand('set batch-setup? false')
  NLCommand('read-parameter-file')
  for(param in names(vary.tab)){
    param.dot <- str_replace_all(param,"-",".") 
    if(is.character(vary.tab[1,param])){
      NLCommand(paste('set ',param,' "',results[results.i,param.dot],'"',sep=''))
    }else{
      NLCommand(paste('set ',param,' ',results[results.i,param.dot],'',sep=''))
    }
  }
  if("tazs" %in% to.log)NLCommand('set log-taz-time-interval 5')
  NLCommand('set go-until-time 125')
  NLCommand('setup')

  NLCommand('time:go-until go-until-time')
  logs[['results']][results.i,names(reporters)] <- tryCatch(NLDoReport(1,"",reporter = paste("(sentence",paste(reporters,collapse=' '),")"),as.data.frame=T,df.col.names=names(reporters)),error=function(e){ NA })
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
#save(logs,file=paste(path.to.inputs,'logs-delhi-smart.Rdata',sep=''))
#load(paste(path.to.inputs,'logs.Rdata',sep=''))

#######################################
# POST PROCESSING
#######################################
# create pen/rep columns, this assumes the first column is the driver input file
for(log.file in to.log){
  logs[[log.file]]$penetration <- as.numeric(unlist(lapply(strsplit(as.character(logs[[log.file]]$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
  logs[[log.file]]$replicate <- as.numeric(unlist(lapply(strsplit(as.character(logs[[log.file]]$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
  logs[[log.file]]$itin.scenario <- unlist(lapply(strsplit(as.character(logs[[log.file]]$driver.input.file),'/driver-schedule',fixed=T),function(x){ tail(unlist(strsplit(x[1],"/",fixed=T)[[1]]),1) }))
  logs[[log.file]]$itin.scenario.named <- logs[[log.file]]$itin.scenario
  logs[[log.file]]$itin.scenario.order <- logs[[log.file]]$itin.scenario
  for(scen.i in names(naming$`driver-input-file`)){
    logs[[log.file]]$itin.scenario.named[logs[[log.file]]$itin.scenario == scen.i] <- naming$`driver-input-file`[[scen.i]][[1]]
    logs[[log.file]]$itin.scenario.order[logs[[log.file]]$itin.scenario == scen.i] <- as.numeric(naming$`driver-input-file`[[as.character(scen.i)]][[2]])
  }
  if('charger.input.file' %in% names(logs[[log.file]])){
    logs[[log.file]]$infrastructure.scenario <- NA
    for(scen.i in names(naming$`charger-input-file`)){
      logs[[log.file]]$infrastructure.scenario[grep(scen.i,as.character(logs[[log.file]]$charger.input.file))] <- scen.i
      logs[[log.file]]$infrastructure.scenario.named[logs[[log.file]]$infrastructure.scenario == scen.i] <- naming$`charger-input-file`[[scen.i]][[1]]
      logs[[log.file]]$infrastructure.scenario.order[logs[[log.file]]$infrastructure.scenario == scen.i] <- as.numeric(naming$`charger-input-file`[[scen.i]][[2]])
    }
  }
  if('vehicle.type.input.file' %in% names(logs[[log.file]])){
    logs[[log.file]]$vehicle.scenario <- NA
    for(scen.i in names(naming$`vehicle-type-input-file`)){
      logs[[log.file]]$vehicle.scenario[grep(scen.i,as.character(logs[[log.file]]$vehicle.type.input.file))] <- scen.i
      logs[[log.file]]$vehicle.scenario.named[logs[[log.file]]$vehicle.scenario == scen.i] <- naming$`vehicle-type-input-file`[[scen.i]][[1]]
      logs[[log.file]]$vehicle.scenario.order[logs[[log.file]]$vehicle.scenario == scen.i] <- as.numeric(naming$`vehicle-type-input-file`[[scen.i]][[2]])
    }
  }
}
#save(logs,file=paste(path.to.inputs,'logs-veh-scens-v2.1.2.Rdata',sep=''))
save(logs,file=paste(path.to.inputs,'logs.Rdata',sep=''))
#load(paste(path.to.inputs,'logs.Rdata',sep=''))

#######################################
# ANALYSIS
#######################################

############################
# Irreducible Delay Analysis
############################

#load(paste(path.to.inputs,'logs-v2.1.2.Rdata',sep=''))
results <- data.table(logs[['results']])
results[,':='(driver.input.file=NULL,charger.input.file=NULL,itin.scenario=NULL,itin.scenario.named=NULL,itin.scenario.order=NULL)]
results <- results[!is.na(infrastructure.scenario.named)]
results <- results[pp('Chargers for ',penetration,'%') == infrastructure.scenario.named]
setkey(results,penetration)
mean.results <- results[,list(num.drivers=mean(num.drivers),objective=mean(objective),total.delay=mean(total.delay),total.delay.cost=mean(total.delay.cost),num.stranded=mean(num.stranded)),by='penetration']

pain <- data.table(subset(logs[['pain']],pain.type%in%c("delay","stranded")))
pain[,':='(driver.input.file=NULL,charger.input.file=NULL,itin.scenario=NULL,itin.scenario.named=NULL,itin.scenario.order=NULL)]
pain <- pain[!is.na(infrastructure.scenario.named)]
pain <- pain[pp('Chargers for ',penetration,'%') == infrastructure.scenario.named]
setkey(pain,penetration,replicate,pain.type)
pain.by.type <- pain[,list(pain.value=sum(pain.value,na.rm=T),num=length(pain.value)),by=c('penetration','replicate','pain.type')]
pain.by.type <- pain.by.type[,list(pain.value=mean(pain.value,na.rm=T),num=mean(num)),by=c('penetration','pain.type')]

#load(paste(path.to.inputs,'logs-veh-scens.Rdata',sep=''))
results.veh <- data.table(logs[['results']])
results.veh[,':='(driver.input.file=NULL,vehicle.type.input.file=NULL,itin.scenario=NULL,itin.scenario.named=NULL,itin.scenario.order=NULL)]
setkey(results.veh,penetration,vehicle.scenario.named)
mean.results.veh <- results.veh[,list(num.drivers=mean(num.drivers),objective=mean(objective),total.delay=mean(total.delay),total.delay.cost=mean(total.delay.cost),num.stranded=mean(num.stranded)),by=c('penetration','vehicle.scenario.named')]
mean.results.veh[vehicle.scenario.named=="All High"]

#########################################
# Home charger availability 50% vs 100%
# ANDY - START RUNNING HERE TO ANALYZE RESULTS
#########################################
scen.names <- c('half-homeless'='50% Home Charging','no-homeless'='100% Home Charging')
charging <- data.table(logs[['charging']])
charging[,':='(time=time-6)]
trips <- data.table(logs[['trip']])
trips[,':='(time=time-6,end.time=end.time-6)]
pain <- data.table(logs[['pain']])
pain[,':='(time=time-6)]
charging[,itin.scenario.named:=scen.names[itin.scenario]]

driver.schedules <- data.table(read.csv(pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/no-homeless/driver-schedule-pen5-rep1-20150212.txt'),sep='\t'))
driver.schedules[,':='(driver=X.X.driver,X.X.driver=NULL)]
setkey(driver.schedules,driver,depart)
driver.home <- driver.schedules[,list(home=home[1]),by=driver]
driver.home[,itin.scenario:='no-homeless']
driver.schedules.half <- data.table(read.csv(pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/half-homeless/driver-schedule-pen5-rep1-20150212.txt'),sep='\t'))
driver.schedules.half[,':='(driver=X.X.driver,X.X.driver=NULL)]
setkey(driver.schedules.half,driver,depart)
driver.home.half <- driver.schedules.half[,list(home=home[1]),by=driver]
driver.home.half[,itin.scenario:='half-homeless']
driver.home <- rbindlist(list(driver.home,driver.home.half),use.names=T,fill=T)

setkey(trips,driver,vehicle.type,itin.scenario)
journey.dists <- trips[,list(dist=sum(distance)),by=c('driver','itin.scenario','vehicle.type')]
#ggplot(journey.dists,aes(x=dist))+geom_histogram()+facet_wrap(itin.scenario~vehicle.type)+labs(x="Distance (km)",y="Count",title=pp("Travel Distances from PEVI output (n=",nrow(journey.dists),")"))+scale_x_continuous(limits=c(0,100))
sum(journey.dists$dist>41.3)/nrow(journey.dists) # 8.5% of journey dists are greater than range of two-wheeler

setkey(driver.home,driver,itin.scenario)
setkey(trips,driver,itin.scenario)
trips <- driver.home[trips]
setkey(charging,driver,itin.scenario)
charging <- driver.home[charging]
charging[,at.home:=location==home]
charging[,list(frac.at.home=sum(at.home,na.rm=T)/length(home)),by='itin.scenario']
#ggplot(charging,aes(x=time,y=energy,colour=factor(charger.level)))+geom_point()+facet_wrap(at.home~itin.scenario)
setkey(charging,charger.level,itin.scenario)
charging[,length(u(charger.id)),by=c('itin.scenario','charger.level')]

ggplot(charging[charger.level<=1],aes(x=time,fill=factor(charger.level)))+geom_histogram(binwidth=1,position='dodge')+facet_wrap(itin.scenario.named~infrastructure.scenario.named)+labs(x="Hour",y="Count",title="Charging Events by Time of Day and Level",fill='Charger Level')

setkey(pain,itin.scenario.named,infrastructure.scenario,pain.type)
pain[time>24*2+6 & time<24*3+6,list(length(time)),by=c('pain.type','infrastructure.scenario','itin.scenario.named')]

# ANDY - END RUNNING HERE TO ANALYZE RESULTS

#charging[,length(charger.id),by=c('itin.scenario','charger.level')]
#ggplot(charging[charger.level<=1],aes(x=time,fill=factor(charger.level)))+geom_histogram(binwidth=1,position='dodge')+facet_wrap(itin.scenario.named~charger.infrastructure.scenario.named)+labs(x="Hour",y="Count",title="Charging Events by Time of Day and Level",fill='Charger Level')+geom_hline(yintercept=c(10200),color='grey')
#charging[time<6,length(charger.id),by=c('itin.scenario','charger.level')]

#charging[,hour:=floor(time)]
#setkey(charging,charger.level,itin.scenario,hour)
#charging[,length(u(driver)),by=c('itin.scenario','charger.level','hour')]

#charging[time<=9.5 & time+duration>=9.5,length(time),by=c('itin.scenario','charger.level')]

#vehs <- data.table(read.csv(pp(pevi.shared,'/data/inputs/vehicle-type-input-file/vehicle-types-scen-delhi-smart.txt'),sep='\t'))
#vehs[,':='(name=X.name,X.name=NULL)]
#batt.caps <- array(vehs$battery.capacity,dimnames=list(vehs$name))

#charging[,battery.cap:=batt.caps[vehicle.type]]
#setkey(charging,itin.scenario,driver)
#socs <- charging[,list(begin.soc=begin.soc[1],end.soc=tail(end.soc,1),type=vehicle.type[1]),by=c('itin.scenario','driver')]
#socs[,battery.cap:=batt.caps[type]]
#socs[,net.energy.diff:=(end.soc - begin.soc)*battery.cap]
#socs[,list(n=length(u(driver)),avg=mean(net.energy.diff),sum=sum(net.energy.diff)),by=itin.scenario]
# total net increase in charge for drivers that *do* charge is 1026 MWh


############################
# Charger utilization for pen 1%
############################
ggplot(ddply(subset(logs[['charging']],penetration==1 & charger.level>0),.(charger.id),nrow),aes(x=V1/2))+geom_histogram(binwidth=1)+labs(x="# of Drivers Served by Individual Chargers per Day")

num.chargers <- list()
for(charger.file in unique(as.character(logs[['charging']]$charger.input.file))){
  scen.named <- logs[['charging']]$infrastructure.scenario.named[which(logs[['charging']]$charger.input.file == charger.file)[1]]
  num.chargers[[scen.named]] <- read.table(str_replace(charger.file,"sheppardc","critter"),skip=1)
  names(num.chargers[[scen.named]]) <- c('taz','L0','L1','L2','L3','L4')
}
charge.sum <- na.omit(ddply(logs[['charging']],.(charger.level,penetration),function(df){
  num.reps <- length(unique(df$replicate))
  if(df$charger.level[1]==0){
    num.chs <- length(unique(subset(logs[['trip']],penetration==df$penetration[1])$driver))/2
  }else{
    num.chs <- sum(num.chargers[[df$infrastructure.scenario.named[1]]][,pp('L',df$charger.level[1])])
  }
  duty.factor <- sum(df$duration)/(48*num.reps*num.chs)
  data.frame(duty.factor=ifelse(duty.factor==Inf,NA,duty.factor))
}))
p <- ggplot(df,aes(x=factor(charger.level),fill=factor(penetration),y=duty.factor))+geom_bar(stat='identity',position="dodge")+facet_wrap(~taz)+labs(title=df$infrastructure.scenario.named[1],x="Charger Level",y="Duty Factor")

ggplot(ddply(ddply(logs[['trip']],.(vehicle.type,replicate),function(df){ sum(df$distance)/length(unique(df$driver)) }),.(vehicle.type),function(df){ data.frame(miles.per.day=mean(df$V1))}),
  aes(x=1,y=miles.per.day))+geom_bar(stat="identity")+ xlab('') + labs(fill='Level') + facet_wrap(~vehicle.type)


# ANALYZE PAIN
ggplot(subset(logs[['pain']],replicate==1 & pain.type%in%c("stranded","delay")),aes(x=time,y=state.of.charge,colour=pain.type,shape=location>0))+geom_point()+facet_grid(~vehicle.scenario.named)

ggplot(logs[['pain']],aes(x=time,y=state.of.charge,colour=pain.type,shape=vehicle.type))+geom_point()+facet_grid(charge.safety.factor~replicate)
ggplot(subset(logs[['pain']],penetration==0.5 & replicate==1 & pain.type=='delay'),aes(x=time,y=state.of.charge,shape=vehicle.type,colour=pain.value))+geom_point()+facet_wrap(~pain.type)
ggplot(subset(logs[['pain']],pain.type=='stranded'),aes(x=time,y=state.of.charge,shape=vehicle.type))+geom_point()+facet_wrap(penetration~replicate)

ggplot(subset(logs[['pain']],pain.type=="delay"),aes(x=time,y=state.of.charge,colour=pain.type,shape=location))+geom_point()+facet_grid(charge.safety.factor~replicate)
ggplot(subset(logs[['pain']],pain.type=="delay"),aes(x=time,y=state.of.charge,colour=pain.type,label=location))+geom_text()+facet_grid(vehicle.scenario.name~replicate)

ggplot(subset(logs[['pain']],pain.type=="delay" & num.simulation.days==2),aes(x=time,y=state.of.charge,colour=vehicle.type,shape=vehicle.type))+geom_point()+facet_grid(itin.scenario.named~replicate)
ggplot(subset(logs[['pain']],pain.type%in%c("delay","stranded") & num.simulation.days==2),aes(x=time,y=state.of.charge,colour=pain.type,shape=vehicle.type))+geom_point()+facet_grid(itin.scenario.named~replicate)

# why do drivers need public chargers?

df <- subset(logs[['pain']],pain.type%in%c("delay","stranded") & itin.scenario=="doubled" & num.simulation.days==2 & replicate==1)
df$pain.value[df$pain.type=="stranded"] <- 5
ggplot(df,aes(x=time,y=location,colour=pain.type,shape=vehicle.type,size=pain.value))+geom_point()+facet_grid(itin.scenario.named~replicate)
df <- subset(df,pain.type=='stranded')
ggplot(df,aes(x=vehicle.type)) + geom_bar() +  labs(x="",y="",title="")
d <- read.table('/Users/sheppardc/Dropbox/serc/pev-colin/pev-shared//data/inputs/driver-input-file/delhi-combined/doubled/driver-schedule-pen0.5-rep1-20140217.txt',header=T)
df$home <- d$home[match(df$driver,d$X.driver)]
load(pp(pevi.shared,'data/DELHI/road-network/routing-with-gateways.Rdata'))
d <- data.table(d,key=c('from','to'))
setkey(time.distance,'from','to')
d <- time.distance[d]
d[,driver:=X.driver]
setkey(d,'driver')
dd <- d[,list(daily.km=sum(km)),by="driver"]
df <- data.table(df,key='driver')
df <- dd[df]

# sum it all up
my.red <- '#e41a1c'
my.blue <- '#377eb8'
ggplot(dd,aes(x=daily.km)) + geom_histogram(binwidth=10) +  labs(x="Daily KMT",y="Frequency") + geom_histogram(binwidth=10,alpha=.5, fill=my.red,data=df) + ggtitle(expression(atop("Daily KMT for All Drivers (black) and Stranded Drivers (red)", atop("(at 0.5% Penetration, 100% Home Chargers and No Public Infrastructure)"))))

# comparing consistent vs quadrupled itins before after 2.0 to 2.1 fixes

all.logs <- list()
for(log in c('pain','charging','results')){
  all.logs[[log]] <- data.frame()
}
load(pp(pevi.shared,"data/inputs/compare/delhi-consistent-vs-quadrupled/logs-v2.0.Rdata")) # base version
for(log in c('pain','charging','results')){
  logs[[log]]$pevi <- '2.0 Base'
  logs[[log]]$num.simulation.days <- 2
  all.logs[[log]] <- rbind(all.logs[[log]],logs[[log]])
}
load(pp(pevi.shared,"data/inputs/compare/delhi-consistent-vs-quadrupled/logs-v2.1.Rdata")) # after morning charging and external homelessness fixes
for(log in c('pain','charging','results')){
  logs[[log]]$pevi <- '2.1 Fixes'
  logs[[log]]$num.simulation.days <- 2
  all.logs[[log]] <- rbind(all.logs[[log]],logs[[log]])
}
load(pp(pevi.shared,"data/inputs/compare/delhi-consistent-vs-quadrupled/logs-v2.1.1.Rdata")) # after morning charging and external homelessness fixes
for(log in c('pain','charging','results')){
  logs[[log]]$pevi <- '2.1.1 Fixes'
  all.logs[[log]] <- rbind(all.logs[[log]],logs[[log]])
}
load(pp(pevi.shared,"data/inputs/compare/delhi-consistent-vs-quadrupled/logs-v2.1.2.Rdata")) # after external time/distance and destination charging fixes
for(log in c('pain','charging','results')){
  logs[[log]]$pevi <- '2.1.2 Fixes'
  all.logs[[log]] <- rbind(all.logs[[log]],logs[[log]])
}
ggplot(subset(all.logs[['pain']],pain.type%in%c("delay","stranded") & penetration==1 & replicate==1),aes(x=time,y=state.of.charge,colour=pain.type,shape=vehicle.type))+geom_point()+facet_grid(pevi ~ itin.scenario.named)
ggplot(subset(all.logs[['pain']],pain.type%in%c("delay","stranded") & pevi=='2.1 Fixes' & penetration==2 & itin.scenario=='quadrupled'),aes(x=time,y=state.of.charge,colour=vehicle.type,shape=vehicle.type))+geom_point()+facet_wrap(~ pain.type)
ggplot(subset(all.logs[['pain']],pain.type%in%c("delay","stranded") & pevi=='2.1.1 Fixes' & penetration==1 & itin.scenario=='quadrupled'),aes(x=time,y=state.of.charge,colour=vehicle.type,shape=vehicle.type))+geom_point()+facet_wrap(~ pain.type)
dev.new()
ggplot(subset(all.logs[['charging']],penetration==2),aes(x=time,y=begin.soc,colour=factor(charger.level)))+geom_point()+facet_grid(pevi ~ itin.scenario.named)
ggplot(subset(all.logs[['charging']],pevi=='2.1 Fixes' & penetration==2 & itin.scenario=='quadrupled'),aes(x=time,y=begin.soc,colour=))+geom_point()+facet_wrap(~charger.level)

# bulk # of delays and strandings
ddply(subset(logs[['pain']],pain.type%in%c("delay","stranded") & num.simulation.days==2),.(pain.type,itin.scenario),function(df){ data.frame(num.pain=nrow(df),sum.pain=sum(df$pain.value))})
ddply(subset(logs[['results']],T),.(itin.scenario),function(df){ data.frame(delay.cost=mean(df$total.delay.cost),delay=mean(df$total.delay))})
# now plot it 
logs[['pain']]$vehicle.scenario.named <- factor(logs[['pain']]$vehicle.scenario.named,c("All Low","Low/Med/High (33/33/33)","All High"))
ggplot(ddply(subset(logs[['pain']],pain.type%in%c("delay","stranded")),.(pain.type,vehicle.scenario.named),nrow),aes(x=vehicle.scenario.named,y=V1,fill=pain.type))+geom_bar(stat='identity',position='dodge')+labs(title="Number of Delay and Stranding Events Over 5 Replicates and a Constant Infrastructure",x="Scenario",y="Number of Events",fill="Event Type")


ddply(subset(logs[['results']],num.simulation.days==2),.(itin.scenario),function(df){ data.frame(delay.cost=mean(df$total.delay.cost/1e6),delay.per.driver=mean(df$total.delay/df$num.drivers),frac.drivers.delayed=mean(df$frac.drivers.delayed),energy.charged=mean(df$energy.charged),electric.miles.driven=mean(df$electric.miles.driven)) })

# how many delays are by the same driver
ddply(subset(logs[['pain']],pain.type=="delay"),.(homeless,replicate),function(df){ data.frame(tot.delay=sum(df$pain.value),delay.wo.repeats=sum(df$pain.value[!duplicated(df$driver)]),frac.uniq=length(unique(df$driver))/nrow(df),n.uniq=length(unique(df$driver))) })
ddply(subset(logs[['pain']],pain.type=="stranded"),.(homeless,replicate),function(df){ data.frame(n=nrow(df)) })

# CHARGING
ggplot(subset(logs[['charging']],charger.level>0),aes(x=time,y=begin.soc,colour=factor(charger.level)))+geom_point()+facet_grid(penetration~replicate)
ggplot(subset(logs[['charging']],charger.level>0),aes(x=time,y=begin.soc,colour=factor(charger.level)))+geom_point()+facet_grid(charge.safety.factor~replicate)

ggplot(subset(logs[['charging']],charger.level>0),aes(x=time,y=begin.soc,colour=factor(charger.level)))+geom_point()+facet_grid(itin.scenario.named~num.simulation.days)
logs[['charging']]$is.external <- F
logs[['charging']]$is.external[logs[['charging']]$location<0] <- T
ggplot(subset(logs[['charging']],charger.level>0 & num.simulation.days==2),aes(x=time,y=begin.soc,colour=is.external))+geom_point()+facet_wrap(~itin.scenario.named)

#  show charging spatially
source(pp(pevi.home,'R/gis-functions.R'))
load.libraries(c('maptools','colorRamps'))
load(pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))
load(pp(pevi.shared,'data/UPSTATE/od.converter.Rdata'))
agg.taz@data$nl.id <- od.converter$new.id[match(agg.taz$agg.id,od.converter$old.id)]
charging.summary <- ddply(subset(logs[['charging']],charger.level>0 & penetration==2 & replicate==1),.(location,charger.level),nrow)
charging.summary <- cast(melt(charging.summary,id.vars=1:2),location~charger.level)
charging.summary[is.na(charging.summary)] <- 0
for(level in 1:4){
  agg.taz@data[,pp('L',level,'.charging.events')] <- charging.summary[match(agg.taz$nl.id,charging.summary$location),as.character(level)]
  agg.taz@data[is.na(agg.taz@data[,pp('L',level,'.charging.events')]),pp('L',level,'.charging.events')] <- 0
}
agg.taz$total.charging.events <- agg.taz$L1.charging.events + agg.taz$L2.charging.events + agg.taz$L3.charging.events + agg.taz$L4.charging.events
cmap <- map.color(agg.taz$total.charging.events,blue2red(50))
shp.to.kml(agg.taz,pp(path.to.inputs,'charging-events.kml'),kmlname="# Charging Events by Charger Type", kmldescription="",borders='white',lwds=1.5,colors=cmap,id.col='shp.id',name.col='name',description.cols=c('agg.id','nl.id',pp('L',1:4,'.charging.events'),'total.charging.events'))




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

# Duty factor by TAZ

existing.chargers <- data.frame(id=c(6,23,27,45),name=c("ARC_Plaza","EKA_Waterfront","EKA_NW101","Redway"))

logs[['charging']]$infrastructure.scenario <- NA
for(scen.i in names(naming$`charger-input-file`)){
  logs[['charging']]$infrastructure.scenario[grep(scen.i,as.character(logs[['charging']]$charger.input.file))] <- scen.i
  logs[['charging']]$infrastructure.scenario.named[logs[['charging']]$infrastructure.scenario == scen.i] <- naming$`charger-input-file`[[scen.i]][[1]]
  logs[['charging']]$infrastructure.scenario.order[logs[['charging']]$infrastructure.scenario == scen.i] <- as.numeric(naming$`charger-input-file`[[scen.i]][[2]])
}
logs[['charging']]$penetration <- as.numeric(unlist(lapply(strsplit(as.character(logs[['charging']]$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
logs[['charging']]$replicate <- as.numeric(unlist(lapply(strsplit(as.character(logs[['charging']]$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))

num.chargers <- list()
for(charger.file in unique(as.character(logs[['charging']]$charger.input.file))){
  scen.named <- logs[['charging']]$infrastructure.scenario.named[which(logs[['charging']]$charger.input.file == charger.file)[1]]
  num.chargers[[scen.named]] <- read.table(charger.file,skip=1)
  names(num.chargers[[scen.named]]) <- c('taz','L0','L1','L2','L3','L4')
}

charge.sum <- na.omit(ddply(logs[['charging']],.(location,charger.level,penetration,infrastructure.scenario.named),function(df){
  num.reps <- length(unique(df$replicate))
  duty.factor <- sum(df$duration)/(24*num.reps*num.chargers[[df$infrastructure.scenario.named[1]]][df$location[1],pp('L',df$charger.level[1])])
  data.frame(duty.factor=ifelse(duty.factor==Inf,NA,duty.factor))
}))

taz.names <- read.csv(pp(pevi.shared,'data/google-earth/ordering-for-TAZ-table.csv'))
charge.sum$taz <- taz.names$name[match(charge.sum$location,taz.names$id)]

df <- subset(charge.sum,charger.level>0 & ! location %in% existing.chargers$id)
p <- ggplot(df,aes(x=factor(charger.level),fill=factor(penetration),y=duty.factor))+geom_bar(stat='identity',position="dodge")+facet_wrap(~taz)+labs(title=df$infrastructure.scenario.named[1],x="Charger Level",y="Duty Factor")

p <- ggplot(df,aes(x=penetration,color=factor(charger.level),y=duty.factor))+geom_point()+geom_line()+facet_wrap(~taz)+labs(title=df$infrastructure.scenario.named[1],x="Penetration",y="Duty Factor",colour="L1/L2")
ggsave(pp(pevi.shared,'maps-results/duty-factors-CEC.pdf'),p,width=11,height=8.5)

pdf(pp(pevi.shared,'maps-results/duty-factors-noL1.pdf'),width=11,height=8.5)
d_ply(subset(charge.sum,charger.level>0),.(infrastructure.scenario.named),function(df){
  p <- ggplot(df,aes(x=factor(charger.level),fill=factor(penetration),y=duty.factor))+geom_bar(stat='identity',position="dodge")+facet_wrap(~taz)+labs(title=df$infrastructure.scenario.named[1],x="Charger Level",y="Duty Factor")
  print(p)
})
dev.off()

fish <- read.csv(pp(pevi.shared,'data/chargers/fisherman-terminal-usage-2012-2013/2013-12-04 FTB EVSE Energy-vs-Time.csv'),stringsAsFactors=F)
names(fish) <- c('date','kwh','cum')
fish$date <- to.posix(fish$date,'%m/%d/%Y')
fish$dow <- as.numeric(strftime(fish$date,'%w'))
fish$is.weekday <- fish$dow>0 & fish$dow<6
fish$dow <- factor(strftime(fish$date,'%a'))
fish$dow <- factor(strftime(fish$date,'%a'),levels=levels(fish$dow)[c(2,6,7,5,1,3,4)])
fish$month <- factor(as.numeric(strftime(fish$date,'%m')))

ggplot(fish,aes(x=date,y=kwh)) + geom_point() + labs(x="Date",y="Energy Dispensed (kWh)",title="Fisherman's Terminal EVSE Utilization")
ggplot(subset(fish,kwh>0),aes(x=factor(dow),y=kwh,colour=is.weekday)) + geom_point() + geom_boxplot() + labs(x="Week Day",y="Energy Dispensed (kWh)",title="Fisherman's Terminal EVSE Utilization")
ggplot(ddply(subset(fish,kwh>0),.(dow),function(df){ data.frame(kwh=sum(df$kwh)) }),aes(x=dow,y=kwh)) + geom_bar(stat='identity') + labs(x="Week Day",y="Energy Dispensed (kWh)",title="Fisherman's Terminal EVSE Utilization")

daily.energy <- sum(subset(fish,date>to.posix('2013-02-01'))$kwh)/as.numeric(difftime(max(subset(fish,date>to.posix('2013-02-01'))$date),min(subset(fish,date>to.posix('2013-02-01'))$date),units='days'))

# what fraction of the kWh were delivered to EVs charging at 3.3kW vs 6.6kW
#(sum(fish$kwh)/316.5- 6.6)/-3.3

# so if there's an average of 3.5 kWh per day delivered and we assume drivers will charge at 6.6kW, that's a 2.2% duty factor for ~100 drivers
#0.02209596

# PEVI predicts ~40% duty factor at the waterfront with 0.5% pen or 750 driver
subset(charge.sum,taz=='EKA_Waterfront')

