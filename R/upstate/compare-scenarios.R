amndSys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape','stringr'))

#exp.name <- commandArgs(trailingOnly=T)[1]
exp.name <- 'upstate-ghg'
path.to.inputs <- pp(pevi.shared,'data/inputs/compare/',exp.name,'/')

#to.log <- c('pain','charging','need-to-charge')
#to.log <- c('pain','charging','tazs','trip')
#to.log <- c('pain','charging')
#to.log <- c('tazs','charging')
to.log <- c('tazs')

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
  for(scen.i in as.numeric(names(naming$`vehicle-type-input-file`))){
    results$vehicle.scenario.named[results$vehicle.scenario == scen.i] <- naming$`vehicle-type-input-file`[[as.character(scen.i)]][[1]]
    results$vehicle.scenario.order[results$vehicle.scenario == scen.i] <- as.numeric(naming$`vehicle-type-input-file`[[as.character(scen.i)]][[2]])
  }
}

# don't run unnecessary scenarios, only for charing demand experiment
if(exp.name %in% c('upstate-ghg')){
  results <- subset(results,(!infrastructure.scenario %in% pp('upstate-final-rec-pen-',c(0.5,1,2))) | 
                            (penetration==0.5 & infrastructure.scenario=='upstate-final-rec-pen-0.5') | 
                            (penetration==1 & infrastructure.scenario=='upstate-final-rec-pen-1') | 
                            (penetration==2 & infrastructure.scenario=='upstate-final-rec-pen-2'))
}

# start NL
tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
#model.path <- paste(pevi.home,"netlogo/PEVI.nlogo",sep='')
model.path <- paste(pevi.home,"netlogo/PEVI-v2.1.1.nlogo",sep='')
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
  NLCommand('set go-until-time 100')
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
save(logs,file=paste(path.to.inputs,'logs-tazonly.Rdata',sep=''))
#load(paste(path.to.inputs,'logs.Rdata',sep=''))

#######################################
# POST PROCESSING
#######################################
# create pen/rep columns, this assumes the first column is the driver input file
for(log.file in to.log){
  names(logs[[log.file]]) <- c('driver.input.file',names(logs[[log.file]])[2:ncol(logs[[log.file]])])
  logs[[log.file]]$penetration <- as.numeric(unlist(lapply(strsplit(as.character(logs[[log.file]]$driver.input.file),'-pen',fixed=T),function(x){ unlist(strsplit(x[2],"-rep",fixed=T)[[1]][1]) })))
  logs[[log.file]]$replicate <- as.numeric(unlist(lapply(strsplit(as.character(logs[[log.file]]$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))
  if('charger.input.file' %in% names(logs[[log.file]])){
    logs[[log.file]]$infrastructure.scenario <- NA
    for(scen.i in names(naming$`charger-input-file`)){
      logs[[log.file]]$infrastructure.scenario[grep(scen.i,as.character(logs[[log.file]]$charger.input.file))] <- scen.i
      logs[[log.file]]$infrastructure.scenario.named[logs[[log.file]]$infrastructure.scenario == scen.i] <- naming$`charger-input-file`[[scen.i]][[1]]
      logs[[log.file]]$infrastructure.scenario.order[logs[[log.file]]$infrastructure.scenario == scen.i] <- as.numeric(naming$`charger-input-file`[[scen.i]][[2]])
    }
  }
}
save(logs,file=paste(path.to.inputs,'logs-tazonly.Rdata',sep=''))
#load(paste(path.to.inputs,'logs.Rdata',sep=''))

#######################################
# ANALYSIS
#######################################
# ANALYZE PAIN
ggplot(logs[['pain']],aes(x=time,y=state.of.charge,colour=pain.type,shape=vehicle.type))+geom_point()+facet_grid(charge.safety.factor~replicate)
ggplot(subset(logs[['pain']],penetration==0.5 & replicate==1 & pain.type=='delay'),aes(x=time,y=state.of.charge,shape=vehicle.type,colour=pain.value))+geom_point()+facet_wrap(~pain.type)
ggplot(subset(logs[['pain']],pain.type=='stranded'),aes(x=time,y=state.of.charge,shape=vehicle.type))+geom_point()+facet_wrap(penetration~replicate)

ggplot(subset(logs[['pain']],pain.type=="delay"),aes(x=time,y=state.of.charge,colour=pain.type,shape=location))+geom_point()+facet_grid(charge.safety.factor~replicate)
ggplot(subset(logs[['pain']],pain.type=="delay"),aes(x=time,y=state.of.charge,colour=pain.type,label=location))+geom_text()+facet_grid(charge.safety.factor~replicate)
ggplot(subset(logs[['pain']],pain.type=="delay"),aes(x=time,y=state.of.charge,colour=vehicle.type,shape=vehicle.type,label=location))+geom_point()+geom_text(aes(y=state.of.charge+.03))+facet_grid(charge.safety.factor~replicate)

# CHARGING
ggplot(subset(logs[['charging']],charger.level>0),aes(x=time,y=begin.soc,colour=factor(charger.level)))+geom_point()+facet_grid(penetration~replicate)
ggplot(subset(logs[['charging']],charger.level>0),aes(x=time,y=begin.soc,colour=factor(charger.level)))+geom_point()+facet_grid(charge.safety.factor~replicate)


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
caps <- c(6.6,2.4,6.6,50)
demand <- ddply(logs[['tazs']],.(replicate,time,penetration),function(df){ colSums(df[,paste('num.L',0:3,sep='')] - df[,paste('num.avail.L',0:3,sep='')]) * caps })
load(pp(pevi.shared,'data/inputs/compare/upstate-ghg/logs-tazonly.Rdata'))
demand <- data.frame(logs[['tazs']][,c('replicate','time','penetration','taz')],t(apply(logs[['tazs']][,paste('num.L',0:3,sep='')] - logs[['tazs']][,paste('num.avail.L',0:3,sep='')],1,function(x){ x * caps })))
names(demand) <- c('replicate','time','taz','pow.L0','pow.L1','pow.L2','pow.L3')
save(demand,file=pp(pevi.shared,'data/inputs/compare/upstate-ghg/demand.Rdata')

save(worst.peaks,file=pp(pevi.shared,'data/inputs/compare/upstate-ghg/peak-demand-from-humboldt.Rdata'))
worst.peaks[,hour:=strftime(peak.datetime,"%H")]
setkey(worst.peaks,'file')
# categorize the circuits as 1 = Urban, 2 = Semi-Urban (small towns), 3 = Rural
worst.peaks[,region.type:=c(1,1,1,1,1,3,3,2,1,1,1,1,1,2,2,3,3,2,2,1,3,2,2,2,3,3,3,3,3,3,2,1,2,2)]

#(Load the demand data if you are starting from a load file)
load(pp(pevi.shared,'data/inputs/compare/upstate-ghg/demand data/demand.Rdata'))
# remove data that is not needed
demand <- subset(demand,taz > 0 & ((infrastructure.scenario =='upstate-final-rec-pen-0.5' & penetration==0.5) |(infrastructure.scenario =='upstate-final-rec-pen-1' & penetration==1.0) | (infrastructure.scenario =='upstate-final-rec-pen-2' & penetration==2.0) | (infrastructure.scenario =='upstate-existing-chargers')))
demand <- data.table(demand)
demand[,time:=as.numeric(time)]
demand[,time:=round(time*4)/4] # round to nearest quarter hour
# convert num chargers to demand
demand[,':='(L0=num.L0-num.avail.L0,L2=num.L2-num.avail.L2,L3=num.L3-num.avail.L3,num.L0=NULL,num.L1=NULL,num.L2=NULL,num.L3=NULL,num.avail.L0=NULL,num.avail.L1=NULL,num.avail.L2=NULL,num.avail.L3=NULL)]
demand[,':='(residential=L0*6.6,public=L2*6.6+L3*50,public.2=L2*6.6,public.3=L3*50)]
# deal with time being off by 6 hours
demand[,time:=time-6]
demand <- demand[time >= 0]
demand[,penetration:=ifelse(penetration==0.5,"0.5% Fleet Penetration",ifelse(penetration==1,"1% Fleet Penetration","2% Fleet Penetration"))]

# extract the max demand from all data in each quarter hour time slot
setkey(demand,infrastructure.scenario.named,penetration,taz,time)
demand.sum <- demand[,list(peak.L0=max(pow.L0),peak.L2=max(pow.L2),peak.L3=max(pow.L3),Residential=max(residential),Public=max(public),peak.public.2=max(public.2),peak.public.3=max(public.3)),by=c('infrastructure.scenario.named','penetration','taz','time')]
demand.sum[,':='(Total=peak.L0+peak.L2+peak.L3,charger.status=ifelse(infrastructure.scenario.named=='Existing Chargers','Existing Chargers','New Chargers'))]

# naming stuff
load(pp(pevi.shared,'data/UPSTATE/od.converter.Rdata'))
demand.sum[,name:=od.converter$name[match(taz,od.converter$new.id)]]
demand.sum[,':='(Time=time,time=NULL,Infrastructure.Scenario.Named=infrastructure.scenario.named,infrastructure.scenario.named=NULL,Penetration=penetration,penetration=NULL,Charger.Status=factor(charger.status),charger.status=NULL)]

setkey(demand.sum,taz)

for(taz.i in unique(demand.sum$taz)){
  taz.name <- demand.sum[J(taz.i)]$name[1]

	pdf(pp(pevi.shared,'data/UPSTATE/results/Charging Demand/Peak-Demand-TAZ-',taz.i,'-',taz.name,'.pdf'),width=8,height=5)
	print(ggplot(melt(demand.sum[J(taz.i)][Charger.Status=="New Chargers"],id.vars=c('Time','Penetration','Infrastructure.Scenario.Named','Charger.Status'),measure.vars=c('Residential','Public','Total'),variable_name='Charger.Type'),aes(x=Time,y=value,group=Charger.Type,colour=Charger.Type))+geom_line()+facet_grid(Charger.Status~Penetration)+scale_x_continuous(limits=c(6,30),breaks=c(0,6,12,18,24,30))+ylab("Peak Charging Demand (kW)")+xlab("Simulation Time (hours)")+labs(title = taz.name))
	dev.off()
}

# categorize as urban rural semi.urban
demand.sum[,Zone.Type:=od.converter$type[match(taz,od.converter$new.id)]]
levels(demand.sum$Zone.Type)<-c('Rural','Semiurban','Urban')
setkey(demand.sum,Infrastructure.Scenario.Named,Penetration,Zone.Type,Time)

demand.sum.sum <- demand.sum[Charger.Status=="New Chargers",list(p0=min(Total),p25=quantile(Total,.25),p50=median(Total),p75=quantile(Total,.75),p100=max(Total),Charger.Status=Charger.Status[1]),by=c('Infrastructure.Scenario.Named','Penetration','Zone.Type','Time')]

ggplot(melt(demand.sum.sum,id.vars=c('Time','Penetration','Infrastructure.Scenario.Named','Zone.Type'),measure.vars=c('p0','p25','p50','p75','p100'),variable_name='Percentile'),aes(x=Time,y=value,group=Percentile,colour=Percentile))+geom_line()+facet_wrap(Zone.Type~Penetration)+scale_x_continuous(limits=c(6,30),breaks=c(0,6,12,18,24,30))+ylab("Peak Charging Demand (kW)")+xlab("Simulation Time (hours)")

# Start the wrapper to pull out individual TAZ data
for(taz.i in unique(demand$taz)){

	taz.demand <- subset(demand,taz==taz.i)

	demand.sum <- ddply(ddply(taz.demand,.(time,replicate,infrastructure.scenario.named,penetration),function(df){ colSums(df[,c('pow.L0','pow.L1','pow.L2','pow.L3')]) }),
                    .(time,infrastructure.scenario.named,penetration),function(df){ rbind( data.frame(level=0,min=min(df$pow.L0),max=max(df$pow.L0),median=median(df$pow.L0),mean=mean(df$pow.L0)),
                                                        data.frame(level=1,min=min(df$pow.L1),max=max(df$pow.L1),median=median(df$pow.L1),mean=mean(df$pow.L1)),
                                                        data.frame(level=2,min=min(df$pow.L2),max=max(df$pow.L2),median=median(df$pow.L2),mean=mean(df$pow.L2)),
                                                        data.frame(level=3,min=min(df$pow.L3),max=max(df$pow.L3),median=median(df$pow.L3),mean=mean(df$pow.L3))) })
	#ggplot(melt(subset(demand.sum,level==0),id.vars=c('time','level','penetration','infrastructure.scenario.named')),aes(x=as.numeric(time),y=value,group=variable,colour=variable))+geom_line()+facet_wrap(penetration~infrastructure.scenario.named)+scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
	#ggplot(melt(subset(demand.sum,level>0),id.vars=c('time','level')),aes(x=as.numeric(time),y=value,group=variable,colour=variable))+geom_line()+facet_wrap(~level)+scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
	
	# Time. Gotta be numeric, and we also need to correct for the 6 hour difference by subtracting 6.
	demand.sum$time <- as.numeric(demand.sum$time)-6

	# Make sure that we remove unmatching data; i.e. 1% penetration with 2% buildout
	demand.sum <- subset(demand.sum,str_detect(infrastructure.scenario.named,as.character(penetration))|infrastructure.scenario.named=='Existing Chargers')

	# Add a variable for exisiting vs. planned chargers that we can facet on
	demand.sum$charger.status <- "Existing Chargers"

	# Separate levels into "residential" (level 0) and "public" (everything else). I know there is a quicker way to do this.

	for(i in 1:nrow(demand.sum)){
		if(demand.sum$infrastructure.scenario.named[i]!='Existing Chargers') {demand.sum$charger.status[i]<-"New Chargers"}
		if(demand.sum$level[i]==3){
			new.row <- nrow(demand.sum)+1
			demand.sum[new.row,] <- demand.sum[i,]
			demand.sum$max[new.row] <- sum(demand.sum$max[(i-3):i])
			demand.sum$level[new.row] <- 'total'
		}
		ifelse(demand.sum$level[i]==0,demand.sum$level[i]<-'residential',demand.sum$level[i]<-'public')
	}

	# Factor charger.status
	demand.sum$charger.status <- as.factor(demand.sum$charger.status)

	# Sum the public charging. Yes, I know I need to be better about using data.table
	demand.sum <- ddply(demand.sum,.(time,infrastructure.scenario.named,penetration,level,charger.status),function(df){
		data.frame(max=sum(df$max))})
	
	# Do some renaming
	demand.sum$penetration <- as.factor(pp(demand.sum$penetration,'% Fleet Penetration'))
	names(demand.sum) <- c('Time','Infrastructure.Scenario.Named','Penetration','Charger.Type','Charger.Status','Max')

	# Plot it and save it.
	pdf(pp(pevi.shared,'data/UPSTATE/results/Charging Demand/TAZ-',taz.i,'_demand.pdf'),width=11,height=8.5)
	print(ggplot(subset(melt(demand.sum,id.vars=c('Time','Charger.Type','Penetration','Infrastructure.Scenario.Named','Charger.Status')),variable=='Max'),aes(x=Time,y=value,group=Charger.Type,colour=Charger.Type))+geom_line()+facet_grid(Charger.Status~Penetration)+scale_x_continuous(limits=c(6,30),breaks=c(0,6,12,18,24,30))+ylab("Charging Demand (kW)")+xlab("Simulation Time (hours)"))
	dev.off()
	
}

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
logtazs')s[['charging']]$replicate <- as.numeric(unlist(lapply(strsplit(as.character(logs[['charging']]$driver.input.file),'-rep',fixed=T),function(x){ unlist(strsplit(x[2],"-",fixed=T)[[1]][1]) })))

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

