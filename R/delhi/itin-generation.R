load.libraries(c('maptools','gpclib','plyr','stringr','ggplot2','doMC','reshape','data.table','colorRamps','rgeos'))
gpclibPermit()
registerDoMC(num.cpu)

make.plots  <- T

path.to.outputs <- pp(pevi.shared,'~/data/inputs/driver-input-file/delhi-uncombined/')
path.to.plots <- pp(pevi.nondrop,'itin-plots/delhi/')

# load and pre-process the survey data
dumb.time.to.hours <- function(x){
  floored <- floor(x/100)
  floored + (x-floored*100)/60
}

do.or.load(pp(pevi.shared,'data/DELHI/itin-generation/hh-survey-with-2wheelers.Rdata'),function(){
  mode.codes <- rbind(read.csv(pp(pevi.shared,'data/DELHI/tdfs-data/mode-codes.csv')),data.frame(mode.id=0,name=""))
  hh <- read.csv(pp(pevi.shared,'data/DELHI/tdfs-data/hh-survey.csv'))
  hh$key <- pp(hh$FORM_NO,hh$T_ZONE,hh$MEMNO,hh$TRIP_NO,sep='-')
  hh <- hh[!duplicated(hh$key),]
  hh <- data.table(hh)
  hh[,':='(form=FORM_NO,taz=T_ZONE,mem=MEMNO,trip=TRIP_NO,FORM_NO=NULL,T_ZONE=NULL,MEMNO=NULL,TRIP_NO=NULL)]
  setkey(hh,form,taz,mem,trip)
  hh$depart  <- dumb.time.to.hours(hh$ST_TIME)
  hh$arrive  <- dumb.time.to.hours(hh$END_TIME)
  hh <- hh[,list(sub.trip=1:3,depart=depart,arrive=arrive,o=ORIGIN,d=DESTIN,purp=TRIP_PURP,mode=c(L1_MODE,L2_MODE,L3_MODE),dist=c(L1_DISTENC,L2_DISTENC,L3_DISTENC)/1e3,time=c(L1_TRVTIME,L2_TRVTIME,L3_TRVTIME)/60,cost=c(L1_TRVCOST,L2_TRVCOST,L3_TRVCOST),wait=c(L1_TRVWAIT,L2_TRVWAIT,L3_TRVWAIT)/60),by=c('form','taz','mem','trip')]
  hh <- hh[!is.na(dist)]
  hh <- hh[is.na(wait),wait:=0]
  hh <- hh[is.na(time),time:=0]
  hh$mode <- mode.codes$name[match(hh$mode,mode.codes$mode.id)]
  # correct the depart / arrive times for the subtrips
  hh <- hh[,list(depart=depart + cumsum(c(0,head(time+wait,-1))),arrive=arrive,sub.trip=sub.trip,o=o,d=d,purp=purp,mode=mode,dist=dist,time=time,cost=cost,wait=wait),by=c('form','taz','mem','trip')]
  hh <- hh[,list(depart=depart,arrive=depart+time+wait,sub.trip=sub.trip,o=o,d=d,purp=purp,mode=mode,dist=dist,time=time,cost=cost,wait=wait),by=c('form','taz','mem','trip')]
  # make consistent any schedules that appear to go back in time
  setkey(hh,'form','taz','mem','depart')
  bad <- hh[,list(bad=sum(tail(depart,-1)<head(arrive,-1)-1e-6,na.rm=T)),by=c('form','taz','mem')]
  hh <- bad[hh]
  # takes a long time
  hh.fixed <- ddply(hh[bad>0],.(form,taz,mem),function(df){
    if(any(df$bad>0) & nrow(df)>1){
      prev.dep <- df$depart
      for(i in 2:nrow(df)){
        if(df$depart[i] < df$arrive[i-1] - 1e-6){
          df$arrive[i] <- df$arrive[i] + df$arrive[i-1] - df$depart[i]
          df$depart[i] <- df$arrive[i-1]
        } 
      }
    }
    df
  })
  hh[,key:=pp(form,taz,mem,trip,sub.trip,sep='-')]
  hh.fixed <- data.table(hh.fixed)
  hh.fixed[,key:=pp(form,taz,mem,trip,sub.trip,sep='-')]
  hh[bad>0,depart:=hh.fixed$depart[match(key,hh.fixed$key)]]
  hh[bad>0,arrive:=hh.fixed$arrive[match(key,hh.fixed$key)]]
  hh[,':='(bad=NULL,key=NULL)]

  hh$speed <- hh$dist/hh$time
  # oh goodie, about half of the distance values are in km and the other half in m, let's just make a blanket division at 0.1 or ~60 mph equiv
  hh[speed<0.1,':='(dist=dist*1000,speed=speed*1000)]
  # inspect the resulting speed distributions
  #ggplot(hh,aes(x=speed))+geom_histogram()+facet_wrap(~mode,scales="free")
  # get rid of modes we're not interested in
  hh <- hh[mode %in% c('Car','Auto','Shared Auto','Shared Taxi','Taxi','Pool Car','Two Wheeler')]

  setkey(hh,'form','taz','mem')
  hh.uniq <- unique(hh)[,list(form,taz,mem)]
  hh.uniq[,journey.id:=1:nrow(hh.uniq)]
  setkey(hh.uniq,'form','taz','mem')
  hh <- hh.uniq[hh]
  setkey(hh.uniq,'journey.id')
  hh[,tours.left.in.journey:=(length(trip)-1):0,by='journey.id']
  hh[,':='(index=1:nrow(hh),home.start= taz==o,home.end= taz==d,begin.int=as.integer(round(depart)),km.int=as.integer(round(dist)))]
  # we need a quick way to see the number of trips per journey
  hh.per <- hh[,length(trip),by="journey.id"]
  # store hh data in a way that allows more optmized access to particular subsets
  hh.by.home <- list()
  hh.by.home[['home.start']] <- data.table(hh[home.start==T],key=c("begin.int","km.int"))
  hh.by.home[['non.home.start']] <- data.table(hh[home.start==F],key=c("begin.int","km.int"))
  frac.end.at.home <- nrow(hh[home.end & tours.left.in.journey==0])/nrow(hh[tours.left.in.journey==0])
  frac.include.home <- nrow(hh[home.start | home.end])/nrow(hh)
  list('hh'=hh,'hh.per'=hh.per,'hh.by.home'=hh.by.home,'frac.end.at.home'=frac.end.at.home,'frac.include.home'=frac.include.home)
})

# load od.agg and agg.taz.data
load(pp(pevi.shared,'data/DELHI/tdfs-data/od-by-mode/od-agg.Rdata'))
load(pp(pevi.shared,'data/DELHI/POLYGON/aggregated-tazs.Rdata'))
agg.taz.data <- agg.taz@data

# add external TAZs to time.distance
do.or.load(pp(pevi.shared,'data/DELHI/road-network/routing-with-gateways.Rdata'),function(){
  load(pp(pevi.shared,'data/DELHI/road-network/routing.Rdata'))
  gates <- as.integer(unique(od.agg$o)[unique(od.agg$o)<0])
  for(gate in gates){
    nearest <- time.distance[from==-gate | to==-gate]
    nearest[,km:=km+10]    # add 10 km 
    nearest[,time:=time+10/40] # assume they're on outskirts and can drive 40 kph
    nearest[,enroute:=ifelse(enroute=='',as.character(-gate),pp(enroute,",",-gate))] # assume they're on outskirts and can drive 40 kph
    nearest[from==-gate,from:=gate]
    nearest[to==-gate,to:=gate]
    nearest <- rbind(nearest,data.table(from=c(gate,-gate),to=c(-gate,gate),enroute='',km=10,time=0.25))
    time.distance <- rbind(time.distance,nearest)
  }
  # grab integer value of km for binning data
  time.distance$km.int <- round(time.distance$km)
  list('time.distance'=time.distance)
})

# Make some plots to describe the HH data and OD distance distribution
if(F){
  hh$mode <- factor(hh$mode,levels=c('Auto','Shared Auto','Car','Pool Car','Taxi','Shared Taxi','Two Wheeler'))
  ggplot(subset(hh,dist<100),aes(x=dist))+geom_histogram()+facet_wrap(~mode)+labs(x="Distance (km)",y="Count",title=pp("Travel Distances from RITES Household Survey (n=",nrow(subset(hh,dist<100)),")"))
  ggplot(hh.per,aes(x=factor(V1)))+geom_histogram()+labs(x="Trips per Person-Day",y="Count",title=pp("Trips per Person-Day from RITES Household Survey (n=",nrow(hh.per),")"))
  ggplot(hh,aes(x=depart))+geom_histogram(binwidth=1)+facet_wrap(~home.start)+labs(x="Departure Time",y="Count",title=pp("Departure Times by whether they begin from home")) + scale_x_continuous(limits=c(0,24))

  od.agg.tmp <- od.agg
  od.agg.tmp[,':='(from=o,to=d,o=NULL,d=NULL)]
  setkey(od.agg.tmp,'from','to')
  setkey(time.distance,'from','to')
  od.agg.tmp <- ddply(time.distance[od.agg.tmp],.(mode),function(df){ 
    data.frame(km=unlist(apply(as.matrix(df),1,function(x){ rep(as.numeric(x['km']),round(as.numeric(x['trips']))) })))
  })
  levels(od.agg.tmp$mode) <- c('Auto','Shared Auto','Car','Pool Car','Taxi','Shared Taxi','Two-Wheeler','Car-x','Two-Wheeler-x')
  ggplot(subset(od.agg.tmp,mode%in%c('Car','Auto','Shared Auto','Shared Taxi','Taxi','Pool Car','Two-Wheeler')),aes(x=km))+geom_histogram()+facet_wrap(~mode)+labs(x="Distance (km)",y="Count",title=pp("Travel Distances from RITES Travel Demand Model (n=",nrow(od.agg.tmp),")"))+scale_x_continuous(limits=c(0,100))
}
    
# Load/Create the home distribution and nearest neighbors list
do.or.load(pp(pevi.shared,"data/DELHI/itin-generation/frac-homes-and-nearest-10.Rdata"),function(){
  # Create the home distribution
  ward <- readShapePoly(pp(pevi.shared,'data/DELHI/POLYGON/income_ward_level_map_delhi_WGS84'))
  the.nas <- ward$Income==0
  ward$Income[the.nas] <- NA
  ward$income.weight <- c(1,1.5,3)[ward$Income]
  ward$income.weight[the.nas] <- weighted.mean(ward$income.weight[!the.nas],ward$POP_2001[!the.nas])
  ward$final.weights <- ward$income.weight * ward$POP_2001 / sum(ward$income.weight * ward$POP_2001)

  # first find the area of each ward on their own
  ward.areas <- rep(NA,length(ward))
  for(ward.ind in 1:length(ward)){
    ward.areas[ward.ind] <- gArea(ward[ward.ind,])
  } 
  ward.frac.in.taz <- matrix(NA,nrow=length(ward),ncol=length(agg.taz),dimnames=list(ward$WARD_NOS,agg.taz$agg_id))
  for(taz.ind in 1:length(agg.taz)){
    taz.area <- gArea(agg.taz[taz.ind,])
    for(ward.ind in 1:length(ward)){
      # now calculate the fraction by area of the ward in the taz 
      ward.frac.in.taz[ward.ind,taz.ind] <- (taz.area + ward.areas[ward.ind] - gArea(gUnion(agg.taz[taz.ind,],ward[ward.ind,]))) / ward.areas[ward.ind]
    }
  }
  # ignore tazs with no overlapping wards, just scale all columns to sum to one
  ward.frac.in.taz <- t(apply(ward.frac.in.taz,1,function(x){ x/sum(x) }))
  
  home.dist <- data.frame(agg.id=agg.taz$agg_id,frac.home=apply(ward.frac.in.taz,2,function(x){ sum(x * ward$final.weights) }))

  # Using population data from report under data/DELHI/demographics.../Delhi-population_projection_report.pdf
  pops <- data.frame(year=seq(2001,2026,by=5),pop=c(13851,16021,18451,21285,24485,27982)*1e3)
  # Using data from 2011 census, total population of Delhi = 18.451e6 and # households = 3.34e6 and # vehicles/household = 0.2071856 # vehicles+2wheelers/household = 0.5958807
  pop.2020 <- predict(lm('pop ~ year + I(year^2)',pops),newdata=data.frame(year=2020))
  num.veh.2020 <- pop.2020 * (3.34e6/18.451e6) * 0.2071856

  # Synergizing with Maggie Witt's numbers we set # vehicles to match those projections
  pop.2027 <- predict(lm('pop ~ year + I(year^2)',pops),newdata=data.frame(year=2027))
  num.veh.2027 <- pop.2027 * 0.521582582 # gives us 15M vehicles total

  taz.10 <- list() # either all neighbors within 10 km or the 10 closest neighbors, whichever yields more, 54% of U.S. rural tours <= 10 km
  for(taz.i in unique(time.distance$from)){
    within10 <- time.distance$to[time.distance$from==taz.i][which(time.distance$km[time.distance$from==taz.i]<=10)]
    closest10 <- time.distance$to[time.distance$from==taz.i][order(time.distance$km[time.distance$from==taz.i])][1:10]
    if(length(within10)>length(closest10)){
      taz.10[[as.character(taz.i)]] <- within10
    }else{
      taz.10[[as.character(taz.i)]] <- closest10
    }
  } 
  list('home.dist'=home.dist,'taz.10'=taz.10,'pop.2020'=pop.2020,'num.veh.2020'=num.veh.2020,'pop.2027'=pop.2027,'num.veh.2027'=num.veh.2027)
})

do.or.load(pp(pevi.shared,"data/DELHI/itin-generation/departure-time-dists.Rdata"),function(){
  # now develop emprical departure CDF's for each grouping of tour types
  ecdfs <- ecdf(hh$depart)
  hours <- seq(-0.25,24,by=0.25)
  epdfs <- diff(ecdfs(hours)) 
  epdfs <- data.frame(hour=as.integer(tail(hours,-1)),epdf=epdfs)
  epdfs <- ddply(epdfs,.(hour),function(df){ data.frame(epdf=sum(df$epdf)) })
  epdfs$epdf[1] <- epdfs$epdf[1] +  epdfs$epdf[nrow(epdfs)]
  epdfs <- h(epdfs,24)
  epdfs$epdf <- epdfs$epdf/sum(epdfs$epdf)
  list('epdfs'=epdfs)
})


######################################################
# GENERATE THE ITINS
######################################################

source(pp(pevi.home,'R/delhi/itin-functions.R',sep=''))

replicates <- 1:8
time.distance$ft <- pp(time.distance$from,' ',time.distance$to)
date.code <- '20150128'
pev.penetration <- 0.05
pev.pen.char <- roundC(pev.penetration,3)
#if(file.exists(pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined-schedule-replicates-',date.code,'.Rdata',sep=''))){
  #load(file=pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined-schedule-replicates-',date.code,'.Rdata',sep=''))
#}else{
  schedule.reps <- list()
#}
if(is.null(schedule.reps[[pev.pen.char]]))schedule.reps[[pev.pen.char]] <- list()
for(replicate in replicates){
  print(paste('Penetration ',pev.penetration,' replicate ',replicate,sep=''))
  schedule.reps[[pev.pen.char]][[as.character(replicate)]] <- data.frame(create.schedule(pev.penetration,2,frac.end.at.home,frac.include.home))
  sched <- schedule.reps[[pev.pen.char]][[as.character(replicate)]][,c('driver','from','to','depart','home')]
  #sched <- read.table(file=paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep='\t',header=T)
  sched$ft <- pp(sched$from,' ',sched$to)

  sched <- join(sched,time.distance,by="ft")
  sched$arrive <- sched$depart + sched$time
  if(names(sched)[1]=="X.driver")names(sched)<-c("driver",names(sched)[2:ncol(sched)])
  sched <- ddply(sched,.(driver),function(df){ 
    if(nrow(df)>1 & any(df$depart[2:nrow(df)] < df$arrive[1:(nrow(df)-1)])){
      while(any(df$depart[2:nrow(df)] < df$arrive[1:(nrow(df)-1)])){
        i <- which(df$depart[2:nrow(df)] < df$arrive[1:(nrow(df)-1)])[1]
        df$depart[(i+1):nrow(df)] <- df$depart[(i+1):nrow(df)]+1
      }
    }
    df
  })
  sched <- sched[,c('driver','from','to','depart','home')]
  names(sched) <- c(';driver','from','to','depart','home')
  write.table(sched,pp(pevi.shared,"data/inputs/driver-input-file/delhi-uncombined/driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-",date.code,".txt",sep=''),sep="\t",row.names=F,quote=F)
  save(schedule.reps,file=pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined/delhi-uncombined-schedule-replicates-',date.code,'-rep-',replicate,'.Rdata',sep=''))
}
schedule.reps.all <- list()
if(is.null(schedule.reps.all[[pev.pen.char]]))schedule.reps.all[[pev.pen.char]] <- list()
for(replicate in 1:8){
  load(pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined/delhi-uncombined-schedule-replicates-',date.code,'-rep-',replicate,'.Rdata',sep=''))
  schedule.reps.all[[pev.pen.char]][[as.character(replicate)]] <- schedule.reps[[pev.pen.char]][[as.character(replicate)]]
}
schedule.reps <- schedule.reps.all
save(schedule.reps,file=pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined/delhi-uncombined-schedule-replicates-',date.code,'.Rdata',sep=''))


###############################################################
# Run through the itins and produce charger permission files
###############################################################
frac.mu <- 0.5

pev.pens <- c(0.005,0.01,0.02)
replicate <- 1
source(pp(pevi.home,'R/delhi/itin-functions.R',sep=''))

num.replicates <- 80
time.distance$ft <- pp(time.distance$from,' ',time.distance$to)
date.code <- '20140217'
pev.penetration <- 0.005
pev.pen.char <- roundC(pev.penetration,3)
if(file.exists(pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined/delhi-uncombined-schedule-replicates-',date.code,'.Rdata',sep=''))){
  load(file=pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined/delhi-uncombined-schedule-replicates-',date.code,'.Rdata',sep=''))
}
for(replicate in 1:num.replicates){
  sched <- schedule.reps[[pev.pen.char]][[as.character(replicate)]][,c('driver','from','to','depart','home')]
  ddply(sched,.(home),function(df){
    # randomly assign drivers to multi-unit dwellings
    n.mu <- round(nrow(df)*frac.mu,0)
    driver.groups <- sample(unique(df$driver))
  })
}

