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

do.or.load(pp(pevi.shared,'data/DELHI/tdfs-data/hh-survey.Rdata'),function(){
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
  hh <- hh[mode %in% c('Car','Auto','Shared Auto','Shared Taxi','Pool Car')]
  list('hh'=hh)
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
    
# Load/Create the home distribution and nearest neighbors list
do.or.load(pp(pevi.shared,"data/DELHI/frac-homes-and-nearest-10.Rdata"),function(){
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
  # Using data from 2011 census, total population of Delhi = 18.451e6 and # households = 3.34e6 and # vehicles/household = 0.2071856
  pop.2020 <- predict(lm('pop ~ year + I(year^2)',pops),newdata=data.frame(year=2020))
  num.veh.2020 <- pop.2020 * (3.34e6/18.451e6) * 0.2071856

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
  list('home.dist'=home.dist,'taz.10'=taz.10,'pop.2020'=pop.2020,'num.veh.2020'=num.veh.2020)
})

if(!file.exists(pp(pevi.shared,'data/UPSTATE/itin-generation/data-preprocessed.Rdata'))){
  if(!file.exists(pp(pevi.shared,'data/UPSTATE/itin-generation/rur-tours.Rdata',sep=''))){
    # Load the NHTS tour dataset
    load(file=pp(pevi.shared,'data/NHTS/tour09.Rdata'))
    rur.tours <- data.table(subset(tours,URBRUR==2 & PMT_POV>0),key='journey.id')
    rur.tours[,tours.left.in.journey:=(length(home.start)-1):0,by='journey.id']

    # get rid of some bad data: NA for TOT_DWEL4, begin, end
    rur.tours <- rur.tours[!rur.tours$journey.id %in% rur.tours$journey.id[is.na(rur.tours$TOT_DWEL4)&rur.tours$tours.left.in.journey>0]]
    rur.tours[,TOT_DWEL4:=ifelse(is.na(TOT_DWEL4),0,TOT_DWEL4)]
    rur.tours <- rur.tours[!rur.tours$journey.id %in% rur.tours$journey.id[is.na(rur.tours$end)]]
    rur.tours$index <- 1:nrow(rur.tours)
    rur.tours.per <- rur.tours[,length(home.start),by="journey.id"]

    # type mappings
    type.map <- list()
    type.map[['hw']] <- c('HW','WH')
    type.map[['ho']] <- c('HO','OH','HH')
    type.map[['ow']] <- c('OW','WO','OO','WW')
    type.map.rev <- melt(type.map)
    names(type.map.rev) <- c('tour.type','purp')

    # add a purp field to rur.tours
    rur.tours$purp <- as.character(type.map.rev$purp[match(rur.tours$TOURTYPE,type.map.rev$tour.type)])

    save(rur.tours,rur.tours.per,type.map,type.map.rev,file=pp(pevi.shared,'data/UPSTATE/itin-generation/rur-tours.Rdata',sep=''))
  }else{
    load(pp(pevi.shared,'data/UPSTATE/itin-generation/rur-tours.Rdata',sep=''))
  }

  # prepare OD data by condensing trip purposes into HW, HO, OW categories
  od.agg.simp <- od.agg.all[,list(from=from,to=to,hw=hw,ho=ho+hs+hsc,ow=oo+wo,hw.am=hw.am,ho.am=ho.am+hs.am+hsc.am,ow.am=oo.am+wo.am,hw.pm=hw.pm,ho.pm=ho.pm+hs.pm+hsc.pm,ow.pm=oo.pm+wo.pm)]

  # by how much do we need to scale the AM / PM hours (AM 6:45-7:45, PM 16:30-17:30) to make the aggregate NHTS match GEATM
  od.24.tot <- sum(od.agg.simp[,list(hw,ho,ow)])
  od.am.tot <- sum(od.agg.simp[,list(hw.am,ho.am,ow.am)])
  od.pm.tot <- sum(od.agg.simp[,list(hw.pm,ho.pm,ow.pm)])
  nh.24.tot <- nrow(rur.tours)
  nh.am.tot <- sum(rur.tours$begin >= 6.75 & rur.tours$begin < 7.75)
  nh.pm.tot <- sum(rur.tours$begin >= 16.5 & rur.tours$begin < 17.5)

  am.scale <- (od.am.tot/od.24.tot)/(nh.am.tot/nh.24.tot)
  pm.scale <- (od.pm.tot/od.24.tot)/(nh.pm.tot/nh.24.tot)
  offpeak.scale <- ((od.24.tot-od.am.tot-od.pm.tot)/od.24.tot )/((nh.24.tot - nh.am.tot - nh.pm.tot)/nh.24.tot)

  rur.tours$begin.int <- as.integer(round(rur.tours$begin))
  rur.tours$begin.int[rur.tours$begin.int==24] <- 0
  rur.tours$tot.miles.int <- as.integer(round(rur.tours$TOT_MILS))
  
  rur.by.type <- list()
  for(type in c('hw','ho','ow')){
    rur.by.type[[type]] <- list()
    rur.by.type[[type]][['home.start']] <- data.table(subset(rur.tours,TOURTYPE %in% type.map[[type]] & home.start),key=c("begin.int","tot.miles.int"))
    rur.by.type[[type]][['non.home.start']] <- data.table(subset(rur.tours,TOURTYPE %in% type.map[[type]] & !home.start),key=c("begin.int","tot.miles.int"))
  }

  # now develop emprical departure CDF's for each grouping of tour types
  ecdfs <- list()
  for(type in c('hw','ho','ow')){
    ecdfs[[type]] <- ecdf(c(rur.by.type[[type]][['home.start']]$begin,rur.by.type[[type]][['non.home.start']]$begin))
  }

  hours <- seq(-0.25,24,by=0.25)
  all.scale <- c(rep(offpeak.scale,4*6+3),rep(am.scale,4),rep(offpeak.scale,1+4*8+2),rep(pm.scale,4),rep(offpeak.scale,3+4*6))

  epdfs <- list()
  epdfs[['hw']] <- diff(ecdfs[['hw']](hours)) * all.scale
  epdfs[['ho']] <- diff(ecdfs[['ho']](hours)) * all.scale
  epdfs[['ow']] <- diff(ecdfs[['ow']](hours)) * all.scale
  epdfs <- data.frame(hour=as.integer(tail(hours,-1)),hw=diff(ecdfs[['hw']](hours)) * all.scale,ho=diff(ecdfs[['ho']](hours)) * all.scale,ow=diff(ecdfs[['ow']](hours)) * all.scale)
  epdfs <- ddply(epdfs,.(hour),function(df){colSums(df[,2:4])})
  epdfs <- epdfs[1:24,]

  # verify that it all sums to 1
  #weighted.mean(colSums(epdfs)[2:4],c(nrow(rur.by.type[['hw']]),nrow(rur.by.type[['ho']]),nrow(rur.by.type[['ow']])))

  save(rur.tours,rur.tours.per,rur.by.type,ecdfs,epdfs,type.map,type.map.rev,od.agg.simp,file=pp(pevi.shared,'data/UPSTATE/itin-generation/data-preprocessed.Rdata'))
}else{
  load(file=pp(pevi.shared,'data/UPSTATE/itin-generation/data-preprocessed.Rdata'))
}

pev.pens <- c(0.005,0.01,0.02,0.04)
replicate <- 1
source(pp(pevi.home,'R/upstate/itin-functions.R',sep=''))
#schedule <- create.schedule(0.001,1,0.922)
#print(paste(nrow(schedule)/length(unique(schedule$driver)),nrow(schedule),length(unique(schedule$driver))))
# see what fraction of drivers end at home?
# sum(ddply(schedule,.(driver),function(df){ df$to[nrow(df)]==df$home[1] })$V1)/length(unique(schedule$driver))

#num.replicates <- 80
#schedule.reps <- list()
#for(pev.penetration in pev.pens){
  #pev.pen.char <- roundC(pev.penetration,3)
  #schedule.reps[[pev.pen.char]] <- list()
  #for(replicate in 1:num.replicates){
    #print(paste('Penetration ',pev.penetration,' replicate ',replicate,sep=''))
    #schedule.reps[[pev.pen.char]][[as.character(replicate)]] <- create.schedule(pev.penetration,1)
    #sched <- schedule.reps[[pev.pen.char]][[as.character(replicate)]][,c('driver','from','to','depart','home')]
    #names(sched) <- c(';driver','from','to','depart','home')
    #write.table(sched,file=paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep='\t',row.names=F,quote=F)
    #save(schedule.reps,file=paste(path.to.outputs,'schedule-replicates-20130129.Rdata',sep=''))
  #}
#}
#save(schedule.reps,file=paste(path.to.outputs,'schedule-replicates-20130129.Rdata',sep=''))

# fix the bug that causes drivers to have impossible itineraries
num.replicates <- 80
time.distance$ft <- pp(time.distance$from,' ',time.distance$to)
date.code <- '20140129'
pev.penetration <- 0.005
pev.pen.char <- roundC(pev.penetration,3)
if(file.exists(pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined-schedule-replicates-',date.code,'.Rdata',sep=''))){
  load(file=pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined-schedule-replicates-',date.code,'.Rdata',sep=''))
}else{
  schedule.reps <- list()
}
if(is.null(schedule.reps[[pev.pen.char]]))schedule.reps[[pev.pen.char]] <- list()
for(replicate in 1:num.replicates){
  print(paste('Penetration ',pev.penetration,' replicate ',replicate,sep=''))
  schedule.reps[[pev.pen.char]][[as.character(replicate)]] <- create.schedule(pev.penetration,1)
  sched <- schedule.reps[[pev.pen.char]][[as.character(replicate)]][,c('driver','from','to','depart','home')]
  #sched <- read.table(file=paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep='\t',header=T)
  sched$ft <- pp(sched$from,' ',sched$to)

  sched <- join(sched,time.distance,by="ft")
  sched$arrive <- sched$depart + sched$hours
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
  write.table(sched,pp(pevi.shared,"data/inputs/driver-input-file/upstate-uncombined/driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-",date.code,".txt",sep=''),sep="\t",row.names=F,quote=F)
}
save(schedule.reps,file=pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined-schedule-replicates-',date.code,'.Rdata',sep=''))

# make plot comparing spatial distribution of trips in itin to GEATM
if(make.plots){
  tot.demand <- sum(od.24.simp[,c('hw','ho','ow')])
  compare.trips <- ddply(od.24.simp,.(from),function(df){ data.frame(source="GEATM",demand=sum(df[,c('hw','ho','ow')])/tot.demand) })
  pev.penetration <- 0.04
  replicate <- 1
  tot.itin <- nrow(sched)
  sched <- read.table(file=paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep='\t',header=T)
  if(names(sched)[1]=="X.driver")names(sched)<-c("driver",names(sched)[2:ncol(sched)])
  compare.trips <- rbind(compare.trips,ddply(sched,.(from),function(df){ data.frame(source="Model",demand=nrow(df)/tot.itin) }))
  compare.trips$source <- reorder(compare.trips$source,rev(as.numeric(compare.trips$source)))
  ggplot(compare.trips,aes(x=factor(from),fill=source,y=100*demand))+geom_bar(position='dodge')
}



# summarize the results
load(file=paste(path.to.outputs,'schedule-replicates-20130129.Rdata',sep=''))
n.scheds <- num.replicates * length(pev.pens)
sum.sched <- data.frame(pen=rep(pev.pens,num.replicates),rep=rep(1:num.replicates,each=length(pev.pens)),n.drivers=rep(NA,n.scheds),n.trips=rep(NA,n.scheds),trips.per.driver=rep(NA,n.scheds),home.rmse=rep(NA,n.scheds),home.maxe=rep(NA,n.scheds),home.max.taz=rep(NA,n.scheds))
for(pev.penetration in pev.pens){
  pev.pen.char <- roundC(pev.penetration,3)
  for(replicate in 1:num.replicates){
    if(!is.null(schedule.reps[[pev.pen.char]][[as.character(replicate)]])){

      sched.home.dist <- ddply(schedule.reps[[pev.pen.char]][[as.character(replicate)]],.(home),function(df){ data.frame(num.drivers=nrow(df)) })
      sched.home.dist$frac <- sched.home.dist$num.drivers / sum(sched.home.dist$num.drivers)
      if(nrow(sched.home.dist) < nrow(agg.taz@data)){
        sched.home.dist <- rbind(sched.home.dist,data.frame(home=which(! 1:nrow(agg.taz@data) %in% sched.home.dist$home),num.drivers=0,frac=0))
      }
      sched.home.dist$real <- home.dist$frac.homes[match(sched.home.dist$home,home.dist$agg.taz)]

      sum.sched[sum.sched$pen == pev.penetration & sum.sched$rep==replicate,3:8] <- c(length(unique(schedule.reps[[pev.pen.char]][[as.character(replicate)]]$driver)),nrow(schedule.reps[[pev.pen.char]][[as.character(replicate)]]),nrow(schedule.reps[[pev.pen.char]][[as.character(replicate)]])/length(unique(schedule.reps[[pev.pen.char]][[as.character(replicate)]]$driver)),sqrt(mean((sched.home.dist$real-sched.home.dist$frac)^2))*100,max(sched.home.dist$real-sched.home.dist$frac)*100,sched.home.dist$home[which.max(sched.home.dist$real-sched.home.dist$frac)])
    }
  }
}

# look into why some drivers have trips seemingly out of order
for(pev.penetration in pev.pens){
  pev.pen.char <- roundC(pev.penetration,3)
  for(replicate in 1:num.replicates){
    if(!is.null(schedule.reps[[pev.pen.char]][[as.character(replicate)]])){
      print(paste('Penetration ',pev.penetration," rep ",replicate,sep=''))
      schedule <- schedule.reps[[pev.pen.char]][[as.character(replicate)]]
      print(unique(schedule$driver)[which(!ddply(schedule,.(driver),function(df){
        df <- df[order(df$depart),]
        nrow(df)==1 | all(df$from[2:nrow(df)] == df$to[1:(nrow(df)-1)])
      })$V1)])
    }
  }
}
# find a repeatable case where a funky driver appears, pen 0.00135
set.seed(1)
schedule <- create.schedule(0.001,1,0.922)
for(pen in seq(0.00105,0.005,by=0.00005)){
  set.seed(1)
  schedule <- create.schedule(pen,1,0.922)
  print(unique(schedule$driver)[which(!ddply(schedule,.(driver),function(df){
    df <- df[order(df$depart),]
    nrow(df)==1 | all(df$from[2:nrow(df)] == df$to[1:(nrow(df)-1)])
  })$V1)])
}



# analyze and plot the schedules, compare them to NHTS and GEATM

ks.tests <- data.frame(penetration=pev.pens,test='tours.per.driver',factor=NA,level=NA,stat=NA,p.value=NA)
ks.tests <- rbind(ks.tests,data.frame(penetration=rep(pev.pens,each=length(unique(rur.tours$TOURTYPE))),test='dwell.time',factor='tour.type',level=rep(unique(rur.tours$TOURTYPE),length(pev.pens)),stat=NA,p.value=NA))
ks.tests <- rbind(ks.tests,data.frame(penetration=rep(pev.pens,each=length(unique(rur.tours$TOURTYPE))),test='departure.time',factor='tour.type',level=rep(unique(rur.tours$TOURTYPE),length(pev.pens)),stat=NA,p.value=NA))
num.vehicles <- data.frame(penetration=pev.pens,expected=NA,scheduled=NA)
synth.tours.per <- list()
dwell.times     <- list()
compute.new <- T

if(make.plots){
  dev.new()
  dev.tours.per <- dev.cur()
  plot(ecdf(rur.tours.per$V1),main="Empirical CDF of # Trips Per Driver")
}

for(pev.penetration in pev.pens){
  pev.pen.char <- roundC(pev.penetration,3)
  pen.i <- which(pev.penetration == pev.pens)+1
  for(replicate in 1:num.replicates){
    if(!is.null(schedule.reps[[pev.pen.char]][[as.character(replicate)]])){
      schedule <- schedule.reps[[pev.pen.char]][[as.character(replicate)]]

      print(paste('Penetration ',pev.penetration," rep ",replicate,sep=''))
      num.vehicles[num.vehicles$penetration==pev.penetration,c('expected','scheduled')] <- c(pev.penetration * 130e3,max(schedule$driver))

      # TOURS PER DRIVER
      # assumes we have already computed rur.tours.per
      if(compute.new) synth.tours.per[[pev.pen.char]] <- ddply(schedule,.(driver),function(df){data.frame(ntours=nrow(df),end.time=df$arrive[nrow(df)])})
      if(make.plots){
        dev.set(dev.tours.per)
        plot(ecdf(synth.tours.per[[pev.pen.char]]$ntours),add=T,col=pen.i,pch=pen.i)
      }
      ks.tests[ks.tests$penetration == pev.penetration & ks.tests$test == "tours.per.driver", c('stat','p.value')] <- unlist(ks.test(synth.tours.per[[pev.pen.char]]$ntours,rur.tours.per$V1)[c('statistic','p.value')])

      # DWELL TIME
      if(compute.new) dwell.times[[pev.pen.char]] <- ddply(schedule,.(driver),function(df){ if(nrow(df)>1){ data.frame(dwell = df$depart[2:nrow(df)]-df$arrive[1:(nrow(df)-1)],type= df$type[1:(nrow(df)-1)],purp= df$purp[1:(nrow(df)-1)]) }} )
      for(type in unique(rur.tours$TOURTYPE)){
        ks.tests[ks.tests$penetration == pev.penetration & ks.tests$test == "dwell.time" & ks.tests$factor == "tour.type" & ks.tests$level == type, c('stat','p.value')] <- unlist(ks.test(dwell.times[[pev.pen.char]]$dwell[dwell.times[[pev.pen.char]]$type==type],rur.tours$TOT_DWEL4[rur.tours$TOURTYPE==type]/60)[c('statistic','p.value')])
      }
      if(make.plots){
        p <- ggplot(rbind(data.frame(dwell.times[[pev.pen.char]],set="SYNTH"),data.frame(driver=NA,dwell=rur.tours$TOT_DWEL4/60,type=rur.tours$TOURTYPE,purp=rur.tours$purp,set="NHTS")),
                 aes(x=dwell))+
          scale_x_continuous(name="Total Tour Dwell Time (hours)")+
          opts(title = paste("Dwell Times for Penetration",pev.pen.char)) +
          geom_bar(aes(fill=set,y=..density..), position="dodge") +
          facet_wrap(~type)
        ggsave(p,file=paste(path.to.plots,"dwell-times-pen",pev.pen.char,"-rep",replicate,".pdf",sep=''),width=8,height=8)

        p <- ggplot(rbind(data.frame(dwell.times[[pev.pen.char]],set="SYNTH"),data.frame(driver=NA,dwell=rur.tours$TOT_DWEL4/60,type=rur.tours$TOURTYPE,purp=rur.tours$purp,set="NHTS")),
                 aes(x=dwell))+
          scale_x_continuous(name="Total Tour Dwell Time (hours)")+
          opts(title = paste("Dwell Times for Penetration",pev.pen.char)) +
          geom_bar(aes(fill=set,y=..density..), position="dodge") +
          facet_wrap(~purp)
        ggsave(p,file=paste(path.to.plots,"dwell-times-by-geatm-pen",pev.pen.char,"-rep",replicate,".pdf",sep=''),width=8,height=8)
      }

      # DEPARTURE TIME
      for(type in unique(rur.tours$TOURTYPE)){
        ks.tests[ks.tests$penetration == pev.penetration & ks.tests$test == "departure.time" & ks.tests$factor == "tour.type" & ks.tests$level == type, c('stat','p.value')] <- unlist(ks.test(schedule$depart[schedule$type==type],rur.tours$begin[rur.tours$TOURTYPE==type])[c('statistic','p.value')])
      }
      if(make.plots){
        p <- ggplot(rbind(data.frame(schedule[,c('depart','type')],set="SYNTH"),data.frame(depart=rur.tours$begin,type=rur.tours$TOURTYPE,set="NHTS")),
                 aes(x=depart))+
          scale_x_continuous(name="Departure Time (hours)")+
          opts(title = paste("Departure Times for Penetration",pev.pen.char)) +
          geom_bar(aes(fill=set,y=..density..), position="dodge") +
          facet_wrap(~type)
        ggsave(p,file=paste(path.to.plots,"departure-times-pen",pev.pen.char,"-rep",replicate,".pdf",sep=''),width=8,height=8)
        p <- ggplot(rbind(data.frame(schedule[,c('depart','purp')],set="SYNTH"),data.frame(depart=rur.tours$begin,purp=rur.tours$purp,set="NHTS")),
                 aes(x=depart))+
          scale_x_continuous(name="Departure Time (hours)")+
          opts(title = paste("Departure Times for Penetration",pev.pen.char)) +
          geom_bar(aes(fill=set,y=..density..), position="dodge") +
          facet_wrap(~purp)
        ggsave(p,file=paste(path.to.plots,"departure-times-by-geatm-pen",pev.pen.char,"-rep",replicate,".pdf",sep=''),width=8,height=8)
      }
    }
  }
}

if(make.plots){
  dev.set(dev.tours.per)
  legend('bottomright',legend=c('NHTS Rural',paste('Synth Pen.',pev.pens)),pch=1:(length(pev.pens)+1),col=1:(length(pev.pens)+1))

  plot(num.vehicles$penetration,num.vehicles$scheduled/num.vehicles$expected,xlab="PEV Penetration",ylab="# Synthetic Vehicles / # Expected Vehicles",main="Ratio of Sythetic to Expected Vehicles",ylim=c(0,max(num.vehicles$scheduled/num.vehicles$expected)*1.1),xlim=c(0,0.27))
  abline(h=1)
  grid()

  # inspect all the ks.tests at once
  ggplot(ks.tests,aes(x=penetration,y=log(p.value+1e-14,10)))+geom_point()+facet_wrap(~test)
  # look within the tests that vary by type
  ggplot(subset(ks.tests,test=='dwell.time'),aes(x=penetration,y=log(p.value+1e-14,10)))+geom_point()+facet_wrap(~level)
  ggplot(subset(ks.tests,test=='departure.time'),aes(x=penetration,y=log(p.value+1e-14,10)))+geom_point()+facet_wrap(~level)
}

if(F){

num.in.transit <- rep(0,24)
for(driver.i in unique(schedule$driver)){
  res <- rep(0,24)
  df <- schedule[schedule$driver==driver.i,]
  res[as.integer(df$depart[1]):as.integer(df$arrive[nrow(df)])] <- 1
  num.in.transit <- num.in.transit + res
}
print(paste("num PEVs expected:",pev.penetration * 130e3))
print(paste("num unique drivers in schedule:",max(schedule$driver)))
print(paste("max num drivers in transit in any hour:",max(num.in.transit)))

schedule$row <- 1:nrow(schedule)
recycled <- ddply(schedule,.(driver),function(df){data.frame(is.recycled=!all(diff(df$row)==1))})
sum(recycled$is.recycled)
sum(recycled$is.recycled)/tail(sort(unique(schedule$driver)),1)

dwell.times <- ddply(schedule,.(driver),function(df){ if(nrow(df)>1){ data.frame(dwell = df$depart[2:nrow(df)]-df$arrive[1:(nrow(df)-1)],type= df$type[1:(nrow(df)-1)]) }} )

dev.new()
dev1 <- dev.cur()
dev.new()
dev2 <- dev.cur()

dev.set(dev1)
ggplot(dwell.times,aes(x=dwell))+
 scale_x_continuous(name="Total Tour Dwell Time (hours)")+
 opts(title = "Synthetic Schedule") +
 geom_histogram(binwidth=1,aes(y=..density..)) +
 facet_wrap(~type)

dev.set(dev2)
ggplot(rur.tours,aes(x=TOT_DWEL4/60))+
 scale_x_continuous(name="Total Tour Dwell Time (hours)")+
 opts(title = "2009 NHTS - Rural US Subset") +
 geom_histogram(binwidth=1,aes(y=..density..)) +
 facet_wrap(~TOURTYPE)

dev.set(dev1)
ggplot(schedule,aes(x=depart))+
 scale_x_continuous(name="Departure Time (hours)")+
 opts(title = "Synthetic Schedule") +
 geom_histogram(binwidth=1,aes(y=..density..)) +
 facet_wrap(~type)

dev.set(dev2)
ggplot(rur.tours,aes(x=begin))+
 scale_x_continuous(name="Departure Time (hours)")+
 opts(title = "2009 NHTS - Rural US Subset") + 
 geom_histogram(binwidth=1,aes(y=..density..)) +
 facet_wrap(~TOURTYPE)

schedule.counts <- ddply(schedule,.(from,to,purp),function(df){ data.frame(count=nrow(df)) } )

#dev.set(dev1)
#ggplot(schedule.counts,aes(x=purp,y=count))+
 #geom_bar(aes(colour=purp))+
 #opts(title = "Synthetic Schedule") +
 #scale_x_discrete(name="Trip Type")+
 #scale_y_continuous(name="# Trips")+
 #facet_grid(from~to)

#melted.od <- melt(od.24.simp,id=c('from','to'))
#dev.set(dev2)
#ggplot(melted.od,aes(x=variable,y=value))+
 #geom_bar(aes(colour=variable))+
 #opts(title = "GEATM - Counts by Trip Type") + 
 #scale_x_discrete(name="Trip Type")+
 #scale_y_continuous(name="# Trips")+
 #facet_grid(from~to)

ddply(schedule.counts,.(purp),function(df){sum(df$count)})
colSums(od.24.simp[,3:5])

synth.tours.per <- ddply(schedule,.(driver),function(df){data.frame(ntours=nrow(df),end.time=df$arrive[nrow(df)])})
if(!exists('rur.tours.per')){ rur.tours.per <- ddply(rur.tours,.(journey.id),nrow) }

dev.set(dev1)
ggplot(synth.tours.per,aes(x=ntours))+
 geom_histogram(aes(y=..density..),binwidth=1)+
 opts(title = "Synthetic Schedule") +
 scale_x_continuous(name="Tours Per Driver")
dev.set(dev2)
ggplot(rur.tours.per,aes(x=V1))+
 geom_histogram(aes(y=..density..),binwidth=1)+
 opts(title = "NHTS") +
 scale_x_continuous(name="Tours Per Driver")

# same as above but in CDF form
h.synth <- hist(synth.tours.per$ntours,plot=F,breaks=0:ceiling(max(synth.tours.per$ntours)))
h.nhts <- hist(rur.tours.per$V1,plot=F,breaks=0:ceiling(max(rur.tours.per$V1)))
dev.set(dev1)
plot(h.synth$breaks[2:length(h.synth$breaks)],cumsum(h.synth$counts)/sum(h.synth$counts),ylim=c(0,1),xlim=c(0,tail(h.nhts$breaks,1)),main="Synthetic")
grid()
dev.set(dev2)
plot(h.nhts$breaks[2:length(h.nhts$breaks)],cumsum(h.nhts$counts)/sum(h.nhts$counts),ylim=c(0,1),xlim=c(0,tail(h.nhts$breaks,1)),main="NHTS")
grid()
limiting.index <- min(length(h.nhts$counts),length(h.synth$counts),length(prob.weights))
plot(h.nhts$breaks[1:limiting.index],cumsum(h.nhts$counts[1:limiting.index])/sum(h.nhts$counts)-cumsum(h.synth$counts[1:limiting.index])/sum(h.synth$counts),ylim=c(-0.5,0.5),xlim=c(0,h.nhts$breaks[limiting.index]),type='b')
grid()

}


