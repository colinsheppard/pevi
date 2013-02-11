library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools'))

make.plots  <- F
num.processors <- 11
registerDoMC(num.processors)

#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
path.to.geatm <- paste(base.path,'pev-shared/data/GEATM-2020/',sep='')
path.to.google <- paste(base.path,'pev-shared/data/google-earth/',sep='')
path.to.shared.inputs <- paste(base.path,'pev-shared/data/inputs/driver-input-file/',sep='')
path.to.pevi <- paste(base.path,'pevi/',sep='')

path.to.outputs <- '~/Dropbox/serc/pev-colin/data/scheduler-optim/'
path.to.ctpp <- '~/Dropbox/serc/pev-colin/data/CTPP/'
path.to.nhts <- '~/Dropbox/serc/pev-colin/data/NHTS/'
path.to.plots <- '~/Dropbox/serc/pev-colin/plots/'

agg.taz <- readShapePoly(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights',sep=''),IDvar="ID")
load(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights-fieldnames.Rdata',sep=''))
names(agg.taz@data) <- c("SP_ID",taz.shp.fieldnames)
rm('taz') 

load(paste(path.to.geatm,"od-aggregated.Rdata",sep=''))
load(paste(path.to.nhts,"TripChaining/chntrp09.Rdata",sep=''))
load(paste(path.to.nhts,"HHV2PUB.Rdata",sep=''))
load(paste(path.to.nhts,"TripChaining/tour09.Rdata",sep=''))

if(!file.exists(paste(path.to.geatm,"od-aggregated.Rdata",sep=''))){
  # load od.24.weighted,od.am.weighted,od.pm.weighted
  load(,file=paste(path.to.geatm,'od_weighted.Rdata',sep=''))
  home.dist <- read.csv(paste(path.to.geatm,'home-distribution.csv',sep=''))
  dist <- read.csv(paste(path.to.geatm,"taz-dist-time.csv",sep=""))
  names(dist) <- c('from','to','miles','time','enroute','perf','perf1')
  taz.10 <- list() # either all neighbors within 10 miles or the 10 closest neighbors, whichever yields more, 54% of rural tours <= 10 miles
  for(taz.i in unique(dist$from)){
    within10 <- dist$to[dist$from==taz.i][which(dist$miles[dist$from==taz.i]<=10)]
    closest10 <- dist$to[dist$from==taz.i][order(dist$miles[dist$from==taz.i])][1:10]
    if(length(within10)>length(closest10)){
      taz.10[[taz.i]] <- within10
    }else{
      taz.10[[taz.i]] <- closest10
    }
  } 
  save(od.24.weighted,od.am.weighted,od.pm.weighted,dist,home.dist,taz.10,file=paste(path.to.geatm,"od-aggregated.Rdata",sep=''))
}else{
  load(paste(path.to.geatm,"od-aggregated.Rdata",sep=''))
}

if(!file.exists(paste(path.to.nhts,"TripChaining/chntrp09.Rdata",sep=''))){
  trips <- read.sas7bdat(paste(path.to.nhts,"TripChaining/chntrp09.sas7bdat",sep=''))
  save(trips,file=paste(path.to.nhts,"TripChaining/chntrp09.Rdata",sep=''))
}else{
  load(paste(path.to.nhts,"TripChaining/chntrp09.Rdata",sep=''))
}
if(!file.exists(paste(path.to.nhts,"HHV2PUB.Rdata",sep=''))){
  houses <- read.csv(paste(path.to.nhts,"HHV2PUB.CSV",sep=''))
  save(houses,file=paste(path.to.nhts,"HHV2PUB.Rdata",sep=''))
}else{
  load(paste(path.to.nhts,"HHV2PUB.Rdata",sep=''))
}
if(!file.exists(paste(path.to.nhts,"TripChaining/tour09.Rdata",sep=''))){
  tours <- read.sas7bdat(paste(path.to.nhts,"TripChaining/tour09.sas7bdat",sep=''))
  tours$TOT_DWEL2[is.nan(tours$TOT_DWEL2)] <- NA
  tours$BEGNTIME <- as.character(tours$BEGNTIME)
  tours$ENDTTIME <- as.character(tours$ENDTTIME)
  tours$HOUSEID  <- as.numeric(as.character(tours$HOUSEID))
  tours$PERSONID  <- as.numeric(as.character(tours$PERSONID))
  tours[,c('HHSTATE','URBRUR')] <- houses[match(tours$HOUSEID,houses$HOUSEID),c('HHSTATE','URBRUR')]
  # convert BEGNTIME and ENDTTIME into decimal hours
  tours$begin <- as.numeric(substr(tours$BEGNTIME,0,2)) + as.numeric(substr(tours$BEGNTIME,3,4))/60
  tours$end   <- as.numeric(substr(tours$ENDTTIME,0,2)) + as.numeric(substr(tours$ENDTTIME,3,4))/60
  # find all of the missing dwell times in the data set, for the rows that have missing data in TOT_DWELL2, 
  # for some reason the total dwell time wasn't estimated for tours
  indices <- 1:(nrow(tours)-1)
  matching.rows <- which(is.na(tours$TOT_DWEL2[indices]) & 
                      tours$HOUSEID[indices] == tours$HOUSEID[indices+1] &
                      tours$PERSONID[indices] == tours$PERSONID[indices+1] )
  tours$TOT_DWEL3[matching.rows] <- tours$TOT_DWEL[matching.rows] + (tours$begin[matching.rows+1] - tours$end[matching.rows])*60
  tours$TOT_DWEL3[which(tours$TOT_DWEL3<0)] <- tours$TOT_DWEL3[which(tours$TOT_DWEL3<0)] + 1440
  tours$TOT_DWEL4 <- tours$TOT_DWEL2
  tours$TOT_DWEL4[which(is.na(tours$TOT_DWEL2))] <- tours$TOT_DWEL3[which(is.na(tours$TOT_DWEL2))]
  tours$journey.id <- as.numeric(factor(paste(tours$HOUSEID,tours$PERSONID)))
  tours$home.start <- substr(tours$TOURTYPE,0,1)=="H"
  tours$home.end   <- substr(tours$TOURTYPE,2,2)=="H"
  save(tours,file=paste(path.to.nhts,"TripChaining/tour09.Rdata",sep=''))
}else{
  load(paste(path.to.nhts,"TripChaining/tour09.Rdata",sep=''))
}

if(make.plots){
  # explore the NHTS Trip Chaining Data
  #tours[1:20,c('HOUSEID','PERSONID','TOUR','TOURTYPE','STOPS','BEGNTIME','ENDTTIME','TOT_DWEL','TOT_DWEL2','TOT_DWEL3','DIST_M','TOT_CMIN','begin','end')]

  # What is the distribution of total dwell time (including time spent at destination) for each tour type
  ggplot(tours,aes(x=TOT_DWEL4/60))+
  scale_x_continuous(name="Total Tour Dwell Time (hours)")+
  opts(title = "2009 National Household Travel Survery - Trip Chaining Dataset") +
  geom_histogram()+
  facet_wrap(~TOURTYPE)

  ggplot(subset(tours,HHSTATE=="CA"),aes(x=TOT_DWEL4/60))+
  scale_x_continuous(name="Total Tour Dwell Time (hours)")+
  opts(title = "2009 NHTS - CA Subset") +
  geom_histogram()+
  facet_wrap(~TOURTYPE)

  ggplot(subset(tours,HHSTATE=="CA" & URBRUR==2),aes(x=TOT_DWEL4/60))+
  scale_x_continuous(name="Total Tour Dwell Time (hours)",limits=c(0,24))+
  opts(title = "2009 NHTS - Rural CA Subset") +
  geom_histogram()+
  facet_wrap(~TOURTYPE)

  ggplot(subset(tours,URBRUR==2),aes(x=TOT_DWEL4/60))+
  scale_x_continuous(name="Total Tour Dwell Time (hours)")+
  opts(title = "2009 NHTS - Rural US Subset") +
  geom_histogram()+
  facet_wrap(~TOURTYPE)

}

# based on comparing the various subsets, use US rural as the basis for the HEVI model, exclude non POV travel and long distance travel (>300 miles)
if(!file.exists(paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))){
  if(!file.exists(paste(path.to.nhts,'rur-tours.Rdata',sep=''))){
    rur.tours <- subset(tours,URBRUR==2 & PMT_POV>0 & TOT_MILS<300)
    rur.tours$tours.left.in.journey <- ddply(rur.tours,.(journey.id),function(df){ data.frame(tours.left.in.journey=(nrow(df)-1):0) },.parallel=T)$tours.left.in.journey
    # get rid of some bad data: NA for TOT_DWEL4, begin, end
    rur.tours <- rur.tours[!rur.tours$journey.id %in% rur.tours$journey.id[is.na(rur.tours$TOT_DWEL4)&rur.tours$tours.left.in.journey>0],]
    rur.tours$TOT_DWEL4[is.na(rur.tours$TOT_DWEL4)] <- 0
    rur.tours <- rur.tours[!rur.tours$journey.id %in% rur.tours$journey.id[is.na(rur.tours$end)],]
    rur.tours$index <- 1:nrow(rur.tours)
    rur.tours.per <- ddply(rur.tours,.(journey.id),nrow,.parallel=T) # takes a long time but is important to verifying that the schedule reflects the NHTS data

    # type mappings
    type.map <- list()
    type.map[['hw']] <- c('HW','WH')
    type.map[['ho']] <- c('HO','OH','HH')
    type.map[['ow']] <- c('OW','WO','OO','WW')
    type.map.rev <- melt(type.map)
    names(type.map.rev) <- c('tour.type','geatm.type')

    # add a geatm.type field to rur.tours
    rur.tours$geatm.type <- as.character(type.map.rev$geatm.type[match(rur.tours$TOURTYPE,type.map.rev$tour.type)])

    save(rur.tours,rur.tours.per,type.map,type.map.rev,file=paste(path.to.nhts,'rur-tours.Rdata',sep=''))
  }else{
    load(paste(path.to.nhts,'rur-tours.Rdata',sep=''))
  }

  # how many unique tours do we have? Answer 831317, 233820 rural
  #length(unique(paste(tours$HOUSEID,tours$PERSONID,tours$TOUR)))

  # What is the distribution of driving distance 
  #ggplot(rur.tours,aes(x=TOT_MILS))+
  #scale_x_continuous(name="Total Tour Distance (miles)")+
  #opts(title = "2009 NHTS - Rural US POV") +
  #geom_histogram()+
  #facet_wrap(~TOURTYPE)

  # What does the departure distribution look like
  #ggplot(rur.tours,aes(x=begin))+
  #scale_x_continuous(name="Departure Time (hour)")+
  #opts(title = "2009 NHTS - Rural US POV") +
  #geom_histogram(binwidth=1)+
  #facet_wrap(~TOURTYPE)

  # What is difference between max dwell time and total dwell time per journey, compare to total miles
  #dwell.to.len <- ddply(rur.tours,.(journey.id),function(df){ data.frame(max.dwell=max(df$TOT_DWEL4,na.rm=T),tot.dwell=sum(df$TOT_DWEL4,na.rm=T),tot.miles=sum(df$TOT_MILS)) },.parallel=T)

  # what is the distribution of the type of the final tour of each journey, answer 92.2% are to home
  #end.tourtype <- ddply(rur.tours,.(journey.id),function(df){ as.character(df$TOURTYPE[nrow(df)]) }) 
  #table(end.tourtype$V1)
  # sum(table(end.tourtype$V1)[c(1,4,7)]/sum(table(end.tourtype$V1)))

  # prepare OD data by condensing trip types into HW, HO, OW categories
  od.24.simp <- od.24.weighted[,c('from','to')]
  od.24.simp$hw <- od.24.weighted[,'hbw']
  od.24.simp$ho <- apply(od.24.weighted[,c('hbshop','hbelem','hbuniv','hbro')],1,sum)
  od.24.simp$ow <- apply(od.24.weighted[,c('nhb','ix','xi','ee')],1,sum)
  od.am.simp <- od.am.weighted[,c('from','to')]
  od.am.simp$hw <- od.am.weighted[,'hbw']
  od.am.simp$ho <- apply(od.am.weighted[,c('hbshop','hbelem','hbuniv','hbro')],1,sum)
  od.am.simp$ow <- apply(od.am.weighted[,c('nhb','ix','xi','ee')],1,sum)
  od.pm.simp <- od.pm.weighted[,c('from','to')]
  od.pm.simp$hw <- od.pm.weighted[,'hbw']
  od.pm.simp$ho <- apply(od.pm.weighted[,c('hbshop','hbelem','hbuniv','hbro')],1,sum)
  od.pm.simp$ow <- apply(od.pm.weighted[,c('nhb','ix','xi','ee')],1,sum)

  # by how much do we need to scale the AM / PM hours (AM 6:45-7:45, PM 16:30-17:30) to make the aggregate NHTS match GEATM
  od.24.tot <- sum(od.24.simp[,3:5])
  od.am.tot <- sum(od.am.simp[,3:5])
  od.pm.tot <- sum(od.pm.simp[,3:5])
  nh.24.tot <- nrow(rur.tours)
  nh.am.tot <- sum(rur.tours$begin >= 6.75 & rur.tours$begin < 7.75)
  nh.pm.tot <- sum(rur.tours$begin >= 16.5 & rur.tours$begin < 17.5)

  am.scale <- (od.am.tot/od.24.tot)/(nh.am.tot/nh.24.tot)
  pm.scale <- (od.pm.tot/od.24.tot)/(nh.pm.tot/nh.24.tot)
  offpeak.scale <- ((od.24.tot-od.am.tot-od.pm.tot)/od.24.tot )/((nh.24.tot - nh.am.tot - nh.pm.tot)/nh.24.tot)

  # subset rur.tours by type
  rur.by.type <- list()
  rur.by.type[['hw']] <- subset(rur.tours,TOURTYPE %in% type.map[['hw']])
  rur.by.type[['ho']] <- subset(rur.tours,TOURTYPE %in% type.map[['ho']])
  rur.by.type[['ow']] <- subset(rur.tours,TOURTYPE %in% type.map[['ow']])

  # now develop emprical departure CDF's for each grouping of tour types
  ecdfs <- list()
  ecdfs[['hw']] <- ecdf(rur.by.type[['hw']]$begin)
  ecdfs[['ho']] <- ecdf(rur.by.type[['ho']]$begin)
  ecdfs[['ow']] <- ecdf(rur.by.type[['ow']]$begin)

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

  save(rur.tours,rur.by.type,rur.tours.per,ecdfs,epdfs,type.map,type.map.rev,od.24.simp,od.am.simp,od.pm.simp,file=paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))
}else{
  load(file=paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))
}

pev.pens <- c(0.005,0.01,0.02,0.04)
replicate <- 1
source(paste(path.to.pevi,'R/create-schedule.R',sep=''))
#schedule <- create.schedule(0.001,1,0.922)
#print(paste(nrow(schedule)/length(unique(schedule$driver)),nrow(schedule),length(unique(schedule$driver))))
# see what fraction of drivers end at home?
# sum(ddply(schedule,.(driver),function(df){ df$to[nrow(df)]==df$home[1] })$V1)/length(unique(schedule$driver))

num.replicates <- 30
schedule.reps <- list()
for(pev.penetration in pev.pens){
  pev.pen.char <- roundC(pev.penetration,3)
  schedule.reps[[pev.pen.char]] <- list()
  for(replicate in 1:num.replicates){
    print(paste('Penetration ',pev.penetration,' replicate ',replicate,sep=''))
    schedule.reps[[pev.pen.char]][[as.character(replicate)]] <- create.schedule(pev.penetration,1)
    sched <- schedule.reps[[pev.pen.char]][[as.character(replicate)]][,c('driver','from','to','depart','home')]
    names(sched) <- c(';driver','from','to','depart','home')
    write.table(sched,file=paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep='\t',row.names=F,quote=F)
    save(schedule.reps,file=paste(path.to.outputs,'schedule-replicates-20130129.Rdata',sep=''))
  }
}
save(schedule.reps,file=paste(path.to.outputs,'schedule-replicates-20130129.Rdata',sep=''))

# fix the bug that causes drivers to have impossible itineraries
num.replicates <- 30
for(pev.penetration in pev.pens){
  pev.pen.char <- roundC(pev.penetration,3)
  for(replicate in 1:num.replicates){
    print(paste('Penetration ',pev.penetration,' replicate ',replicate,sep=''))
    sched <- read.table(file=paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep='\t',header=T)
    sched$ft <- paste(sched$from,sched$to)
    dist$ft <- paste(dist$from,dist$to)
    sched <- join(sched,dist,by="ft")
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
    write.table(sched,paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep="\t",row.names=F,quote=F)
  }
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
      if(compute.new) dwell.times[[pev.pen.char]] <- ddply(schedule,.(driver),function(df){ if(nrow(df)>1){ data.frame(dwell = df$depart[2:nrow(df)]-df$arrive[1:(nrow(df)-1)],type= df$type[1:(nrow(df)-1)],geatm.type= df$geatm.type[1:(nrow(df)-1)]) }} )
      for(type in unique(rur.tours$TOURTYPE)){
        ks.tests[ks.tests$penetration == pev.penetration & ks.tests$test == "dwell.time" & ks.tests$factor == "tour.type" & ks.tests$level == type, c('stat','p.value')] <- unlist(ks.test(dwell.times[[pev.pen.char]]$dwell[dwell.times[[pev.pen.char]]$type==type],rur.tours$TOT_DWEL4[rur.tours$TOURTYPE==type]/60)[c('statistic','p.value')])
      }
      if(make.plots){
        p <- ggplot(rbind(data.frame(dwell.times[[pev.pen.char]],set="SYNTH"),data.frame(driver=NA,dwell=rur.tours$TOT_DWEL4/60,type=rur.tours$TOURTYPE,geatm.type=rur.tours$geatm.type,set="NHTS")),
                 aes(x=dwell))+
          scale_x_continuous(name="Total Tour Dwell Time (hours)")+
          opts(title = paste("Dwell Times for Penetration",pev.pen.char)) +
          geom_bar(aes(fill=set,y=..density..), position="dodge") +
          facet_wrap(~type)
        ggsave(p,file=paste(path.to.plots,"dwell-times-pen",pev.pen.char,"-rep",replicate,".pdf",sep=''),width=8,height=8)

        p <- ggplot(rbind(data.frame(dwell.times[[pev.pen.char]],set="SYNTH"),data.frame(driver=NA,dwell=rur.tours$TOT_DWEL4/60,type=rur.tours$TOURTYPE,geatm.type=rur.tours$geatm.type,set="NHTS")),
                 aes(x=dwell))+
          scale_x_continuous(name="Total Tour Dwell Time (hours)")+
          opts(title = paste("Dwell Times for Penetration",pev.pen.char)) +
          geom_bar(aes(fill=set,y=..density..), position="dodge") +
          facet_wrap(~geatm.type)
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
        p <- ggplot(rbind(data.frame(schedule[,c('depart','geatm.type')],set="SYNTH"),data.frame(depart=rur.tours$begin,geatm.type=rur.tours$geatm.type,set="NHTS")),
                 aes(x=depart))+
          scale_x_continuous(name="Departure Time (hours)")+
          opts(title = paste("Departure Times for Penetration",pev.pen.char)) +
          geom_bar(aes(fill=set,y=..density..), position="dodge") +
          facet_wrap(~geatm.type)
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

schedule.counts <- ddply(schedule,.(from,to,geatm.type),function(df){ data.frame(count=nrow(df)) } )

#dev.set(dev1)
#ggplot(schedule.counts,aes(x=geatm.type,y=count))+
 #geom_bar(aes(colour=geatm.type))+
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

ddply(schedule.counts,.(geatm.type),function(df){sum(df$count)})
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


