library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC'))

num.processors <- 10
registerDoMC(num.processors)

path.to.outputs <- '~/Dropbox/serc/pev-colin/data/scheduler-optim/'
path.to.old.outputs <- '~/Dropbox/serc/pev-colin/data/scheduler-optim-try1/'
path.to.geatm <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
path.to.ctpp <- '~/Dropbox/serc/pev-colin/data/CTPP/'
path.to.nhts <- '~/Dropbox/serc/pev-colin/data/NHTS/'
path.to.pevi <- '~/Dropbox/serc/pev-colin/pevi/'
path.to.plots <- '~/Dropbox/serc/pev-colin/plots/'

load(paste(path.to.geatm,"od-aggregated.Rdata",sep=''))
load(paste(path.to.nhts,"TripChaining/chntrp09.Rdata",sep=''))
load(paste(path.to.nhts,"HHV2PUB.Rdata",sep=''))
load(paste(path.to.nhts,"TripChaining/tour09.Rdata",sep=''))
load(file=paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))


# play with deriving a schedule based on trip numbers and pev penetration

if(!file.exists(paste(path.to.geatm,"od-aggregated.Rdata",sep=''))){
  od.24.new <- read.csv(paste(path.to.geatm,"od_24_new.csv",sep=""))
  od.am.new <- read.csv(paste(path.to.geatm,"od_am_new.csv",sep=""))
  od.pm.new <- read.csv(paste(path.to.geatm,"od_pm_new.csv",sep=""))
  dist <- read.table(paste(path.to.geatm,"taz-dist-time.txt",sep=""),sep="\t",header=T)
  names(dist) <- c('from','to','demand','miles','time')
  dist$time <- dist$time/60
  save(od.24.new,od.am.new,od.pm.new,dist,file=paste(path.to.geatm,"od-aggregated.Rdata",sep=''))
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

# based on comparing the various subsets, use US rural as the basis for the HEVI model, exclude non POV travel and long distance travel (>300 miles)
if(!file.exists(paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))){
  rur.tours <- subset(tours,URBRUR==2 & PMT_POV>0 & TOT_MILS<300)
  rur.tours$tours.left.in.journey <- ddply(rur.tours,.(journey.id),function(df){ data.frame(tours.left.in.journey=(nrow(df)-1):0) },.parallel=T)$tours.left.in.journey
  # get rid of some bad data: NA for TOT_DWEL4, begin, end
  rur.tours <- rur.tours[!rur.tours$journey.id %in% rur.tours$journey.id[is.na(rur.tours$TOT_DWEL4)&rur.tours$tours.left.in.journey>0],]
  rur.tours$TOT_DWEL4[is.na(rur.tours$TOT_DWEL4)] <- 0
  rur.tours <- rur.tours[!rur.tours$journey.id %in% rur.tours$journey.id[is.na(rur.tours$end)],]
  rur.tours$index <- 1:nrow(rur.tours)

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

  # prepare OD data by condensing trip types into HW, HO, OW categories
  od.24.simp <- od.24.new[,c('from','to')]
  od.24.simp$hw <- od.24.new[,'hbw']
  od.24.simp$ho <- apply(od.24.new[,c('hbshop','hbelem','hbuniv','hbro')],1,sum)
  od.24.simp$ow <- apply(od.24.new[,c('nhb','ix','xi','ee')],1,sum)
  od.am.simp <- od.am.new[,c('from','to')]
  od.am.simp$hw <- od.am.new[,'hbw']
  od.am.simp$ho <- apply(od.am.new[,c('hbshop','hbelem','hbuniv','hbro')],1,sum)
  od.am.simp$ow <- apply(od.am.new[,c('nhb','ix','xi','ee')],1,sum)
  od.pm.simp <- od.pm.new[,c('from','to')]
  od.pm.simp$hw <- od.pm.new[,'hbw']
  od.pm.simp$ho <- apply(od.pm.new[,c('hbshop','hbelem','hbuniv','hbro')],1,sum)
  od.pm.simp$ow <- apply(od.pm.new[,c('nhb','ix','xi','ee')],1,sum)

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

  # type mappings
  type.map <- list()
  type.map[['hw']] <- c('HW','WH')
  type.map[['ho']] <- c('HO','OH','HH')
  type.map[['ow']] <- c('OW','WO','OO','WW')
  type.map.rev <- melt(type.map)
  names(type.map.rev) <- c('tour.type','geatm.type')

  # add a geatm.type field to rur.tours
  rur.tours$geatm.type <- as.character(type.map.rev$geatm.type[match(rur.tours$TOURTYPE,type.map.rev$tour.type)])

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

  rur.tours.per <- ddply(rur.tours,.(journey.id),nrow,.parallel=T) # takes a long time but is important to verifying that the schedule reflects the NHTS data

  # the following were found using an optimization that attempted to match the NHTS distribution of tours per driver to the synthetic schedulel 
  #prob.weights <- list()
  #prob.weights[['0.03']] <- c(1e-6,0.66,1,0.79,0.93,0.40)
  #prob.weights[['0.05']] <- c(0.07,0.9,0.5,0.17,0.4,0.5)

  save(rur.tours,rur.by.type,rur.tours.per,ecdfs,epdfs,type.map,type.map.rev,od.24.simp,od.am.simp,od.pm.simp,prob.weights,file=paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))
}else{
  load(file=paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))
}

source(paste(path.to.pevi,'R/create-schedule.R',sep=''))
pev.pens <- c(0.005,0.01,0.02,0.04,0.08)
replicate <- 1
prob.weights <- data.frame(pen=pev.pens,'0'=NA,'1'=NA,'2'=NA,'3'=NA,'4'=NA,'5'=NA)
schedule <- list()

if(!file.exists(paste(path.to.outputs,'schedules-20120425.Rdata',sep=''))){
  # collect the the probability weights determined by the optimization, 
  # use them to create a single schedule for each penetration
  for(pev.penetration in pev.pens){
      pev.pen.char <- roundC(pev.penetration,2)
      print(paste('Penetration ',pev.penetration,sep=''))

      load(paste(path.to.outputs,"0saved-state-pen",pev.penetration*100,".Rdata",sep=''))
      prob.weights[prob.weights$pen == pev.penetration, 2:7] <- apply(all.ptx[,1:6,gen.num-1],2,mean)
      
      schedule[[pev.pen.char]] <- create.schedule(pev.penetration,prob.weights[prob.weights$pen == pev.penetration,2:7])
  }
  save(schedule,prob.weights,file=paste(path.to.outputs,'schedules-20120425.Rdata',sep=''))
}else{
  load(file=paste(path.to.outputs,'schedules-20120425.Rdata',sep=''))
}

for(pev.penetration in pev.pens){
  pev.pen.char <- roundC(pev.penetration,2)
  print(pev.pen.char)
  print(nrow(schedule[[pev.pen.char]])/length(unique(schedule[[pev.pen.char]]$driver)))
}

#[1] "0.01"
#[1] 2.406499
#[1] "0.02"
#[1] 2.513825
#[1] "0.03"
#[1] 2.548458
#[1] "0.04"
#[1] 2.580726
#[1] "0.05"
#[1] 2.677658
#[1] "0.10"
#[1] 2.781892
#[1] "0.15"
#[1] 2.850939
#[1] "0.20"
#[1] 2.986366
#[1] "0.25"
#[1] 3.003536



# analyze and plot the schedules, compare them to NHTS and GEATM

ks.tests <- data.frame(penetration=pev.pens,test='tours.per.driver',factor=NA,level=NA,stat=NA,p.value=NA)
ks.tests <- rbind(ks.tests,data.frame(penetration=rep(pev.pens,each=length(unique(rur.tours$TOURTYPE))),test='dwell.time',factor='tour.type',level=rep(unique(rur.tours$TOURTYPE),length(pev.pens)),stat=NA,p.value=NA))
ks.tests <- rbind(ks.tests,data.frame(penetration=rep(pev.pens,each=length(unique(rur.tours$TOURTYPE))),test='departure.time',factor='tour.type',level=rep(unique(rur.tours$TOURTYPE),length(pev.pens)),stat=NA,p.value=NA))
num.vehicles <- data.frame(penetration=pev.pens,expected=NA,scheduled=NA)
synth.tours.per <- list()
dwell.times     <- list()
compute.new <- T
make.plots  <- T

if(make.plots){
  #dev.new()
  #dev.tours.per <- dev.cur()
  #plot(ecdf(rur.tours.per$V1),main="Empirical CDF of # Trips Per Driver")
}

for(pev.penetration in pev.pens){
  pev.pen.char <- roundC(pev.penetration,2)
  pen.i <- which(pev.penetration == pev.pens)+1

  print(paste('Penetration ',pev.penetration,sep=''))
  num.vehicles[num.vehicles$penetration==pev.penetration,c('expected','scheduled')] <- c(pev.penetration * 130e3,max(schedule[[pev.pen.char]]$driver))

  # TOURS PER DRIVER
  # assumes we have already computed rur.tours.per
  if(compute.new) synth.tours.per[[pev.pen.char]] <- ddply(schedule[[pev.pen.char]],.(driver),function(df){data.frame(ntours=nrow(df),end.time=df$arrive[nrow(df)])})
  if(make.plots){
    #dev.set(dev.tours.per)
    #plot(ecdf(synth.tours.per[[pev.pen.char]]$ntours),add=T,col=pen.i,pch=pen.i)
  }
  ks.tests[ks.tests$penetration == pev.penetration & ks.tests$test == "tours.per.driver", c('stat','p.value')] <- unlist(ks.test(synth.tours.per[[pev.pen.char]]$ntours,rur.tours.per$V1)[c('statistic','p.value')])

  # DWELL TIME
  if(compute.new) dwell.times[[pev.pen.char]] <- ddply(schedule[[pev.pen.char]],.(driver),function(df){ if(nrow(df)>1){ data.frame(dwell = df$depart[2:nrow(df)]-df$arrive[1:(nrow(df)-1)],type= df$type[1:(nrow(df)-1)],geatm.type= df$geatm.type[1:(nrow(df)-1)]) }} )
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
    ggsave(p,file=paste(path.to.plots,"dwell-times-pen",pev.pen.char,".pdf",sep=''),width=8,height=8)

    p <- ggplot(rbind(data.frame(dwell.times[[pev.pen.char]],set="SYNTH"),data.frame(driver=NA,dwell=rur.tours$TOT_DWEL4/60,type=rur.tours$TOURTYPE,geatm.type=rur.tours$geatm.type,set="NHTS")),
             aes(x=dwell))+
      scale_x_continuous(name="Total Tour Dwell Time (hours)")+
      opts(title = paste("Dwell Times for Penetration",pev.pen.char)) +
      geom_bar(aes(fill=set,y=..density..), position="dodge") +
      facet_wrap(~geatm.type)
    ggsave(p,file=paste(path.to.plots,"dwell-times-by-geatm-pen",pev.pen.char,".pdf",sep=''),width=8,height=8)
  }

  # DEPARTURE TIME
  for(type in unique(rur.tours$TOURTYPE)){
    ks.tests[ks.tests$penetration == pev.penetration & ks.tests$test == "departure.time" & ks.tests$factor == "tour.type" & ks.tests$level == type, c('stat','p.value')] <- unlist(ks.test(schedule[[pev.pen.char]]$depart[schedule[[pev.pen.char]]$type==type],rur.tours$begin[rur.tours$TOURTYPE==type])[c('statistic','p.value')])
  }
  if(make.plots){
    p <- ggplot(rbind(data.frame(schedule[[pev.pen.char]][,c('depart','type')],set="SYNTH"),data.frame(depart=rur.tours$begin,type=rur.tours$TOURTYPE,set="NHTS")),
             aes(x=depart))+
      scale_x_continuous(name="Departure Time (hours)")+
      opts(title = paste("Departure Times for Penetration",pev.pen.char)) +
      geom_bar(aes(fill=set,y=..density..), position="dodge") +
      facet_wrap(~type)
    ggsave(p,file=paste(path.to.plots,"departure-times-pen",pev.pen.char,".pdf",sep=''),width=8,height=8)
    p <- ggplot(rbind(data.frame(schedule[[pev.pen.char]][,c('depart','geatm.type')],set="SYNTH"),data.frame(depart=rur.tours$begin,geatm.type=rur.tours$geatm.type,set="NHTS")),
             aes(x=depart))+
      scale_x_continuous(name="Departure Time (hours)")+
      opts(title = paste("Departure Times for Penetration",pev.pen.char)) +
      geom_bar(aes(fill=set,y=..density..), position="dodge") +
      facet_wrap(~geatm.type)
    ggsave(p,file=paste(path.to.plots,"departure-times-by-geatm-pen",pev.pen.char,".pdf",sep=''),width=8,height=8)
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


# Generate replicates of each penetration level and save as schedule file for use in simulation
num.replicates <- 5
schedule.reps <- list()
for(pev.penetration in pev.pens){
  pev.pen.char <- roundC(pev.penetration,2)
  schedule.reps[[pev.pen.char]] <- list()
  for(replicate in 1:num.replicates){
    print(paste('Penetration ',pev.penetration,' replicate ',replicate,sep=''))

    schedule.reps[[pev.pen.char]][[as.character(replicate)]] <- create.schedule(pev.penetration,prob.weights[prob.weights$pen == pev.penetration,2:7])

    write.table(schedule[[pev.pen.char]],file=paste(path.to.pevi,"inputs/driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20120425.txt",sep=''),sep='\t',row.names=F,quote=F)
  }
}
save(schedule.reps,file=paste(path.to.outputs,'schedule-replicates-20120425.Rdata',sep=''))

if(F){

# plot the results of the optimizations
#ggplot(melt(prob.weights,id.vars='pen'),aes(x=as.numeric(variable)-1,y=value))+geom_point(aes(colour=as.factor(pen)))+geom_line(aes(colour=as.factor(pen)))

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


