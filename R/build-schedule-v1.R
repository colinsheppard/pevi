library(sas7bdat)
library(plyr)
library(ggplot2)
library(gtools)
roundC<-function(x,dg=1){ formatC(x,format='f',digits=dg) }


path.to.geatm <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
path.to.ctpp <- '~/Dropbox/serc/pev-colin/data/CTPP/'
path.to.nhts <- '~/Dropbox/serc/pev-colin/data/NHTS/'

# play with deriving a schedule based on trip numbers and pev penetration

if(!file.exists(paste(path.to.geatm,"od-aggregated.Rdata",sep=''))){
  od.24.new <- read.table(paste(path.to.geatm,"od_24_new.txt",sep=""),sep="",header=T)
  od.am.new <- read.table(paste(path.to.geatm,"od_am_new.txt",sep=""),sep="",header=T)
  od.pm.new <- read.table(paste(path.to.geatm,"od_pm_new.txt",sep=""),sep="",header=T)
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
  rur.tours$tours.left.in.journey <- ddply(rur.tours,.(journey.id),function(df){ data.frame(tours.left.in.journey=(nrow(df)-1):0) })$tours.left.in.journey
  # get rid of some bad data: NA for TOT_DWEL4, begin, end
  rur.tours <- rur.tours[!rur.tours$journey.id %in% rur.tours$journey.id[is.na(rur.tours$TOT_DWEL4)&rur.tours$tours.left.in.journey>0],]
  rur.tours$TOT_DWEL4[is.na(rur.tours$TOT_DWEL4)] <- 0
  rur.tours <- rur.tours[!rur.tours$journey.id %in% rur.tours$journey.id[is.na(rur.tours$end)],]
  rur.tours$index <- 1:nrow(rur.tours)

  # how many unique tours do we have? Answer 831317, 233820 rural
  #length(unique(paste(tours$HOUSEID,tours$PERSONID,tours$TOUR)))

  # What is the distribution of driving distance 
  ggplot(rur.tours,aes(x=TOT_MILS))+
  scale_x_continuous(name="Total Tour Distance (miles)")+
  opts(title = "2009 NHTS - Rural US POV") +
  geom_histogram()+
  facet_wrap(~TOURTYPE)

  # What does the departure distribution look like
  ggplot(rur.tours,aes(x=begin))+
  scale_x_continuous(name="Departure Time (hour)")+
  opts(title = "2009 NHTS - Rural US POV") +
  geom_histogram(binwidth=1)+
  facet_wrap(~TOURTYPE)

  # prepare OD data by condensing trip types into HW, HO, OW categories
  od.24.simp <- od.24.new[,c('from.taz.new','to.taz.new')]
  names(od.24.simp) <- c('from','to')
  od.24.simp$hw <- od.24.new[,'hbw']
  od.24.simp$ho <- apply(od.24.new[,c('hbshop','hbelem','hbuniv','hbro')],1,sum)
  od.24.simp$ow <- apply(od.24.new[,c('nhb','ix','xi','ee')],1,sum)
  od.am.simp <- od.am.new[,c('from.taz.new','to.taz.new')]
  names(od.am.simp) <- c('from','to')
  od.am.simp$hw <- od.am.new[,'hbw']
  od.am.simp$ho <- apply(od.am.new[,c('hbshop','hbelem','hbuniv','hbro')],1,sum)
  od.am.simp$ow <- apply(od.am.new[,c('nhb','ix','xi','ee')],1,sum)
  od.pm.simp <- od.pm.new[,c('from.taz.new','to.taz.new')]
  names(od.pm.simp) <- c('from','to')
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

  save(rur.tours,rur.by.type,ecdfs,epdfs,type.map,od.24.simp,od.am.simp,od.pm.simp,file=paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))
}else{
  load(file=paste(path.to.nhts,'data-preprocessed-for-scheduling.Rdata',sep=''))
}

# create the counts based on the specified pev penetration
for(pev.penetration in c(0.01,0.02,0.03,0.05,0.1,0.15,0.2,0.25)){
  for(replicate in 1:5){
    print(paste('Penetration ',pev.penetration,' replicate ',replicate,sep=''))
    od.counts <- cbind(od.24.simp[,c('from','to')],od.24.simp[,c('hw','ho','ow')]*pev.penetration)
    names(od.counts) <- c('from','to','hw.mean','ho.mean','ow.mean')
    # explode the od.counts frame to be on an hourly basis scaled by the epdfs
    od.counts <- data.frame(from    = rep(od.counts$from,each=24),
                            to      = rep(od.counts$to,each=24),
                            hour    = rep(0:23,nrow(od.counts)),
                            hw.mean = rep(od.counts$hw.mean,each=24)*rep(epdfs$hw,nrow(od.counts)),
                            ho.mean = rep(od.counts$ho.mean,each=24)*rep(epdfs$ho,nrow(od.counts)),
                            ow.mean = rep(od.counts$ow.mean,each=24)*rep(epdfs$ow,nrow(od.counts)))

    # do the random draws to convert the mean trips to discrete numbers
    od.counts[,c('hw','ho','ow')] <- t(apply(od.counts[,c('hw.mean','ho.mean','ow.mean')],1,function(row){ apply(as.matrix(row,byrow=T),1,rpois,n=1) }))
    # now sort by hour and shuffle otherwise
    od.counts <- ddply(od.counts,.(hour),function(df){df[sample(1:nrow(df)),]})
    od.counts[,c('hw.orig','ho.orig','ow.orig')] <- od.counts[,c('hw','ho','ow')]

    tazs <- unique(od.counts$from)

    # make the schedule
    od.counts[,c('hw','ho','ow')] <- od.counts[,c('hw.orig','ho.orig','ow.orig')]
    dist.thresh <- data.frame(under=c(3,seq(5,40,by=5),seq(50,100,by=25),seq(150,300,by=50)),
                              miles=c(3,rep(5,8),10,rep(25,2),rep(50,4))) # miles
    depart.thresh <- 3 # hours
    max.journey.len <- 15 # max(ddply(rur.tours,.(journey.id),nrow)$V1) # takes a long time to run
    schedule <- data.frame(driver=rep(NA,sum(od.counts[,c('hw','ho','ow')])))
    schedule$from   <- NA
    schedule$to     <- NA
    schedule$depart <- NA
    schedule$arrive <- NA
    schedule$type   <- NA
    cand.schedule <- schedule[1:max.journey.len,]
    driver.i <- 1
    num.trips <- 1

    for(od.i in 1:nrow(od.counts)){
      if(od.i%%1000 == 0)print(paste("progress: ",roundC(od.i/nrow(od.counts)*100,1),"%",sep=''))
      to.i <- od.counts$to[od.i]
      from.i <- od.counts$from[od.i]
      hour <- od.counts$hour[od.i]
      if(to.i == from.i){
        dists <- data.frame(miles=0,time=0)
      }else{
        dists <- dist[dist$to == to.i & dist$from == from.i,c('miles','time')]
      }
      for(type in c('hw','ho','ow')){
        if(od.counts[od.i,type]<=0)next
        # grab the indices of the tours that are close in time and distance, and in the case of home-based travel, starting from home
        if(type=='ow'){
          cands <- which( abs(rur.by.type[[type]]$TOT_MILS-dists$miles)<dist.thresh$miles[findInterval(rur.by.type[[type]]$TOT_MILS,dist.thresh$under)+1] & 
                          abs(rur.by.type[[type]]$begin-hour+0.5)<depart.thresh )
        }else{
          cands <- which( rur.by.type[[type]]$home.start &
                          abs(rur.by.type[[type]]$TOT_MILS-dists$miles)<dist.thresh$miles[findInterval(rur.by.type[[type]]$TOT_MILS,dist.thresh$under)+1] & 
                          abs(rur.by.type[[type]]$begin-hour+0.5)<depart.thresh )
        }
        if(length(cands)==0){
          print(paste('warning: no candidate tours found for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep='')) 
          next
        }else if(length(cands)==1){
          shuffled.cands <- cands
        }else{
          shuffled.cands <- sample(cands)
        }
        new.driver.i <- driver.i
        # loop through the cands in random order until a enough consistent journeys are found, to satisfy the demand for drivers 
        for(cand in shuffled.cands){
          use.cand <- T
          cand.schedule.i <- 1
          rur.tours.i <- rur.by.type[[type]]$index[cand]
          journey <- rur.tours[rur.tours.i:(rur.tours.i+rur.tours$tours.left.in.journey[rur.tours.i]), c('begin','end','TOT_DWEL4','journey.id','TOT_MILS','TOURTYPE')] 
          depart <- od.counts$hour[od.i]+runif(1)
          arrive <- depart + dists$time
          if(arrive >= 24){
            arrive <- arrive - 24
          }
          cand.schedule[cand.schedule.i,] <- data.frame(new.driver.i,from.i,to.i,depart,arrive,journey$TOURTYPE[1])
          cand.schedule.i <- cand.schedule.i + 1
          if(arrive > depart & nrow(journey) > 1){
            for(journey.i in 2:nrow(journey)){
              # first see if the dwell time puts us into tomorrow, if so we're done
              depart <- cand.schedule$arrive[cand.schedule.i-1] + journey$TOT_DWEL4[journey.i-1]/60
              if(depart >= 24)break

              # now find a new TAZ within dist.thresh of the NHTS schedule 
              new.hour <- as.integer(depart)
              new.from <- cand.schedule$to[cand.schedule.i-1]
              new.to.cands <- dist$to[dist$from==new.from & abs(dist$miles - journey$TOT_MILS[journey.i]) < dist.thresh$miles[findInterval(journey$TOT_MILS[journey.i],dist.thresh$under)+1]]
              new.to.cands <- od.counts$to[od.counts$from==new.from & od.counts$to %in% new.to.cands & od.counts$hour == new.hour & od.counts[,type] > 0]
              if(length(new.to.cands)==0){
                use.cand <- F
                break
              }else if(length(new.to.cands)==1){
                new.to <- new.to.cands
              }else{
                new.to <- sample(new.to.cands,1)
              }
              if(new.to == new.from){
                new.dists <- c(0,0)
              }else{
                new.dists <- dist[dist$to == new.to & dist$from == new.from,c('miles','time')]
              }
              arrive <- depart + dists$time
              if(arrive >= 24){
                arrive <- arrive - 24
              }
              cand.schedule[cand.schedule.i,] <- data.frame(new.driver.i,new.from,new.to,depart,arrive,journey$TOURTYPE[journey.i])
              cand.schedule.i <- cand.schedule.i + 1
            }
          }
          if(use.cand){
            schedule[num.trips:(num.trips+cand.schedule.i-2),] <- na.omit(cand.schedule)
            num.trips <- num.trips + cand.schedule.i - 1
            for(row.i in 1:(cand.schedule.i-1)){
              od.row <- which(od.counts$from==cand.schedule$from[row.i] & od.counts$to == cand.schedule$to[row.i] & od.counts$hour == as.integer(cand.schedule$depart[row.i]))
              if(row.i==1 & od.row != od.i)stop('stop!')
              od.counts[od.row,type] <- od.counts[od.row,type] - 1 
            }
            cand.schedule[,] <- NA 
            if(od.counts[od.i,type] <= 0)break
            new.driver.i <- new.driver.i + 1
          }
        }
        if(od.counts[od.i,type]>0){
          print(paste('no consistent journeys: scheduling one-legged journeys instead for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep='')) 
          for(cand in shuffled.cands){
            rur.tours.i <- rur.by.type[[type]]$index[cand]
            journey <- rur.tours[rur.tours.i:(rur.tours.i+rur.tours$tours.left.in.journey[rur.tours.i]), c('begin','end','TOT_DWEL4','journey.id','TOT_MILS','TOURTYPE')] 
            depart <- od.counts$hour[od.i]+runif(1)
            arrive <- depart + dists$time
            if(arrive >= 24){
              arrive <- arrive - 24
            }
            schedule[num.trips,] <- data.frame(new.driver.i,from.i,to.i,depart,arrive,journey$TOURTYPE[1])
            num.trips <- num.trips + 1
            for(row.i in 1:(cand.schedule.i-1)){
              od.row <- which(od.counts$from==cand.schedule$from[row.i] & od.counts$to == cand.schedule$to[row.i] & od.counts$hour == as.integer(cand.schedule$depart[row.i]))
              od.counts[od.row,type] <- od.counts[od.row,type] - 1 
            }
            if(od.counts[od.i,type] <= 0)break
            new.driver.i <- new.driver.i + 1
          }
          if(od.counts[od.i,type]>0){
            print(paste('not enough tours to satisfy demand for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep='')) 
          }
        }
        driver.i <- new.driver.i + 1
      }
    }

    # next steps / improvements
    # use the type of the tour to prioritize new.to TAZ, e.g. WH should take them home
    # figure out why there are too many 1-tour journeys getting into the schedule

    # post process the schedule to verify that it matches the OD and NHTS data

    schedule$type <- as.factor(schedule$type)
    levels(schedule$type) <- levels(rur.tours$TOURTYPE)

    #write.table(schedule,file=paste(path.to.geatm,"driver-schedule-pen0",pev.penetration*100,"-2011-03-29.txt",sep=''),sep='\t',row.names=F,quote=F)
    write.table(schedule,file=paste(path.to.geatm,"../../capstone-pev/NetLogo/Model files/driver-schedule-pen0",pev.penetration*100,"-rep",replicate,"-2011-04-09.txt",sep=''),sep='\t',row.names=F,quote=F)
  }
}

if(F){
# for now, we hack the schedule to get over the bug that's overproducing trips in certain hours and types
expected.hw.11 <- sum(od.counts[od.counts$hour==10,c('hw.mean')])
actual.hw.11 <- length(unique(subset(schedule,depart>10 & depart<11 & type=='HW')$driver)) 
for(i in 1:(actual.hw.11-expected.hw.11)){
  sub.inds <- which(schedule$depart>=10 & schedule$depart<11 & schedule$type=='HW')
  if(length(sub.inds)==1){
    remove.i <- sub.inds
  }else{
    remove.i <- sample(sub.inds,1)
  }
  if(remove.i==1){
    schedule <- schedule[2:remove.i,]
  }else if(remove.i==nrow(schedule)){
    schedule <- schedule[1:(remove.i-1),]
  }else{
    schedule <- schedule[c(1:(remove.i-1),(remove.i+1):nrow(schedule)),]
  }
}

dwell.times <- ddply(schedule,.(driver),function(df){ if(nrow(df)>1){ data.frame(dwell = df$depart[2:nrow(df)]-df$arrive[1:(nrow(df)-1)],type= df$type[1:(nrow(df)-1)]) }} )

dev.new()
dev1 <- dev.cur()
ggplot(dwell.times,aes(x=dwell))+
 scale_x_continuous(name="Total Tour Dwell Time (hours)")+
 opts(title = "Synthetic Schedule") +
 geom_histogram(binwidth=1) +
 facet_wrap(~type)

dev.new()
dev2 <- dev.cur()
ggplot(rur.tours,aes(x=TOT_DWEL4/60))+
 scale_x_continuous(name="Total Tour Dwell Time (hours)")+
 opts(title = "2009 NHTS - Rural US Subset") +
 geom_histogram(binwidth=1) +
 facet_wrap(~TOURTYPE)

dev.set(dev1)
ggplot(schedule,aes(x=depart))+
 scale_x_continuous(name="Departure Time (hours)")+
 opts(title = "Synthetic Schedule") +
 geom_histogram(binwidth=1) +
 facet_wrap(~type)

dev.set(dev2)
ggplot(rur.tours,aes(x=begin))+
 scale_x_continuous(name="Departure Time (hours)")+
 opts(title = "2009 NHTS - Rural US Subset") + 
 geom_histogram(binwidth=1) +
 facet_wrap(~TOURTYPE)

schedule$type.group <- NA
schedule$type.group[schedule$type %in% type.map[['hw']]] <- 'hw'
schedule$type.group[schedule$type %in% type.map[['ho']]] <- 'ho'
schedule$type.group[schedule$type %in% type.map[['ow']]] <- 'ow'
schedule.counts <- ddply(schedule,.(from,to,type.group),function(df){ data.frame(count=nrow(df)) } )

dev.set(dev1)
ggplot(schedule.counts,aes(x=type.group,y=count))+
 geom_bar(aes(colour=type.group))+
 opts(title = "Synthetic Schedule") +
 scale_x_discrete(name="Trip Type")+
 scale_y_continuous(name="# Trips")+
 facet_grid(from~to)

melted.od <- melt(od.24.simp,id=c('from','to'))
dev.set(dev2)
ggplot(melted.od,aes(x=variable,y=value))+
 geom_bar(aes(colour=variable))+
 opts(title = "GEATM - Counts by Trip Type") + 
 scale_x_discrete(name="Trip Type")+
 scale_y_continuous(name="# Trips")+
 facet_grid(from~to)

ddply(schedule.counts,.(type.group),function(df){sum(df$count)})
colSums(od.24.simp[,3:5])

synth.tours.per <- ddply(schedule,.(driver),nrow)
rur.tours.per <- ddply(rur.tours,.(journey.id),nrow)

dev.set(dev1)
ggplot(synth.tours.per,aes(x=V1))+
 geom_histogram()+
 opts(title = "Synthetic Schedule") +
 scale_x_continuous(name="Tours Per Driver")

dev.set(dev2)
ggplot(rur.tours.per,aes(x=V1))+
 geom_histogram()+
 opts(title = "GEATM") +
 scale_x_continuous(name="Tours Per Driver")

}

