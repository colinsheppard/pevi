create.schedule <- function(pev.penetration,scale.dist.thresh=1){
  environment(pick.driver) <- sys.frame(sys.nframe())
  environment(find.consistent.journey) <- sys.frame(sys.nframe())
   #for testing
   pev.penetration <- .001
   scale.dist.thresh <- 1
   set.seed(1)

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
  num.tazs <- length(tazs)
  
  # start the data structure to store a list of drivers available to be redispatched
  # the structure is nested lists indexed by [from.taz]][[count | drivers]]
  # and will then contain a data frame with 2 columns, driver.id and at.home 
  available.drivers <- list()
  available.drivers[[num.tazs]] <- list()
  for(taz.i in 1:num.tazs){
    num.counts <- sum(od.counts[od.counts$to==taz.i,c('hw','ho','ow')])
    if(num.counts < 1)num.counts <- 1
    available.drivers[[taz.i]][['drivers']] <- data.frame(driver.id=rep(NA,num.counts),at.home=NA,hour=NA)
    available.drivers[[taz.i]][['count']] <- 0
  }
  expected.num.drivers <- pev.penetration * 150e3 # 150 is based off of linear project of total vehicles assuming 10 years from now
  
  home.drivers <- list()
  home.drivers[[num.tazs]] <- list()
  driver.count <- 0
  for(taz.i in 1:num.tazs){
    n.drivers <- rpois(1,home.dist$frac.home[home.dist$taz==taz.i] * expected.num.drivers)
    home.drivers[[taz.i]] <- list(driver=c(),count=0)
    if(n.drivers>0){
      for(i in 1:n.drivers){
        driver.count <- driver.count + 1
        home.drivers[[taz.i]][['driver']] <- c(home.drivers[[taz.i]][['driver']],driver.count)
        home.drivers[[taz.i]][['count']] <- home.drivers[[taz.i]][['count']] + 1
      }
    }
  }
  tot.drivers <- sum(unlist(sapply(home.drivers,function(x){x[['count']]})))
  num.home.drivers <- tot.drivers
  print(paste("num.drivers",num.home.drivers,"num.trips",sum(od.counts[,c('hw','ho','ow')]),"trips.per.driver",sum(od.counts[,c('hw','ho','ow')])/num.home.drivers))

  # make the schedule
  od.counts[,c('hw','ho','ow')] <- od.counts[,c('hw.orig','ho.orig','ow.orig')]
  dist.thresh <- data.frame(under=c(3,seq(5,40,by=5),seq(50,100,by=25),seq(150,300,by=50)),
                            miles=c(3,rep(5,8),10,rep(25,2),rep(50,4))) # miles
  dist.thresh$miles <- dist.thresh$miles * scale.dist.thresh
  depart.thresh <- 3 # hours
  max.journey.len <- 15 # max(ddply(rur.tours,.(journey.id),nrow)$V1) # takes a long time to run
  schedule <- data.frame(driver=rep(NA,sum(od.counts[,c('hw','ho','ow')])))
  schedule$from   <- NA
  schedule$to     <- NA
  schedule$depart <- NA
  schedule$arrive <- NA
  schedule$type   <- NA
  schedule$geatm.type <- NA
  schedule$home <- NA
  
  cand.schedule <- schedule[1:max.journey.len,]
  num.trips <- 1
  recycle.drivers.thresh <- 0.0
  max.length.remaining <- max(rur.tours$tours.left.in.journey)

  # od.i <- which(apply(od.counts[,c('hw','ho','ow')],1,function(x){ all(x>0) }))[1]
  for(od.i in 1:nrow(od.counts)){
    if(od.i%%5000 == 0)print(paste("pev ",pev.penetration," rep ",replicate," progress: ",roundC(od.i/nrow(od.counts)*100,1),"%",sep=''))
    to.i <- od.counts$to[od.i]
    from.i <- od.counts$from[od.i]
    hour <- od.counts$hour[od.i]
    # we increase the working threshold at the boundaries (near hour 0 and 24) to account for the fact that we can't go off the edges
    # and for the fact that demand is lower at night so the pool get's small quickly
    if(hour < depart.thresh){
      depart.thresh.modified <- 2*depart.thresh - hour
    }else if(hour > 24 - depart.thresh){
      depart.thresh.modified <- 2*depart.thresh - (24-hour)
    }else{
      depart.thresh.modified <- depart.thresh
    }
    dists <- dist[dist$to == to.i & dist$from == from.i,c('miles','time')]
    for(type in c('hw','ho','ow')){
      if(od.counts[od.i,type]<=0)next
      # grab the indices of the tours that are close in time and distance, and in the case of home-based travel, starting from home
      if(type=='ow'){
        cands <- which( !rur.by.type[[type]]$home.start &
                        abs(rur.by.type[[type]]$TOT_MILS-dists$miles)<dist.thresh$miles[findInterval(rur.by.type[[type]]$TOT_MILS,dist.thresh$under)+1] & 
                        abs(rur.by.type[[type]]$begin-hour+0.5)<depart.thresh.modified )
      }else{
        cands <- which( rur.by.type[[type]]$home.start &
                        abs(rur.by.type[[type]]$TOT_MILS-dists$miles)<dist.thresh$miles[findInterval(rur.by.type[[type]]$TOT_MILS,dist.thresh$under)+1] & 
                        abs(rur.by.type[[type]]$begin-hour+0.5)<depart.thresh.modified )
      }
      if(length(cands)==0){
        stop(paste('error: no candidate tours found for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep=''))
        next
      }else if(length(cands)==1){
        shuffled.cands <- cands
      }else{
        shuffled.cands <- sample(cands)
      }
      driver <- pick.driver(type,from.i,to.i,hour)
      driver.i <- driver$driver.i
      driver.home <- driver$driver.home
      #print(paste(driver.i,driver.home))
      
      # loop through the cands in random order until a enough consistent journeys are found, to satisfy the demand for drivers 
      for(cand in shuffled.cands){
        use.cand <- T
        cand.schedule[,] <- NA 
        rur.tours.i <- rur.by.type[[type]]$index[cand]
        journey <- rur.tours[rur.tours.i:(rur.tours.i+rur.tours$tours.left.in.journey[rur.tours.i]), c('begin','end','TOT_DWEL4','journey.id','TOT_MILS','TOURTYPE','home.start','home.end','geatm.type')] 
        depart <- od.counts$hour[od.i]+runif(1)
        arrive <- depart + dists$time
        if(arrive >= 24){
          arrive <- arrive - 24
        }
        cand.schedule[1,] <- data.frame(driver.i,from.i,to.i,depart,arrive,journey$TOURTYPE[1],as.character(journey$geatm.type[1]),driver.home,stringsAsFactors=F)
        if(arrive >= depart & nrow(journey) > 1){
          new.cand.schedule <- find.consistent.journey(2,cand.schedule,journey,dists,dist.thresh)
          if(is.logical(new.cand.schedule)){
            use.cand <- F
          }else{
            cand.schedule <- new.cand.schedule
          }
        }
        if(use.cand){
          if(!is.na(driver$to.erase.inds)){
            schedule <- rbind(schedule[-driver$to.erase.inds,],schedule[10000001:(10000000+length(driver$to.erase.inds)),])
          }
          if(driver.i > tot.drivers) tot.drivers <- driver.i
          n.cand.trips <- length(na.omit(cand.schedule$driver))
          schedule[num.trips:(num.trips+n.cand.trips-1),] <- cand.schedule[1:n.cand.trips,]
          if(any(is.na(cand.schedule$home[1:n.cand.trips])))stop(paste('na in home',num.trips,(num.trips+n.cand.trips-1)))
          num.trips <- num.trips + n.cand.trips
          for(row.i in 1:n.cand.trips){
            od.row <- which(od.counts$from==cand.schedule$from[row.i] & od.counts$to == cand.schedule$to[row.i] & od.counts$hour == as.integer(cand.schedule$depart[row.i]))
            if(row.i==1 & od.row != od.i)stop('stop!')
            od.counts[od.row,cand.schedule$geatm.type[row.i]] <- od.counts[od.row,cand.schedule$geatm.type[row.i]] - 1 
          }
          if(driver.i %in% home.drivers[[driver.home]]$driver){
            home.drivers[[driver.home]]$driver <- home.drivers[[driver.home]]$driver[-which(home.drivers[[driver.home]]$driver==driver.i)]
            home.drivers[[driver.home]]$count <- home.drivers[[driver.home]]$count - 1
            num.home.drivers <- num.home.drivers - 1 
          }
          # is the driver already listed in available drivers?  if so remove before adding again to the appropriate place
          already.there.taz <- which(sapply(available.drivers,function(x){ any(x[['drivers']]$driver.id==driver.i) }))
          if(length(already.there.taz)>0){
            available.drivers[[already.there.taz]][['count']] <- available.drivers[[already.there.taz]][['count']] - 1
            available.driver.i <- which(available.drivers[[already.there.taz]][['drivers']]$driver.id == driver.i)
            if(available.driver.i < nrow(available.drivers[[already.there.taz]][['drivers']])){
              available.drivers[[already.there.taz]][['drivers']][available.driver.i:(nrow(available.drivers[[already.there.taz]][['drivers']])-1),] <- available.drivers[[already.there.taz]][['drivers']][(available.driver.i+1):nrow(available.drivers[[already.there.taz]][['drivers']]),]
            }
            available.drivers[[already.there.taz]][['drivers']][nrow(available.drivers[[already.there.taz]][['drivers']]),] <- NA
          }
          available.drivers[[cand.schedule$to[n.cand.trips]]][['count']] <- available.drivers[[cand.schedule$to[n.cand.trips]]][['count']] + 1
          available.drivers[[cand.schedule$to[n.cand.trips]]][['drivers']][available.drivers[[cand.schedule$to[n.cand.trips]]][['count']],] <- data.frame(driver.id=driver.i,
                                                                                        at.home=cand.schedule$to[n.cand.trips]==driver.home,
                                                                                        hour=ceiling(cand.schedule$arrive[n.cand.trips]))
          if(od.counts[od.i,type] <= 0)break
          driver <- pick.driver(type,from.i,to.i,hour)
          driver.i <- driver$driver.i
          driver.home <- driver$driver.home
        }
      }
      if(od.counts[od.i,type]>0){
        print(paste('no consistent journeys: scheduling one-legged journeys instead for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep='')) 
        stop()
      }
    } # end foreach type
  } # end foreach row in od
  schedule$type <- as.factor(schedule$type)
  levels(schedule$type) <- levels(rur.tours$TOURTYPE)
  return(na.omit(ddply(schedule[order(schedule$driver),],.(driver),function(df){ df[order(df$depart),] })))
}

#cand.schedule[2:nrow(cand.schedule),] <- NA

# vars in this function which must be altered on the global environment: journey, dist, dists, od.counts, dist.thresh
find.consistent.journey <- function(journey.i,cand.schedule,journey,dists,dist.thresh){
  #print(paste(journey.i, paste(na.omit(cand.schedule$from),na.omit(cand.schedule$to),collapse="---",sep=",")))
  if(journey.i > nrow(journey))return(cand.schedule)
  
  # first see if the dwell time puts us into tomorrow, if so we're done
  depart <- cand.schedule$arrive[journey.i-1] + journey$TOT_DWEL4[journey.i-1]/60
  if(depart >= 24)return(cand.schedule)

  # now find a new TAZ within dist.thresh of the NHTS schedule 
  new.hour <- as.integer(depart)
  new.from <- cand.schedule$to[journey.i-1]
  new.type <- journey$geatm.type[journey.i] 
  # if the journey tourtype is to home, we restrict the search to going to the home taz
  if(journey$home.end[journey.i]){
    new.to.cands <- dist$to[  dist$to==driver.home & dist$from==new.from & abs(dist$miles - journey$TOT_MILS[journey.i]) < dist.thresh$miles[findInterval(journey$TOT_MILS[journey.i],dist.thresh$under)+1] ]
  }else{
    # otherwise, look for any TAZ that is near enough
    new.to.cands <- dist$to[  dist$from==new.from & abs(dist$miles - journey$TOT_MILS[journey.i]) < dist.thresh$miles[findInterval(journey$TOT_MILS[journey.i],dist.thresh$under)+1] ]
  }
  new.to.cands <- od.counts$to[od.counts$from==new.from & od.counts$to %in% new.to.cands & od.counts$hour == new.hour & od.counts[,new.type] > 0]
  if(length(new.to.cands)==0){
    return(F)
  }else{
    if(length(new.to.cands)>1)new.to.cands <- sample(new.to.cands) # shuffle if needed
    for(new.to in new.to.cands){
      new.dists <- dist[dist$to == new.to & dist$from == new.from,c('miles','time')]
      arrive <- depart + dists$time
      if(arrive >= 24){
        arrive <- arrive - 24
      }
      cand.schedule[journey.i,] <- data.frame(driver.i,new.from,new.to,depart,arrive,journey$TOURTYPE[journey.i],new.type,driver.home,stringsAsFactors=F)
      new.cand.schedule <- find.consistent.journey(journey.i+1,cand.schedule,journey,dists,dist.thresh)
      if(!is.logical(new.cand.schedule))return(new.cand.schedule)
    }
    return(F)
  }
}

pick.driver <- function(type,from.i,to.i,hour){
  driver.i <- NA
  driver.home <- NA
  to.erase.inds <- NA
  if(num.home.drivers > 0){
    if(type == 'ow'){
      cand.homes <- taz.10[[from.i]]
      cand.home.counts <- sapply(cand.homes,function(x){ home.drivers[[x]]$count })
      if(sum(cand.home.counts)>0){
        #print("home driver other")
        driver.home <- sample.one(cand.homes[cand.home.counts>0])
        driver.i <- sample.one(home.drivers[[driver.home]]$driver)
      }
    }else{
      if(home.drivers[[from.i]]$count > 0){
        #print("home driver home-based")
        driver.home <- from.i
        driver.i <- sample.one(home.drivers[[driver.home]]$driver)
      }
    }
    if(is.na(driver.i)){
      # are there any available drivers, if yes, are any of them available at this time and 
      if(available.drivers[[from.i]][['count']] > 0 & any( available.drivers[[from.i]][['drivers']]$hour <= hour & (!available.drivers[[from.i]][['drivers']]$at.home | type!='ow'), na.rm=T)){
        #print("available driver used")
        available.driver.i <- sample.one(which(available.drivers[[from.i]][['drivers']]$hour <= hour & (!available.drivers[[from.i]][['drivers']]$at.home | type!='ow')))
        driver.i <- available.drivers[[from.i]][['drivers']]$driver.id[available.driver.i]
        driver.home <- subset(schedule,driver==driver.i)$home[1]
      }else{
        # can we canibalize the schedule, throw away a couple of trips of a driver that's otherwise consistent
        if(type == 'ow'){
          cand.trips <- which(schedule$to == from.i & schedule$arrive < hour & schedule$home != from.i)
        }else{
          cand.trips <- which(schedule$to == from.i & schedule$arrive < hour & schedule$home == from.i)
        }
        if(length(cand.trips)>0){
          print(paste('found a trip to cannibalize',type,from.i,to.i,hour))
          prev.trip <- sample.one(cand.trips)
          driver.i <- schedule$driver[prev.trip]
          driver.home <- schedule$home[prev.trip]
          to.erase.inds <- which(schedule$driver == schedule$driver[prev.trip] & schedule$arrive > schedule$arrive[prev.trip])
          if(length(to.erase.inds)==0){ to.erase.inds <- NA }
        }else{
          print(paste("no available drivers, creating one"))
          driver.i <- tot.drivers + 1
          if(type == 'ow'){
            driver.home <- sample(taz.10[[from.i]],1)
          }else{
            driver.home <- from.i
          }
        }
      }
    }
  }
  return( list(driver.i=driver.i,driver.home=driver.home, to.erase.inds=to.erase.inds) )
}

sample.one <- function(x){
  if(length(x)==1){
    return(x)
  }else{
    return(sample(x,1))
  }
}


## debugging stuff
if(F){
  
  order.sched <- na.omit(schedule[order(schedule$driver),])

  find.problem <- function(df){ 
    if(nrow(df)==1){ 
      return(F) 
    }else{
      return(any(df$to[1:(nrow(df)-1)] != df$from[2:nrow(df)]))
    }
  }

  inconsist.drivers <- ddply(order.sched,.(driver),find.problem)

}
