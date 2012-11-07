create.schedule <- function(pev.penetration,prob.weights,target.trips.per.driver,scale.dist.thresh=1){
  prob.weights.all <- c(prob.weights,rep(1,20))
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
  expected.num.drivers <- pev.penetration * 130e3

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
  cand.schedule <- schedule[1:max.journey.len,]
  driver.count <- 0
  num.trips <- 1
  recycle.drivers.thresh <- 0.0
  max.length.remaining <- max(rur.tours$tours.left.in.journey)

  for(od.i in 1:nrow(od.counts)){
    if(od.i%%5000 == 0)print(paste("pev ",pev.penetration," rep ",replicate," progress: ",roundC(od.i/nrow(od.counts)*100,1),"%",sep=''))
    to.i <- od.counts$to[od.i]
    from.i <- od.counts$from[od.i]
    home.taz <- from.i
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
    if(to.i == from.i){
      dists <- data.frame(miles=0.7777,time=0.08333) # these are the median values of distance and time from NHTS for all trips less than 1 mile in distance
                                                     # using 0 produced buggy results
    }else{
      dists <- dist[dist$to == to.i & dist$from == from.i,c('miles','time')]
    }
    for(type in c('hw','ho','ow')){
      if(od.counts[od.i,type]<=0)next
      # grab the indices of the tours that are close in time and distance, and in the case of home-based travel, starting from home
      if(type=='ow'){
        cands <- which( abs(rur.by.type[[type]]$TOT_MILS-dists$miles)<dist.thresh$miles[findInterval(rur.by.type[[type]]$TOT_MILS,dist.thresh$under)+1] & 
                        abs(rur.by.type[[type]]$begin-hour+0.5)<depart.thresh.modified )
      }else{
        cands <- which( rur.by.type[[type]]$home.start &
                        abs(rur.by.type[[type]]$TOT_MILS-dists$miles)<dist.thresh$miles[findInterval(rur.by.type[[type]]$TOT_MILS,dist.thresh$under)+1] & 
                        abs(rur.by.type[[type]]$begin-hour+0.5)<depart.thresh.modified )
      }
      if(length(cands)==0){
        print(paste('error: no candidate tours found for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep='')) 
        stop()
        next
      }else if(length(cands)==1){
        shuffled.cands <- cands
      }else{
        shuffled.cands <- sample(cands,prob=prob.weights.all[rur.by.type[[type]]$tours.left.in.journey[cands]+1])
        #shuffled.cands <- rev(ddply(data.frame(cand=cands,left=rur.by.type[[type]]$tours.left.in.journey[cands]),.(left),function(df){df[sample(1:nrow(df)),]})$cand)
      }
      # loop through the cands in random order until a enough consistent journeys are found, to satisfy the demand for drivers 
      for(cand in shuffled.cands){
        use.cand <- T
        cand.schedule.i <- 1
        cand.schedule[,] <- NA 
        rur.tours.i <- rur.by.type[[type]]$index[cand]
        journey <- rur.tours[rur.tours.i:(rur.tours.i+rur.tours$tours.left.in.journey[rur.tours.i]), c('begin','end','TOT_DWEL4','journey.id','TOT_MILS','TOURTYPE','home.start','home.end','geatm.type')] 
        depart <- od.counts$hour[od.i]+runif(1)
        arrive <- depart + dists$time
        if(arrive >= 24){
          arrive <- arrive - 24
        }
        driver.count <- driver.count + 1
        new.driver.i <- driver.count
        is.repeat.driver <- F
        if(driver.count >= recycle.drivers.thresh * expected.num.drivers){
          #print("count over expected")
          if(available.drivers[[from.i]][['count']] > 0){
            #print("available drivers found")
            if(any( available.drivers[[from.i]][['drivers']]$hour <= hour & journey$home.start[1] == available.drivers[[from.i]][['drivers']]$at.home, na.rm=T )){
              #print("available driver used")
              cand.repeat.driver.rows <- which(available.drivers[[from.i]][['drivers']]$hour <= hour & journey$home.start[1] == available.drivers[[from.i]][['drivers']]$at.home)
              if(length(cand.repeat.driver.rows)>1) cand.repeat.driver.rows <- sample(cand.repeat.driver.rows,1)
              new.driver.i <- available.drivers[[from.i]][['drivers']]$driver.id[cand.repeat.driver.rows]
              available.drivers[[from.i]][['count']] <- available.drivers[[from.i]][['count']] - 1
              if(cand.repeat.driver.rows < nrow(available.drivers[[from.i]][['drivers']])){
                available.drivers[[from.i]][['drivers']][cand.repeat.driver.rows:(nrow(available.drivers[[from.i]][['drivers']])-1),] <- available.drivers[[from.i]][['drivers']][(cand.repeat.driver.rows+1):nrow(available.drivers[[from.i]][['drivers']]),]
              }
              available.drivers[[from.i]][['drivers']][nrow(available.drivers[[from.i]][['drivers']]),] <- NA
              driver.count <- driver.count - 1
              is.repeat.driver <- T
            }
          }
        }
        cand.schedule[cand.schedule.i,] <- data.frame(new.driver.i,from.i,to.i,depart,arrive,journey$TOURTYPE[1],as.character(journey$geatm.type[1]),stringsAsFactors=F)
        cand.schedule.i <- cand.schedule.i + 1
        if(arrive >= depart & nrow(journey) > 1){
          for(journey.i in 2:nrow(journey)){
            # first see if the dwell time puts us into tomorrow, if so we're done
            depart <- cand.schedule$arrive[cand.schedule.i-1] + journey$TOT_DWEL4[journey.i-1]/60
            if(depart >= 24)break

            # now find a new TAZ within dist.thresh of the NHTS schedule 
            new.hour <- as.integer(depart)
            new.from <- cand.schedule$to[cand.schedule.i-1]
            new.type <- journey$geatm.type[journey.i] 
            # if the journey tourtype is to home, we restrict the search to going to the home taz
            if(journey$home.end[journey.i]){
              new.to.cands <- dist$to[  dist$to==home.taz & dist$from==new.from & abs(dist$miles - journey$TOT_MILS[journey.i]) < dist.thresh$miles[findInterval(journey$TOT_MILS[journey.i],dist.thresh$under)+1] ]
              new.to.cands <- od.counts$to[od.counts$from==new.from & od.counts$to %in% new.to.cands & od.counts$hour == new.hour & od.counts[,new.type] > 0]
            }else{
              # otherwise, look for any TAZ that is near enough
              new.to.cands <- dist$to[  dist$from==new.from & abs(dist$miles - journey$TOT_MILS[journey.i]) < dist.thresh$miles[findInterval(journey$TOT_MILS[journey.i],dist.thresh$under)+1] ]
              new.to.cands <- od.counts$to[od.counts$from==new.from & od.counts$to %in% new.to.cands & od.counts$hour == new.hour & od.counts[,new.type] > 0]
            }
            if(length(new.to.cands)==0){
              use.cand <- F
              break
            }else if(length(new.to.cands)==1){
              new.to <- new.to.cands
            }else{
              new.to <- sample(new.to.cands,1)
            }
            if(new.to == new.from){
              new.dists <- c(0.7777,0.08333)
            }else{
              new.dists <- dist[dist$to == new.to & dist$from == new.from,c('miles','time')]
            }
            arrive <- depart + dists$time
            if(arrive >= 24){
              arrive <- arrive - 24
            }
            cand.schedule[cand.schedule.i,] <- data.frame(new.driver.i,new.from,new.to,depart,arrive,journey$TOURTYPE[journey.i],new.type,stringsAsFactors=F)
            cand.schedule.i <- cand.schedule.i + 1
          }
        }
        if(use.cand){
          #tryCatch( schedule[num.trips:(num.trips+cand.schedule.i-2),] <- na.omit(cand.schedule), warning=browser )
          schedule[num.trips:(num.trips+cand.schedule.i-2),] <- na.omit(cand.schedule)
          num.trips <- num.trips + cand.schedule.i - 1
          for(row.i in 1:(cand.schedule.i-1)){
            od.row <- which(od.counts$from==cand.schedule$from[row.i] & od.counts$to == cand.schedule$to[row.i] & od.counts$hour == as.integer(cand.schedule$depart[row.i]))
            if(row.i==1 & od.row != od.i)stop('stop!')
            od.counts[od.row,cand.schedule$geatm.type[row.i]] <- od.counts[od.row,cand.schedule$geatm.type[row.i]] - 1 
          }
          available.drivers[[cand.schedule$to[cand.schedule.i-1]]][['count']] <- available.drivers[[cand.schedule$to[cand.schedule.i-1]]][['count']] + 1
          available.drivers[[cand.schedule$to[cand.schedule.i-1]]][['drivers']][available.drivers[[cand.schedule$to[cand.schedule.i-1]]][['count']],] <- data.frame(driver.id=new.driver.i,
                                                                                        at.home=cand.schedule$to[cand.schedule.i-1]==home.taz,
                                                                                        hour=ceiling(cand.schedule$arrive[cand.schedule.i-1]))
          if(od.counts[od.i,type] <= 0)break
        }else if(!is.repeat.driver){
          driver.count <- driver.count - 1
        }
      }
      if(od.counts[od.i,type]>0){
        print(paste('no consistent journeys: scheduling one-legged journeys instead for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep='')) 
        for(cand in shuffled.cands){
          rur.tours.i <- rur.by.type[[type]]$index[cand]
          journey <- rur.tours[rur.tours.i:(rur.tours.i+rur.tours$tours.left.in.journey[rur.tours.i]), c('begin','end','TOT_DWEL4','journey.id','TOT_MILS','TOURTYPE','geatm.type')] 
          depart <- od.counts$hour[od.i]+runif(1)
          arrive <- depart + dists$time
          if(arrive >= 24){
            arrive <- arrive - 24
          }
          driver.count <- driver.count + 1
          schedule[num.trips,] <- data.frame(driver.count,from.i,to.i,depart,arrive,journey$TOURTYPE[1],journey$geatm.type[1])
          num.trips <- num.trips + 1
          for(row.i in 1:(cand.schedule.i-1)){
            od.row <- which(od.counts$from==cand.schedule$from[row.i] & od.counts$to == cand.schedule$to[row.i] & od.counts$hour == as.integer(cand.schedule$depart[row.i]))
            od.counts[od.row,type] <- od.counts[od.row,type] - 1 
          }
          if(od.counts[od.i,type] <= 0)break
        }
        if(od.counts[od.i,type]>0){
          print(paste('not enough tours to satisfy demand for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep='')) 
          stop()
        }
      }
    }
  }
  schedule$type <- as.factor(schedule$type)
  levels(schedule$type) <- levels(rur.tours$TOURTYPE)
  return(schedule)

}

