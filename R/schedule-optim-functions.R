
evaluate.fitness <- function(ptx){
  if(!exists('cl')){
    print('starting new cluster')
    cl <- makeSOCKcluster(rep("localhost",num.cpu))
  }
  numrows <- nrow(ptx)
  breaks <- seq(1,numrows,by=ceiling(numrows/length(cl)))
  break.pairs <- list()
  for(i in 1:(length(breaks)-1)){
    break.pairs[[i]] <- c(breaks[i],breaks[i+1]-1)
  }
  break.pairs[[i+1]] <- c(breaks[i+1],numrows)

  results<-clusterEvalQ(cl,rm(list=ls()))
  clusterExport(cl,c('create.schedule.batch','pev.penetration','rur.tours','rur.by.type','rur.tours.per','epdfs','od.24.simp','dist','roundC','path.to.outputs'))
  clusterEvalQ(cl,library('plyr'))
  rm('results')
  results<-clusterApply(cl,break.pairs,fun='create.schedule.batch',ptx=ptx)
  results<-unlist(results)

  return(results)
}

stop.criteria <- function(fit,gen.num){
  if(all(fit==Inf))return(F)
  # true if all deviations of fitnesses from the min are less than threshold OR if we've hit max iterations
  return(all((fit-min(fit))/abs(min(fit))<stop.params$diff.from.best.threshold) | all(fit<0.025) | gen.num>=de.params$max.iter)
}

create.schedule.batch <- function(break.pair,ptx){
  ll<-break.pair[1]
  ul<-break.pair[2]
  break.pair.code <- paste("node ",paste(break.pair,collapse=","),":",sep='')
  batch.results <- array(NA,length(ll:ul))
  i <- 1

  for(ptx.i in ll:ul){
    scale.dist.thresh <- ptx[ptx.i,1]
    prob.weights.all <- c(ptx[ptx.i,2:ncol(ptx)],rep(1,20))
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

    inconsistent.schedule <- F

    for(od.i in 1:nrow(od.counts)){
    #for(od.i in 1:50){
      if(od.i%%2000 == 0)cat(paste("pev ",pev.penetration,", progress: ",roundC(od.i/nrow(od.counts)*100,1),"%",sep=''),file=paste(path.to.outputs,"/logfile.txt",sep=''),append=T,fill=T,labels=break.pair.code)
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
        cands <- which(F)
        depart.thresh.modified.used <- depart.thresh.modified
        dist.thresh.used <- dist.thresh
        while(length(cands)==0){
          # grab the indices of the tours that are close in time and distance, and in the case of home-based travel, starting from home
          if(type=='ow'){
            cands <- which( abs(rur.by.type[[type]]$TOT_MILS-dists$miles)<dist.thresh.used$miles[findInterval(rur.by.type[[type]]$TOT_MILS,dist.thresh.used$under)+1] & 
                            abs(rur.by.type[[type]]$begin-hour+0.5)<depart.thresh.modified.used )
          }else{
            cands <- which( rur.by.type[[type]]$home.start &
                            abs(rur.by.type[[type]]$TOT_MILS-dists$miles)<dist.thresh.used$miles[findInterval(rur.by.type[[type]]$TOT_MILS,dist.thresh.used$under)+1] & 
                            abs(rur.by.type[[type]]$begin-hour+0.5)<depart.thresh.modified.used )
          }
          if(length(cands)==0){
            #depart.thresh.modified.used <- depart.thresh.modified.used + 0.5 
            dist.thresh.used$miles <- dist.thresh.used$miles * 1.1
            #cat(paste("no candidates found, relaxing thresholds to",depart.thresh.modified.used,dist.thresh.used$miles[1]),file=paste(path.to.outputs,"/logfile.txt",sep=''),append=T,fill=T,labels=break.pair.code)
          }
        }
        if(length(cands)==1){
          shuffled.cands <- cands
        }else{
          shuffled.cands <- sample(cands,prob=prob.weights.all[rur.by.type[[type]]$tours.left.in.journey[cands]+1])
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
              if(row.i==1 & od.row != od.i){
                cat("Stopping becuase row.i is 1 and od.row != od.i",file=paste(path.to.outputs,"/logfile.txt",sep=''),append=T,fill=T,labels=break.pair.code)
                stop('stop!')
              }
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
          #print(paste('no consistent journeys: scheduling one-legged journeys instead for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep='')) 
          #cat(paste('no consistent journeys: scheduling one-legged journeys instead for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep=''),file=paste(path.to.outputs,"/logfile.txt",sep=''),append=T,fill=T,labels=break.pair.code) 
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
            #print(paste('not enough tours to satisfy demand for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep='')) 
            #cat(paste('not enough tours to satisfy demand for ',dists$miles,' miles at ',hour,' hour for type ',type,' and od.i ',od.i,sep=''),file=paste(path.to.outputs,"/logfile.txt",sep=''),append=T,fill=T,labels=break.pair.code) 
            inconsistent.schedule <- T
            break
          }
        }
      }
      if(inconsistent.schedule)break
    }
    if(inconsistent.schedule){
      batch.results[i] <- Inf
    }else{
      schedule$type <- as.factor(schedule$type)
      levels(schedule$type) <- levels(rur.tours$TOURTYPE)
      #return(schedule)

      # 2.37 is the target number of trips per driver
      batch.results[i] <- abs(2.37 - nrow(schedule)/length(unique(schedule$driver)))
    }

    #synth.tours.per <- ddply(schedule,.(driver),function(df){data.frame(ntours=nrow(df),end.time=df$arrive[nrow(df)])})
    #if(!exists('rur.tours.per')){ rur.tours.per <- ddply(rur.tours,.(journey.id),nrow) }

    #h.synth <- hist(synth.tours.per$ntours,plot=F,breaks=0:ceiling(max(synth.tours.per$ntours)))
    #h.nhts <- hist(rur.tours.per$V1,plot=F,breaks=0:ceiling(max(rur.tours.per$V1)))
    #limiting.index <- min(length(h.nhts$counts),length(h.synth$counts),length(ncol(ptx)))
    #batch.results[i] <- sum((cumsum(h.nhts$counts[1:limiting.index])/sum(h.nhts$counts)-cumsum(h.synth$counts[1:limiting.index])/sum(h.synth$counts))^2)
    i <- i+1 
  }
  cat(paste('results: ',paste(batch.results,collpase=","),sep=''),file=paste(path.to.outputs,"/logfile.txt",sep=''),append=T,fill=T,labels=break.pair.code) 
  return(batch.results)
}

### plot.ptx(ptx,dimensions=1:2)
# this will create an animation of the particles over time projected onto the 2 specified dimensions
plot.ptx <- function(ptx,num.gens=1,dimensions=1:2,decision.vars=decision.vars,quantize.range=1){
  #fit.range <- round(10*range(ptx[,'fitness',],na.rm=T))
  fits <- as.numeric(ptx[,'fitness',])
  fits <- fits[fits<Inf]
  fit.range <- round(quantize.range * range(fits,na.rm=T))
  cols <- diverge_hcl(diff(fit.range)+1)
  library(animation)
  oopt = ani.options(interval = 0.5, nmax = num.gens, ani.dev = png, withprompt=F,
    ani.type = "png",
    title = paste("DE Evolution Results"),
    description  = paste("Projection onto dimensions:",paste(apply(decision.vars[dimensions,c('mod.name','param.name')],1,paste,collapse="."),collapse=", "))
  )
  #xdim <- seq(50,100,by=0.1)
  #ydim <- seq(50,100,by=0.1)
  #ack <- array(NA,c(length(xdim),length(ydim)))
  #for(i in 1:length(xdim)){
    #for(j in 1:length(ydim)){
      #ack[i,j] <- ackley(c(xdim[i]-75,ydim[j]-75))
    #}
  #}
  ani.start()
  for(gen in 1:num.gens){
    #contour(xdim,ydim,ack,col="grey",
      #xlim=as.numeric(decision.vars[dimensions[1],c('lbound','ubound')]),
      #ylim=as.numeric(decision.vars[dimensions[2],c('lbound','ubound')]),
      #xlab=paste(decision.vars[1,c('mod.name','param.name')],collapse="."),
      #ylab=paste(decision.vars[2,c('mod.name','param.name')],collapse="."),
      #main=paste("Gen:",gen))
    col.ind <- round(quantize.range * ptx[,'fitness',gen])
    col.ind <- col.ind - round(min(fits,na.rm=T)) + 1
    col.ind[col.ind==Inf] <- length(cols)
    
    plot(ptx[,dimensions[1],gen],ptx[,dimensions[2],gen],
      col=cols[col.ind],
      bg=cols[col.ind],pch=23,
      xlim=as.numeric(decision.vars[dimensions[1],c('lbound','ubound')]),
      ylim=as.numeric(decision.vars[dimensions[2],c('lbound','ubound')]),
      xlab=paste(decision.vars[1,c('mod.name','param.name')],collapse="."),
      ylab=paste(decision.vars[2,c('mod.name','param.name')],collapse="."),
      main=paste("Gen:",gen))
    #points(75,75,pch="X",col="red")
  }
  ani.stop()
}
plot.ptx.alldim <- function(ptx,num.gens=1,decision.vars=decision.vars,quantize.range=1,width=1500,height=1500,no.ani=F,pt.cex=1){
  #fit.range <- round(10*range(ptx[,'fitness',],na.rm=T))
  fits <- as.numeric(ptx[,'fitness',])
  fits <- fits[fits<Inf]
  fit.range <- round(quantize.range * range(fits,na.rm=T))
  if(log(diff(fit.range),base=10)>2){ # implies more than 100 levels of color, let's increase the spacing
    quantize.range <- 10^(-(floor(log(diff(fit.range),base=10))-1))
    fit.range <- round(quantize.range * range(fits,na.rm=T))
  }
  cols <- diverge_hcl(diff(fit.range)+2)
  cols[length(cols)] <- "green"
  library(animation)
  oopt = ani.options(interval = 0.5, nmax = num.gens, ani.dev = png, withprompt=F,
    ani.type = "png",
    title = paste("DE Evolution Results"),
    description  = "",
    ani.width=width, ani.height=height
  )
  n <- nrow(decision.vars)
  layout.matrix <- matrix(1:n^2,n,byrow=T)
  saved.inf <- array(NA,c(dim(ptx)[1]*dim(ptx)[3],dim(ptx)[2]-1))
  
  if(!no.ani){
    ani.start()
    gen.start <- 1
  }else{
    gen.start <- num.gens
  }
  for(gen in gen.start:num.gens){
    col.ind <- round(quantize.range * ptx[,'fitness',gen])
    col.ind <- col.ind - fit.range[1] + 1
    n.inf <- sum(col.ind==Inf)
    if(n.inf>0){
      n.inf.saved <- sum(!is.na(saved.inf[,1]))
      saved.inf[(n.inf.saved+1):(n.inf.saved+n.inf),] <- ptx[col.ind==Inf,1:(dim(ptx)[2]-1),gen]
    }
    col.ind[col.ind==Inf] <- length(cols)
  
    nf<-layout(layout.matrix)
    for(dim.i in 1:n){
      if(dim.i > 1){
        for(skip in 1:(dim.i-1)){
          if(dim.i==n & skip==1){
            plot.new()
            text(0.5,0.5,gen,font=2,cex=2)      
          }else if(dim.i==n & skip==2){
            fit.to.plot <- ptx[,'fitness',gen]
            fit.to.plot[fit.to.plot==Inf] <- NA
            hist(ptx[,'fitness',gen],main="Fitness (Inf removed)")
          }else{
            plot.new()
          }
        }
      }
      dim.name <- paste(decision.vars[dim.i,'name'],collapse=".")
      brks <- seq(decision.vars[dim.i,'lbound'],decision.vars[dim.i,'ubound'],length.out=25)
      par(mar=c(3.1,4.1,3.1,3.1))
      #print(ptx[,dim.i,gen])
      hist(ptx[,dim.i,gen],xlab="",main=dim.name,breaks=brks)
      mtext(dim.name,side=4,font=2,cex=0.8)
      if(dim.i==n)next
      for(dim.i2 in (dim.i+1):n){
        par(mar=c(2.1,2.1,1.1,1.1))
        plot(ptx[,dim.i2,gen],ptx[,dim.i,gen],
          col=cols[col.ind],
          bg=cols[col.ind],pch=23,font.lab=2,cex.lab=1.3,
          cex=pt.cex,
          xlim=as.numeric(decision.vars[dim.i2,c('lbound','ubound')]),
          ylim=as.numeric(decision.vars[dim.i,c('lbound','ubound')]),
          main="")
        points(na.omit(saved.inf[,dim.i2]),na.omit(saved.inf[,dim.i]),col="lightgreen",pch='.',cex=3)
      }
    }
  }
  if(!no.ani){
    ani.stop()
  }
}
