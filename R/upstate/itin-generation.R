load.libraries(c('maptools','gpclib','plyr','stringr','ggplot2','doMC','reshape','data.table','DEoptim','colorRamps'))
gpclibPermit()
registerDoMC(num.cpu)

make.plots  <- T

path.to.outputs <- pp(pevi.shared,'~/data/inputs/driver-input-file/upstate-uncombined/')
path.to.plots <- pp(pevi.nondrop,'itin-plots/')

# load od.agg.all, agg.taz.data, time.distance
load(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-agg-tricounty.Rdata'))
# make sure time.distance does not have dups
setkey(time.distance,'from','to')
time.distance <- unique(time.distance)
time.distance$miles.int <- round(time.distance$miles)

# load NSSR subset of CHTS
load(file=pp(pevi.shared,'data/CHTS/nssr-subset.Rdata'))

# the trips purposes for from the SRTA model
purps <- c("ho","hs","hsc" "hw","oo","wo")

# Load/Create the home distribution and nearest neighbors list
if(!file.exists(pp(pevi.shared,"data/UPSTATE/demographics/frac-homes-and-nearest-10.Rdata"))){
  # Create the home distribution
  load(file=pp(pevi.shared,'data/UPSTATE/demographics/taz-dem.Rdata'))
  setkey(dem,'agg.id')
  srta.pops <- data.table(dem[!is.na(agg.id),list(agg.id=agg.id[1],pop=sum(Population)),by="agg.id"],key="agg.id")
  setkey(agg.taz.data,'agg.id')
  agg.taz.data <- srta.pops[agg.taz.data]
  agg.taz.data[,':='(population=ifelse(is.na(population),pop,population),pop=NULL,agg.id.1=NULL)]

  # Verify populations look right
  #load(pp(pevi.shared,"data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata"))
  #agg.taz$population <- agg.taz.data$population[match(agg.taz$agg.id,agg.taz.data$agg.id)]
  #source(pp(pevi.home,'R/gis-functions.R'))
  #c.map <- paste(map.color(agg.taz$population,blue2red(50)),'7F',sep='')
  #shp.to.kml(agg.taz,pp(pevi.shared,'data/UPSTATE/kml/AggregatedTAZsByPopulation.kml'),'Aggregated TAZs','','red',2,c.map,'shp.id','name',c('name','agg.id','population'))

  # Using population data by county from google, project to 2020 population
  pops <- data.frame(year=rep(seq(2002,2012,by=2),3),county=c(rep('sha',6),rep('teh',6),rep('sis',6)),pop=c(171.1,176.5,178.5,180.5,177.3,178.6,
                                                                                                            57,58.8,60.4,61.1,63.7,63.4,
                                                                                                            43.9,44.2,44.2,44.5,45.0,44.2))
  pops.2020 <- predict(lm('pop ~ county + year:county -1',pops),newdata=data.frame(year=2020,county=c('sha','teh','sis'))) * 1000
  names(pops.2020) <- c('sha','teh','sis')

  home.dist <- agg.taz.data[,list(agg.id=agg.id,frac.home=population/sum(population,na.rm=T))]
  setkey(od.agg.all,'from')
  tot.trips <- data.table(od.agg.all[,list(agg.id=from,tot=sum(tot)),by="from"],key='agg.id')
  tot.trips[,frac.trips:=tot/sum(tot)]
  home.dist <- tot.trips[home.dist]
  tot.frac.x <- sum(subset(home.dist,agg.id<0)$frac.trips)
  home.dist[,x:=agg.id<0]
  home.dist[,frac.home:=ifelse(x,frac.trips,frac.home*(1-tot.frac.x))]


  taz.10 <- list() # either all neighbors within 10 miles or the 10 closest neighbors, whichever yields more, 54% of rural tours <= 10 miles
  for(taz.i in unique(time.distance$from)){
    within10 <- time.distance$to[time.distance$from==taz.i][which(time.distance$miles[time.distance$from==taz.i]<=10)]
    closest10 <- time.distance$to[time.distance$from==taz.i][order(time.distance$miles[time.distance$from==taz.i])][1:10]
    if(length(within10)>length(closest10)){
      taz.10[[as.character(taz.i)]] <- within10
    }else{
      taz.10[[as.character(taz.i)]] <- closest10
    }
  } 
  save(home.dist,taz.10,pops.2020,file=pp(pevi.shared,"data/UPSTATE/demographics/frac-homes-and-nearest-10.Rdata"))
}else{
  load(file=pp(pevi.shared,"data/UPSTATE/demographics/frac-homes-and-nearest-10.Rdata"))
}

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
#for(pev.penetration in pev.pens){
  pev.penetration <- 0.005
  pev.pen.char <- roundC(pev.penetration,3)
  schedule.reps[[pev.pen.char]] <- list()
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
    write.table(sched,pp(pevi.shared,"data/inputs/driver-input-file/upstate-uncombined/driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20140122.txt",sep=''),sep="\t",row.names=F,quote=F)
  }
#}
save(schedule.reps,file=pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined-schedule-replicates-20140122.Rdata',sep=''))

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


