#load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools','animation','colorRamps','DAAG'))
load.libraries(c('plyr','reshape','maptools','animation','colorRamps','data.table'))

path.to.ani <- paste(pevi.shared,'data/UPSTATE/animation/',sep='')
path.to.outputs <- paste(path.to.ani,'outputs-',ani.code,'/',sep='')

source(paste(pevi.home,"R/gis-functions.R",sep=''))

load(pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))
load(pp(pevi.shared,'data/UPSTATE/od-converter.Rdata'))
agg.taz$new.id <- od.converter$new.id[match(agg.taz$agg.id,od.converter$old.id)]
agg.coords <- coordinates(agg.taz)
agg.taz$x <- agg.coords[,1]
agg.taz$y <- agg.coords[,2]


do.or.load(pp(pevi.shared,'data/UPSTATE/routing.Rdata'),function(){
  load.libraries('RPostgreSQL')
  dbuser<-"pev"
  dbpassword<-""
  dbname<-"upstate_route"
  dbhost<-"localhost"

  con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	
  route <- dbGetQuery(con,'SELECT from_taz,to_taz,x1 AS start_lon,y1 AS start_lat,x2 AS end_lon,y2 AS end_lat,route_order
                      FROM taz_route AS r 
                      LEFT JOIN ways AS w ON w.gid = r.ways_id')
  route <- data.table(route,key=c('from_taz','to_taz'))
  route[,dist:=c(0.01,sqrt((head(start_lon,-1)-tail(start_lon,-1))^2+(head(start_lat,-1)-tail(start_lat,-1))^2)),by=c('from_taz','to_taz')]
  list('route'=route)
})

######################################################################################################################################################
#  Everything above this comment should be run for any/all of the below animations
#  everything after is specific to each individual animation
######################################################################################################################################################

#####################################################################
## map optim
#####################################################################

do.or.load(pp(path.to.ani,'taz-map.Rdata'),function(){
  #simplify the map
  taz.map <- agg.taz
  taz.map$sparse <- agg.taz$new.id
  taz.map$sparse[grep("RED_",taz.map$name)] <- max(agg.taz$new.id)+1
  taz.map$sparse[grep("COT_",taz.map$name)] <- max(agg.taz$new.id)+2
  taz.map$sparse[grep("AND_",taz.map$name)] <- max(agg.taz$new.id)+3
  taz.map$sparse[grep("SHA_",taz.map$name)] <- max(agg.taz$new.id)+4
  taz.map <- unionSpatialPolygons(taz.map,taz.map$sparse)

  # now remove the bogus holes
  for(i in 1:length(taz.map@polygons)){
    #tmp.taz.map@polygons <- taz.map@polygons[[i]]
    to.keep <- which(unlist(lapply(slot(taz.map@polygons[[i]],"Polygons"),function(x){ !slot(x,'hole') })))
    new.polys <- list()
    new.i <- 1
    for(j in to.keep){
      new.polys[[new.i]] <- slot(taz.map@polygons[[i]],"Polygons")[[j]]
      new.i <- new.i + 1
    }
    slot(taz.map@polygons[[i]],"Polygons") <- new.polys
    #checkPolygonsHoles(taz.map@polygons[[i]])
  }
  list('taz.map'=taz.map)
})

do.or.load(pp(path.to.ani,'routes-for-ani.Rdata'),function(){
  route[,from_taz:=od.converter$new.id[match(from_taz,od.converter$old.id)]]
  route[,to_taz:=od.converter$new.id[match(to_taz,od.converter$old.id)]]
  setkey(route,'from_taz','to_taz','route_order')

  routes <- dlply(route,.(from_taz,to_taz),function(df){
    SpatialLines(list(Lines(Line(df[,c('start_lon','start_lat')]),1)))
  })
  setkey(route,'start_lat','start_lon','from_taz','to_taz')
  setkey(route,'start_lat','start_lon')
  route.non.overlapping <- unique(route)
  setkey(route.non.overlapping,'from_taz','to_taz','route_order')
  i<-0
  all.routes <- SpatialLines(dlply(route.non.overlapping,.(from_taz,to_taz),function(df){
    i <<- i+1
    Lines(Line(df[,c('start_lon','start_lat')]),i)
  }))
  list('routes'=routes,'all.routes'=all.routes)
})

do.or.load(pp(pevi.shared,'data/UPSTATE/shapefiles/ca-counties.Rdata'),function(){
  ca.counties <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/ca-counties'))
  list('ca.counties'=ca.counties)
})

load(pp(pevi.shared,'data/inputs/compare/upstate-animation/logs.Rdata'))

#my.colors <- function(x){
  #colorRampPalette(c(colors()[556],'black'))(10)[round(x*10)]
#}
home.cexes <- seq(1,4,length.out=40)

trip.locs <- function(trips,t,is.pain=F){
  ddply(trips,.(driver),function(df){
    if(df$origin[1]==df$destination[1]){
      sub <- data.frame(start_lon=agg.coords[which(agg.taz$new.id==df$origin[1]),1],start_lat=agg.coords[which(agg.taz$new.id==df$origin[1]),2])
    }else{
      sub <- data.frame(slot(slot(slot(routes[[pp(df$origin[1],df$destination[1],sep='.')]],'lines')[[1]],'Lines')[[1]],'coords'))
    }
    scale <- max(0,min(1,(t-df$time[1])/(df$end.time[1]-df$time[1])))
    ind <- round((nrow(sub)-1)*scale+1)
    data.frame(lon=sub$start_lon[ind],lat=sub$start_lat[ind],soc=ifelse(df$vehicle.type=="leaf",(df$begin.soc[1] - scale*(df$begin.soc[1]-df$end.soc[1])),1),type=df$vehicle.type,pain=ifelse(is.pain,df$pain.value,NA))
  })
}

ani.routes <- function(){
  my.grey <- colors()[190]
  my.blue <- colors()[600]

  for(t in seq(start.t,max.t,by=step.size)){
    par(bg = my.grey)
    plot(ca.counties[which(ca.counties$NAME%in%c('Siskiyou','Tehama'))],lwd=1.5)
    plot(taz.map,col=my.grey,lwd=1.5,add=T)
    plot(all.routes,col=colors()[350],lwd=4,add=T)
    # NOTE: something is off on the time, everything is 6 hours ahead, need to adjust this in results
    text(-123.64,40.9,labels=paste('Hour:',roundC(as.integer(t-6),0)),cex=2,col='white')
    trips <- subset(sched,time<=t & end.time>t)
    locs <- trip.locs(trips,t)
    points(locs$lon,locs$lat,col=c('black',my.grey)[locs$type],pch=c(15,17)[locs$type],cex=1)

    ch.pub.sub <- ddply(subset(ch,time <= t & time + duration >= t & charger.level>0),.(location),nrow)
    points(agg.coords[match(ch.pub.sub$location,agg.taz$new.id),1]-0.02,agg.coords[match(ch.pub.sub$location,agg.taz$new.id),2],col="green",cex=ch.pub.sub$V1,pch=16)
    points(agg.coords[match(ch.pub.sub$location,agg.taz$new.id),1]-0.02,agg.coords[match(ch.pub.sub$location,agg.taz$new.id),2],col="black",cex=ch.pub.sub$V1,pch=1,lwd=1.5)
    ch.home.sub <- ddply(subset(ch,time <= t & time + duration >= t & charger.level==0),.(location),nrow)
    points(agg.coords[match(ch.home.sub$location,agg.taz$new.id),1]+0.02,agg.coords[match(ch.home.sub$location,agg.taz$new.id),2],col=my.blue,cex=home.cexes[ch.home.sub$V1],pch=16)
    points(agg.coords[match(ch.home.sub$location,agg.taz$new.id),1]+0.02,agg.coords[match(ch.home.sub$location,agg.taz$new.id),2],col="black",cex=home.cexes[ch.home.sub$V1],pch=1,lwd=1.5)

    delay.locs <- trip.locs(unique(sched)[delay[time <= t & time + abs(pain.value) > t]][!is.na(time)],t,T)
    points(delay.locs$lon,delay.locs$lat,col='orange',pch=5,lwd=3,cex=(1.5 + 3*delay.locs$pain/5))
    strand.locs <- trip.locs(unique(sched)[strand[time <= t & time + 2 > t]][!is.na(time)],t,T)
    points(strand.locs$lon,strand.locs$lat,col='red',pch=5,lwd=3,cex=5)
  }
}

step.size = 2.5/60
evses <- c(0.5,1,2)
evses <- 0.5
start.t <- 6

for(evse.i in evses){
  tazs <- subset(logs[['tazs']],penetration == evse.i)
  sched <- data.table(subset(logs[['trip']],penetration == evse.i),key='driver')
  sched[,':='(origin=abs(sched$origin),destination=abs(sched$destination))]
  delay <- data.table(subset(logs[['pain']],penetration == evse.i & pain.type %in% c("delay")),key='driver')
  strand <- data.table(subset(logs[['pain']],penetration == evse.i & pain.type %in% c("stranded")),key='driver')
  ch <- subset(logs[['charging']],penetration == evse.i & location > 0)
  max.t <- max(ch$time + ch$duration)
  #max.t <- 6.5

  make.dir(pp(path.to.ani,"ani-",evse.i))
  ani.options(ffmpeg="/usr/local/bin/ffmpeg",outdir=pp(path.to.ani,"ani-",evse.i),ani.width=700,ani.height=800)
  saveVideo({
      par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, 
          cex.main = 1)
      ani.options(interval = 5/60, nmax = max.t/step.size+100)
  ani.routes()
  },video.name=paste("map-optim-pen-",evse.i,".mp4",sep=''),other.opts="-b 3200k -s 700x800",clean=T) # make res divisible by 16
  if(dev.cur()!=1)dev.off()
}

#####################################################################
## trips as barplot
#####################################################################

step.size <- 5/60

disttime <- read.csv(file=paste(path.to.geatm,'taz-dist-time.csv',sep=''))[,1:4]
names(disttime) <- c('from','to','miles','time')
sched <- read.table(file=paste(path.to.shared.inputs,"driver-schedule-pen2-rep1-20130129.txt",sep=''),sep='\t',header=T)
names(sched) <- c('driver','from','to','depart','home')
sched <- sched[order(sched$depart),]
sched <- join(sched,disttime)

ani.tours <- function(){
  max.dist <- max(location(max.t)$miles)
  n.drivers <- length(unique(scheds$driver))
  ch.levels <- ddply(ch,.(driver),function(df){ df$charger.level[1] })$V1
  for(t in seq(0,max.t,by=step.size)){
  #for(t in seq(17,20,by=5/60)){
    loc <- locs[[as.character(t)]]
    bp <- barplot(loc$miles[tour.order],xlim=c(0,max.dist),horiz=T,col=c('black',my.grey)[as.numeric(loc$type)],border=NA,space=0)
    points(tour.dist$miles[tour.order],bp,type='l',lty=3,col=my.grey,lwd=2)
    text(max.dist*0.75,max(bp),labels=paste('Hour:',roundC(t,0)))
    # mark the charging events
    ch.sub <- subset(ch,time <= t & time + duration >= t)
    are.charging <- (sorted.driver.ids %in% ch.sub$driver)
    are.charging.col.index <- 1 + ch.sub$charger.level[order(ch.sub$tour.order)]
    pn.sub <- subset(pain,time <= t & time + abs(pain.value) > t)
    are.in.pain <- (sorted.driver.ids %in% pn.sub$driver)
    are.in.pain.col.index <- 1 + as.numeric(pn.sub$pain.value >= 3)[order(pn.sub$tour.order)]
    pain.cex <- (1.5 + 3*pn.sub$pain.value/5)[order(pn.sub$tour.order)]
    points(loc$miles[tour.order][are.charging[tour.order]],
           bp[are.charging[tour.order]],pch=16,
           col=c(my.blue,'green','green','green')[are.charging.col.index],
           cex=c(1,0.5,1,1.5)[are.charging.col.index])
    points(loc$miles[tour.order][are.in.pain[tour.order]],
           bp[are.in.pain[tour.order]],pch=5,lwd=2,
           col=c('orange','red')[are.in.pain.col.index],
           cex=pain.cex)
  }
}

for(evse.i in evses){
  tazs <- read.csv(paste(path.to.outputs,"tazs-out-",evse.i,".csv",sep=''))
  tr <- read.csv(paste(path.to.outputs,"trip-out-",evse.i,".csv",sep=''))
  pain <- subset(read.csv(paste(path.to.outputs,"pain-out-",evse.i,".csv",sep='')),pain.type=="delay")
  ch <- read.csv(paste(path.to.outputs,"charging-out-",evse.i,".csv",sep=''))
  max.t <- max(ch$time + ch$duration)
  tr <- tr[,c('driver','origin','destination','time','vehicle.type')]
  names(tr) <- c('driver','from','to','depart','type')
  # sort by depart time
  tr <- tr[order(tr$depart),]
  tr <- join(tr,disttime)
  scheds <- ddply(tr,.(driver),function(df){
    res <- data.frame(time=rep(0,2+2*nrow(df)),dist=0)
    for(i in 1:nrow(df)){
      res$time[2+2*(i-1)] <- df$depart[i]
      res$time[3+2*(i-1)] <- df$depart[i]+df$time[i]
      res$dist[2+2*(i-1)] <- res$dist[1+2*(i-1)]
      res$dist[3+2*(i-1)] <- res$dist[2+2*(i-1)] + df$miles[i]
    }
    res[nrow(res),] <- c(max.t,res$dist[nrow(res)-1])
    res$type <- df$type[1]
    res
  })

  tour.dist <- ddply(sched,.(driver),function(df){ data.frame(miles=sum(df$miles)) })
  tour.order <- rev(order(tour.dist$miles))
  ch$tour.order <- match(ch$driver,tour.dist$driver[tour.order])
  pain$tour.order <- match(pain$driver,tour.dist$driver[tour.order])

  sorted.driver.ids <- sort(unique(scheds$driver))

  my.grey <- colors()[190]
  my.blue <- colors()[600]

  location <- function(time){ 
    ddply(scheds,.(driver),function(df){
      data.frame(miles=approx(df$time,df$dist,time)$y,type=df$type[1])
    })
  }

  if(!file.exists(paste(path.to.outputs,"locs-",evse.i,".Rdata",sep=''))){
    locs <- list()
    for(t in seq(0,max.t,by=step.size)){
      locs[[as.character(t)]] <- location(t)
    }
    save(locs,file=paste(path.to.outputs,"locs-",evse.i,".Rdata",sep=''))
  }else{
    load(paste(path.to.outputs,"locs-",evse.i,".Rdata",sep=''))
  }

  ani.options(ffmpeg="/usr/local/bin/ffmpeg",outdir=paste(path.to.ani,"ani-",ani.code,sep=''))
  saveVideo({
    par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, 
          cex.main = 1)
    ani.options(interval = 10/60, nmax = max.t/step.size+100)
    ani.tours()
  },video.name=paste("barplot-iter-",evse.i,".mov",sep=''),other.opts="-b 1500k -s 900x900",clean=T) # make res divisible by 16
}

