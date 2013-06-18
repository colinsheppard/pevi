library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools','animation','colorRamps','DAAG'))

base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'

ani.code <- '50-50'
ani.code <- 'base-fixed'

if(ani.code=="50-50"){
  evses <- c(1,15,30,45)
}else{
  evses <- 1:5 # which correspond to iters 1 20 24 28 36 or the break points between each pen
}

path.to.ani <- paste(base.path,'data/animations/',sep='')
path.to.pevi.outputs <- paste(path.to.ani,'outputs-',ani.code,'/',sep='')
path.to.geatm <- paste(base.path,'pev-shared/data/GEATM-2020/',sep='')
path.to.google <- paste(base.path,'pev-shared/data/google-earth/',sep='')
path.to.shared.inputs <- paste(base.path,'pev-shared/data/inputs/driver-input-file/',sep='')
path.to.pevi <- paste(base.path,'pevi/',sep='')

source(paste(path.to.pevi,"R/gis-functions.R",sep=''))

agg.taz <- readShapePoly(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights',sep=''),IDvar="ID")
load(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights-fieldnames.Rdata',sep=''))
names(agg.taz@data) <- c("SP_ID",taz.shp.fieldnames)
rm('taz') 

# load od.24.weighted,od.am.weighted,od.pm.weighted
load(paste(path.to.pevi,'inputs/routing-corrected.Rdata',sep=''))

route.ordered.sub <- route.ordered[,c('from_taz','to_taz','start_lon','start_lat','end_lon','end_lat')]

######################################################################################################################################################
#  Everything above this comment should be run for any/all of the below animations
#  everything after is specific to each individual animation
######################################################################################################################################################

pev.penetration <- 0.01
replicate <- 1
pev.pen.char <- roundC(pev.penetration,3)
sched <- read.table(file=paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep='\t',header=T)
names(sched) <- c('driver','from','to','depart','home')
sched$ft <- paste(sched$from,sched$to)

tazs <- read.csv(paste("~/Documents/serc/pev/tazs-out.csv",sep=''))
tazs <- subset(tazs,time<=32)

# sort by depart time
sched <- sched[order(sched$depart),]

# for now subset this bad boy
#sched <- sched[1:20,]

agg.taz.coords <- coordinates(agg.taz)
home.cexes <- seq(1,4,length.out=40)

path.to.plots <- '~/Dropbox/serc/pev-colin/plots/'
if(!file.exists(paste(path.to.plots,'route-inds-for-animation.Rdata',sep=''))){
  rt.inds <- list()
  for(from.i in 1:52){
    rt.inds[[from.i]] <- list()
    for(to.i in (1:52)[-from.i]){
      rt.inds[[from.i]][[to.i]] <- which(route.ordered.sub$from_taz == from.i & route.ordered.sub$to_taz == to.i)
    }
  }
  save(rt.inds,file=paste(path.to.plots,'route-inds-for-animation.Rdata',sep=''))
}else{
  load(paste(path.to.plots,'route-inds-for-animation.Rdata',sep=''))
}

ani.routes <- function(){
  trail.len <- 15
  from.to.df <- data.frame(t(combn(52,2)))
  names(from.to.df) <- c('from','to')
  from.to.df$num <- 0
  from.to <- list()
  for(i in 1:trail.len){ from.to[[i]] <- from.to.df }
  cols <- c(colorRampPalette(c("black",colors()[556]))(trail.len-1),'#FF0000')
  trail.i <- 0
  for(t in seq(0,max(tazs$time),by=5/60)){
    t.routes <- t %% 24
    
    plot(agg.taz,col=colors()[170])
    text(-124.5022,41.111,labels=paste('Hour:',roundC(t.routes,0)))
    trail.i <- trail.i + 1
    trail.i <- (trail.i-1)%%trail.len+1
    from.to[[trail.i]]$num <- 0
    trips <- subset(sched,depart<=t.routes & depart>t.routes-5/60)
    if(nrow(trips)>0){
      for(trip.i in 1:nrow(trips)){
        from.i <- trips$from[trip.i]
        to.i <- trips$to[trip.i]
        if(from.i==to.i){
          next
        }else if(to.i < from.i){
          from.i.temp <- from.i
          from.i <- to.i
          to.i <- from.i.temp
        }
        found.row <- which(from.to[[trail.i]]$from==from.i & from.to[[trail.i]]$to==to.i)
        from.to[[trail.i]]$num[found.row] <- from.to[[trail.i]]$num[found.row] + 1   
      }
    }
      
    if(i<=trail.len){
      trail.inds <- 1:trail.i
      col.count <- trail.len-trail.i+1
    }else{
      trail.inds <- 1:trail.i
      if(trail.i<trail.len)trail.inds <- c((trail.i+1):trail.len,trail.inds)
      col.count <- 1
    }
      
    for(trail.plot.i in trail.inds){
      trip.plot.inds <- which(from.to[[trail.plot.i]]$num > 0)
      if(length(trip.plot.inds)==0)next
      for(trip.plot.i in trip.plot.inds){
        plot.from.i <- from.to[[trail.plot.i]]$from[trip.plot.i]
        plot.to.i   <- from.to[[trail.plot.i]]$to[trip.plot.i]
        segments(route.ordered.sub$start_lon[rt.inds[[plot.from.i]][[plot.to.i]]],
                  route.ordered.sub$start_lat[rt.inds[[plot.from.i]][[plot.to.i]]],
                  route.ordered.sub$end_lon[rt.inds[[plot.from.i]][[plot.to.i]]],
                  route.ordered.sub$end_lat[rt.inds[[plot.from.i]][[plot.to.i]]],col=cols[col.count],lwd=2*from.to[[trail.plot.i]]$num[trip.plot.i])
      }
      col.count <- col.count + 1
    }
    # Charging
    t.charging <- ifelse(t>30,t %% 24,t)
		for(i in 1:52){
			num.public.events <- sum(subset(tazs,taz==i & abs(time-t.charging)<0.00001)[,c('num.L1','num.L2','num.L3')]) -  sum(subset(tazs,taz==i & abs(time-t.charging)<0.000001)[,c('num.avail.L1','num.avail.L2','num.avail.L3')])
			num.home.events <- min(40,sum(subset(tazs,taz==i & abs(time-t.charging)<0.00001)[,c('num.L0')]) -  sum(subset(tazs,taz==i & abs(time-t.charging)<0.000001)[,c('num.avail.L0')]))
			
			# Public charging events currently max out at 4, use if logic to re-size dot to match scale. More complicted logic for home charging, which maxes at 38.
      agg.taz.row <- which(agg.taz$id == i)
			points(agg.taz.coords[agg.taz.row,1],agg.taz.coords[agg.taz.row,2],pch=16,col='#9FEE00',cex=num.public.events)
			points(agg.taz.coords[agg.taz.row,1],agg.taz.coords[agg.taz.row,2],pch=1,col='black',cex=num.public.events)
      if(num.home.events>0){
        points(agg.taz.coords[agg.taz.row,1],agg.taz.coords[agg.taz.row,2],pch=0,col='#5CCCCC',cex=home.cexes[num.home.events])
      }
    }
  }
}

ani.options(ffmpeg="/usr/local/bin/ffmpeg")
saveVideo({
    par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, 
        cex.main = 1)
    ani.options(interval = 10/60, nmax = 24*30)
ani.routes()
},video.name="test.mp4",other.opts="-b 1500k -s 800x950") # make res divisible by 16

#####################################################################
## map optim
#####################################################################

trip.locs <- function(trips,t,is.pain=F){
  ddply(trips,.(driver),function(df){
    if(df$origin[1]==df$destination[1]){
      sub <- data.frame(start_lon=coordinates(agg.taz)[which(agg.taz$id==df$origin[1]),1],start_lat=coordinates(agg.taz)[which(agg.taz$id==df$origin[1]),2])
    }else{
      sub <- subset(routes,from_taz==df$origin[1] & to_taz==df$destination[1])
    }
    scale <- max(0,min(1,(t-df$time[1])/(df$end.time[1]-df$time[1])))
    ind <- round((nrow(sub)-1)*scale+1)
    data.frame(lon=sub$start_lon[ind],lat=sub$start_lat[ind],soc=ifelse(df$vehicle.type=="leaf",(df$begin.soc[1] - scale*(df$begin.soc[1]-df$end.soc[1])),1),type=df$vehicle.type,pain=ifelse(is.pain,df$pain.value,NA))
  })
}

#simplify the map
taz.map <- agg.taz
taz.map$sparse <- 1:52
taz.map$sparse[grep("EKA",taz.map$name)] <- 53
taz.map$sparse[grep("ARC",taz.map$name)] <- 54
taz.map$sparse[grep("MCK",taz.map$name)] <- 55
taz.map$sparse[grep("FOR",taz.map$name)] <- 56
taz.map <- unionSpatialPolygons(taz.map,taz.map$sparse)
coords <- coordinates(agg.taz)

routes <- ddply(route.ordered.sub,.(from_taz,to_taz),function(df){
   df$dists <- c(0.01,sqrt((head(df$start_lon,nrow(df)-1)-tail(df$start_lon,nrow(df)-1))^2+(head(df$start_lat,nrow(df)-1)-tail(df$start_lat,nrow(df)-1))^2)) 
   if(df$from_taz[1]==df$to_taz[1]){
     data.frame(start_lon=coords[which(agg.taz$id==df$from_taz[1]),1],start_lat=coords[which(agg.taz$id==df$from_taz[1]),2],
                end_lon=coords[which(agg.taz$id==df$from_taz[1]),1],end_lat=coords[which(agg.taz$id==df$from_taz[1]),2])
   }else{
     subset(df,dists>0.001)
   }
})

#my.colors <- function(x){
  #colorRampPalette(c(colors()[556],'black'))(10)[round(x*10)]
#}
home.cexes <- seq(1,4,length.out=40)

ani.routes <- function(){
  my.grey <- colors()[190]
  my.blue <- colors()[600]

  for(t in seq(0,max.t,by=step.size)){
    plot(taz.map,col=my.grey)
    segments(routes$start_lon,
             routes$start_lat,
             routes$end_lon,
             routes$end_lat,col='white',lwd=4)
    text(-124.4,41.111,labels=paste('Hour:',roundC(as.integer(t),0)),cex=2)
    trips <- subset(sched,time<=t & end.time>t)
    locs <- trip.locs(trips,t)
    points(locs$lon,locs$lat,col=c('black',my.grey)[locs$type],pch=c(15,17)[locs$type],cex=1)

    ch.pub.sub <- ddply(subset(ch,time <= t & time + duration >= t & charger.level>0),.(location),nrow)
    points(coords[match(ch.pub.sub$location,agg.taz$id),1]-0.02,coords[match(ch.pub.sub$location,agg.taz$id),2],col="green",cex=ch.pub.sub$V1,pch=16)
    points(coords[match(ch.pub.sub$location,agg.taz$id),1]-0.02,coords[match(ch.pub.sub$location,agg.taz$id),2],col="black",cex=ch.pub.sub$V1,pch=1,lwd=1.5)
    ch.home.sub <- ddply(subset(ch,time <= t & time + duration >= t & charger.level==0),.(location),nrow)
    points(coords[match(ch.home.sub$location,agg.taz$id),1]+0.02,coords[match(ch.home.sub$location,agg.taz$id),2],col=my.blue,cex=home.cexes[ch.home.sub$V1],pch=16)
    points(coords[match(ch.home.sub$location,agg.taz$id),1]+0.02,coords[match(ch.home.sub$location,agg.taz$id),2],col="black",cex=home.cexes[ch.home.sub$V1],pch=1,lwd=1.5)

    pn.sub <- subset(pain,time <= t & time + abs(pain.value) > t)
    pn.locs <- trip.locs(join(pn.sub,sched,by="driver",match="first"),t,T)
    points(pn.locs$lon,pn.locs$lat,col=c('orange','red')[1+as.numeric(pn.locs$pain>=3)],pch=5,lwd=3,cex=(1.5 + 3*pn.locs$pain/5))
  }
}

step.size = 2.5/60

for(evse.i in evses){
  tazs <- read.csv(paste(path.to.pevi.outputs,"tazs-out-",evse.i,".csv",sep=''))
  sched <- read.csv(paste(path.to.pevi.outputs,"trip-out-",evse.i,".csv",sep=''))
  pain <- subset(read.csv(paste(path.to.pevi.outputs,"pain-out-",evse.i,".csv",sep='')),pain.type=="delay")
  ch <- read.csv(paste(path.to.pevi.outputs,"charging-out-",evse.i,".csv",sep=''))
  max.t <- max(ch$time + ch$duration)

  ani.options(ffmpeg="/usr/local/bin/ffmpeg",outdir=paste(path.to.ani,"ani-",ani.code,sep=''),ani.width=750,ani.height=900)
  saveVideo({
      par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, 
          cex.main = 1)
      ani.options(interval = 5/60, nmax = max.t/step.size+100)
  ani.routes()
  },video.name=paste("map-optim-iter-",evse.i,".mov",sep=''),other.opts="-b 3200k -s 750x900",clean=T) # make res divisible by 16
  if(dev.cur()!=1)dev.off()
}

#####################################################################
## trips as barplot
#####################################################################

# note we use the inputs from the "map-optim" directory which were based on 
# the itinerary driver-schedule-pen4-rep1-20130129

pev.penetration <- 0.04
replicate <- 1
step.size <- 5/60
pev.pen.char <- roundC(pev.penetration,3)

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
  tr <- read.csv(paste(path.to.ani,"itin-barplot/trip-out-",evse.i,".csv",sep=''))
  pain <- subset(read.csv(paste(path.to.ani,"itin-barplot/pain-out-",evse.i,".csv",sep='')),pain.type=="delay")
  ch <- read.csv(paste(path.to.ani,"itin-barplot/charging-out-",evse.i,".csv",sep=''))
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

  if(!file.exists(paste(path.to.ani,"itin-barplot/locs-",evse.i,".Rdata",sep=''))){
    locs <- list()
    for(t in seq(0,max.t,by=step.size)){
      locs[[as.character(t)]] <- location(t)
    }
    save(locs,file=paste(path.to.ani,"itin-barplot/locs-",evse.i,".Rdata",sep=''))
  }else{
    load(paste(path.to.ani,"itin-barplot/locs-",evse.i,".Rdata",sep=''))
  }

  ani.options(ffmpeg="/usr/local/bin/ffmpeg",outdir=paste(path.to.ani,"itin-barplot/ani",sep=''))
  saveVideo({
    par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, 
          cex.main = 1)
    ani.options(interval = 10/60, nmax = max.t/step.size+100)
    ani.tours()
  },video.name=paste("barplot-iter-",evse.i,".mov",sep=''),other.opts="-b 1500k -s 900x900",clean=T) # make res divisible by 16
}

