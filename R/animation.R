library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools','animation'))

base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
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

# load od.24.weighted,od.am.weighted,od.pm.weighted
load(paste(path.to.pevi,'inputs/routing-corrected.Rdata',sep=''))

route.ordered.sub <- route.ordered[,c('from_taz','to_taz','start_lon','start_lat','end_lon','end_lat')]

pev.penetration <- 0.04
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
