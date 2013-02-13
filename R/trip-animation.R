library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools','animation'))

gpclibPermit()
num.processors <- 11
registerDoMC(num.processors)
#
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

# sort by depart time
sched <- sched[order(sched$depart),]

# for now subset this bad boy
#sched <- sched[1:20,]

hum.shp <- unionSpatialPolygons(agg.taz,rep(1,nrow(agg.taz@data)))

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
  cols <- c(colorRampPalette(c("black",colors()[556]))(trail.len-1),colors()[552])
  trail.i <- 0
  for(t in seq(0,24,by=5/60)){
    plot(agg.taz,col=colors()[170])
    trail.i <- trail.i + 1
    trail.i <- (trail.i-1)%%trail.len+1
    from.to[[trail.i]]$num <- 0
    trips <- subset(sched,depart<=t & depart>t-5/60)
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
    # ADD code to plot charging events
    # use points command to add circle
#sum(subset(tazs,taz==1 & abs(time-t)<0.00001)[,c('num.L1','num.L2','num.L3')]) -  sum(subset(tazs,taz==1 & abs(time-t)<0.000001)[,c('num.avail.L1','num.avail.L2','num.avail.L3')])
#points(coordinates(agg.taz)[1,1],coordinates(agg.taz)[1,2],pch=1,col='black',cex=4)
#points(coordinates(agg.taz)[1,1],coordinates(agg.taz)[1,2],pch=16,col='yellow',cex=4)
    
  }
}

ani.options(ffmpeg="/usr/local/bin/ffmpeg")
saveVideo({
    par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, 
        cex.main = 1)
    ani.options(interval = 5/60, nmax = 24*30)
ani.routes()
},video.name="test.mp4",other.opts="-b 1500k -s 800x950") # make res divisible by 16

