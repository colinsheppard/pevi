library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools','animation'))

gpclibPermit()
num.processors <- 11
registerDoMC(num.processors)

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
print(paste('Penetration ',pev.penetration,' replicate ',replicate,sep=''))
sched <- read.table(file=paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep='\t',header=T)
names(sched) <- c('driver','from','to','depart','home')
sched$ft <- paste(sched$from,sched$to)

# sort by depart time
sched <- sched[order(sched$depart),]

# for now subset this bad boy
sched <- sched[1:20,]

hum.shp <- unionSpatialPolygons(agg.taz,rep(1,nrow(agg.taz@data)))

rt.inds <- list()
for(from.i in 1:52){
  rt.inds[[from.i]] <- list()
  for(to.i in (1:52)[-from.i]){
    rt.inds[[from.i]][[to.i]] <- which(route.ordered.sub$from_taz == from.i & route.ordered.sub$to_taz == to.i)
  }
}

ani.routes <- function(){
  from.to <- list()
  trail.len <- 20
  cols <- colorRampPalette(c("black","red"))( trail.len )
  for(i in 1:nrow(sched)){
    plot(hum.shp,col='black')
    trail.i <- (i-1)%%trail.len+1
    from.i <- sched$from[i]
    to.i <- sched$to[i]
    if(from.i==to.i){
      from.to[[trail.i]] <- list(from=NA,to=NA)
      next
    }
    if(i<=trail.len){
      from.to[[trail.i]] <- list(from=from.i,to=to.i)
      trail.inds <- 1:trail.i
      col.count <- trail.len-i+1
    }else{
      from.to[[trail.i]]$from <- from.i
      from.to[[trail.i]]$to   <- to.i
      trail.inds <- 1:trail.i
      if(trail.i<trail.len)trail.inds <- c((trail.i+1):trail.len,trail.inds)
      col.count <- 1
    }
    
    for(trail.plot.i in trail.inds){
      plot.from.i <- from.to[[trail.plot.i]]$from
      if(is.na(plot.from.i))next
      plot.to.i <- from.to[[trail.plot.i]]$to
      segments(route.ordered.sub$start_lon[rt.inds[[plot.from.i]][[plot.to.i]]],
                route.ordered.sub$start_lat[rt.inds[[plot.from.i]][[plot.to.i]]],
                route.ordered.sub$end_lon[rt.inds[[plot.from.i]][[plot.to.i]]],
                route.ordered.sub$end_lat[rt.inds[[plot.from.i]][[plot.to.i]]],col=cols[col.count],lwd=3)
      col.count <- col.count + 1
    }
  }
}

ani.options(ffmpeg="/usr/local/bin/ffmpeg")
saveVideo({
    par(mar = c(3, 3, 1, 0.5), mgp = c(2, 0.5, 0), tcl = -0.3, cex.axis = 0.8, cex.lab = 0.8, 
        cex.main = 1)
    ani.options(interval = 0.01, nmax = 300)
ani.routes()
},video.name="test.mp4",other.opts="-b 300k")

