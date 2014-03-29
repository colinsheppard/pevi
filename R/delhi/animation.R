#load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools','animation','colorRamps','DAAG'))
load.libraries(c('plyr','reshape','maptools','animation','colorRamps','data.table'))

path.to.ani <- paste(pevi.shared,'data/DELHI/animation/',sep='')
path.to.outputs <- paste(path.to.ani,'outputs/',sep='')
make.dir(path.to.outputs)

source(paste(pevi.home,"R/gis-functions.R",sep=''))

load(pp(pevi.shared,'data/DELHI/POLYGON/AggregatedTAZsCleaned.Rdata'))

do.or.load(pp(pevi.shared,'data/DELHI/road-network/routing-for-animation.Rdata'),function(){
  load.libraries('RPostgreSQL')
  dbuser<-"pev"
  dbpassword<-""
  dbname<-"delhi"
  dbhost<-"localhost"

  con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	
  route <- dbGetQuery(con,'SELECT from_taz,to_taz,ST_X(n.startpoint) AS start_lon,ST_Y(n.startpoint) AS start_lat,ST_X(n.endpoint) AS end_lon,ST_Y(n.endpoint) AS end_lat,route_order
                      FROM taz_route AS r 
                      LEFT JOIN network AS n ON n.gid = r.net_id')
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

do.or.load(pp(path.to.ani,'routes-for-ani.Rdata'),function(){
  load.libraries('RPostgreSQL')
  dbuser<-"pev"
  dbpassword<-""
  dbname<-"delhi"
  dbhost<-"localhost"

  con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	
  setkey(route,'from_taz','to_taz','route_order')
  route.with.ends <- route[,list(start_lon=c(start_lon,tail(end_lon,1)),start_lat=c(start_lat,tail(end_lat,1)),end_lon=c(end_lon,NA),end_lat=c(end_lat,NA),route_order=c(route_order,length(route_order)+1)),by=c('from_taz','to_taz')]

  routes <- dlply(route.with.ends,.(from_taz,to_taz),function(df){
    SpatialLines(list(Lines(Line(df[,c('start_lon','start_lat')]),1)))
  })
  #setkey(route.with.ends,'start_lat','start_lon','end_lat','end_lon','from_taz','to_taz')
  #setkey(route.with.ends,'start_lat','start_lon','end_lat','end_lon')
  #route.with.ends[,key:=pp(start_lat,start_lon,end_lat,end_lon,sep='-')]
  #route.with.ends[,dups:=duplicated(key)]
  #setkey(route.non.overlapping,'from_taz','to_taz','route_order')
  #i<-0
  #all.routes <- SpatialLines(dlply(route.with.ends,.(from_taz,to_taz),function(df){
    #if(all(df$dups)){
      #the.lines <- list(Line(df[1:2,c('start_lon','start_lat')]))
    #}else{
      #non.dups <- which(!df$dups)
      #inds.to.group <- data.frame(beg.ind=c(1,non.dups[which(diff(non.dups)>1)+1]),end.ind=c(non.dups[which(diff(non.dups)>1)],tail(non.dups,1)))
      #inds.to.group <- subset(inds.to.group,beg.ind==end.ind)
      #if(nrow(inds.to.group)==0){
        #the.lines <- list(Line(df[1:2,c('start_lon','start_lat')]))
      #}else{
        #inds.to.group$row <- 1:nrow(inds.to.group)
        #the.lines <- dlply(inds.to.group,.(row),function(df2){
          #Line(df[(df2$beg.ind):(df2$end.ind),c('start_lon','start_lat')])
        #})
      #}
    #}
    #i <<- i+1
    #Lines(the.lines,i)
  #}))
  all.routes <- dbGetQuery(con,'SELECT ST_X(n.startpoint) AS start_lon,ST_Y(n.startpoint) AS start_lat,ST_X(n.endpoint) AS end_lon,ST_Y(n.endpoint) AS end_lat
                      FROM network AS n 
                      WHERE n.gid IN 
                        (SELECT DISTINCT(net_id) AS gid FROM taz_route)')
  #all.routes$group <- NA
  #all.routes <- data.table(all.routes,key=c('start_lon','start_lat'))
  #i<-1
  #group <- 1
  #while(length(i) > 0){
    #if(nrow(all.routes[all.routes[i,list(end_lon,end_lat)]])>0){
      #all.routes$group[i] <- group
      #all.routes$group[which(all.routes$start_lon==all.routes$end_lon[i] & all.routes$start_lat==all.routes$end_lat[i])[1]] <- group
      #group <- group + 1
    #}
    #i <- which(is.na(all.routes$group))[1]
  #}
  #the.lines <- dlply(all.routes,.(group),function(df){
    #Line(rbind(as.matrix(df[,c('start_lon','start_lat')]),as.matrix(tail(df[,c('end_lon','end_lat'),1)])))
  #})

  all.routes$row <- 1:nrow(all.routes)
  the.lines <- dlply(all.routes,.(row),function(df){
    Line(rbind(as.matrix(df[,c('start_lon','start_lat')]),as.matrix(df[,c('end_lon','end_lat')])))
  })
  all.routes <- SpatialLines(list(Lines(the.lines,'1')))
  list('routes'=routes,'all.routes'=all.routes)
})

#plot(routes[[1]],xlim=bbox(all.routes)[1,],ylim=bbox(all.routes)[2,],col='red')
#for(i in 2:length(routes))
  #plot(routes[[i]],add=T,col='red')
#}
#plot(all.routes,add=T)

load(pp(pevi.shared,'data/inputs/compare/delhi-animation/logs.Rdata'))

#my.colors <- function(x){
  #colorRampPalette(c(colors()[556],'black'))(10)[round(x*10)]
#}
home.cexes <- seq(1,4,length.out=40)

my.grey <- colors()[190]
level.nums <- c(0:4)
num.levels <- length(level.nums)
angs <- head(seq(0,2*pi,length.out=num.levels+1),-1)
sins <- -sin(angs)
coss <- cos(angs)
sep.radius <- 0.008
my.purp <- '#984ea3'
my.oran <- '#ff7f00'
my.red <- '#e41a1c'
my.blue <- '#377eb8'
my.green <- '#4daf4a'
charger.cols <- c(my.green,my.blue,my.purp,my.oran,my.red)
charger.caps <- c(1.5,1.5,6.6,50,400)

trip.locs <- function(trips,t,is.pain=F){
  ddply(trips,.(driver),function(df){
    if(df$origin[1]==df$destination[1]){
      sub <- data.frame(start_lon=agg.taz$x[which(agg.taz$agg.id==df$origin[1])],start_lat=agg.taz$y[which(agg.taz$agg.id==df$origin[1])])
    }else{
      sub <- data.frame(slot(slot(slot(routes[[pp(df$origin[1],df$destination[1],sep='.')]],'lines')[[1]],'Lines')[[1]],'coords'))
    }
    scale <- max(0,min(1,(t-df$time[1])/(df$end.time[1]-df$time[1])))
    ind <- round((nrow(sub)-1)*scale+1)
    data.frame(lon=sub$start_lon[ind],lat=sub$start_lat[ind],soc=ifelse(df$vehicle.type=="leaf",(df$begin.soc[1] - scale*(df$begin.soc[1]-df$end.soc[1])),1),type=df$vehicle.type,pain=ifelse(is.pain,df$pain.value,NA))
  })
}

ani.routes <- function(){

  for(t in seq(start.t,max.t,by=step.size)){
    par(bg = my.grey)
    plot(agg.taz,col=my.grey,lwd=1.5)
    plot(all.routes,col=colors()[350],lwd=4,add=T)
    # NOTE: something is off on the time, everything is 6 hours ahead, need to adjust this in results
    text(76.8796,28.7283,labels=paste('Hour:',roundC(as.integer(t-6),0)),cex=2,col='white')
    trips <- subset(sched,time<=t & end.time>t)
    locs <- trip.locs(trips,t)
    points(locs$lon,locs$lat,col=c('black',my.grey)[locs$type],pch=c(15,17)[locs$type],cex=1)

    #for(lev in level.nums){
      #sep.i <- which(lev == level.nums)

      #ch.pub.sub <- ddply(subset(ch,time <= t & time + duration >= t & charger.level == lev),.(location),nrow)
      #ch.pub.sub$kw <- ch.pub.sub$V1 * charger.caps[sep.i]
      
      #loc.inds <- match(ch.pub.sub$location,agg.taz$agg.id)
      #points(agg.taz$x[loc.inds]+sep.radius*sins[sep.i],agg.taz$y[loc.inds]+sep.radius*coss[sep.i],col=charger.cols[sep.i],cex=ch.pub.sub$kw/10,pch=16)
      #points(agg.taz$x[loc.inds]+sep.radius*sins[sep.i],agg.taz$y[loc.inds]+sep.radius*coss[sep.i],col='black',cex=ch.pub.sub$kw/10,pch=1,lwd=1.5)
    #}
    for(loc in agg.taz$agg.id){
      ch.pub.sub <- ddply(subset(ch,time <= t & time + duration >= t & location == loc),.(charger.level),nrow)
      ch.pub.sub$kw <- ch.pub.sub$V1 * charger.caps[match(ch.pub.sub$charger.level,level.nums)]
      ch.pub.sub <- ch.pub.sub[rev(order(ch.pub.sub$kw)),]
      
      loc.ind <- which(loc == agg.taz$agg.id)
      for(ch.sub.i in 1:nrow(ch.pub.sub)){
        sep.i <- which(ch.pub.sub$charger.level[ch.sub.i] == level.nums)
        points(agg.taz$x[loc.ind]+sep.radius*sins[sep.i],agg.taz$y[loc.ind]+sep.radius*coss[sep.i],col=charger.cols[sep.i],cex=ch.pub.sub$kw[ch.sub.i]/10,pch=16)
        points(agg.taz$x[loc.ind]+sep.radius*sins[sep.i],agg.taz$y[loc.ind]+sep.radius*coss[sep.i],col='black',cex=ch.pub.sub$kw[ch.sub.i]/10,pch=1,lwd=1.5)
      }
    }

    delay.locs <- trip.locs(unique(sched)[delay[time <= t & time + abs(pain.value) > t]][!is.na(time)],t,T)
    points(delay.locs$lon,delay.locs$lat,col='orange',pch=5,lwd=3,cex=(1.5 + 3*delay.locs$pain/5))
    strand.locs <- trip.locs(unique(sched)[strand[time <= t & time + 2 > t]][!is.na(time)],t,T)
    points(strand.locs$lon,strand.locs$lat,col='red',pch=5,lwd=3,cex=5)
  }
}

step.size = 2.5/60
evses <- c(0.5,1,2)
start.t <- 6

for(evse.i in evses){
  tazs <- subset(logs[['tazs']],penetration == evse.i)
  sched <- data.table(subset(logs[['trip']],penetration == evse.i),key='driver')
  sched[,':='(origin=abs(sched$origin),destination=abs(sched$destination))]
  delay <- data.table(subset(logs[['pain']],penetration == evse.i & pain.type %in% c("delay") & location>0),key='driver')
  strand <- data.table(subset(logs[['pain']],penetration == evse.i & pain.type %in% c("stranded") & location > 0),key='driver')
  ch <- subset(logs[['charging']],penetration == evse.i & location > 0)
  max.t <- max(ch$time + ch$duration)
  max.t <- 32

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

