load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','RPostgreSQL','colorRamps','reshape','doMC'))
registerDoMC(10)
gpclibPermit()

source(paste(pevi.home,'R/gis-functions.R',sep=''))

# First load the original network file from RITES
net <- readShapeLines(pp(pevi.shared,'data/DELHI/road-network/Delhi_Link_polyline_polyline_WGS84.shp'))
net$link.key <- pp(net$LK_FRM,'--',net$LK_TO) # unique key

# Now append the data table curated by Andy to the attributes of the shape file and write back out to shape
net.data <- read.csv(pp(pevi.shared,'data/DELHI/tdfs-data/delhi-road-info.csv'),stringsAsFactors=F)
net.data$link.key <- pp(net.data$LK_FRM,'--',net.data$LK_TO) # unique key
net.data <- subset(net.data,link.key %in% net$link.key)
# deal with dup's
net.data <- net.data[!duplicated(net.data$link.key),]
# turn RUNTIME and JOURTIME into units of hours (from seconds)
net.data$RUNTIME <- net.data$RUNTIME/3600
net.data$JOURTIME <- net.data$JOURTIME/3600
net.data$DELAY <- net.data$DELAY/3600
# deal with missing data (use priv_speed)
na.rows <- is.na(net.data$RUNTIME)
net.data$PRIV_SPEED[net.data$LK_FRM==1865 & net.data$LK_TO==377] <- 1
net.data$JOURTIME[na.rows] <- net.data$LINK_LEN_M[na.rows]/1000/net.data$PRIV_SPEED[na.rows]
net.data$RUNTIME[na.rows] <- net.data$JOURTIME[na.rows] * median(net.data$RUNTIME,na.rm=T)/median(net.data$JOURTIME,na.rm=T)
net.data$DELAY[na.rows] <- net.data$JOURTIME[na.rows] - net.data$RUNTIME[na.rows] 
net.data <- merge(net@data,data.frame(link.key=net.data$link.key,net.data[,-which(names(net.data)%in%names(net@data))]),by="link.key")
net@data <- net.data[match(net$link.key,net.data$link.key),]
writeLinesShape(net,pp(pevi.shared,'data/DELHI/road-network/delhi_network.shp'))

# some quick analysis to look at the relationship between average speed, runtime and jourtime
plot.dat<-data.frame(priv.speed.kph=net.data$PRIV_SPEED,runtime.speed.kph=net.data$LINK_LEN_M/1000/(net.data$RUNTIME/3600),jourtime.speed.kph=net.data$LINK_LEN_M/1000/(net.data$JOURTIME/3600))
plot.dat[plot.dat==Inf] <- NA
cor(na.omit(plot.dat))

###############################################################
# From SHELL, follow steps titled LOAD DELHI NETWORK in file postgis/delhi.sh
###############################################################

###############################################################
# If you haven't done so already, aggregate the TAZs into 
# the final set using TAZ-aggregate.R
###############################################################

if(!file.exists(pp(pevi.shared,'data/DELHI/POLYGON/AggregatedTAZsCleaned.Rdata'))){
  agg.taz <- readShapePoly(pp(pevi.shared,'data/DELHI/POLYGON/AggregatedTAZsCleaned'))
  names(agg.taz@data)<-c('SP_ID','agg.id','name','shp.id')
  agg.taz$x <- coordinates(agg.taz)[,1]
  agg.taz$y <- coordinates(agg.taz)[,2]
  save(agg.taz,file=pp(pevi.shared,'data/DELHI/POLYGON/AggregatedTAZsCleaned.Rdata'))
}else{
  load(pp(pevi.shared,'data/DELHI/POLYGON/AggregatedTAZsCleaned.Rdata'))
}

agg.taz.data <- data.table(agg.taz@data)
setkey(agg.taz.data,'agg.id')

agg.taz.data$nn.id <- 0 # here 'nn' stands for 'nearest node'
agg.taz.data$nn.x <- 0.0
agg.taz.data$nn.y <- 0.0

n <- nrow(agg.taz.data)

dt <- data.frame(from=rep(1:n,n),to=rep(1:n,each=n),distance=rep(NA,n*n),time=rep(NA,n*n),enroute=rep(NA,n*n))

dbuser<-"pev"
dbpassword<-""
dbname<-"delhi"
dbhost<-"localhost"

con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	

# Find the nearest node in the road network to the points selected for the TAZs
net <- dbGetQuery(con,'select gid,start_id,ST_X(startpoint) AS x,ST_Y(endpoint) AS y from network')
for(taz.i in 1:n){
  xy <- agg.taz.data[taz.i,list(x,y)]
  net$d1 <- sqrt((xy$x-net$x)^2 + (xy$y-net$y)^2)

  min.node <- net$start_id[which.min(net$d1)]
  min.x <- net$x[which.min(net$d1)]
  min.y <- net$y[which.min(net$d1)]
  agg.taz.data[taz.i,':='(nn.id=min.node,nn.x=min.x,nn.y=min.y)]
}
plot(coordinates(agg.taz))

dbSendQuery(con,paste("CREATE TABLE taz_route (
  id          serial PRIMARY KEY,
  from_taz    integer NOT NULL,
  to_taz      integer NOT NULL,
  net_id      integer NOT NULL,
  distance    double precision NOT NULL,
  time        double precision NOT NULL,
  time_delayed  double precision NOT NULL,
  route_order integer NOT NULL
)"))

dbGetQuery(con,paste("DELETE FROM taz_route"))
# Build the routes between each taz pair and store them in taz-routes 
for(taz.i in 1:(n-1)){
  taz.i.id <- agg.taz.data$agg.id[taz.i]
  cat(paste(taz.i,",",sep=''))
  for(taz.j in (taz.i+1):n){
    taz.j.id <- agg.taz.data$agg.id[taz.j]
    route <- dbGetQuery(con,pp("SELECT network.gid,network.runtime AS time,network.jourtime AS time_delayed,network.link_len_m AS distance
   FROM network
   JOIN
   (SELECT * FROM pgr_astar('
      SELECT gid AS id, 
          start_id::int4 AS source, 
          end_id::int4 AS target, 
          runtime::float8 AS cost,
          ST_X(startpoint) AS x1, 
          ST_Y(startpoint) AS y1, 
          ST_X(endpoint) AS x2, 
          ST_Y(endpoint) AS y2
      FROM network',
      ",agg.taz.data$nn.id[taz.i],",
      ",agg.taz.data$nn.id[taz.j],",
      false,
      false)) AS route
   ON
   network.gid = route.id2"))
                               
    n.route <- nrow(route)
    route$order <- 1:n.route

    values.string.to <-  paste(paste("('",rep(taz.i.id,n.route),"','",rep(taz.j.id,n.route),"','",route$gid,"','",route$distance,"','",route$time,"','",route$time_delayed,"','",route$order,"')",sep=''),collapse=",")
    values.string.from <-  paste(paste("('",rep(taz.j.id,n.route),"','",rep(taz.i.id,n.route),"','",route$gid,"','",route$distance,"','",route$time,"','",route$time_delayed,"','",rev(route$order),"')",sep=''),collapse=",")
    dbGetQuery(con,paste("INSERT INTO taz_route (from_taz,to_taz,net_id,distance,time,time_delayed,route_order) VALUES ",values.string.to,sep=''))
    dbGetQuery(con,paste("INSERT INTO taz_route (from_taz,to_taz,net_id,distance,time,time_delayed,route_order) VALUES ",values.string.from,sep=''))
  }
}

# pull the full set of routes, collapse them into a single matrix
routes <- data.table(dbGetQuery(con,paste("SELECT * FROM taz_route")))
setkey(routes,'from_taz','to_taz')
routes[,distance:=distance/1000] # from m into km
time.distance <- routes[,list(km=sum(distance),time=sum(time)),by=c('from_taz','to_taz')]
time.distance[,':='(from=from_taz,to=to_taz,from_taz=NULL,to_taz=NULL)]

## Note, lengths are in KM
dbSendQuery(con,paste("CREATE OR REPLACE VIEW taz_route_sp AS
SELECT r.id,r.net_id,r.from_taz,r.to_taz,r.time,r.time_delayed,r.distance,r.route_order,w.start_id,w.end_id,w.geom FROM taz_route AS r
   LEFT JOIN network AS w ON w.gid = r.net_id"))

###############################################################
# If necessary, load the aggregated TAZs into postgis
###############################################################
#cd pev-shared/data/DELHI/POLYGON/
#shp2pgsql AggregatedTAZsCleaned | psql -U pev -d delhi

# finally find enroute
routes[,':='(net.id=net_id,net_id=NULL)]
net.in.tazs <- data.table(net.id=unique(routes$net.id),agg.id=NA,key='net.id')
for(net.id in net.in.tazs$net.id){
  res <- dbGetQuery(con,pp("SELECT agg_id FROM aggregatedtazscleaned WHERE ST_Contains(geom,(SELECT startpoint FROM network WHERE gid=",net.id,"))"))
  if(length(res)>0){ 
    net.in.tazs$agg.id[net.in.tazs$net.id == net.id] <- res
  }
}
print(pp("couldn't find taz for net: ",pp(net.in.tazs$net.id[is.na(net.in.tazs$agg.id)],collapse=',')))
setkey(routes,'net.id')
routes <- net.in.tazs[routes]

setkey(routes,'from_taz','to_taz')
enroute <- routes[,list(enroute=pp(grep(pp('^',c(from_taz,to_taz),'$',collapse='$|^'),unique(na.omit(unlist(agg.id))),invert=T,value=T),collapse=',')),by=c('from_taz','to_taz')]
enroute[,':='(from=from_taz,to=to_taz,from_taz=NULL,to_taz=NULL)]
setkey(enroute,'from','to')
setkey(time.distance,'from','to')
# join to the time.dist
time.distance <- enroute[time.distance]

# We need the distance for intra-taz travel, which requires the area of each TAZ
load(pp(pevi.shared,'data/DELHI/taz-aggregation-mapping.Rdata'))
agg.area <- data.table(taz.data,key='agg.id')[,list(area=sum(IN_SQM)),by='agg.id']
agg.area <- agg.area[,area:=area/1000^2]
intra.td <- data.table(from=unique(time.distance$from),to=unique(time.distance$from),km=sqrt(agg.area$area[match(unique(time.distance$from),agg.area$agg.id)])/2,enroute='')
intra.td[,time:=km/median(net.data$LINK_LEN_M/1000/net.data$RUNTIME,na.rm=T)]
time.distance <- rbind(time.distance,intra.td[,list(from,to,enroute,km,time)])

# write it out for the Netlogo
setkey(time.distance,'from','to')
for.nl <- data.frame(time.distance[,list(from,to,km,time,enroute=pp('"',enroute,'"'),perf=1)])
names(for.nl) <- c(';from','to','km','time','enroute','perf')
write.table(for.nl,file=pp(pevi.shared,'data/inputs/OD-delhi/delhi-od-data.txt'),row.names=F,quote=F,sep='\t')

# oh yes, and produce a delayed time.distance version
time.distance.delayed <- routes[,list(km=sum(distance),time=sum(time_delayed)),by=c('from_taz','to_taz')]
time.distance.delayed[,':='(from=from_taz,to=to_taz,from_taz=NULL,to_taz=NULL)]
setkey(time.distance.delayed,'from','to')
time.distance.delayed <- time.distance.delayed[time.distance]
time.distance.delayed[,':='(time=ifelse(is.na(time),time.1,time),time.1=NULL,km=km.1,km.1=NULL)]
# write it out for the Netlogo
setkey(time.distance.delayed,'from','to')
for.nl <- data.frame(time.distance.delayed[,list(from,to,km,time,enroute=pp('"',enroute,'"'),perf=1)])
names(for.nl) <- c(';from','to','km','time','enroute','perf')
write.table(for.nl,file=pp(pevi.shared,'data/inputs/OD-delhi/delhi-od-data-congested.txt'),row.names=F,quote=F,sep='\t')

save(routes,time.distance,time.distance.delayed,file=pp(pevi.shared,'data/DELHI/road-network/routing.Rdata'))
