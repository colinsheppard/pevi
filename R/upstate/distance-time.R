load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','RPostgreSQL','colorRamps','reshape','doMC'))
registerDoMC(10)
gpclibPermit()

source(paste(pevi.home,'R/gis-functions.R',sep=''))

load(pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))
load(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-agg-tricounty.Rdata')) # loads od.agg.all, agg.taz.data, time.distance

agg.coords <- data.table(data.frame(agg.id=agg.taz$agg.id,x=coordinates(agg.taz)[,1],y=coordinates(agg.taz)[,2]),key='agg.id')
setkey(agg.taz.data,'agg.id')
agg.taz.data <- agg.coords[agg.taz.data[agg.id>0]]

agg.taz.data$nn.id <- 0 # here 'nn' stands for 'nearest node'
agg.taz.data$nn.x <- 0.0
agg.taz.data$nn.y <- 0.0

n <- nrow(agg.taz.data)

dt <- data.frame(from=rep(1:n,n),to=rep(1:n,each=n),distance=rep(NA,n*n),time=rep(NA,n*n),enroute=rep(NA,n*n))

dbuser<-"pev"
dbpassword<-""
dbname<-"upstate_route"
dbhost<-"localhost"

con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	

# Find the nearest node in the road network to the points selected for the TAZs, note that we filter out nodes that are Centroids b/c they are connected
# to fake pieces of road used by Caltrans at some point for feeding their model
ways <- dbGetQuery(con,'select gid,osm_id,x1,y1,x2,y2,length,source,target from ways')
for(taz.i in 1:n){
  xy <- agg.taz.data[taz.i,list(x,y)]
  ways$d1 <- sqrt((xy$x-ways$x1)^2 + (xy$y-ways$y1)^2)
  #ways$d2 <- sqrt((xy$x-ways$x2)^2 + (xy$y-ways$y2)^2)
  #min.ds <- c(which.min(ways$d1),which.min(ways$d2))
  #s.or.t <- which.min(c(ways$d1[min.ds[1]],ways$d2[min.ds[2]]))
  #min.d <- min.ds[s.or.t]
  #min.node <- ifelse(s.or.t==1,ways$source[min.d],ways$target[min.d])
  #min.x <- ifelse(s.or.t==1,ways$x1[min.d],ways$x2[min.d])
  #min.y <- ifelse(s.or.t==1,ways$y1[min.d],ways$y2[min.d])

  min.node <- ways$source[which.min(ways$d1)]
  min.x <- ways$x1[which.min(ways$d1)]
  min.y <- ways$y1[which.min(ways$d1)]
  agg.taz.data[taz.i,':='(nn.id=min.node,nn.x=min.x,nn.y=min.y)]
}
save(agg.taz.data,pp(pevi.shared,'data/UPSTATE/driving-distances/routing.Rdata'))

CREATE TABLE taz_route (
  id          serial PRIMARY KEY,
  from_taz    integer NOT NULL,
  to_taz      integer NOT NULL,
  ways_id     integer NOT NULL,
  cost        double precision NOT NULL,
  route_order integer NOT NULL
);

# adjust the costs based on road class

road.class <- data.frame(class=c('motorway','motorway_link'),class.id=c(101,102),weight=c(1,1))
road.class <- rbind(road.class,data.frame(class=c('bridleway','cycleway','footway','pedestrian','path','road','steps','track'),class.id=c(120,118,119,114,117,100,122,113),weight=999))
road.class <- rbind(road.class,data.frame(class=pp('grade',1:5),class.id=c(301:305),weight=3.5))
road.class <- rbind(road.class,data.frame(class=c('primary','primary_link','trunk','trunk_link'),class.id=c(106,107,104,105),weight=1.05))
road.class <- rbind(road.class,data.frame(class=c('secondary','secondary_link'),class.id=c(108,124),weight=1.5))
road.class <- rbind(road.class,data.frame(class=c('tertiary','tertiary_link','unclassified'),class.id=c(109,125,123),weight=2))
road.class <- rbind(road.class,data.frame(class=c('residential','service','services'),class.id=c(110,112,115),weight=3))

for(rc.i in 1:nrow(road.class)){
  dbGetQuery(con,pp('UPDATE ways SET cost=length*',road.class$weight[rc.i],' WHERE class_id=',road.class$class.id[rc.i]))
}

dbGetQuery(con,paste("DELETE FROM taz_route"))
# Build the routes between each taz pair and store them in taz-routes 
for(taz.i in 1:(n-1)){
  taz.i.id <- agg.taz.data$agg.id[taz.i]
  cat(paste(taz.i,",",sep=''))
  for(taz.j in (taz.i+1):n){
    taz.j.id <- agg.taz.data$agg.id[taz.j]
    route <- dbGetQuery(con,pp("SELECT gid,ways.length AS cost
       FROM ways JOIN
       (SELECT * FROM shortest_path_astar('
          SELECT gid AS id, 
              source, 
              target, 
              cost::float8,
              x1,y1,x2,y2
          FROM ways',
          ",agg.taz.data$nn.id[taz.i],",",agg.taz.data$nn.id[taz.j],",
          false,
          false)) AS route ON ways.gid = route.edge_id;",sep=""))
    n.route <- nrow(route)
    
    route$order <- 1:n.route

    values.string.to <-  paste(paste("('",rep(taz.i.id,n.route),"','",rep(taz.j.id,n.route),"','",route$gid,"','",route$cost,"','",route$order,"')",sep=''),collapse=",")
    values.string.from <-  paste(paste("('",rep(taz.j.id,n.route),"','",rep(taz.i.id,n.route),"','",route$gid,"','",route$cost,"','",rev(route$order),"')",sep=''),collapse=",")
    dbGetQuery(con,paste("INSERT INTO taz_route (from_taz,to_taz,ways_id,cost,route_order) VALUES ",values.string.to,sep=''))
    dbGetQuery(con,paste("INSERT INTO taz_route (from_taz,to_taz,ways_id,cost,route_order) VALUES ",values.string.from,sep=''))
  }
}


# Note, lengths are in KM
CREATE OR REPLACE VIEW taz_route_sp AS
SELECT r.id,r.from_taz,r.to_taz,r.cost,r.route_order,w.length,w.source,w.target,w.the_geom,rt.name as road_type FROM taz_route AS r
   LEFT JOIN ways AS w ON w.gid = r.ways_id
   LEFT JOIN classes AS rt ON w.class_id = rt.id;

CREATE OR REPLACE VIEW ways_with_class AS
SELECT w.*,rt.name as class_name FROM ways AS w
   LEFT JOIN classes AS rt ON w.class_id = rt.id;
     
# finally find enroute
route <- data.table(dbGetQuery(con,'SELECT * FROM taz_route'),key='ways_id')
ways.in.tazs <- data.table(ways_id=unique(route$ways_id),agg.id=NA,key='ways_id')
for(way.id in ways.in.tazs$ways_id){
  res <- dbGetQuery(con,pp("SELECT agg_id FROM taz_polys WHERE ST_Contains(geom,(SELECT ST_Point(x1,y1) FROM ways WHERE gid=",way.id,"))"))
  if(length(res)>0){ 
    ways.in.tazs$agg.id[ways.in.tazs$ways_id == way.id] <- res
  }
}
print(pp("couldn't find taz for ways: ",pp(ways.in.tazs$ways_id[is.na(ways.in.tazs$agg.id)],collapse=',')))

# Load the conversion table from old TAZ to new TAZ: od.converter
load(pp(pevi.shared,'data/UPSTATE/od.converter.Rdata'))

route <- ways.in.tazs[route]
route$agg.id.new <- od.converter$new.id[match(route$agg.id,od.converter$old.id)]
route$from.new <- od.converter$new.id[match(route$from_taz,od.converter$old.id)]
route$to.new <- od.converter$new.id[match(route$to_taz,od.converter$old.id)]
setkey(route,'from.new','to.new')
enroute <- route[,list(enroute=pp(grep(pp('^',c(from.new,to.new),'$',collapse='$|^'),unique(na.omit(unlist(agg.id.new))),invert=T,value=T),collapse=',')),by=c('from.new','to.new')]
enroute[,':='(from=from.new,to=to.new,from.new=NULL,to.new=NULL)]
setkey(enroute,'from','to')
load(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-agg-tricounty-with-old-time-distance-matrix.Rdata')) # loads od.agg.all, agg.taz.data, time.distance
time.distance[,':='(from=od.converter$new.id[match(from,od.converter$old.id)],to=od.converter$new.id[match(to,od.converter$old.id)])]
setkey(time.distance,'from','to')
# join to the time.dist
time.distance <- enroute[time.distance]

# now we finalize the time distance matrix to only include non-external TAZs and to have the renumbered convention
time.distance <- time.distance[from>0 & to>0]
# NA's at this point are for the X,X pairs, make 'em blank
time.distance[from==to,enroute:='']

# rename the TAZs
save(od.agg.all,agg.taz.data,time.distance,file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-agg-tricounty.Rdata')) 

#finally, write it out for the Netlogo
for.nl <- data.frame(time.distance[,list(from,to,miles,hours,enroute=pp('"',enroute,'"'),perf=1)])
names(for.nl) <- c(';from','to','miles','time','enroute','perf')
write.table(for.nl,file=pp(pevi.shared,'data/inputs/OD-upstate/upstate-od-data-update.txt'),row.names=F,quote=F,sep='\t')


