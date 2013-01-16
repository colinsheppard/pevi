library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','RPostgreSQL','colorRamps','reshape','doMC'))
registerDoMC(10)
gpclibPermit()

path.to.geatm  <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))

taz <- readShapePoly(paste(path.to.pevi,'inputs/development/aggregated-taz',sep=''))
load(paste(path.to.pevi,'inputs/development/aggregated-taz-fieldnames.Rdata',sep=''))
names(taz@data) <- c('row',agg.taz.shp.fieldnames)
for(i in 1:nrow(taz@data)){
  taz@data$shp.id[i] <- as.numeric(slot(slot(taz[i,],"polygons")[[1]],"ID"))
}

pts <- readShapePoints(paste(path.to.google,'TAZ-Centers',sep=''))

pts@data$x <- coordinates(pts)[,1]
pts@data$y <- coordinates(pts)[,2]
pts@data$nn.id <- NA # here 'nn' stands for 'nearest node'
pts@data$nn.x <- NA
pts@data$nn.y <- NA

n <- nrow(taz@data)

dt <- data.frame(from=rep(1:n,n),to=rep(1:n,each=n),distance=rep(NA,n*n),time=rep(NA,n*n),enroute=rep(NA,n*n))

dbuser<-"pev"
dbpassword<-""
dbname<-"pev"
dbhost<-"localhost"

con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	

# Find the nearest node in the road network to the points selected for the TAZs, note that we filter out nodes that are Centroids b/c they are connected
# to fake pieces of road used by Caltrans at some point for feeding their model
for(taz.i in 1:n){
  pts@data[taz.i,c('nn.id','nn.x','nn.y')] <- dbGetQuery(con,paste("SELECT n.id,ST_X(n.geom),ST_Y(n.geom) FROM node AS n WHERE ST_DWithin(n.geom,ST_GeomFromText('POINT(",paste(pts@data[taz.i,c('x','y')],collapse=' '),")',0),(SELECT MIN(ST_Distance(n.geom,ST_GeomFromText('POINT(",paste(pts@data[taz.i,c('x','y')],collapse=' '),")',0))) FROM node AS n))",sep=''))
}

CREATE TABLE taz_route (
  id          serial PRIMARY KEY,
  from_taz    integer NOT NULL,
  to_taz      integer NOT NULL,
  network_id  integer NOT NULL,
  time_cost   double precision NOT NULL,
  start_elev   double precision NOT NULL,
  end_elev     double precision NOT NULL,
  gradient     double precision NOT NULL,
  route_order integer NOT NULL
);

dbGetQuery(con,paste("DELETE FROM taz_route"))
# Build the routes between each taz pair and store them in taz-routes 
for(taz.i in 1:(n-1)){
  taz.i.id <- taz@data$id[which(taz@data$name==pts@data$Name[taz.i])]
  cat(paste(taz.i,",",sep=''))
  for(taz.j in (taz.i+1):n){
    taz.j.id <- taz@data$id[which(taz@data$name==pts@data$Name[taz.j])]
    route <- dbGetQuery(con,paste("SELECT gid,cost
       FROM network JOIN
       (SELECT * FROM shortest_path_astar('
          SELECT gid AS id, 
              start_id::int4 AS source, 
              end_id::int4 AS target, 
              (length/ab_speed)::float8 AS cost,
              ST_X(startpoint) AS x1, 
              ST_Y(startpoint) AS y1, 
              ST_X(endpoint) AS x2, 
              ST_Y(endpoint) AS y2
          FROM network',
          ",pts@data$nn.id[taz.i],",",pts@data$nn.id[taz.j],",
          false,
          false)) AS route ON network.gid = route.edge_id;",sep=""))

      n.route <- nrow(route)
      route$order <- 1:n.route

    values.string.to <-  paste(paste("('",rep(taz.i.id,n.route),"','",rep(taz.j.id,n.route),"','",route$gid,"','",route$cost,"',0,0,-99,'",route$order,"')",sep=''),collapse=",")
    values.string.from <-  paste(paste("('",rep(taz.j.id,n.route),"','",rep(taz.i.id,n.route),"','",route$gid,"','",route$cost,"',0,0,-99,'",rev(route$order),"')",sep=''),collapse=",")
    dbGetQuery(con,paste("INSERT INTO taz_route (from_taz,to_taz,network_id,time_cost,start_elev,end_elev,gradient,route_order) VALUES ",values.string.to,sep=''))
    dbGetQuery(con,paste("INSERT INTO taz_route (from_taz,to_taz,network_id,time_cost,start_elev,end_elev,gradient,route_order) VALUES ",values.string.from,sep=''))
  }
}


CREATE VIEW taz_route_sp AS
SELECT r.id,r.from_taz,r.to_taz,r.time_cost,r.start_elev,r.end_elev,r.gradient,r.route_order,n.length,n.ab_speed AS speed,n.start_id,n.end_id,n.geom FROM taz_route AS r
   LEFT JOIN network AS n ON n.gid = r.network_id;
     
  
# pull the elevation (in m) from the northcoast raster and calculate the gradient (note that 'length' is in miles ST_Distance_Sphere is in meters and elev is in feet) from the start-end points
#net.ids <- dbGetQuery(con,paste("SELECT DISTINCT(network_id),gradient FROM taz_route WHERE gradient < -90",sep=''))
net.ids <- dbGetQuery(con,paste("SELECT DISTINCT(network_id),gradient FROM taz_route",sep=''))
for(net.id in net.ids$network_id){
  cat(paste(which(net.id==net.ids$network_id),", ",sep=''))
  elevs <- dbGetQuery(con,paste('SELECT points.gid,points.length,
        ST_Distance_Sphere(points.geom1,points.geom2)*0.000621371 AS dist,
        ((ST_Value(img.rast,points.geom2) - ST_Value(img.rast,points.geom1))/(ST_Distance_Sphere(points.geom1,points.geom2))) AS gradient
      FROM north_coast AS img, 
        (SELECT gid,length,CASE WHEN startpoint = ST_Pointn(dumpedgeom,1) THEN ST_Pointn(dumpedgeom,generate_series(1,ST_NPoints(dumpedgeom)-1)) ELSE ST_Pointn(dumpedgeom,generate_series(ST_NPoints(dumpedgeom),2,-1)) END AS geom1, CASE WHEN startpoint = ST_Pointn(dumpedgeom,1) THEN ST_Pointn(dumpedgeom,generate_series(2,ST_NPoints(dumpedgeom))) ELSE ST_Pointn(dumpedgeom,generate_series(ST_NPoints(dumpedgeom)-1,1,-1)) END AS geom2 FROM network WHERE gid=',net.id,') AS points',sep=''))
  res <- dbGetQuery(con,paste("UPDATE taz_route SET gradient=",weighted.mean(elevs$gradient,elevs$dist)," WHERE network_id=",net.id,sep=''))
}

# define a function to do the ordering for use in ddply
order.route <- function(df){
  end.pts <- as.numeric(names(which(table(c(df$start_id,df$end_id))==1)))
  st.id <- end.pts[pts$taz.id[match(end.pts,pts$nn.id)] == df$from_taz[1]]
  ordered.inds <- c()
  for(i in 1:nrow(df)){
    found.i <-  which(df$start_id %in% st.id | df$end_id %in% st.id)
    if(length(found.i)>1)found.i <- found.i[-which(found.i %in% ordered.inds)]
    ordered.inds  <- c(ordered.inds,found.i)
    st.id <- df[found.i,c('start_id','end_id')][-which(df[found.i,c('start_id','end_id')] %in% st.id)]
  }
  # save this new data frame
  ndf<-data.frame(df[ordered.inds,],order=1:nrow(df))
  # now reverse rows that are arranged in the wrong direction
  while(any(diff(ndf$end_id)==0)){
    i <- which(diff(ndf$end_id)==0)+1
    st.id <- ndf$start_id[i]
    ndf$start_id[i] <- ndf$end_id[i]
    ndf$end_id[i] <- st.id
    ndf$gradient[i] <- -ndf$gradient[i]
  }
  ndf$cum.dist <- cumsum(ndf$length)
  #return the new df 
  ndf
}

# assemble the routes into order based on start/end ids

ggplot(subset(route.ordered,abs(gradient)<.05),aes(x=cum.dist,y=gradient*100))+geom_point()+facet_wrap(~name,scales='free_x')
ggplot(subset(route.ordered,abs(gradient)<.05),aes(x=cum.dist,y=gradient*100))+geom_point()+facet_wrap(~name,scales='free')
ggplot(subset(route.ordered,abs(gradient)<.05),aes(x=ab_speed,y=gradient*100))+geom_point()+facet_wrap(~name)
# plot gradient vs speed and size by length
ggplot(subset(route.ordered,abs(gradient)<.05),aes(x=ab_speed,y=gradient*100,size=length,colour=length))+geom_point(position='jitter',height=0,width=2)+facet_wrap(~name,scales="free_y")

# make 2D histograms 
x.bins <- c(-0.6,seq(-0.05,0.05,by=0.01),0.6)
x.labs <- c("[-0.6,-0.05)","[-0.05,-0.04)","[-0.04,-0.03)","[-0.03,-0.02)","[-0.02,-0.01)","[-0.01,0.00)","[0.00,0.01)","[0.01,0.02)","[0.02,0.03)","[0.03,0.04)","[0.04,0.05)","[0.05,0.6)")
y.bins <- c(0,25,40,50,60,70)
y.labs <- c("[0,25)","[25,40)","[40,50)","[50,60)","[60,70)")
route <- dbGetQuery(con,paste("SELECT r.from_taz,r.to_taz,r.gradient,n.length,n.start_id,n.end_id,n.ab_speed,ST_X(n.startpoint) AS start_lon,ST_Y(n.startpoint) AS start_lat,ST_X(n.endpoint) AS end_lon,ST_Y(n.endpoint) AS end_lat FROM taz_route AS r LEFT JOIN network AS n ON r.network_id=n.gid",sep=''))
#route <- dbGetQuery(con,paste("SELECT r.from_taz,r.to_taz,r.gradient,n.length,n.start_id,n.end_id,n.ab_speed,ST_X(startpoint) AS startx,ST_Y(startpoint) AS starty,ST_X(endpoint) AS endx,ST_Y(endpoint) AS endy FROM taz_route AS r LEFT JOIN network AS n ON r.network_id=n.gid",sep=''))
route.ordered <- ddply(route,.(from_taz,to_taz),order.route,.parallel=T)
route.ordered$to.name <- taz@data$name[match(route.ordered$to_taz,taz@data$id)]
route.ordered$from.name <- taz@data$name[match(route.ordered$from_taz,taz@data$id)]
route.ordered$gradient.binned <- factor(x.labs[findInterval(route.ordered$gradient,x.bins)],levels=x.labs)
route.ordered$speed.binned    <- factor(y.labs[findInterval(route.ordered$ab_speed,y.bins)],levels=y.labs)
route.hists <- cast(melt(route.ordered,id.vars=c('to.name','from.name','gradient.binned','speed.binned'),measure.vars=c('length')),gradient.binned ~ speed.binned ~ variable ~ to.name ~ from.name,fun.aggregate=sum)
save(route.ordered,route.hists,file=paste(path.to.pevi,'inputs/routing.Rdata',sep=''))
load(paste(path.to.pevi,'inputs/routing.Rdata',sep=''))

#pts$name <- as.character(pts$Name)
#pts$taz.id <- taz$id[match(pts$name,taz$name)]
#for(from.id in 1:52){
  #for(to.id in 1:52){
    #if(from.id == to.id)next
    #row.i <- which(route.ordered$from_taz==from.id & route.ordered$to_taz==to.id & route.ordered$order==1)
    #if(any(pts$nn.id == route.ordered$end_id[row.i])){
      #sub.inds <- which(route.ordered$from_taz==from.id & route.ordered$to_taz==to.id)
      #route.ordered$gradient[sub.inds] <- -route.ordered$gradient[sub.inds]
      #temp.start <- route.ordered$start_id[sub.inds]
      #route.ordered$start_id[sub.inds] <- route.ordered$end_id[sub.inds]
      #route.ordered$end_id[sub.inds] <- temp.start
    #}
    #if(! pts$Name[pts$nn.id==route.ordered$start_id[row.i]] == route.ordered$from.name[row.i]){
      #sub.inds <- which(route.ordered$from_taz==from.id & route.ordered$to_taz==to.id)
      #route.ordered$gradient[sub.inds] <- -route.ordered$gradient[sub.inds]
      #temp.start <- route.ordered$start_id[sub.inds]
      #route.ordered$start_id[sub.inds] <- route.ordered$end_id[sub.inds]
      #route.ordered$end_id[sub.inds] <- temp.start
      #route.ordered$order[sub.inds] <- max(route.ordered$order[sub.inds]) - route.ordered$order[sub.inds] + 1
    #}
  #}
#}
#route.hists <- cast(melt(route.ordered,id.vars=c('to.name','from.name','gradient.binned','speed.binned'),measure.vars=c('length')),gradient.binned ~ speed.binned ~ variable ~ to.name ~ from.name,fun.aggregate=sum)
#save(route.ordered,route.hists,file=paste(path.to.pevi,'inputs/routing-corrected.Rdata',sep=''))

# finally find enroute
taz.nodes <- data.frame(node.id=sort(unique(route.ordered$end_id)),taz=NA)
for(node.id in taz.nodes$node.id){
  res <- dbGetQuery(con,paste("SELECT id FROM aggregated_taz WHERE ST_Contains(geom,(SELECT geom FROM node WHERE id=",node.id,"))"))
  if(length(res)==0){ 
    print(paste("couldn't find taz for node ",node.id))
  }else{
    taz.nodes$taz[taz.nodes$node.id == node.id] <- res
  }
}
# damn edge cases, had to look this up in GEarth
taz.nodes$taz[taz.nodes$node.id == 2457] <- 19
route.ordered$end.taz <- taz.nodes$taz[match(route.ordered$end_id,taz.nodes$node.id)]
save(route.ordered,route.hists,file=paste(path.to.pevi,'inputs/routing-corrected.Rdata',sep=''))

route.dir <- paste(path.to.plots,"gradient-analysis/",sep='')
make.dir(route.dir)
for(from.id in 1:52){
  from.name <- taz@data$name[match(from.id,taz@data$id)]

  pdf.file <- paste(route.dir,'/gradient-by-speed-and-length-',from.name,'.pdf',sep='')
  p<-ggplot(subset(route.ordered,from_taz == from.id & abs(gradient)<.05),aes(x=ab_speed,y=gradient*100,size=length,colour=length))+geom_point(position='jitter',height=0,width=2)+facet_wrap(~to.name,scales="free_y")+labs(title=as.character(from.name))
  ggsave(pdf.file,plot=p,width=21,height=13)
}

for(from.name in dimnames(route.hists)$from.name){
  for(to.name in dimnames(route.hists)$to.name){
    write.table("",file=paste(path.to.pevi,"inputs/route-hists.csv",sep=''),append=T,sep=',',row.names=F,col.names=F)
    write.table(data.frame(from=from.name,to=to.name),file=paste(path.to.pevi,"inputs/route-hists.csv",sep=''),append=T,sep=',',row.names=F,col.names=T)
    cat(",",file=paste(path.to.pevi,"inputs/route-hists.csv",sep=''),append=T)
    write.table(route.hists[,,1,from.name,to.name],file=paste(path.to.pevi,"inputs/route-hists.csv",sep=''),append=T,sep=',')
  }
}

# finally, summarize the routes to get the distance/time/performance/enroute for each pairing
load(file=paste(path.to.pevi,'inputs/routing-corrected.Rdata',sep=''))
load(file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))
disttime <- ddply(route.ordered,.(from_taz,to_taz),function(df){ data.frame(miles=sum(df$length),time=sum(df$length/df$ab_speed),enroute=paste(unique(df$end.taz),collapse=","),perf=subset(perf,from==df$from_taz[1] & to==df$to_taz[1])$perf)})
names(disttime) <- c(';from','to','miles','time','enroute','perf')
disttime <- rbind(disttime,data.frame(from=unique(disttime$from),to=unique(disttime$from),miles=sqrt(taz$ACRES[match(unique(disttime$from),taz$id)]*0.001563)/2,time=sqrt(taz$ACRES[match(unique(disttime$from),taz$id)]*0.001563)/30/2,enroute='',perf=1))
disttime <- ddply(disttime[order(disttime$from),],.(from),function(df){ df[order(df$to),] })
write.csv(disttime,file=paste(path.to.geatm,'taz-dist-time.csv',sep=''),row.names=F)
write.table(disttime,file=paste(path.to.pevi,'inputs/taz-dist-time.txt',sep=''),row.names=F,sep="\t")

