load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','RPostgreSQL','colorRamps','reshape','doMC','rgeos'))
registerDoMC(10)
gpclibPermit()

source(paste(pevi.home,'R/gis-functions.R',sep=''))

dbuser<-"pev"
dbpassword<-""
dbname<-"upstate"
dbhost<-"localhost"

con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	

# Following are setup commands that only need to happen once
#dbSendQuery(con,'update network set speed_limi=NULL where "speed_limi"=0;')
#dbSendQuery(con,'ALTER TABLE network ADD COLUMN cost float8')
#dbSendQuery(con,'ALTER TABLE network ADD COLUMN length float8')
#dbSendQuery(con,'UPDATE network set length=ST_Length(geom)*0.000621371')
#dbSendQuery(con,'UPDATE network set cost=length/coalesce(speed_limi,0.00000001)')
#dbSendQuery(con,'CREATE TABLE taz_route (
  #id          serial PRIMARY KEY,
  #from_taz    integer NOT NULL,
  #to_taz      integer NOT NULL,
  #network_id  integer NOT NULL,
  #time_cost   double precision NOT NULL,
  #start_elev   double precision NOT NULL,
  #end_elev     double precision NOT NULL,
  #gradient     double precision NOT NULL,
  #route_order integer NOT NULL
#);')

dbGetQuery(con,paste("DELETE FROM taz_route"))

net <- readWKT(dbGetQuery(con,pp("SELECT ST_AsText(geom) AS geom FROM network WHERE speed_limi>30"))$geom)
# find the overall bbox
bbox <- net[[1]]@bbox
for(i in 2:length(net)){
  if(net[[i]]@bbox[1,1] < bbox[1,1])net[[i]]@bbox[1,1] -> bbox[1,1]
  if(net[[i]]@bbox[1,2] > bbox[1,2])net[[i]]@bbox[1,2] -> bbox[1,2]
  if(net[[i]]@bbox[2,1] < bbox[2,1])net[[i]]@bbox[2,1] -> bbox[2,1]
  if(net[[i]]@bbox[2,2] > bbox[2,2])net[[i]]@bbox[2,2] -> bbox[2,2]
}
plot(net[[1]],xlim=bbox[1,],ylim=bbox[2,],col='lightgrey',lwd=0.75)
for(i in 2:length(net)){
  plot(net[[i]],col='lightgrey',lwd=0.75,add=T)
}

nodes <- c(16362,504,1,60,12329,17174,17125,5751,206,32,16436,15982,1362,13286,12234,11420,2841,1662,3083,11098,3606)
nn <- combn(nodes,2)
#for(i in 1:ncol(nn)){
i<-23
  cat(paste(i,",",sep=''))
  start <- nn[1,i]
  end   <- nn[2,i]
  start.pt <- readWKT(dbGetQuery(con,pp("SELECT ST_AsText(geom) AS geom FROM node WHERE id=",start))$geom)
  end.pt <- readWKT(dbGetQuery(con,pp("SELECT ST_AsText(geom) AS geom FROM node WHERE id=",end))$geom)
  route <- dbGetQuery(con,paste("SELECT id,net1.cost,ST_AsText(geom) AS geom
       FROM network AS net1 JOIN
       (SELECT * FROM shortest_path_astar('
          SELECT id, 
              start_id::int4 AS source, 
              end_id::int4 AS target, 
              cost,
              ST_X(startpoint) AS x1, 
              ST_Y(startpoint) AS y1, 
              ST_X(endpoint) AS x2, 
              ST_Y(endpoint) AS y2
          FROM network AS net2',
          ",start,",",end,",
          false,
          false)) AS route ON net1.id = route.edge_id;",sep=""))

  route.sp <- readWKT(route$geom)

  points(route.sp,type='l',color='red',lwd=2)
  if(nrow(route)>0){
    stop('')
  }
#}


require(rgdal)
dsn<-"pgsql:dbname='upstate';user=pev;host=localhost"
ogrListLayers(dsn)


