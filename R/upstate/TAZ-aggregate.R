######################################################################################################
# UPSTATE TAZs
######################################################################################################
taz <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/Shasta_TAZ'))

# Load the aggregation polygons
agg.polys <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/ProposedAggregatedTAZs'))
agg.polys$Name <- as.character(agg.polys$Name)
agg.polys$Name[agg.polys$Name=="BigBen"] <- "BigBend"

taz.centroids <-SpatialPointsDataFrame(coordinates(taz),data=data.frame(longitude= coordinates(taz)[,1],latitude= coordinates(taz)[,2]))
agg.mapping <- data.frame(name=over(taz.centroids,agg.polys)$Name)
agg.mapping$agg.id <- as.numeric(agg.mapping$name)+100
agg.taz.shp <- unionSpatialPolygons(taz,agg.mapping$agg.id)
taz$agg.id <- agg.mapping$agg.id
taz$name <- agg.mapping$name
# here I had to correct for fact that a proposed polygon didn't catch the centroid of the original TAZ
taz$agg.id[which(taz$TAZ==904)] <- 104
taz$name[which(taz$TAZ==904)] <- "AND_OxYoke"
agg.taz.data <- ddply(taz@data[!is.na(taz@data$agg.id),],.(agg.id),function(df){ 
  data.frame(name=df$name[1],area=sum(df$AREA),jurisdiction=pp(unique(df$JURISDCTN),collapse=","),stringsAsFactors=F)
})
row.names(agg.taz.data) <- agg.taz.data$agg.id
agg.taz.shp <- SpatialPolygonsDataFrame(agg.taz.shp,agg.taz.data)
agg.taz.shp$name <- as.character(agg.taz.shp$name)
agg.taz.shp$shp.id <- unlist(lapply(agg.taz.shp@polygons,function(x){slot(x,'ID')}))
writePolyShape(agg.taz.shp,pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZs'))
shp.to.kml(agg.taz.shp,pp(pevi.shared,'data/UPSTATE/kml/AggregatedTAZs.kml'),'Aggregated TAZs','','red',2,'#00000000','agg.id','name',c('name','agg.id'))

taz.data <- data.table(taz@data)
save(taz.data,file=pp(pevi.shared,'data/UPSTATE/shapefiles/taz-aggregation-mapping.Rdata'))

# Here I'm faking up some EVSE and PEVs to put into a shapefile as points for making up pretty slide to visualize the process:
use.rows <- sample(1:nrow(taz@data),60)
fake.evse <-SpatialPointsDataFrame(coordinates(taz)[use.rows,],data=data.frame(longitude= coordinates(taz)[use.rows,1],latitude= coordinates(taz)[use.rows,2]))
writePointsShape(fake.evse,pp(pevi.shared,'data/UPSTATE/shapefiles/FakeEVSE'))

rds <- readShapeLines(pp(pevi.shared,'data/UPSTATE/shapefiles/Upstate_Roads'))
rd.coords <- ldply(coordinates(rds),function(l){ l[[1]] })
use.rows <- sample(1:nrow(rd.coords),200)
fake.pevs <-SpatialPointsDataFrame(rd.coords[use.rows,],data=data.frame(longitude=rd.coords[use.rows,1],latitude=rd.coords[use.rows,2]))
plot(fake.pevs)
writePointsShape(fake.pevs,pp(pevi.shared,'data/UPSTATE/shapefiles/FakePEVs'))

# Here I manually cleaned up the polygons (inner rings were scattered throughout) and re-saved to AggregatedTAZs.kml/AggregatedTAZs.shp
agg.taz <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZs'))

# Load distance-time
load(pp(pevi.shared,'data/UPSTATE/driving-distances/taz_time_distance.Rdata'))
# Load the OD data
load(file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-by-purpose.Rdata'))

odp$from.agg <- taz$agg.id[match(odp$from,taz$TAZ)]
odp$to.agg <- taz$agg.id[match(odp$to,taz$TAZ)]
odp$from.agg[is.na(odp$from.agg)] <- odp$from[is.na(odp$from.agg)]
odp$to.agg[is.na(odp$to.agg)] <- odp$to[is.na(odp$to.agg)]

od.agg <- ddply(odp,.(from.agg,to.agg),function(df){
  colSums(df[,3:(ncol(df)-2)]) 
})
names(od.agg) <- c('from','to',names(od.agg)[3:ncol(od.agg)])

purp.cols <- c('com','ho','hs','hsc','hw','oo','wo')
tot.demand <- ddply(od.agg,.(from),function(df){ data.frame(demand=sum(df[,purp.cols])) })
agg.taz$total.demand <- tot.demand$demand[match(agg.taz$agg.id,tot.demand$from)]

c.map <- paste(map.color(agg.taz$total.demand,blue2red(50)),'7F',sep='')
shp.to.kml(agg.taz,pp(pevi.shared,'data/UPSTATE/kml/AggregatedTAZs.kml'),'Aggregated TAZs','','white',2,c.map,'shp.id','name',c('name','agg.id','total.demand'))

# Save as Rdata to minimize issues with converting formats
#save(agg.taz,file=pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZs.Rdata'))
load(file=pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZs.Rdata'))
# Save od data
#save(od.agg,file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-aggregated.Rdata'))
load(file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-aggregated.Rdata'))

# Now add point TAZs to taz spatial data as lat/lon circles, note that the ID we choose should not collide with agg.taz polygon IDs
load(file=pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs.Rdata'))
id.start <- tail(as.numeric(unlist(lapply(agg.taz@polygons,function(l){ slot(l,"ID") }))),1) + 1
pt.tazs.coords <- data.frame(row=id.start:(id.start-1+nrow(pt.taz@data)),coordinates(pt.taz))
pt.tazs.poly <- SpatialPolygons(dlply(pt.tazs.coords,.(row),function(df){ 
  coords <- data.frame( lon=df$coords.x1 + 0.01 * sin(seq(0,2*pi,by=pi/8)),
              lat=df$coords.x2 + 0.01 * cos(seq(0,2*pi,by=pi/8)))
  Polygons(list(Polygon(coords)),df$row[1])
}),1:nrow(pt.tazs.coords))
blank.df <- data.frame(matrix(NA,nrow(pt.tazs.coords),ncol(agg.taz@data)))
names(blank.df) <- names(agg.taz@data)
rownames(blank.df) <- pt.tazs.coords$row
pt.tazs.df <- SpatialPolygonsDataFrame(pt.tazs.poly,blank.df)
agg.taz <- rbind(agg.taz,pt.tazs.df)
pt.rows <- which(is.na(agg.taz$agg.id))
agg.taz$agg.id[pt.rows] <- 200 + 1:(length(pt.rows))
agg.taz$name <- as.character(agg.taz$name)
agg.taz$name[pt.rows] <- pt.taz$Name
agg.taz$total.demand[pt.rows] <- pt.taz$trips_from
agg.taz$population <- NA
agg.taz$population[pt.rows] <- pt.taz$population
agg.taz$employment <- NA
agg.taz$employment[pt.rows] <- pt.taz$employment
agg.taz$shp.id[pt.rows] <- pt.tazs.coords$row
agg.taz$point <- agg.taz$agg.id > 200

# Load the Greater Redding area to use as an exclusion criterion
redding <- readOGR(pp(pevi.shared,'data/UPSTATE/kml/GreaterReddingArea.kml'),'GreaterReddingArea.kml')
proj4string(agg.taz) <- CRS('+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs')
agg.taz.centroids <-SpatialPointsDataFrame(coordinates(agg.taz),data=data.frame(longitude=coordinates(agg.taz)[,1],latitude=coordinates(agg.taz)[,2]),proj4string=CRS(proj4string(agg.taz)))
agg.mapping <- data.frame(name=over(agg.taz.centroids,redding)$Name)
agg.taz$near.redding <- !is.na(agg.mapping$name)

agg.taz$jurisdiction[which(coordinates(agg.taz)[,2]>41 & agg.taz$point)] <- 'Siskiyou'
agg.taz$jurisdiction[which(coordinates(agg.taz)[,2]<41 & agg.taz$point)] <- 'Tehama'

# Change name of Mt. Shasta TAZ
agg.taz$name[agg.taz$name=="Mt. Shasta"] <- "MtShasta"

#save(agg.taz,file=pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))
load(file=pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))

