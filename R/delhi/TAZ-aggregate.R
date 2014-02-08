######################################################################################################
# DELHI TAZs
######################################################################################################
load.libraries(c('maptools'))
taz <- readShapePoly(pp(pevi.shared,'data/DELHI/POLYGON/Delhi_Zones_Boundary_region_TDF_WGS84'))

# Load the proposed aggregation polygons
pro.agg.polys <- readShapePoly(pp(pevi.shared,'data/DELHI/POLYGON/proposed-aggregations'))
pro.agg.polys$Name <- as.character(pro.agg.polys$Name)

taz.centroids <-SpatialPointsDataFrame(coordinates(taz),data=data.frame(longitude= coordinates(taz)[,1],latitude= coordinates(taz)[,2]))
agg.mapping <- data.frame(name=over(taz.centroids,pro.agg.polys)$Name)
agg.mapping$agg.id <- as.numeric(agg.mapping$name)
agg.taz.shp <- unionSpatialPolygons(taz,agg.mapping$agg.id)

# here I export the aggregated TAZs to cleanup in qgis as they contain a bunch of stray lines
taz$agg.id <- agg.mapping$agg.id
taz$name <- agg.mapping$name
agg.taz.data <- ddply(taz@data[!is.na(taz@data$agg.id),],.(agg.id),function(df){ 
  data.frame(name=df$name[1],stringsAsFactors=F)
})
row.names(agg.taz.data) <- agg.taz.data$agg.id
agg.taz.shp <- SpatialPolygonsDataFrame(agg.taz.shp,agg.taz.data)
agg.taz.shp$name <- as.character(agg.taz.shp$name)
agg.taz.shp$shp.id <- unlist(lapply(agg.taz.shp@polygons,function(x){slot(x,'ID')}))
writePolyShape(agg.taz.shp,pp(pevi.shared,'data/DELHI/POLYGON/AggregatedTAZs'))

# Now load the cleaned polys
agg.taz <- readShapePoly(pp(pevi.shared,'data/DELHI/POLYGON/AggregatedTAZsCleaned'))

# Save the mapping for later use
taz.data <- data.table(taz@data)
save(taz.data,file=pp(pevi.shared,'data/DELHI/taz-aggregation-mapping.Rdata'))

###############################################################
# If you haven't done so already, complete the calculation of
# distance/time in distance-time.R
###############################################################

# Load distance-time
load(pp(pevi.shared,'data/DELHI/taz_time_distance.Rdata'))
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

