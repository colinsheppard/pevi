######################################################################################################
# TAZ AGGREGATE
# 
# Take the original TAZ data and corresponding travel demand data and aggregated
# based on polygons generated in google earth (and converted to shape files in QGIS)
######################################################################################################

library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','rgdal','XML','plotKML'))
gpclibPermit()

path.to.geatm  <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
path.to.humveh <- '~/Dropbox/serc/pev-colin/data/Vehicle-Registration/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'
path.to.parcel <- '~/Dropbox/serc/pev-colin/data/HUM-PARCELS/'

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))

taz <- readShapePoly(paste(path.to.geatm,'Shape_Files/taz-LATLON.shp',sep=''))

# load the OD data
od.24.old <- read.table(paste(path.to.geatm,'OD_Tables/OD by type 24 hr (2020).txt',sep=''),header=FALSE, sep=",",colClasses='numeric')
names(od.24.old) <- c('from','to','hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')
od.am.old <- read.table(paste(path.to.geatm,'OD_Tables/OD by type AM (2020).txt',sep=''),header=FALSE, sep=",",colClasses='numeric')
names(od.am.old) <- c('from','to','hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')
od.pm.old <- read.table(paste(path.to.geatm,'OD_Tables/OD by type PM (2020).txt',sep=''),header=FALSE, sep=",",colClasses='numeric')
names(od.pm.old) <- c('from','to','hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')

# Sum demand to get a total
od.24.sum <- ddply(od.24.old,.(from),function(df){ sum(df$demand) })
names(od.24.sum) <- c('taz','demand')
taz@data$total.demand <- od.24.sum$demand[match(taz@data$NEWTAZ,od.24.sum$taz)]
taz@data$total.demand.per.acre <- taz@data$total.demand / taz@data$ACRES

# Make a plot of total demand disaggregated
#trellis.par.set(sp.theme())
#spplot(taz,'total.demand')
#spplot(taz,'total.demand.per.acre')

# Load the aggregation polygons
agg.polys <- readShapePoly(paste(path.to.google,'ProposedAggregation.shp',sep=''))
taz.centroids <-SpatialPointsDataFrame(coordinates(taz),data=data.frame(longitude= coordinates(taz)[,1],latitude= coordinates(taz)[,2]))
agg.mapping <- data.frame(name=over(taz.centroids,agg.polys)$Name)
agg.mapping$agg.id <- as.numeric(agg.mapping$name)
agg.taz.shp <- unionSpatialPolygons(taz,agg.mapping$agg.id)
aggregate.data <- function(df){ 
 return( colSums(df[,c('AREA','ACRES','SHAPE_AREA')]) ) 
}
taz@data$agg.id <- agg.mapping$agg.id
agg.taz.data <- ddply(taz@data,.(agg.id),aggregate.data)
agg.taz.shp <- SpatialPolygonsDataFrame(agg.taz.shp,agg.taz.data)

# add new zone numbers corresponding to old zone numbers to the dataframe
od.24.old$from.new <- taz$agg.id[match(od.24.old$from,taz$NEWTAZ)]
od.24.old$to.new <- taz$agg.id[match(od.24.old$to,taz$NEWTAZ)]
od.am.old$from.new <- taz$agg.id[match(od.am.old$from,taz$NEWTAZ)]
od.am.old$to.new <- taz$agg.id[match(od.am.old$to,taz$NEWTAZ)]
od.pm.old$from.new <- taz$agg.id[match(od.pm.old$from,taz$NEWTAZ)]
od.pm.old$to.new <- taz$agg.id[match(od.pm.old$to,taz$NEWTAZ)]

# for now, omit the rows with NA, which correspond to the TAZ id's in the OD data which don't have a
# corresponding entry in the TAZ shape file (ID's 11-20)
# I think this also omits ID's 1-10
od.24.new <- ddply(na.omit(od.24.old),.(from.new,to.new),function(df){ 
  c(sum(df$hbw),sum(df$hbshop),sum(df$hbelem),sum(df$hbuniv),sum(df$hbro),sum(df$nhb),sum(df$ix),sum(df$xi),sum(df$ee),sum(df$demand)) })
names(od.24.new) <- c('from','to','hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')
od.am.new <- ddply(na.omit(od.am.old),.(from.new,to.new),function(df){
  c(sum(df$hbw),sum(df$hbshop),sum(df$hbelem),sum(df$hbuniv),sum(df$hbro),sum(df$nhb),sum(df$ix),sum(df$xi),sum(df$ee),sum(df$demand)) })
names(od.am.new) <- c('from','to','hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')
od.pm.new <- ddply(na.omit(od.pm.old),.(from.new,to.new),function(df){ 
  c(sum(df$hbw),sum(df$hbshop),sum(df$hbelem),sum(df$hbuniv),sum(df$hbro),sum(df$nhb),sum(df$ix),sum(df$xi),sum(df$ee),sum(df$demand)) })
names(od.pm.new) <- c('from','to','hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')

save(od.24.new,od.am.new,od.pm.new,od.24.old,od.am.old,od.pm.old,file=paste(path.to.geatm,'od-old-and-new.Rdata',sep=''))

# check that the sum of the rows equals the value in the sum column
# they are currently not quite equal and I suspect this is from omiting zones 1-10
od.24.new$row.sum <- rowSums(od.24.new[,3:11])

# store total traffic leaving each new zone
od.24.sum.from <- ddply(od.24.new,.(from),function(df){ sum(df$demand) })
names(od.24.sum.from) <- c('taz','demand')
od.24.sum.to <- ddply(od.24.new,.(to),function(df){ sum(df$demand) })
names(od.24.sum.to) <- c('taz','demand')

if(names(agg.taz.shp@data)[1]!="id")names(agg.taz.shp@data) <- c('id',names(agg.taz.shp@data)[2:ncol(agg.taz.shp@data)]) # make first column of shape data 'id' instead of 'agg.id'

# add total demand to the existing shape data
agg.taz.shp@data$total.demand.from <- od.24.sum.from$demand[agg.taz.shp$id]
agg.taz.shp@data$total.demand.to <- od.24.sum.to$demand[agg.taz.shp$id]

trellis.par.set(sp.theme())
spplot(agg.taz.shp,c('total.demand.from','total.demand.to'))

# look at where people from each TAZ are going to 
agg.names <- as.character(ddply(agg.mapping,.(agg.id),function(df){ df[1,]})$name)
agg.taz.shp@data$name <- agg.names[agg.taz.shp@data$id]

#agg.points <- fortify(agg.taz.shp,region="id")
#agg.df <- join(agg.points,agg.taz.shp@data,by="id")

#ggplot(agg.df,aes(long,lat,group=name))+
  #geom_polygon(aes(fill=total.demand)) +
  #geom_path(color="white") +
  #coord_equal()

#make.dir(paste(path.to.plots,'demand',sep=''))
#for(agg.taz.id in agg.taz.shp$id){
  #taz.name <- str_replace_all(as.character(agg.mapping$name[agg.mapping$agg.id == agg.taz.id][1]),'-','.')
  #if(!is.na(as.numeric(substr(taz.name,1,1)))) taz.name <- paste('HWY',taz.name,sep='')
  #new.col.name.to <- paste(taz.name,'.to',sep='')
  #new.col.name.from <- paste(taz.name,'.of.total',sep='')
  
  #agg.taz.shp@data[,new.col.name.to] <- subset(od.24.new,from==agg.taz.id)$demand[agg.taz.shp$id]/sum(subset(od.24.new,from==agg.taz.id)$demand)*100
  #agg.taz.shp@data[,new.col.name.from] <- subset(od.24.new,to==agg.taz.id)$demand[agg.taz.shp$id]/sum(od.24.new$demand)*100
  #pdf(file=paste(path.to.plots,'demand/',taz.name,'.pdf',sep=''),16,12)
  #trellis.par.set(sp.theme())
  #print(spplot(agg.taz.shp,c(new.col.name.to,new.col.name.from)))
  #dev.off()
#}
#spplot(agg.taz.shp,names(agg.taz.shp@data)[grep('of.total',names(agg.taz.shp@data))], names.attr= agg.taz.shp$name,colorkey=list(space="bottom"))

# write the data to a shapefile, note we need to write the names of the fields separately b/c they get truncated due to ESRI format limitations
writePolyShape(agg.taz.shp,paste(path.to.pevi,'inputs/development/aggregated-taz',sep=''))
agg.taz.shp.fieldnames <- names(agg.taz.shp@data)
save(agg.taz.shp.fieldnames,file=paste(path.to.pevi,'inputs/development/aggregated-taz-fieldnames.Rdata',sep=''))


# EXTRA PLOTTING IN G-EARTH

# write the data to KML file with colors related to traffic demand
c.map <- paste(map.color(agg.taz.shp@data$total.demand.from,blue2red(50)),'7F',sep='')
shp.to.kml(agg.taz.shp,paste(path.to.pevi,'inputs/development/aggregated-taz.kml',sep=''),'Aggregated TAZs','Color denotes total daily demand','red',1.5,c.map,name.col='name',description.cols=names(agg.taz.shp@data))

c.map <- paste(map.color(taz@data$total.demand,blue2red(50)),'7F',sep='')
 shp.to.kml(taz,paste(path.to.pevi,'inputs/development/disaggregated-taz.kml',sep=''),'Disaggregated TAZs','Color denotes total daily demand','white',1.5,c.map,name.col='NEWTAZ',description.cols=c('total.demand','NEWTAZ','ACRES'),id.col='ID')

for.c.map <- log(taz@data$total.demand.per.acre[-which(taz@data$total.demand.per.acre==Inf)])
for.c.map[for.c.map<=0] <- 0 
c.map <- paste(map.color(for.c.map,blue2red(50)),'7F',sep='')
 shp.to.kml(taz[-which(taz@data$total.demand.per.acre==Inf),],paste(path.to.pevi,'inputs/development/disaggregated-taz-per-acre.kml',sep=''),'Disaggregated TAZs','Color denotes total daily demand per acre','white',1.5,c.map,name.col='NEWTAZ',description.cols=c('total.demand.per.acre','total.demand','NEWTAZ','ACRES'),id.col='ID')


