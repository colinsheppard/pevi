######################################################################################################
# OD Processing
# 
# The Upstate model requires some processing of the OD data provided by SRTA in adddition to the 
# creation of travel demand data based on CHTS
######################################################################################################

load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','rgdal','XML','plotKML','rgeos','doMC'))
gpclibPermit()
registerDoMC(num.cpu)

# Load the OD data
od <- read.table(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2010/sh10_adjvehtrips_3per_3occ.txt'),header=T)

# For now, aggregate occupancy levels into a single column
od$trips.24 <- od$trips.1occ.24 + od$trips.2occ.24 + od$trips.3occ.24
od$trips.am <- od$trips.1occ.am + od$trips.2occ.am + od$trips.3occ.am
od$trips.pm <- od$trips.1occ.pm + od$trips.2occ.pm + od$trips.3occ.pm
od <- od[,c(1,2,(ncol(od)-2):ncol(od))]

# Zero rows are excluded from the data, add a row for from==292 for convenience in processing 
od <- rbind(od,data.frame(from=292,to=101,trips.24=0,trips.am=0,trips.pm=0))

# Find # trips in/out of each TAZ per capita
tot.trips <- data.frame(taz=sort(unique(od$from)),orig=ddply(od,.(from),function(df){ sum(df$trips.24) })$V1,dest=ddply(od,.(to),function(df){ sum(df$trips.24) })$V1)

taz <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/Shasta_TAZ'))
taz.centroids <- data.frame(taz=taz$TAZ,longitude=coordinates(taz)[,1],latitude=coordinates(taz)[,2])

taz.centroids.sp <-SpatialPointsDataFrame(coordinates(taz),data=taz.centroids)
redd <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/GreaterReddingArea'))
taz.centroids.sp$near.redding <- !is.na(over(taz.centroids.sp,redd)$Name)

dem <- read.csv(pp(pevi.shared,'data/UPSTATE/demographics/TAZ-Demography-2004-Base-Scenario.csv'))
dem$long  <-  taz.centroids$long[match(dem$TAZ,taz.centroids$taz)]
dem$lat   <-  taz.centroids$lat[match(dem$TAZ,taz.centroids$taz)]
dem$near.redding <-  taz.centroids.sp$near.redding[match(dem$TAZ,taz.centroids.sp$taz)]
dem$trips.from  <- tot.trips$orig[match(dem$TAZ,tot.trips$taz)]
dem$trips.to    <- tot.trips$dest[match(dem$TAZ,tot.trips$taz)]

# get rid of data from within redding area, very low populations and the outliers in terms of trips/capita (college and industrial TAZs)
to.remove <- c(975,1136)
dem.sub <- subset(dem,!near.redding & Population>20 & !TAZ %in% to.remove)

# Explore / Analyze / Consider
ggplot(dem.sub,aes(x=Population,y=trips.from/Population))+geom_point()+facet_wrap(~Community)
ggplot(dem.sub,aes(x=Population,y=trips.from/Population))+geom_point()+facet_wrap(~Community)
ggplot(dem.sub,aes(x=TOTAL.1,y=trips.from/Population))+geom_point()
ggplot(subset(dem,!near.redding),aes(x=long,y=lat,size=trips.from/Population,colour=TOTAL.1))+geom_point()
pairs(dem.sub[,c('trips.to','Population','Service.Commercial','Office','School','Restaurant','TOTAL','TOTAL.1','Pop.HU')],pch='.')
summary(lm('trips.from ~ Population + Service.Commercial + Office + School + Restaurant + TOTAL + TOTAL.1 + Pop.HU',dem.sub))

# load the poinTAZS
pt.taz <- readShapePoints(pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs'))
write.csv(pt.taz@data,pp(pevi.shared,'data/UPSTATE/demographics/pt-tazs.csv'))
# in here I manually added demog data
pt.taz.data <- read.csv(pp(pevi.shared,'data/UPSTATE/demographics/pt-tazs.csv'))
fit.from <- lm('trips.from ~ Population + TOTAL.1',dem.sub)
fit.to <- lm('trips.to ~ Population + TOTAL.1',dem.sub)
pt.taz$Population <- pt.taz.data$population[match(pt.taz$Name,pt.taz.data$name)]
pt.taz$TOTAL.1 <- pt.taz.data$employment[match(pt.taz$Name,pt.taz.data$name)]
pt.taz$trips.from <- predict(fit.from,newdata=pt.taz@data)
pt.taz$trips.to <- predict(fit.to,newdata=pt.taz@data)
