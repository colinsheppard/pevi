######################################################################################################
# OD Processing
# 
# The Upstate model requires some processing of the OD data provided by SRTA in adddition to the 
# creation of travel demand data based on CHTS
######################################################################################################

load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','rgdal','XML','plotKML','rgeos','doMC','reshape'))
gpclibPermit()
registerDoMC(num.cpu)

# Load the OD data
od <- read.table(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/sh20_adjvehtrips_3per_3occ.txt'),header=T)

# For now, aggregate occupancy levels into a single column
od$trips.24 <- od$trips.1occ.24 + od$trips.2occ.24 + od$trips.3occ.24
od$trips.am <- od$trips.1occ.am + od$trips.2occ.am + od$trips.3occ.am
od$trips.pm <- od$trips.1occ.pm + od$trips.2occ.pm + od$trips.3occ.pm
od <- od[,c(1,2,(ncol(od)-2):ncol(od))]

# For 2010 OD matrix, zero rows are excluded from the data, add a row for from==292 for convenience in processing 
#od <- rbind(od,data.frame(from=292,to=101,trips.24=0,trips.am=0,trips.pm=0))


# Take the pa matrix and distribute it by time of day according to the od distribution
if(!file.exists(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-by-purpose.Rdata'))){
  pa <- read.table(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/sh20_vehtrips_7purp_3occ.txt'),header=T)
  ## create an omni-directional id over which we can sum PA/AP pairs in both directions
  pa$omni <- apply(pa,1,function(x){
    pp(sort(x[1:2]),collapse=".")
  })
  purp.cols <- c('com','ho','hs','hsc','hw','oo','wo')
  pa <- pa[,c('p','a',purp.cols,'tot','omni')]
  # the following data were from a DOT report for the city of Jacksonville Florida, only used b/c quicker than 
  # analyzing CHTS for the same data, but this should be done eventually
  temporal.dists <- data.frame(pa.ap=c(rep('pa',7),rep('ap',7)),
    purp=c('hw','hs','ho','wo','oo','hsc','com','hw','hs','ho','wo','oo','hsc','com'),
    am=c(.67,.1,.46,.12,.12,.67,.20,.03,.03,.09,.13,.13,.03,.2),
    pm=c(.06,.28,.17,.29,.29,.06,.25,.67,.38,.39,.29,.29,.66,.3))
  temporal.dists <- cast(melt(temporal.dists,id.vars=c('pa.ap','purp'),measure.vars=c('am','pm')),'pa.ap ~ purp ~ variable')
  #pa.ap <- ddply(pa,.(omni),function(df){
    #colSums(df[,c('hw','hs','ho','wo','oo','hsc','com','tot')])
  #})
  #od$omni <- apply(od,1,function(x){
    #pp(sort(x[1:2]),collapse=".")
  #})
  #od.do <- ddply(od,.(omni),function(df){
    #colSums(df[,c('trips.24','trips.am','trips.pm')])
  #})
  #od.do$frac.am <- od.do$trips.am / od.do$trips.24
  #od.do$frac.am[od.do$frac.am>1] <- NA
  #od.do$frac.pm <- od.do$trips.pm / od.do$trips.24
  #od.do$frac.pm[od.do$frac.pm>1] <- NA
  #pa.z <- apply(pa.ap[,c('hw','hs','ho','wo','oo','hsc','com')],1,function(x){ sum(x==0) })
  odp <- data.frame(expand.grid(unique(c(od$from,od$to)),unique(c(od$from,od$to))))
  names(odp) <- c('from','to')
  odp$omni <- apply(odp,1,function(x){
    pp(sort(x[1:2]),collapse=".")
  })
  odp <- ddply(odp,.(from,to),function(df){
    pa.row <- subset(pa,p==df$from & a==df$to)[,purp.cols]/2
    if(nrow(pa.row)==0)pa.row[1,] <- 0
    ap.row <- subset(pa,a==df$from & p==df$to)[,purp.cols]/2
    if(nrow(ap.row)==0)ap.row[1,] <- 0
    am.row <-  pa.row * temporal.dists['pa',,'am'] + ap.row * temporal.dists['ap',,'am']
    pm.row <-  pa.row * temporal.dists['pa',,'pm'] + ap.row * temporal.dists['ap',,'pm']
    all.day.row <- colSums(subset(pa,omni==df$omni)[,purp.cols])/2
    data.frame(t(all.day.row),am.row,pm.row)
  },.parallel=T)
  names(odp) <- c('from','to',purp.cols,pp(purp.cols,'.am'),pp(purp.cols,'.pm'))
  save(odp,file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-by-purpose.Rdata'))
}else{
  load(file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-by-purpose.Rdata'))
}

taz <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/Shasta_TAZ'))
taz.centroids <- data.frame(taz=taz$TAZ,longitude=coordinates(taz)[,1],latitude=coordinates(taz)[,2])

taz.centroids.sp <-SpatialPointsDataFrame(coordinates(taz),data=taz.centroids)
redd <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/GreaterReddingArea'))
taz.centroids.sp$near.redding <- !is.na(over(taz.centroids.sp,redd)$Name)

# Find # trips in/out of each TAZ per capita
tot.trips <- data.frame(taz=sort(unique(odp$from)),orig=ddply(odp,.(from),function(df){ sum(df[,purp.cols]) })$V1,dest=ddply(odp,.(to),function(df){ sum(df[,purp.cols]) })$V1)

dem <- read.csv(pp(pevi.shared,'data/UPSTATE/demographics/TAZ-Demography-2004-Base-Scenario.csv'))
dem$long  <-  taz.centroids$long[match(dem$TAZ,taz.centroids$taz)]
dem$lat   <-  taz.centroids$lat[match(dem$TAZ,taz.centroids$taz)]
dem$near.redding <-  taz.centroids.sp$near.redding[match(dem$TAZ,taz.centroids.sp$taz)]
dem$trips.from  <- tot.trips$orig[match(dem$TAZ,tot.trips$taz)]
dem$trips.to    <- tot.trips$dest[match(dem$TAZ,tot.trips$taz)]

# get rid of data from within redding area, very low populations and the outliers in terms of trips/capita (college and industrial TAZs)
to.remove <- c(975,1136)
dem.sub <- subset(dem,!near.redding & Population>20 & !TAZ %in% to.remove)
names(dem.sub) <- str_replace(str_replace(names(dem.sub),"Population","population"),"TOTAL.1","employment")

# Explore / Analyze / Model
#ggplot(dem.sub,aes(x=Population,y=trips.from/Population))+geom_point()+facet_wrap(~Community)
#ggplot(dem.sub,aes(x=Population,y=trips.from/Population))+geom_point()+facet_wrap(~Community)
#ggplot(dem.sub,aes(x=TOTAL.1,y=trips.from/Population))+geom_point()
#ggplot(subset(dem,!near.redding),aes(x=long,y=lat,size=trips.from/Population,colour=TOTAL.1))+geom_point()
#pairs(dem.sub[,c('trips.to','Population','Service.Commercial','Office','School','Restaurant','TOTAL','TOTAL.1','Pop.HU')],pch='.')
#summary(lm('trips.from ~ Population + Service.Commercial + Office + School + Restaurant + TOTAL + TOTAL.1 + Pop.HU',dem.sub))

# load the pointTAZS
pt.taz <- readShapePoints(pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs'))
if(!"trips_from" %in% names(pt.taz@data)){
  #write.csv(pt.taz@data,pp(pevi.shared,'data/UPSTATE/demographics/pt-tazs.csv'))
  # in here I manually added demog data
  pt.taz.data <- read.csv(pp(pevi.shared,'data/UPSTATE/demographics/pt-tazs.csv'))
  fit.from <- lm('trips.from ~ population + employment',dem.sub)
  fit.to <- lm('trips.to ~ population + employment',dem.sub)
  pt.taz$population <- pt.taz.data$population[match(pt.taz$Name,pt.taz.data$Name)]
  pt.taz$employment <- pt.taz.data$employment[match(pt.taz$Name,pt.taz.data$Name)]
  pt.taz$trips_from <- predict(fit.from,newdata=pt.taz@data)
  pt.taz$trips_to <- predict(fit.to,newdata=pt.taz@data)
  pt.taz$Name <- as.character(pt.taz$Name)
  pt.taz$Name[pt.taz$Name=="Platina Center"] <- "Platina"
  pt.taz@data <- pt.taz@data[,c(1,(ncol(pt.taz@data)-3):ncol(pt.taz@data))]
  write.csv(pt.taz@data,pp(pevi.shared,'data/UPSTATE/demographics/pt-tazs.csv'))
  writePointsShape(pt.taz,pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs'))
}
save(pt.taz,file=pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs.Rdata'))


