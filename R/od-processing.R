######################################################################################################
# OD Processing
# 
# The Upstate model requires some processing of the OD data provided by SRTA in adddition to the 
# creation of travel demand data based on CHTS
######################################################################################################

load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','XML','plotKML','rgeos','doMC','reshape','data.table'))
gpclibPermit()
registerDoMC(num.cpu)

# Take the pa matrix and distribute it by time of day according to the od distribution
if(!file.exists(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-by-purpose.Rdata'))){
  # Load the OD data
  od <- read.table(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/sh20_adjvehtrips_3per_3occ.txt'),header=T)

  # For now, aggregate occupancy levels into a single column
  od$trips.24 <- od$trips.1occ.24 + od$trips.2occ.24 + od$trips.3occ.24
  od$trips.am <- od$trips.1occ.am + od$trips.2occ.am + od$trips.3occ.am
  od$trips.pm <- od$trips.1occ.pm + od$trips.2occ.pm + od$trips.3occ.pm
  od <- od[,c(1,2,(ncol(od)-2):ncol(od))]

  # For 2010 OD matrix, zero rows are excluded from the data, add a row for from==292 for convenience in processing 
  #od <- rbind(od,data.frame(from=292,to=101,trips.24=0,trips.am=0,trips.pm=0))

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
if(file.exists(pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs.Rdata'))){
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
}else{
  load(file=pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs.Rdata'))
}

# load the agg.taz that include the pt TAZs 
# (these are created by TAZ-aggregate.R which requires the point TAZs developed above)
load(file=pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))

# Load od.agg, the aggregated OD matrix
load(file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-aggregated.Rdata'))

# Load taz.time.distance
load(pp(pevi.shared,'data/UPSTATE/driving-distances/taz_time_distance.Rdata'))

# add ids to dist/time matrix
taz.time.distance$origin.id <- agg.taz$agg.id[match(taz.time.distance$origin.taz,agg.taz$name)] 
taz.time.distance$destination.id <- agg.taz$agg.id[match(taz.time.distance$destination.taz,agg.taz$name)] 

#fric.ids <- subset(agg.taz,!point & !near.redding)$agg.id
#fric.ids <- subset(agg.taz,!point)$agg.id
#fric.dts <- subset(taz.time.distance,origin.id %in% fric.ids & destination.id %in% fric.ids)[,c('origin.id','destination.id','miles')]
#fric.dts$key <- pp(fric.dts$origin.id,"_",fric.dts$destination.id)
#fric.od <- subset(od.agg,from %in% fric.ids & to %in% fric.ids)
#fric.od$key <- pp(fric.od$from,"_",fric.od$to)
#fric.m <- melt(fric.od[,c(ncol(fric.od),3:(ncol(fric.od)-1))],id.vars='key')
#fric.m$dist <- fric.dts$miles[match(fric.m$key,fric.dts$key)]
#fric.m$value <- round(fric.m$value)
#fric.mm <- ddply(fric.m,.(key,variable),function(df){ data.frame(dist=rep(df$dist,df$value)) })
#ggplot(fric.m,aes(x=dist))+geom_histogram()+facet_wrap(~variable)

# Load CHTS for NSSR
load(file=pp(pevi.shared,'data/CHTS/nssr-subset.Rdata'))
setkey(nssr.place,"td.purpose")
dist.by.purp <- dlply(subset(nssr.place,tripdistance<400),.(td.purpose),function(df){ df$tripdistance})

# specify gateways from SRTA that will become internalized and lose their status as gateways in new matrix associate the 
# shasta TAZs that should be assumed to flow through those gateways en-route to SIS or TEH

gates.to.intern <- list('1'=list('Siskiyou'=as.numeric(grep('111|107|152|117',subset(agg.taz@data,agg.id<200)$agg.id,value=T,invert=T)),'Tehama'=c()),
                        '3'=list('Siskiyou'=c(111,107,152,117),'Tehama'=c()),
                        '13'=list('Tehama'=120,'Siskiyou'=c()),
                        '12'=list('Tehama'=as.numeric(grep('120',subset(agg.taz@data,agg.id<200)$agg.id,value=T,invert=T)),'Siskiyou'=c()))

orig.gates <- unique(od.agg$from)[which(unique(od.agg$from)<100)]
gates.to.keep <- orig.gates[-match(as.numeric(names(gates.to.intern)),orig.gates)]
gates.to.intern.ids <- as.numeric(names(gates.to.intern))
sha.tazs <- agg.taz$agg.id[which(agg.taz$agg.id>=100 & agg.taz$agg.id<200)]

new.tazs <- agg.taz$agg.id

# produce a unique P/A matrix for each case (SIS vs TEH) from the aggregated OD matrix and the total demand estimates on the point TAZs
purps <- c('ho','hs','hsc','hw','oo','wo')
new.pa <- expand.grid(juris=c('Siskiyou','Tehama'),taz=new.tazs,purp=purps)
new.pa$trips <- NA
new.pa <- data.table(new.pa)
setkey(new.pa,'taz')
agg.dt <- data.table(agg.taz@data)
agg.dt[,taz:=agg.id]
setkey(agg.dt,"taz")
new.pa.temp <- new.pa[agg.dt]
new.pa <- subset(new.pa.temp,agg.id<200 | juris==jurisdiction)[,list(taz=taz,juris=juris,purp=purp,trips=trips)]

od.agg <- as.data.table(od.agg)

setkey(od.agg,'from')
od.agg.sums <- od.agg[,list(ho=sum(ho),hs=sum(hs),hsc=sum(hsc),hw=sum(hw),oo=sum(oo),wo=sum(wo),tot=sum(c(ho,hs,hsc,hw,oo,wo))),by="from"]
od.agg.sums <- od.agg.sums[,':='(ho=ho/tot,hs=hs/tot,hsc=hsc/tot,hw=hw/tot,oo=oo/tot,wo=wo/tot)]
od.agg.sums.m <- data.table(melt(od.agg.sums,id.vars='from',measure.vars=c('ho','hs','hsc','hw','oo','wo')),key=c('from'))

# look into potential demographic relationships
#dem <- data.table(dem)
#dem[,from:=TAZ]
#dem[,':='(from=TAZ,population=Population,Population=NULL,employment=TOTAL.1)]
#setkey(dem,"from")
#od.agg.sums.m <- dem[od.agg.sums.m]
#ggplot(od.agg.sums.m,aes(x=population,y=value,shape=near.redding))+geom_point()+facet_wrap(~variable)

# just find the mean ratio by purp
setkey(od.agg.sums.m,"variable")
mean.frac.purp <- od.agg.sums.m[,list(frac=mean(value,na.rm=T)),by=variable][,':='(purp=variable,variable=NULL)]

# for SHA the p's and a's are taken as the trips to/from the gates which need to be internalized
setkey(new.pa,'juris','taz','purp')
od.agg.m <- data.table(melt(od.agg,id.vars=c('from','to'),measure.vars=c('ho','hs','hsc','hw','oo','wo')),key=c('from','to'))[,':='(purp=variable,variable=NULL,trips=value,value=NULL)]
for(gate in gates.to.intern.ids){
  for(juris in c('Siskiyou','Tehama')){
    if(length(gates.to.intern[[as.character(gate)]][[juris]])>0){
      prods <- od.agg.m[J(gates.to.intern[[as.character(gate)]][[juris]],gate),list(trips=sum(trips)),by=c("from","purp")]
      prods[,':='(taz=from,from=NULL,juris=juris)]
      setkey(prods,'juris','taz','purp')
      new.pa <- prods[new.pa]
      new.pa[,':='(trips=ifelse(is.na(trips),trips.1,trips),trips.1=NULL)]
    }
  }
}

# for SIS and TEH p's and a's come from the total demand modeled above are are 
# disaggregated according to the average non-redding trip purpose distribution
setkey(new.pa,"taz","purp")
setkey(mean.frac.purp,"purp")
setkey(agg.dt,"taz")
new.pa.with.tot <- data.table(agg.dt[new.pa],key="purp")[,':='(name=NULL,agg.id=NULL,jurisdiction=NULL,area=NULL,shp.id=NULL,population=NULL,employment=NULL,point=NULL,near.redding=NULL)]
new.pa <- mean.frac.purp[new.pa.with.tot]
new.pa[,':='(trips=ifelse(is.na(trips),total.demand*frac,trips),total.demand=NULL,frac=NULL)]
#ggplot(new.pa,aes(x=taz,y=trips,fill=purp))+geom_bar(stat='identity')+facet_wrap(~juris)


