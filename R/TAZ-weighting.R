######################################################################################################
# TAZ WEIGHTING
# 
# Take the aggregated TAZ data and corresponding travel demand data and use vehicle registration data
# by zip and population data by tract to weight the travel demand data and the distribution of homes
# needed by the itinerary builder
######################################################################################################

library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','rgdal','XML','plotKML','rgeos','doMC'))
gpclibPermit()
registerDoMC(7)

path.to.geatm  <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
path.to.humveh <- '~/Dropbox/serc/pev-colin/data/Vehicle-Registration/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'
path.to.parcel <- '~/Dropbox/serc/pev-colin/data/HUM-PARCELS/'

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))

# load disaggregated tazs and travel demand data
taz <- readShapePoly(paste(path.to.geatm,'Shape_Files/taz-LATLON.shp',sep=''))
if(names(taz@data)[1]!="id")names(taz@data) <- c('id',names(taz@data)[2:ncol(taz@data)]) # make first column of shape data 'id' instead of 'ID'

# load aggregated tazs
agg.taz <- readShapePoly(paste(path.to.pevi,'inputs/development/aggregated-taz',sep=''))
load(paste(path.to.pevi,'inputs/development/aggregated-taz-fieldnames.Rdata',sep=''))
names(agg.taz@data) <- c('row',agg.taz.shp.fieldnames)

# load the od data, both dis/aggregated
load(file=paste(path.to.geatm,'od-old-and-new.Rdata',sep=''))

if(!file.exists(paste(path.to.geatm,'../CA-ZIPS/hum-zip-shp.Rdata',sep=''))){
  zips    <- readShapePoly(paste(path.to.geatm,'../CA-ZIPS/tl_2010_06_zcta510.shp',sep=''))
  zips@data$INTPTLAT10 <- as.numeric(as.character(zips@data$INTPTLAT10))
  zips@data$INTPTLON10 <- as.numeric(as.character(zips@data$INTPTLON10))
  hum.zips<- which(zips@data$INTPTLAT10 < 42 & zips@data$INTPTLAT10 > 39.5 & zips@data$INTPTLON10 < -123 & zips@data$INTPTLON10 > -124.5 )
  zips <- zips[hum.zips,]
  save(zips,file=paste(path.to.geatm,'../CA-ZIPS/hum-zip-shp.Rdata',sep=''))
}else{
  load(paste(path.to.geatm,'../CA-ZIPS/hum-zip-shp.Rdata',sep=''))
}

tracts <- readShapePoly(paste(path.to.geatm,'../HUM-CENSUS-TRACTS/hum-census-tracts.shp',sep=''))
tracts <- tracts[-which(tracts@data$NAME10=='9901'),]
tracts@data$GEOID10 <- as.numeric(as.character(tracts@data$GEOID10))
tracts@data$NAMELSAD10 <- as.character(tracts@data$NAMELSAD10)
tracts@data$NAME10 <- as.character(tracts@data$NAME10)
tract.pop <- read.csv(paste(path.to.geatm,'../HUM-CENSUS-TRACTS/2010-population-by-tract.csv',sep=''))
tracts@data$population <- tract.pop$total.population[match(tracts@data$GEOID10,tract.pop$GEO.id)]
tracts@data$ID <- unlist(lapply(tracts@polygons,function(x){slot(x,'ID')}))
hh <- read.csv(paste(path.to.geatm,'../HUM-CENSUS-TRACTS/household-size.csv',sep=''),skip=2)

#c.map <- paste(map.color(tracts@data$population,blue2red(50)),'7F',sep='')
#shp.to.kml(tracts,paste(path.to.google,'humboldt-census-tracts.kml',sep=''),'Population','','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('NAMELSAD10','population','NAME10'))

# Join Humboldt parcel data with land use data (note: experimented with this but never actually used parcel data for anything)
if(!file.exists(paste(path.to.parcel,'apn.Rdata',sep=''))){
  land.use <- read.dbf(paste(path.to.parcel,'Hum-Land-Use.dbf',sep=''))
  apn <- readShapePoly(paste(path.to.parcel,'Hum-Parcels-LongLat.shp',sep=''))
  apn@data <- join(apn@data,land.use[,c('APN','USECODE','NEIGHCODE',"SITHSNBR","SITHSNBRSF","SITSTDIR","SITSTNAME", "SITSTTYPE","SITCITY","SITZIP","ZONING","GEN_PLAN")],by="APN")
  use.codes <- read.csv(paste(path.to.parcel,'usecodes.csv',sep=''),colClasses=c('character','character','character','numeric'))
  apn@data <- join(apn@data,use.codes,by="NEIGHCODE")
  apn@data$color <- ifelse( apn@data$type == 'residential','#E300007F',ifelse(apn@data$type=='commercial','#004FE37F',ifelse(apn@data$type=='industrial','#00C4107F','#0000007F')))
  apn@data$id <- sapply(slot(apn,'polygons'),function(x){ as.numeric(slot(x,'ID')) })
  apn@data$use<-as.character(apn@data$use)
  apn@data$type<-as.character(apn@data$type)
  apn@data$EXLU4<-as.character(apn@data$EXLU4)
  apn@data$ZONING<-as.character(apn@data$ZONING)
  apn.cent <- SpatialPoints(coordinates(apn))
  save(apn,apn.cent,file=paste(path.to.parcel,'apn.Rdata',sep=''))
  #apn.sub <- apn[apn.cent[,1]>-124.14117 & apn.cent[,1] < -124.086455 & apn.cent[,2] < 40.874601 & apn.cent[,2] > 40.799047,]

  #shp.to.kml(apn.sub,paste(path.to.parcel,'apn-subset.kml',sep=''),'Humboldt Parcels','Color denotes use category','white',0.5,apn.sub@data$color,name.col='APN',description.cols=c('EXLU4','ZONING','use','type','weight'))
  #shp.to.kml(apn,paste(path.to.parcel,'apn.kml',sep=''),'Humboldt Parcels','Color denotes use category','white',0.5,apn@data$color,name.col='APN',description.cols=c('EXLU4','ZONING','use','type','weight'))
}else{
  load(file=paste(path.to.parcel,'apn.Rdata',sep=''))
}

# associate parcels with the TAZs, zips, and tracts that contain them
apn$taz <- overlay(apn.cent,taz)
apn$agg.taz <- overlay(apn.cent,agg.taz)
apn$zip <- overlay(apn.cent,zips)
apn$tract <- overlay(apn.cent,tracts)

# look at residential parcels, make a modifier to the "weights" that makes the number of units / parcel sum up to the tract population 
res.apn <- subset(apn@data,type=="residential" & ! use == "Vacant Single Family Residential")
res.sum <- ddply(res.apn,.(tract),function(df){ 
  hh.row <- which(hh$id2==tracts$GEOID10[df$tract[1]])
  units.per.parcel <- tracts$population[df$tract[1]] / nrow(df) / hh$avg.household.size[hh.row]
  weight.mod <- units.per.parcel/(sum(df$weight)/nrow(df))
  data.frame(
    n.unit.est=sum(df$weight,na.rm=T),
    n.parcels=nrow(df),
    pop=tracts$population[df$tract[1]],
    n.units=hh$num.units[hh.row],
    units.per.parcel=units.per.parcel,
    weight.mod = weight.mod,
    n.units.est = sum(df$weight * weight.mod)
  )
})
plot(hh$num.units.occupied[-31],res.sum$n.units.est[order(tracts$GEOID10)])
abline(0,1)
# modify the weights
res.apn <- ddply(res.apn,.(tract),function(df){ 
                 df$weight <- df$weight * subset(res.sum,tract==df$tract[1])$weight.mod
                 df
})
res.apn$pop <- res.apn$weight * hh$avg.household.size[match(tracts$GEOID10[res.apn$tract],hh$id2)]

# now do the pop's come out? YES
dply(res.apn,.(tract),function(df){ 
  hh.row <- which(hh$id2==tracts$GEOID10[df$tract[1]])
  data.frame(
    pop.est=sum(df$weight,na.rm=T)*hh$avg.household.size[hh.row],
    pop=tracts$population[df$tract[1]]
  )
})


if(!file.exists(paste(path.to.geatm,"zip-and-tract-fraction-in-taz-matrix.Rdata",sep=''))){
  # get the data frame ready for storage of the results which will hold the fraction of each zip (the columns) by area that 
  # are in each TAZ (the rows), where the 'null' column contains the fraction of the TAZ with no associated zip code
  zips.in.taz <- as.data.frame(matrix(0,nrow(taz@data),nrow(zips@data)+1))
  names(zips.in.taz) <- c('null',as.character(zips@data$ZCTA5CE10))
  row.names(zips.in.taz) <- as.character(sort(taz@data$NEWTAZ))

  tazs.in.tract <- as.data.frame(matrix(0,nrow(tracts@data),nrow(taz@data)))
  names(tazs.in.tract) <- as.character(sort(taz@data$NEWTAZ))
  row.names(tazs.in.tract) <- as.character(tracts@data$NAME10) 

  # precalculate the area of each zip/tract polygons to reduce redundancy
  zip.areas <- list()
  for(zip.i in names(zips.in.taz)[2:ncol(zips.in.taz)]){
    zip.code <- which(zips@data$ZCTA5CE10 == zip.i)
    zip.areas[[zip.i]] <- gArea(zips[zip.code,])
  }
  tract.areas <- list()
  for(tract.id in as.character(tracts@data$NAME10)){
    tract.i <- which(tracts@data$NAME10 == tract.id)
    tract.areas[[tract.id]] <- gArea(tracts[tract.i,])
  }

  for(taz.id in row.names(zips.in.taz)){
    print(taz.id)
    taz.i <- which(taz@data$NEWTAZ == taz.id)
    taz.area <- gArea(taz[taz.i,])
    for(zip.code in names(zips.in.taz)[2:ncol(zips.in.taz)]){
      zip.i <- which(zips@data$ZCTA5CE10 == zip.code)
      # now calculate the fraction by area of zip.code in taz.id
      zips.in.taz[taz.id,zip.code] <- (taz.area + zip.areas[[zip.code]] - gArea(gUnion(taz[taz.i,],zips[zip.i,]))) / taz.area
    }
    for(tract.id in as.character(tracts@data$NAME10)){
      tract.i <- which(tracts@data$NAME10 == tract.id)
      # now calculate the fraction by area of taz.id in tract.id
      tazs.in.tract[tract.id,taz.id] <- (taz.area + tract.areas[[tract.id]] - gArea(gUnion(taz[taz.i,],tracts[tract.i,]))) / tract.areas[[tract.id]]
    }
  }

  # for zips, make null contain all of remaining area not attributed to zip polygons
  zips.in.taz[,'null'] <- apply(zips.in.taz[,2:ncol(zips.in.taz)],1,function(x){ 1-sum(x) }) 
  
  # remove 0 columns which represent zip codes or tracts outside of humboldt
  zips.in.taz <- zips.in.taz[,-which(apply(zips.in.taz,2,sum)<=0)]
  save(zips.in.taz,tazs.in.tract,file=paste(path.to.geatm,"zip-and-tract-fraction-in-taz-matrix.Rdata",sep=''))
}else{
  load(paste(path.to.geatm,"zip-and-tract-fraction-in-taz-matrix.Rdata",sep=''))
}

# use the population data and tracts.in.taz, determine the population of each taz
match.tracts.in.shp <- match(dimnames(tazs.in.tract)[[1]],tracts$NAME10)
taz.pops <- apply(tazs.in.tract,2,function(x){ sum(x * tracts$population[match.tracts.in.shp]) })
taz@data$population <- taz.pops[taz$id]

# now load up the data providing the fraction of EV's and Hybrids in Humboldt by zipcode and year from 2003-2011
load(paste(path.to.humveh,'veh.Rdata',sep=''))  # veh
load(file=paste(path.to.humveh,'tot-frac-by-year.Rdata',sep='')) # tot.by.year, frac.by.year
aggregate.fracs <- ddply(veh,.(FUEL.TYPE,year),function(df){ data.frame(frac=sum(df$COUNT,na.rm=T)/subset(tot.by.year,year==df$year[1])$count) })
frac.weight.by.zip.year <- ddply(frac.by.zip.year,.(FUEL.TYPE,zip.city,year),function(df){ data.frame(frac.weight=df$frac/subset(aggregate.fracs,year==df$year[1] & FUEL.TYPE==df$FUEL.TYPE[1])$frac) })

# plot those weights
ggplot(subset(frac.weight.by.zip.year,FUEL.TYPE%in%c("GAS/ELEC","ELECTRIC")),aes(x=year,y=frac.weight))+geom_bar(stat="identity",position="dodge",aes(fill=FUEL.TYPE))+facet_wrap(~zip.city)+scale_y_continuous(name="Ratio of Zip-Level Penetration to Aggregated Penetration")

# do it again but aggregate EV/Hybrids first 
frac.weight.by.zip.year.simple <- ddply(subset(frac.by.zip.year,FUEL.TYPE%in%c("GAS/ELEC","ELECTRIC")),.(zip.city,year),function(df){ data.frame(zip=df$zip[1],frac.weight=sum(df$frac)/sum(subset(aggregate.fracs,year==df$year[1] & FUEL.TYPE %in% df$FUEL.TYPE)$frac)) })
#ggplot(frac.weight.by.zip.year.simple,aes(x=year,y=frac.weight))+geom_bar(stat="identity")+facet_wrap(~zip.city)+scale_y_continuous(name="Ratio of Zip-Level Penetration to Aggregate Penetration")

# Finally, take the average ratio in each zip code over the 7 year time frame
frac.est <- ddply(frac.weight.by.zip.year.simple,.(zip),function(df){ data.frame(frac.weight=mean(df$frac.weight)) })

# now dump results from PO Box only zip codes 95502 95518 95534 into their surrounding zips that actually have geographic extension
frac.est$frac.weight[frac.est$zip==95521] <- sum(frac.est$frac.weight[frac.est$zip%in%c(95521,95518)])
frac.est$frac.weight[frac.est$zip==95503] <- sum(frac.est$frac.weight[frac.est$zip%in%c(95503,95534)])
frac.est$frac.weight[frac.est$zip==95501] <- sum(frac.est$frac.weight[frac.est$zip%in%c(95501,95502)])
frac.est <- frac.est[!frac.est$zip %in% c(95502,95518,95534),]

# for zip codes that weren't in the polk data (mostly border zips outside of humboldt with tiny fractions inside TAZs), we add rows to frac.est make them have a weight of 1
frac.est <- rbind(frac.est,data.frame(zip=names(zips.in.taz)[! names(zips.in.taz) %in% frac.est$zip],frac.weight=1))

# plot them
zips@data$frac.weight <- frac.est$frac.weight[match(zips$ZCTA5CE10,frac.est$zip)]
zips$ID <- sapply(slot(zips, "polygons"),function(x){ slot(x,'ID')})
c.map <- paste(map.color(zips@data$frac.weight,blue2red(50)),'7F',sep='')
shp.to.kml(zips,paste(path.to.google,'zips-with-pev-penetartion-weights.kml',sep=''),'Penetration By Zip','','white',1.5,c.map,id.col='ID',name.col='ZCTA5CE10',description.cols=c('ZCTA5CE10','frac.weight'))

# now apply these estimates to the matrix
w.by.penetration <- frac.est$frac.weight[match(names(zips.in.taz),frac.est$zip)]
w.by.penetration.matrix <- t(apply(zips.in.taz,1,function(x){ x * w.by.penetration }))
taz.weights.by.penetration <- apply(w.by.penetration.matrix,1,sum)
taz@data$penetration.weights.unscaled <- taz.weights.by.penetration[taz$id]
# scale these weights evenly so that the weighted sum of od trips is equivalent before and after
od.sums <- ddply(od.24.new,.(from),function(df){ sum(df$demand) })
taz.weights.by.penetration <- taz.weights.by.penetration * sum(od.sums$V1) / sum(od.sums$V1 * taz.weights.by.penetration) 

# do the weighting
od <- od.24.new
od.weighted <- od
for(taz.i in 1:length(taz.weights.by.penetration)){
  newtaz <- as.numeric(names(taz.weights.by.penetration[taz.i]))
  cat(newtaz)
  od.from.inds  <- which(od$from==newtaz)
  od.to.inds    <- which(od$to  ==newtaz)
  od.weighted$demand[od.from.inds] <-  od.weighted$demand[od.from.inds] * taz.weights.by.penetration[taz.i] 
  od.weighted$demand[od.to.inds] <-  od.weighted$demand[od.to.inds] * taz.weights.by.penetration[taz.i] 
}

weighting.factors <- od.weighted$demand / od$demand
weighting.factors[is.nan(weighting.factors)] <- 1
od.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee')] <- od.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee')] * weighting.factors
od.24.weighted <- od.weighted
od.am.weighted <- od.am.new
od.am.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')] <- od.am.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')] * weighting.factors
od.pm.weighted <- od.pm.new
od.pm.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')] <- od.pm.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')] * weighting.factors

write.csv(od.24.weighted,paste(path.to.geatm,'od_24_weighted.csv',sep=''),row.names=F)
write.csv(od.am.weighted,paste(path.to.geatm,'od_am_weighted.csv',sep=''),row.names=F)
write.csv(od.pm.weighted,paste(path.to.geatm,'od_pm_weighted.csv',sep=''),row.names=F)

# use population and penetration weightings to determine the distribution of driver homes
home.distrib <- data.frame(taz=taz$id,frac.homes=(taz$population * taz$penetration.weights.unscaled)/sum(taz$population * taz$penetration.weights.unscaled))
taz@data$frac.homes <- home.distrib$frac.homes
write.csv(home.distrib,paste(path.to.geatm,'home-distribution.csv',sep=''),row.names=F)

# plot in g-earth the ratio 
taz@data$weighted.demand <- ddply(od.weighted,.(from),function(df){ sum(df$demand) })$V1[taz$id]
taz@data$penetration.weights <- taz.weights.by.penetration[taz$id]
taz$ID <- sapply(slot(taz, "polygons"),function(x){ slot(x,'ID')})

writePolyShape(taz,paste(path.to.pevi,'inputs/development/aggregated-taz-with-weights',sep=''))
taz.shp.fieldnames <- names(taz@data)
save(taz.shp.fieldnames,file=paste(path.to.pevi,'inputs/development/aggregated-taz-with-weights-fieldnames.Rdata',sep=''))

c.map <- paste(map.color(taz@data$penetration.weights,blue2red(50)),'7F',sep='')
shp.to.kml(taz,paste(path.to.pevi,'inputs/development/penetration-weighting.kml',sep=''),'Penetration Weighting','Color denotes weighted derived from vehicle registration data.','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','total.demand.from','weighted.demand','penetration.weights','penetration.weights.unscaled'))

c.map <- paste(map.color(taz@data$population,blue2red(50)),'7F',sep='')
shp.to.kml(taz,paste(path.to.pevi,'inputs/development/population-in-tazs.kml',sep=''),'Population','Color denotes weighted derived from vehicle registration data.','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','population','total.demand.from'))

c.map <- paste(map.color(taz@data$frac.homes,blue2red(50)),'7F',sep='')
shp.to.kml(taz,paste(path.to.pevi,'inputs/development/frac-driver-homes.kml',sep=''),'Home Distribution','Color denotes fraction of drivers with home in each TAZ.','green',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','population','penetration.weights.unscaled','frac.homes'))
