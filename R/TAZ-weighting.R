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
agg.taz@data$ID <- unlist(lapply(agg.taz@polygons,function(x){slot(x,'ID')}))

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
#plot(hh$num.units.occupied[-31],res.sum$n.units.est[order(tracts$GEOID10)])
#abline(0,1)
# modify the weights
res.apn <- ddply(res.apn,.(tract),function(df){ 
                 df$weight.mod <- df$weight * subset(res.sum,tract==df$tract[1])$weight.mod
                 df
})
res.apn$pop <- res.apn$weight.mod * hh$avg.household.size[match(tracts$GEOID10[res.apn$tract],hh$id2)]

# now do the pop's come out? YES
ddply(res.apn,.(tract),function(df){ 
  hh.row <- which(hh$id2==tracts$GEOID10[df$tract[1]])
  data.frame(
    pop.est=sum(df$weight.mod,na.rm=T)*hh$avg.household.size[hh.row],
    pop=tracts$population[df$tract[1]]
  )
})

# sum the population in each taz and add to shp file
taz.pop <- ddply(res.apn,.(agg.taz),function(df){ data.frame(pop=sum(df$pop))}) 
agg.taz@data$population <- taz.pop$pop[order(taz.pop$agg.taz)]  # note that agg.taz is the ROW in agg.taz shp, 

#c.map <- paste(map.color(agg.taz@data$population,blue2red(50)),'7F',sep='')
#shp.to.kml(agg.taz,paste(path.to.google,'population-in-tazs.kml',sep=''),'Population','Color denotes population based on parcel level analysis','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','population','total.demand.from'))

# now load up the data providing the fraction of EV's and Hybrids in Humboldt by zipcode and year from 2003-2011
load(paste(path.to.humveh,'veh.Rdata',sep=''))  # veh
load(file=paste(path.to.humveh,'tot-frac-by-year.Rdata',sep='')) # tot.by.year, frac.by.year
aggregate.fracs <- ddply(veh,.(FUEL.TYPE,year),function(df){ data.frame(frac=sum(df$COUNT,na.rm=T)/subset(tot.by.year,year==df$year[1])$count) })
frac.weight.by.zip.year <- ddply(frac.by.zip.year,.(FUEL.TYPE,zip.city,year),function(df){ data.frame(frac.weight=df$frac/subset(aggregate.fracs,year==df$year[1] & FUEL.TYPE==df$FUEL.TYPE[1])$frac) })
# plot those weights
#ggplot(subset(frac.weight.by.zip.year,FUEL.TYPE%in%c("GAS/ELEC","ELECTRIC")),aes(x=year,y=frac.weight))+geom_bar(stat="identity",position="dodge",aes(fill=FUEL.TYPE))+facet_wrap(~zip.city)+scale_y_continuous(name="Ratio of Zip-Level Penetration to Aggregated Penetration")

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
# blocksburg had no EVs or Hybrids, give them the min value (instead of 1)
frac.est <- rbind(frac.est,data.frame(zip=95514,frac.weight=min(frac.est$frac.weight)))

# plot them
zips@data$frac.weight <- frac.est$frac.weight[match(zips$ZCTA5CE10,frac.est$zip)]
zips$ID <- sapply(slot(zips, "polygons"),function(x){ slot(x,'ID')})
#c.map <- paste(map.color(zips@data$frac.weight,blue2red(50)),'7F',sep='')
#shp.to.kml(zips,paste(path.to.google,'zips-with-pev-penetartion-weights.kml',sep=''),'Penetration By Zip','','white',1.5,c.map,id.col='ID',name.col='ZCTA5CE10',description.cols=c('ZCTA5CE10','frac.weight'))

# now associate the weights with the parcels
res.apn$pen.weight <- frac.est$frac.weight[match(zips$ZCTA5CE10[res.apn$zip],frac.est$zip)]
res.apn$pen.weight[is.na(res.apn$pen.weight)] <- 1

# now aggregate the weights to the tazs using a weighted.mean to population weight the result
taz.weights.by.penetration <- ddply(res.apn,.(agg.taz),function(df){ data.frame(pen.weight=weighted.mean(df$pen.weight,df$pop)) })
# scale these weights evenly so that the weighted sum of od trips is equivalent before and after
od.sums <- ddply(od.24.new,.(from),function(df){ sum(df$demand) })
taz.weights.by.penetration$pen.weight.scaled <- taz.weights.by.penetration$pen.weight * sum(od.sums$V1) / sum(od.sums$V1 * taz.weights.by.penetration$pen.weight) 
agg.taz$pen.weight.unscaled <- taz.weights.by.penetration$pen.weight[order(taz.weights.by.penetration$agg.taz)]
agg.taz$pen.weight.scaled <- taz.weights.by.penetration$pen.weight.scaled[order(taz.weights.by.penetration$agg.taz)]

# do the weighting
od <- od.24.new
od.weighted <- od
for(taz.i in 1:nrow(taz.weights.by.penetration)){
  taz.id <- agg.taz$id[taz.i]
  cat(taz.id)
  od.from.inds  <- which(od$from==taz.id)
  od.to.inds    <- which(od$to  ==taz.id)
  od.weighted$demand[od.from.inds] <-  od.weighted$demand[od.from.inds] * taz.weights.by.penetration$pen.weight.scaled[taz.i] 
  od.weighted$demand[od.to.inds] <-  od.weighted$demand[od.to.inds] * taz.weights.by.penetration$pen.weight.scaled[taz.i] 
}

weighting.factors <- od.weighted$demand / od$demand
weighting.factors[is.nan(weighting.factors)] <- 1
od.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee')] <- od.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee')] * weighting.factors
od.24.weighted <- od.weighted
od.am.weighted <- od.am.new
od.am.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')] <- od.am.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')] * weighting.factors
od.pm.weighted <- od.pm.new
od.pm.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')] <- od.pm.weighted[,c('hbw','hbshop','hbelem','hbuniv','hbro','nhb','ix','xi','ee','demand')] * weighting.factors
save(od.24.weighted,od.am.weighted,od.pm.weighted,file=paste(path.to.geatm,'od_weighted.Rdata',sep=''))

# use population and penetration weightings to determine the distribution of driver homes
home.distrib <- data.frame(agg.taz=agg.taz$id,frac.homes=(agg.taz$population * agg.taz$pen.weight.unscaled)/sum(agg.taz$population * agg.taz$pen.weight.unscaled))
agg.taz@data$frac.homes <- home.distrib$frac.homes
write.csv(home.distrib,paste(path.to.geatm,'home-distribution.csv',sep=''),row.names=F)

# plot in g-earth the ratio 
agg.taz@data$weighted.demand <- ddply(od.weighted,.(from),function(df){ sum(df$demand) })$V1[agg.taz$id]

writePolyShape(agg.taz,paste(path.to.pevi,'inputs/development/aggregated-taz-with-weights',sep=''))
taz.shp.fieldnames <- names(agg.taz@data)
save(taz.shp.fieldnames,file=paste(path.to.pevi,'inputs/development/aggregated-taz-with-weights-fieldnames.Rdata',sep=''))

c.map <- paste(map.color(agg.taz@data$pen.weight.scaled,blue2red(50)),'7F',sep='')
shp.to.kml(agg.taz,paste(path.to.google,'penetration-weighting.kml',sep=''),'Penetration Weighting','Color denotes weighted derived from vehicle registration data.','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','total.demand.from','weighted.demand','pen.weight.scaled','pen.weight.unscaled','frac.homes'))

c.map <- paste(map.color(taz@data$frac.homes,blue2red(50)),'7F',sep='')
shp.to.kml(taz,paste(path.to.pevi,'inputs/development/frac-driver-homes.kml',sep=''),'Home Distribution','Color denotes fraction of drivers with home in each TAZ.','green',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','population','penetration.weights.unscaled','frac.homes'))

# use overall traffic density of the weighted data to assign up to three chargers to each TAZ
traffic.density <- ddply(od.24.weighted,.(from),function(df){ data.frame(demand=sum(df$demand)) })
traffic.density$num.chargers <- round(2 * traffic.density$demand/max(traffic.density$demand))
traffic.density$num.chargers.L3 <- round(0.85 * traffic.density$demand/max(traffic.density$demand))
chargers <- data.frame(TAZ=1:52,L0=rep(1,52),L1=0,L2=traffic.density$num.chargers,L3=traffic.density$num.chargers.L3)
names(chargers) <- c(";TAZ","L0","L1","L2","L3")
base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/charger-input-file/',sep='')
write.table(chargers,file=paste(path.to.inputs,'/chargers-scen9.txt',sep=''),sep="\t",row.names=F,quote=F)

