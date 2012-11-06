######################################################################################################
# TAZ WEIGHTING
# 
# Take the aggregated TAZ data and corresponding travel demand data and use vehicle registration data
# by zip and population data by tract to weight the travel demand data and the distribution of homes
# needed by the itinerary builder
######################################################################################################

library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','rgdal','XML','plotKML','rgeos'))
gpclibPermit()

path.to.geatm  <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
path.to.humveh <- '~/Dropbox/serc/pev-colin/data/Vehicle-Registration/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'
path.to.parcel <- '~/Dropbox/serc/pev-colin/data/HUM-PARCELS/'

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))

# load aggregated tazs and travel demand data
taz <- readShapePoly(paste(path.to.pevi,'inputs/development/aggregated-taz',sep=''))
load(paste(path.to.pevi,'inputs/development/aggregated-taz-fieldnames.Rdata',sep=''))
names(taz@data) <- c('row',agg.taz.shp.fieldnames)
od.24.new <- read.csv(paste(path.to.geatm,'od_24_new.csv',sep=''))
od.am.new <- read.csv(paste(path.to.geatm,'od_am_new.csv',sep=''))
od.pm.new <- read.csv(paste(path.to.geatm,'od_pm_new.csv',sep=''))

zips    <- readShapePoly(paste(path.to.geatm,'../CA-ZIPS/tl_2010_06_zcta510.shp',sep=''))
zips@data$INTPTLAT10 <- as.numeric(as.character(zips@data$INTPTLAT10))
zips@data$INTPTLON10 <- as.numeric(as.character(zips@data$INTPTLON10))
hum.zips<- which(zips@data$INTPTLAT10 < 42 & zips@data$INTPTLAT10 > 39.5 & zips@data$INTPTLON10 < -123 & zips@data$INTPTLON10 > -124.5 )

tracts <- readShapePoly(paste(path.to.geatm,'../HUM-CENSUS-TRACTS/hum-census-tracts.shp',sep=''))
tracts <- tracts[-which(tracts@data$NAME10=='9901'),]
tracts@data$GEOID10 <- as.numeric(as.character(tracts@data$GEOID10))
tracts@data$NAMELSAD10 <- as.character(tracts@data$NAMELSAD10)
tract.pop <- read.csv(paste(path.to.geatm,'../HUM-CENSUS-TRACTS/2010-population-by-tract.csv',sep=''))
tracts@data$population <- tract.pop$total.population[match(tracts@data$GEOID10,tract.pop$GEO.id)]
tracts@data$ID <- unlist(lapply(tracts@polygons,function(x){slot(x,'ID')}))
#c.map <- paste(map.color(tracts@data$population,blue2red(50)),'7F',sep='')
#shp.to.kml(tracts,paste(path.to.google,'humboldt-census-tracts.kml',sep=''),'Population','','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('NAMELSAD10','population'))


# finding two overlapping polygons
#taz.id <- which(taz@data$ID==519)
#zip.id <- which(zips@data$ZCTA5CE10==95549)
#plot(taz[taz.id,],col='#ee229922',add=T)
#plot(zips[zip.id,],col='#3399ff22',add=T)

if(!file.exists(paste(path.to.geatm,"zip-and-tract-fraction-in-taz-matrix.Rdata",sep=''))){
  # get the data frame ready for storage of the results which will hold the fraction of each zip (the columns) by area that 
  # are in each TAZ (the rows), where the 'null' column contains the fraction of the TAZ with no associated zip code
  zips.in.taz <- as.data.frame(matrix(0,nrow(taz@data),length(hum.zips)+1))
  names(zips.in.taz) <- c('null',as.character(zips[hum.zips,]@data$ZCTA5CE10))
  row.names(zips.in.taz) <- as.character(sort(taz@data$id))

  tracts.in.taz <- as.data.frame(matrix(0,nrow(taz@data),nrow(tracts@data)))
  names(tracts.in.taz) <- as.character(tracts@data$NAME10)
  row.names(tracts.in.taz) <- as.character(sort(taz@data$id))

  # precalculate the area of each zip/tract polygons to reduce redundancy
  zip.areas <- list()
  for(zip.i in names(zips.in.taz)[2:ncol(zips.in.taz)]){
    zip.id <- which(zips@data$ZCTA5CE10 == zip.i)
    zip.areas[[zip.i]] <- gArea(zips[zip.id,])
  }
  tract.areas <- list()
  for(tract.i in names(tracts.in.taz)){
    tract.id <- which(tracts@data$NAME10 == tract.i)
    tract.areas[[tract.i]] <- gArea(tracts[tract.id,])
  }

  for(taz.i in row.names(zips.in.taz)){
    print(taz.i)
    taz.id <- which(taz@data$id == taz.i)
    taz.area <- gArea(taz[taz.id,])
    cum.frac <- 0
    for(zip.i in names(zips.in.taz)[2:ncol(zips.in.taz)]){
      if(cum.frac >= 1)next
      zip.id <- which(zips@data$ZCTA5CE10 == zip.i)
      # now calculate the fraction by area of zip.i in taz.i
      zips.in.taz[taz.i,zip.i] <- (taz.area + zip.areas[[zip.i]] - gArea(gUnion(taz[taz.id,],zips[zip.id,]))) / taz.area
      cum.frac <- cum.frac + zips.in.taz[taz.i,zip.i]
    }
    cum.frac <- 0
    for(tract.i in names(tracts.in.taz)[1:ncol(tracts.in.taz)]){
      if(cum.frac >= 1)next
      tract.id <- which(tracts@data$NAME10 == tract.i)
      # now calculate the fraction by area of tract.i in taz.i
      tracts.in.taz[taz.i,tract.i] <- (taz.area + tract.areas[[tract.i]] - gArea(gUnion(taz[taz.id,],tracts[tract.id,]))) / taz.area
      cum.frac <- cum.frac + tracts.in.taz[taz.i,tract.i]
    }
  }
  # for zips, make null contain all of remaining area not attributed to zip polygons
  zips.in.taz[,'null'] <- apply(zips.in.taz[,2:ncol(zips.in.taz)],1,function(x){ 1-sum(x) }) 
  # for tracts, deal with tazs containing external boxes by rescaling to 1 
  tracts.in.taz <- t(apply(tracts.in.taz,1,function(x){ x / sum(x) }))
  
  # remove 0 columns which represent zip codes or tracts outside of humboldt
  zips.in.taz <- zips.in.taz[,-which(apply(zips.in.taz,2,sum)<=0)]
  save(zips.in.taz,tracts.in.taz,file=paste(path.to.geatm,"zip-and-tract-fraction-in-taz-matrix.Rdata",sep=''))
}else{
  load(paste(path.to.geatm,"zip-and-tract-fraction-in-taz-matrix.Rdata",sep=''))
}

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

# for zip codes that weren't in the polk data (mostly border zips outside of humboldt with tiny fractions inside TAZs), we add rows to frac.est make them have a weight of 1
frac.est <- rbind(frac.est,data.frame(zip=names(zips.in.taz)[! names(zips.in.taz) %in% frac.est$zip],frac.weight=1))

# load the distance-time matrix for the aggregated TAZ data
d <- read.csv(paste(path.to.geatm,'taz-dist-time.csv',sep=''))

# create corresponding matrix of weights on each row of distance matrix where a quadratic relationship favoring proximity is established
n.taz <- sqrt(nrow(d))
w.by.distance <- matrix(0,n.taz,n.taz)
max.d <- max(d$miles)
for(taz.i in unique(d$from)){
  w.by.distance[taz.i,taz.i] <- 0.5
  d.sub <- subset(d,from==taz.i & to!=taz.i)
  top.20.inds <- order(d$miles[d$from==taz.i & d$to!=taz.i])[1:round(n.taz/5)]
  w.by.distance[taz.i,d.sub$to[top.20.inds]] <- (max.d-d$miles[top.20.inds])^10/sum((max.d-d$miles[top.20.inds])^10)/2
}

# now apply these estimates to the matrix
w.by.penetration <- frac.est$frac.weight[match(names(zips.in.taz),frac.est$zip)]
w.by.penetration.matrix <- t(apply(zips.in.taz,1,function(x){ x * w.by.penetration }))
taz.weights.by.penetration <- apply(w.by.penetration.matrix,1,sum)
# scale these weights evenly so that the weighted sum of od trips is equivalent before and after
od.sums <- ddply(od,.(from),function(df){ sum(df$demand) })
taz.weights.by.penetration <- taz.weights.by.penetration * sum(od.sums$V1) / sum(od.sums$V1 * taz.weights.by.penetration) 

# for testing
od <- od.24.new
od.weighted <- od
for(taz.i in 1:length(taz.weights.by.penetration)){
  newtaz <- as.numeric(names(taz.weights.by.penetration[taz.i]))
  cat(newtaz)
  od.from.inds  <- which(od$from==newtaz)
  od.to.inds    <- which(od$to  ==newtaz)
  od.from.diff  <- (taz.weights.by.penetration[taz.i] - 1) * sum(od$demand[od.from.inds])
  od.to.diff  <- (taz.weights.by.penetration[taz.i] - 1) * sum(od$demand[od.to.inds])
  neg.inds <- c()

  # now distribute those diffs based on the weights.by.distance matrix
  for(distance.weight.i in which(w.by.distance[taz.i,] > 0)){
    distance.weight     <- w.by.distance[taz.i,distance.weight.i]
    distance.weight.taz <- as.numeric(names(taz.weights.by.penetration[distance.weight.i]))
    #from
    from.to.distribute  <- od.from.diff * distance.weight
    distance.weight.from.inds <- which(od$from==distance.weight.taz)
    if(length(distance.weight.from.inds)>0){
      from.distributed.additive <- from.to.distribute * od$demand[distance.weight.from.inds] / sum(od$demand[distance.weight.from.inds])
      from.distributed.multiplicative <- (from.distributed.additive + od.weighted$demand[distance.weight.from.inds])/ od.weighted$demand[distance.weight.from.inds]
      from.distributed.multiplicative[is.nan(from.distributed.multiplicative) | abs(from.distributed.multiplicative) == Inf] <- 1
      od.weighted[distance.weight.from.inds,ncol(od)] <- od.weighted[distance.weight.from.inds,ncol(od)] * from.distributed.multiplicative
      if(any(is.nan(od.weighted[distance.weight.from.inds,ncol(od)]))) stop('here 1')
      neg.inds <- which(od.weighted[distance.weight.from.inds,ncol(od)] < 0)
      while(length(neg.inds)>0){
        # redistrib the negative demand to the positive ones
        with.negs <- od.weighted[distance.weight.from.inds,ncol(od)]
        pos.redist <- -sum(with.negs[neg.inds])
        with.negs[neg.inds] <- 0
        if(sum(with.negs)>0){
          if(sum(with.negs)-pos.redist > 0){
            od.weighted[distance.weight.from.inds,ncol(od)] <- with.negs * (sum(with.negs)-pos.redist)/sum(with.negs)
          }else{
            od.weighted[distance.weight.from.inds,ncol(od)] <- 0
          }
        }
        if(any(is.nan(od.weighted[distance.weight.from.inds,ncol(od)]))) stop('here 2')
        neg.inds <- which(od.weighted[distance.weight.from.inds,ncol(od)] < 0)
      }
    }
    #to
    to.to.distribute    <- od.to.diff * distance.weight
    distance.weight.to.inds   <- which(od$to==distance.weight.taz)
    if(length(distance.weight.to.inds)>0){
      to.distributed.additive <- to.to.distribute * od$demand[distance.weight.to.inds] / sum(od$demand[distance.weight.to.inds])
      to.distributed.multiplicative <- (to.distributed.additive + od.weighted$demand[distance.weight.to.inds])/od.weighted$demand[distance.weight.to.inds]
      to.distributed.multiplicative[is.nan(to.distributed.multiplicative)| abs(to.distributed.multiplicative) == Inf] <- 1
      od.weighted[distance.weight.to.inds,ncol(od)] <- od.weighted[distance.weight.to.inds,ncol(od)] * to.distributed.multiplicative
      if(any(is.nan(od.weighted[distance.weight.to.inds,ncol(od)]))) stop('here 3')
      neg.inds <- which(od.weighted[distance.weight.to.inds,ncol(od)] < 0)
      while(length(neg.inds)>0){
        # redistrib the negative demand to the positive ones
        with.negs <- od.weighted[distance.weight.to.inds,ncol(od)]
        pos.redist <- -sum(with.negs[neg.inds])
        with.negs[neg.inds] <- 0
        if(sum(with.negs)>0){
          if(sum(with.negs)-pos.redist > 0){
            od.weighted[distance.weight.to.inds,ncol(od)] <- with.negs * (sum(with.negs)-pos.redist)/sum(with.negs)
          }else{
            od.weighted[distance.weight.to.inds,ncol(od)] <- 0
          }
        }
        if(any(is.nan(od.weighted[distance.weight.to.inds,ncol(od)]))) stop('here 4')
        neg.inds <- which(od.weighted[distance.weight.to.inds,ncol(od)] < 0)
      }
    }
  }
}

# plot in g-earth the difference in total demand
taz@data$weighted.demand <- ddply(od.weighted,.(from),function(df){ sum(df$demand) })$V1[taz$id]
taz@data$penetration.weights <- taz.weights.by.penetration[taz$id]
taz$ID <- sapply(slot(taz, "polygons"),function(x){ slot(x,'ID')})

c.map <- paste(map.color(taz@data$weighted.demand,blue2red(50)),'7F',sep='')
shp.to.kml(taz,paste(path.to.pevi,'inputs/development/penetration-weighting.kml',sep=''),'Penetration Weighting','Color denotes total daily demand','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','total.demand.from','weighted.demand','penetration.weights'))



