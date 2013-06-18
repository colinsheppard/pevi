######################################################################################################
# TAZ AGGREGATE
# 
# Take the original TAZ data and corresponding travel demand data and aggregated
# based on polygons generated in google earth (and converted to shape files in QGIS)
######################################################################################################

library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','rgdal','XML','plotKML'))
gpclibPermit()

base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
path.to.geatm <- paste(base.path,'data/GEATM-2020/',sep='')
path.to.google <- paste(base.path,'pev-shared/data/google-earth/',sep='')
path.to.humveh <- '~/Dropbox/serc/pev-colin/data/Vehicle-Registration/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'
path.to.parcel <- '~/Dropbox/serc/pev-colin/data/HUM-PARCELS/'

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))

emfac.years <- c(2005,2010,2020)
emfac.years <- c(2010,2020)

for(yr in emfac.years){

  # Load OD data from newer HCOAG model, since this is for GHG analysis, we don't need AM/PM
  taz <- readShapePoly(paste(path.to.geatm,'../HCOAG/shapefiles/Humboldt TA.shp',sep=''))
  od.names <- read.csv(paste(path.to.geatm,'../HCOAG/',yr,'/od-am-vehicle-trips.DCC',sep=''),skip=2)
  od.am.old <- read.csv(paste(path.to.geatm,'../HCOAG/',yr,'/od-am-vehicle-trips.csv',sep=''),header=F)
  od.pm.old <- read.csv(paste(path.to.geatm,'../HCOAG/',yr,'/od-pm-vehicle-trips.csv',sep=''),header=F)
  od.op.old <- read.csv(paste(path.to.geatm,'../HCOAG/',yr,'/od-op-vehicle-trips.csv',sep=''),header=F)
  names(od.am.old) <- c("from","to",as.character(od.names$Rows[2:(nrow(od.names)-1)]),"demand")
  names(od.pm.old) <- c("from","to",as.character(od.names$Rows[2:(nrow(od.names)-1)]),"demand")
  names(od.op.old) <- c("from","to",as.character(od.names$Rows[2:(nrow(od.names)-1)]),"demand")
  od.24.old <- od.am.old[,c('from','to')]
  od.24.old$demand <- od.am.old$demand + od.pm.old$demand + od.op.old$demand
  taz$NEWTAZ <- taz$TAZ
  aggregate.data <- function(df){
    sum(df$AREA)
  }

  # Sum demand to get a total
  od.24.sum <- ddply(od.24.old,.(from),function(df){ sum(df$demand) })
  names(od.24.sum) <- c('taz','demand')
  taz@data$total.demand <- od.24.sum$demand[match(taz@data$NEWTAZ,od.24.sum$taz)]

  # For GHG Analysis, we want to include external travel so we need to load the appropriate shp data
  agg.polys <- readShapePoly(paste(path.to.google,'proposed-aggregations/ProposedAggregations-with-external.shp',sep=''))

  taz.centroids <-SpatialPointsDataFrame(coordinates(taz),data=data.frame(longitude= coordinates(taz)[,1],latitude= coordinates(taz)[,2]))
  agg.mapping <- data.frame(name=over(taz.centroids,agg.polys)$Name)
  agg.mapping$agg.id <- as.numeric(agg.mapping$name)
  agg.taz.shp <- unionSpatialPolygons(taz,agg.mapping$agg.id)
  taz@data$agg.id <- agg.mapping$agg.id
  agg.taz.data <- ddply(taz@data[!is.na(taz@data$agg.id),],.(agg.id),aggregate.data)
  agg.taz.shp <- SpatialPolygonsDataFrame(agg.taz.shp,agg.taz.data)

  # add new zone numbers corresponding to old zone numbers to the dataframe
  od.24.old$from.new <- taz$agg.id[match(od.24.old$from,taz$NEWTAZ)]
  od.24.old$to.new <- taz$agg.id[match(od.24.old$to,taz$NEWTAZ)]

  # for now, omit the rows with NA, which correspond to the TAZ id's in the OD data which don't have a
  # corresponding entry in the TAZ shape file (ID's 11-20)
  # I think this also omits ID's 1-10
  od.24.new <- ddply(od.24.old,.(from.new,to.new),function(df){ sum(df$demand) })
  names(od.24.new) <- c('from','to','demand')

  # To include external trips, remove the "na.omit" command from the ddply blocks above
  save(od.24.new,od.24.old,file=paste(path.to.geatm,'../HCOAG/',yr,'/od-',yr,'-old-and-new-including-external-trips.Rdata',sep=''))
}


# Produce EMFAC Inputs

library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools'))

make.plots  <- F
num.processors <- 11
registerDoMC(num.processors)
emfac.years <- c(2010,2020)


base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
path.to.humveh <- paste(base.path,'data/Vehicle-Registration/',sep='')
path.to.geatm <- paste(base.path,'data/GEATM-2020/',sep='')
path.to.ghg <- paste(base.path,'data/ghg-analysis/',sep='')
path.to.google <- paste(base.path,'pev-shared/data/google-earth/',sep='')
path.to.shared.inputs <- paste(base.path,'pev-shared/data/inputs/driver-input-file/',sep='')
path.to.pevi <- paste(base.path,'pevi/',sep='')

path.to.outputs <- '~/Dropbox/serc/pev-colin/data/scheduler-optim/'
path.to.ctpp <- '~/Dropbox/serc/pev-colin/data/CTPP/'
path.to.nhts <- '~/Dropbox/serc/pev-colin/data/NHTS/'
path.to.plots <- '~/Dropbox/serc/pev-colin/plots/'

agg.taz <- readShapePoly(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights',sep=''),IDvar="ID")
load(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights-fieldnames.Rdata',sep=''))
names(agg.taz@data) <- c("SP_ID",taz.shp.fieldnames)

disttime <- read.csv(file=paste(path.to.geatm,'taz-dist-time.csv',sep=''))

if(!file.exists(paste(path.to.geatm,'od-all-old-and-new-including-external-trips.Rdata',sep=''))){
  for(scenario.year in emfac.years){
    load(file=paste(path.to.geatm,'../HCOAG/',scenario.year,'/od-',scenario.year,'-old-and-new-including-external-trips.Rdata',sep=''))

    if(scenario.year==emfac.years[1]){
      od.24.all <- od.24.new
      od.24.all$scenario <- scenario.year
    }else{
      od.24.new$scenario <- scenario.year
      od.24.all <- rbind(od.24.all,od.24.new)
    }
  }
  save(od.24.all,file=paste(path.to.geatm,'od-all-old-and-new-including-external-trips.Rdata',sep=''))
}else{
  load(paste(path.to.geatm,'od-all-old-and-new-including-external-trips.Rdata',sep=''))
}

if(!file.exists(paste(path.to.pevi,'inputs/routing-prepped-for-ghg-analysis.Rdata',sep=''))){
  load(paste(path.to.pevi,'inputs/routing-corrected.Rdata',sep=''))
  y.bins <- seq(0,70,by=5)
  y.labs <- c("[0,5)","[5,10)","[10,15)","[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50)","[50,55)","[55,60)","[60,65)","[65,70)")
  #route.ordered$speed.binned    <- factor(y.labs[findInterval(route.ordered$ab_speed,y.bins)],levels=y.labs)
  route.ordered$speed.binned    <- y.bins[findInterval(route.ordered$ab_speed,y.bins)]

  route.ordered.sub <- route.ordered[,c('from_taz','to_taz','length','speed.binned')]
  names(route.ordered.sub) <- c('from','to','length','speed')
  routes.agg <- ddply(route.ordered.sub,.(from,to),function(df){ data.frame(length=sum(df$length),speed=weighted.mean(df$speed,df$length))})
  route.ordered.sub <- rbind(route.ordered.sub,data.frame(from=subset(disttime,from==to)$from,to=subset(disttime,from==to)$to,length=subset(disttime,from==to)$miles,speed=y.bins[findInterval(mean(subset(routes.agg,length<10)$speed),y.bins)]))
  route.ordered.sub <- ddply(route.ordered.sub,.(from,to,speed),function(df){ data.frame(length=sum(df$length)) })

  # dump some of the distance into other bins assuming a normal distibution of speed with a mean of speed
  obj.85 <- function(avg,lim,sd){
    (qnorm(.85,avg,sd)-lim)^2
  }
  find.mean <- function(lim,sd=2){
    optimize(obj.85,lower=0,upper=70,lim=lim,sd=sd)$minimum
  }
  sp.dist <- function(sp,dist){
    sp.bins <- seq(0,70,by=5)
    res <- diff(c(0,pnorm(sp.bins,find.mean(sp,3),3)))
    res <- res*dist/sum(res)
    names(res) <- sp.bins
    tail(res,-1)
  }
  #for(sp in tail(y.bins,-1)){
    #if(sp==5){
      #plot(y.bins,sp.dist(sp,100),type='l',col=sp/5)
    #}else{
      #points(y.bins,sp.dist(sp,100),type='l',col=sp/5)
    #}
  #}
  route.ordered.sub.new <- route.ordered.sub
  route.ordered.sub.new$length <- 0
  for(from in unique(route.ordered.sub.new$from)){
    for(to in unique(route.ordered.sub.new$to)){
      for(sp in unique(route.ordered.sub.new$speed)){
        not.there <- seq(5,70,by=5)[!seq(5,70,by=5) %in% route.ordered.sub.new$speed[route.ordered.sub.new$from==from & route.ordered.sub.new$to==to]]
        if(length(not.there)>0) route.ordered.sub.new <- rbind(route.ordered.sub.new,data.frame(from=from,to=to,speed=not.there,length=0))
        to.distrib <- route.ordered.sub$length[route.ordered.sub$from==from & route.ordered.sub$to==to & route.ordered.sub$speed==sp]
        if(length(to.distrib)>0)route.ordered.sub.new$length[route.ordered.sub.new$from==from & route.ordered.sub.new$to==to] <- route.ordered.sub.new$length[route.ordered.sub.new$from==from & route.ordered.sub.new$to==to] + sp.dist(sp,to.distrib)
      }
    }
  }
  save(route.ordered.sub.new,file=paste(path.to.pevi,'inputs/routing-prepped-for-ghg-analysis.Rdata',sep=''))
}else{
  load(paste(path.to.pevi,'inputs/routing-prepped-for-ghg-analysis.Rdata',sep=''))
}

route.ordered.sub.joined <- join(na.omit(od.24.all[,c('from','to','demand','scenario')]),route.ordered.sub.new)

munis <- data.frame(muni = "Eureka", shp.row = grep("EKA_",agg.taz$name), id = agg.taz$id[grep("EKA_",agg.taz$name)])
munis <- rbind(munis,data.frame(muni = "Arcata", shp.row = grep("ARC_",agg.taz$name), id = agg.taz$id[grep("ARC_",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Fortuna", shp.row = grep("FOR_",agg.taz$name), id = agg.taz$id[grep("FOR_",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Trinidad", shp.row = grep("Trinidad",agg.taz$name), id = agg.taz$id[grep("Trinidad",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Ferndale", shp.row = grep("Ferndale",agg.taz$name), id = agg.taz$id[grep("Ferndale",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Rio Dell", shp.row = grep("RioDell",agg.taz$name), id = agg.taz$id[grep("RioDell",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Blue Lake", shp.row = grep("BlueLake",agg.taz$name), id = agg.taz$id[grep("BlueLake",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Humboldt (NC)", shp.row = which(!agg.taz$id %in% munis$id), id = agg.taz$id[which(!agg.taz$id %in% munis$id)]))

# now break the vmt out by vehicle type for each muni
load(paste(path.to.humveh,'veh.Rdata',sep=''))  
load(paste(path.to.geatm,'../CA-ZIPS/hum-zip-shp.Rdata',sep=''))

zips.in.taz <- overlay(SpatialPoints(coordinates(agg.taz)),zips)
# hard code the zips for those whose centroids fall in no-man's land
zips.in.taz[31] <- which(zips$ZCTA5CE10 == 95519)
zips.in.taz[33] <- which(zips$ZCTA5CE10 == 95555)
zips.in.taz[44] <- which(zips$ZCTA5CE10 == 95573)
zips.in.taz[48] <- which(zips$ZCTA5CE10 == 95565)
zips.in.taz[52] <- which(zips$ZCTA5CE10 == 95526)

agg.taz$zip <- as.numeric(as.character(zips$ZCTA5CE10[zips.in.taz]))

munis$zip <- agg.taz$zip[match(munis$id,agg.taz$id)]

# Use 2010 as the baseline for the distribution of all vehicles by type
# Use 2012 as the projection of the spatial distribution of PEVs
veh.sub <- subset(veh,year==2010)
veh.types <- read.csv(paste(path.to.humveh,"../ghg-analysis/veh-reg-melted.csv",sep=''))
types.sub <- subset(veh.types,year==2010)
pev.sub <- na.omit(ddply(subset(veh.types,veh.tech.code%in%c("LDA-GHybd","LDA-ELECTRIC","LDT1-GHybd","LDT2-GHybd") & year==2012),.(zip),numcolwise(sum)))[,c('zip','num')]
pev.sub$frac <- pev.sub$num/sum(pev.sub$num)

# Assumptions about electric miles driven for a given penetration of pevs
# 
# Effect of PHEV/BEV breakdown at 2% pen, doubling BEV from 25% to 50% increases EMT by 2.6%
#   at 1% pen, doubling increase EMT by 3%
# Effect of infrastructure: 3 chargers -> 40 chargers results in ~2% increase in electric VMT
#
# Assuming 40 chargers:
# Avg EMT by Pen: 1% 26048.07
#                 2% 51343.08

# Scale emt to the 2020 population used in the remainder of the analysis
pop.2010 <- 127136
pop.2020 <- 134512
pevi.pop.2020 <- 154955

emt <- data.frame(pen=c(0,0.01,0.02),emt=c(0,26048.07,51343.08),num.drivers=c(0,1549.55,3026.20))
emt$emt <- emt$emt * pop.2020 / pevi.pop.2020
emt$num.drivers <- emt$num.drivers * pop.2020 / pevi.pop.2020

for(scenario.year in emfac.years){
  munis.demand <- ddply(munis,.(muni),function(df){
    sub.od <- subset(route.ordered.sub.joined,scenario == scenario.year & from %in% df$id)
    ddply(sub.od,.(speed),function(ddf){ data.frame(vmt=sum(ddf$demand*ddf$length)) })
  })


  if(scenario.year == emfac.years[1]){
    # plot the results to inspect
    #ggplot(munis.demand,aes(x=speed,y=vmt))+geom_point()+facet_wrap(~muni)+scale_y_log10()
    munis.demand.by.tech <- ddply(munis,.(muni),function(df){
      tot.num <- sum(subset(types.sub,zip %in% unique(df$zip))$num)
      techs <- ddply(subset(types.sub,zip %in% unique(df$zip)),.(veh.tech.code),function(dff){ data.frame(frac=sum(dff$num)/tot.num) })
      ddply(subset(munis.demand,muni==df$muni[1]),.(speed),function(dff){
        data.frame(tech=techs$veh.tech.code,vmt=techs$frac * dff$vmt)
      })
    })
    #ggplot(munis.demand.by.tech,aes(x=speed,y=vmt))+geom_point()+facet_grid(tech~muni)+scale_y_log10()

    munis.demand.by.tech.all <- munis.demand.by.tech
    munis.demand.by.tech.all$scenario <- scenario.year
    write.csv(cast(melt(munis.demand.by.tech,id.vars=c("muni","tech","speed"),measure.vars="vmt"),muni + tech ~ speed),
            paste(path.to.humveh,"../ghg-analysis/year-",scenario.year,"-vmt-by-muni-speed-and-type.csv",sep=''))
  }else{
    for(pev.pen in c(0,0.01,0.02)){
      tot.emt <- subset(emt,pev.pen==pen)$emt
      tot.vmt <- sum(munis.demand$vmt)
      munis.demand.by.tech <- ddply(munis,.(muni),function(df){
        # now redistribute VMT based on EMT, but use "num" as a proxy for VMT as it will be proportionally distributed anyway
        types.sub.emt.adjusted <- ddply(na.omit(types.sub),.(zip),function(dff){
          if(sum(dff$veh.tech.code=="LDA-ELECTRIC")==0){
            dff <- rbind(dff,dff[1,])
            dff$veh.tech.code[nrow(dff)] <- "LDA-ELECTRIC"
            dff$num[nrow(dff)] <- 0
          }
          if(sum(dff$veh.tech.code=="LDA-GAS")==0){
            dff <- rbind(dff,dff[1,])
            dff$veh.tech.code[nrow(dff)] <- "LDA-GAS"
            dff$num[nrow(dff)] <- 0
          }
          zip.num <- sum(dff$num)
          zip.frac <- ifelse(dff$zip[1]%in%pev.sub$zip,subset(pev.sub,zip==dff$zip[1])$frac,0)
          dff$num[dff$veh.tech.code=="LDA-ELECTRIC"] <- dff$num[dff$veh.tech.code=="LDA-ELECTRIC"] + zip.num * zip.frac * tot.emt / tot.vmt
          dff$num[dff$veh.tech.code=="LDA-GAS"] <- dff$num[dff$veh.tech.code=="LDA-GAS"] - zip.num * zip.frac * tot.emt / tot.vmt
          dff
        })

        tot.num <- sum(subset(types.sub.emt.adjusted,zip %in% unique(df$zip))$num)
        techs <- ddply(subset(types.sub.emt.adjusted,zip %in% unique(df$zip)),.(veh.tech.code),function(dff){ data.frame(frac=sum(dff$num)/tot.num) })
        ddply(subset(munis.demand,muni==df$muni[1]),.(speed),function(dff){
          data.frame(tech=techs$veh.tech.code,vmt=techs$frac * dff$vmt)
        })
      })
      munis.demand.by.tech$scenario <- paste(scenario.year,"-pen",roundC(pev.pen*100,0),sep='')
      munis.demand.by.tech.all <- rbind(munis.demand.by.tech.all,munis.demand.by.tech)
      munis.demand.by.tech <- munis.demand.by.tech[,1:(ncol(munis.demand.by.tech)-1)]

      write.csv(cast(melt(munis.demand.by.tech,id.vars=c("muni","tech","speed"),measure.vars="vmt"),muni + tech ~ speed),
              paste(path.to.humveh,"../ghg-analysis/year-",scenario.year,"-pen-",roundC(pev.pen*100,0),"-vmt-by-muni-speed-and-type.csv",sep=''))
    }
  }
}

ddply(munis.demand.by.tech.all,.(scenario),function(df){ data.frame(vmt=sum(df$vmt)) })
#  scenario     vmt
#      2010 3551179
#      2020 3507915

# TRANSCAD TOTAL VMT
# 2010
# 3.366e6
# 2015
# 3.324e6
# 2020
# 3.265e6
# 2025
# 3.561e6
# 2035
# 3.889e6

sum(subset(od.24.all,scenario==scenario.year)$demand)
# 2005: 382638.9
# 2010: 416905.9  # but 79965.91 are NA
# 2020: 408773.4


##############################################################
#  Rig sales fractions to match our penetration scenarios
##############################################################

age <- read.csv(paste(path.to.ghg,"age-dist-2020.csv",sep=''))
age <- subset(age,model.year>2010 & model.year<=2020)
sf <- subset(read.csv(paste(path.to.ghg,"sales-fractions-default.csv",sep='')),year>2010)

pens <- c(0.01,0.02)

sum.series <- function(r){
  (sum(10*r^(1:10))-num.pev)^2
}
sum.series2 <- function(r){
  (sum(age$pop*0.1*r^(9:0))-num.pev)^2
}


for(pen in pens){
  num.pev <- pen*pop.2020
  #rate <- optimize(sum.series,lower=1,upper=10)$minimum
  rate <- optimize(sum.series2,lower=0,upper=1)$minimum
  pevs <- age$pop*0.1*rate^(9:0)
  percent.pevs <- data.frame(year=2011:2020,pp=pevs/age$pop*100)
  sf.new <- ddply(sf,.(year),function(df){
    df.e <- subset(df,type=="exhaust")
    df.v <- subset(df,type=="evap")
    y <- df.e$year[1]
    to.move <- subset(percent.pevs,year==y)$pp  
    df.e$percent[df.e$tech=="ZEV"] <-  df.e$percent[df.e$tech=="ZEV"] + to.move
    df.e$percent[df.e$tech%in%c("PZEV","LEV II","ULEV II")] <-  df.e$percent[df.e$tech%in%c("PZEV","LEV II","ULEV II")] - df.e$percent[df.e$tech%in%c("PZEV","LEV II","ULEV II")]/sum(df.e$percent[df.e$tech%in%c("PZEV","LEV II","ULEV II")])*to.move
    df.v$percent <- df.v$percent - to.move/2
    df.v$percent[df.v$tech=="ZEV"] <- df.v$percent[df.v$tech=="ZEV"] + 1.5*to.move
    rbind(df.e,df.v)
  })
  write.csv(cast(melt(sf.new,id.vars=c('tech','year','type'),measure.vars='percent'),type + tech ~year),
            paste(path.to.humveh,"../ghg-analysis/sales-fractions-for-",pen*100,"-percent-penetration.csv",sep=''))
}

