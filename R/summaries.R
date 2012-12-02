library(colinmisc)
#load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','RPostgreSQL','colorRamps','reshape'))
load.libraries(c('ggplot2','plyr'))
#gpclibPermit()

#path.to.geatm  <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
#path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'

#source(paste(path.to.pevi,'R/gis-functions.R',sep=''))

#taz <- readShapePoly(paste(path.to.pevi,'inputs/development/aggregated-taz',sep=''))
#load(paste(path.to.pevi,'inputs/development/aggregated-taz-fieldnames.Rdata',sep=''))
#names(taz@data) <- c('row',agg.taz.shp.fieldnames)
#for(i in 1:nrow(taz@data)){
  #taz@data$shp.id[i] <- as.numeric(slot(slot(taz[i,],"polygons")[[1]],"ID"))
#}

dr <- read.csv(paste(path.to.pevi,"netlogo/driver-summary-out.csv",sep='')) #,stringsAsFactors=F

ggplot(dr,aes(x=vehicle.type,y=value))+geom_boxplot()+facet_wrap(~metric,scales='free_y')

dr.summed <- ddply(dr,.(vehicle.type,metric),function(df){ data.frame(value=sum(df$value)) })

ggplot(dr,aes(x=vehicle.type,y=value))+geom_bar(stat='identity')+facet_wrap(~metric,scales='free_y')

ch <- read.csv(paste(path.to.pevi,"netlogo/charging-out.csv",sep='')) #,stringsAsFactors=F

ggplot(ch,aes(x=time))+geom_histogram(binwidth=1)+facet_grid(vehicle.type ~ charger.level)
ggplot(ch,aes(x=duration))+geom_histogram(binwidth=0.25)+facet_grid(vehicle.type ~ charger.level)
ggplot(ch,aes(x=energy))+geom_histogram(binwidth=1)+facet_grid(vehicle.type ~ charger.level)
ggplot(ch,aes(x=time,y=duration,colour=factor(charger.level)))+geom_point()+facet_wrap(~vehicle.type)
