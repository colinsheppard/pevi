library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools'))

make.plots  <- F
num.processors <- 11
registerDoMC(num.processors)

#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
path.to.humveh <- paste(base.path,'data/Vehicle-Registration/',sep='')
path.to.geatm <- paste(base.path,'pev-shared/data/GEATM-2020/',sep='')
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

load(file=paste(path.to.geatm,'od-old-and-new-including-external-trips.Rdata',sep=''))
load(paste(path.to.pevi,'inputs/routing-corrected.Rdata',sep=''))
y.bins <- seq(0,70,by=5)
y.labs <- c("[0,25)","[25,40)","[40,50)","[50,60)","[60,70)")
route.ordered$speed.binned    <- factor(y.labs[findInterval(route.ordered$ab_speed,y.bins)],levels=y.labs)

route.ordered.sub <- route.ordered[,c('from_taz','to_taz','length','ab_speed')]
names(route.ordered.sub) <- c('from','to','length','speed')
routes.agg <- ddply(route.ordered.sub,.(from,to),function(df){ data.frame(length=sum(df$length),speed=weighted.mean(df$speed,df$length))})
route.ordered.sub <- rbind(route.ordered.sub,data.frame(from=subset(disttime,from==to)$from,to=subset(disttime,from==to)$to,length=subset(disttime,from==to)$miles,speed=mean(subset(routes.agg,length<10)$speed)))
route.ordered.sub <- ddply(route.ordered.sub,.(from,to,speed),function(df){ data.frame(length=sum(df$length)) })

route.ordered.sub.joined <- join(na.omit(od.24.new[,c('from','to','demand')]),route.ordered.sub)

munis <- data.frame(muni = "Eureka", shp.row = grep("EKA_",agg.taz$name), id = agg.taz$id[grep("EKA_",agg.taz$name)])
munis <- rbind(munis,data.frame(muni = "Arcata", shp.row = grep("ARC_",agg.taz$name), id = agg.taz$id[grep("ARC_",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Fortuna", shp.row = grep("FOR_",agg.taz$name), id = agg.taz$id[grep("FOR_",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Trinidad", shp.row = grep("Trinidad",agg.taz$name), id = agg.taz$id[grep("Trinidad",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Ferndale", shp.row = grep("Ferndale",agg.taz$name), id = agg.taz$id[grep("Ferndale",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Rio Dell", shp.row = grep("RioDell",agg.taz$name), id = agg.taz$id[grep("RioDell",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Blue Lake", shp.row = grep("BlueLake",agg.taz$name), id = agg.taz$id[grep("BlueLake",agg.taz$name)]))
munis <- rbind(munis,data.frame(muni = "Humboldt (NC)", shp.row = which(!agg.taz$id %in% munis$id), id = agg.taz$id[which(!agg.taz$id %in% munis$id)]))

munis.demand <- ddply(munis,.(muni),function(df){
  sub.od <- subset(route.ordered.sub.joined,from %in% df$id)
  ddply(sub.od,.(speed),function(ddf){ data.frame(vmt=sum(ddf$demand*ddf$length)) })
})
munis.demand$vmt <- munis.demand$demand * munis.demand$length

# plot the results to inspect
ggplot(munis.demand,aes(x=speed,y=vmt))+geom_point()+facet_wrap(~muni)+scale_y_log10()

# now break the vmt out by vehicle type for each muni
load(paste(path.to.humveh,'veh.Rdata',sep=''))  



