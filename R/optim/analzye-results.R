library(colinmisc)
Sys.setenv(NOAWT=1)
load.libraries(c('yaml','stringr','RNetLogo','maptools','reshape'))

base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/optim/',sep='')
path.to.outputs <- paste(base.path,'pev-shared/data/outputs/optim/',sep='')
path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
nl.path <- "/Applications/NetLogo\ 5.0.3"
model.path <- paste(path.to.pevi,"netlogo/PEVI-nolog.nlogo",sep='')

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))
source(paste(path.to.pevi,"R/optim/optim-functions.R",sep='')) # note this will in turn call optim-config, objectives, and constraints
source(paste(path.to.pevi,"R/optim/optim-config.R",sep=''))
source(paste(path.to.pevi,"R/optim/objectives.R",sep=''))
source(paste(path.to.pevi,"R/optim/constraints.R",sep=''))
source(paste(path.to.pevi,"R/reporters-loggers.R",sep=''))

# load aggregated tazs
agg.taz <- readShapePoly(paste(path.to.pevi,'inputs/development/aggregated-taz-with-weights',sep=''))
load(paste(path.to.pevi,'inputs/development/aggregated-taz-with-weights-fieldnames.Rdata',sep=''))
names(agg.taz@data) <- c('row',taz.shp.fieldnames)
agg.taz@data$ID <- unlist(lapply(agg.taz@polygons,function(x){slot(x,'ID')}))

optim.code <- 'min-cost-constrained-by-frac-delayed'
#optim.code <- 'min-cost-constrained-by-num-stranded'

for(pev.penetration in c(0.005,0.01,0.02,0.04)){
  #pev.penetration <- 0.01
  load(paste(path.to.outputs,optim.code,"/0saved-state-pen",pev.penetration*100,".Rdata",sep=''))
  base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
  path.to.pevi <- paste(base.path,'pevi/',sep='')
  path.to.inputs <- paste(base.path,'pev-shared/data/inputs/optim/',sep='')
  path.to.outputs <- paste(base.path,'pev-shared/data/outputs/optim/',sep='')
  path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
  nl.path <- "/Applications/NetLogo\ 5.0.3"
  model.path <- paste(path.to.pevi,"netlogo/PEVI-nolog.nlogo",sep='')

  final.gen <- gen.num - 1
  ptx.m <- melt(data.frame(ptx=1:(nrow(all.ptx[,,1])),all.ptx[,1:(ncol(all.ptx[,,1])-1),final.gen]),id.vars=c("ptx"))
  ptx.m$taz <- as.numeric(unlist(lapply(strsplit(substr(as.character(ptx.m$variable),2,nchar(as.character(ptx.m$variable))-1),".",fixed=T),function(x){ x[[1]] })))
  ptx.m$level <- as.numeric(unlist(lapply(strsplit(as.character(ptx.m$variable),"L",fixed=T),function(x){ x[2] })))
  ptx.m$name <- agg.taz$name[match(ptx.m$taz,agg.taz$id)]

  tot.by.taz <- ddply(ptx.m,.(taz),function(df){
                     sum.weights <- rep(1,nrow(df))
                     sum.weights[df$level==3] <- 2
                     data.frame(charger.score=2*mean(df$value*sum.weights),L2=mean(df$value[df$level==2]),L3=mean(df$value[df$level==3]))})
  agg.taz@data$L2 <- roundC(tot.by.taz$L2[match(agg.taz$id,tot.by.taz$taz)],1)
  agg.taz@data$L3 <- roundC(tot.by.taz$L3[match(agg.taz$id,tot.by.taz$taz)],1)
  agg.taz@data$charger.score <- tot.by.taz$charger.score[match(agg.taz$id,tot.by.taz$taz)]
  c.map <- paste(map.color(agg.taz@data$charger.score,blue2red(50)),'7F',sep='')
  shp.to.kml(agg.taz,paste(path.to.google,'optim/',optim.code,'-pen',100*pev.penetration,'.kml',sep=''),paste('Pen ',100*pev.penetration,'% Optimization: ',optim.code,sep=''),'Color denotes total chargers in each TAZ with L3 counting for 2 chargers (click to get actual # chargers).','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','L2','L3','weighted.demand','frac.homes'))
}

