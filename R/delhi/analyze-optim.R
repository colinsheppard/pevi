library(colinmisc)
Sys.setenv(NOAWT=1)
load.libraries(c('ggplot2','yaml','stringr','RNetLogo','maptools','reshape','colorRamps','caTools'))
source(pp(pevi.home,'R/gis-functions.R'))

# unfortunately, results from res-none are worthless
#scenarios <- c('base','res-none','homeless','half-homeless')
scenarios <- c('homeless','half-homeless','no-homeless')

chs <- data.frame()
opts <- data.frame()
for(optim.code in scenarios){
  for(seed in c(20:30)){
    hist.file <- pp(pevi.shared,'data/outputs/optim-new/delhi-',optim.code,'-seed',seed,'/charger-buildout-history.Rdata')
    final.evse.file <- pp(pevi.shared,'data/outputs/optim-new/delhi-',optim.code,'-seed',seed,'/delhi-',optim.code,'-seed',seed,'-pen2-final-infrastructure.txt')
    if(file.exists(final.evse.file)){
      load(hist.file)
      charger.buildout.history$scenario <- optim.code
      charger.buildout.history$seed     <- seed
      charger.buildout.history$obj      <- 'new'
      chs <- rbind(chs,charger.buildout.history)
      load(pp(pevi.shared,'data/outputs/optim-new/delhi-',optim.code,'-seed',seed,'/optimization-history.Rdata'))
      opt.history$scenario <- optim.code
      opt.history$seed     <- seed
      opts <- rbind(opts,opt.history)
    }
  }
}
names(chs) <- c('TAZ',tail(names(chs),-1))
chs <- data.table(chs,key=c('scenario','seed','penetration','iter'))

ch.fin <- ddply(subset(chs,TAZ>0),.(scenario,seed,penetration),function(df){
  subset(df,iter==max(iter))
})
ch.fin.unshaped <- ch.fin

winners <- ddply(opts,.(scenario,seed,penetration,iteration),function(df){
  df[which.min(df$obj),]
})
winners$cost <- c(0.5,5,25,400)[match(winners$level,1:4)]
winners$cost[winners$level==2 & winners$scenario=="L2-10k"] <- 10
winners$cost[winners$level==2 & winners$scenario=="L2-12.5k"] <- 12.5
winners$cost[winners$level==2 & winners$scenario=="L2-20k"] <- 20
winners$cost[winners$level==2 & winners$scenario=="L2-30kW"] <- 10
winners <- ddply(winners,.(scenario,seed),function(df){
  df <- as.data.frame(data.table(df,key=c('penetration','iteration')))
  data.frame(df,cum.cost=cumsum(df$cost))
})
winners$delay <- winners$mean.delay.cost

################################################
# PLOTTING
################################################
if(F){
  ggplot(subset(winners,delay < 200e6),aes(x=cum.cost/1e3,y=delay/1e6,colour=factor(scenario))) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="")+facet_wrap(~penetration)

  # look at the # chargers in each taz
  ch.fin <- subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3))
  ggplot(subset(ch.fin,scenario=='homeless'),aes(x=factor(TAZ),y=value,fill=level)) + geom_bar(stat='identity') + facet_grid(penetration~seed) + labs(x="",y="",title="")

  # compare seeds against each other for each TAZ / level
  ch.fin <- cast(subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),scenario=='homeless' & level%in%pp('L',1:3)),TAZ + level + penetration ~ seed)
  ggplot(ch.fin,aes(x=`21`,y=`22`,colour=level)) + geom_point() + facet_wrap(~penetration) + labs(x="",y="",title="")+geom_abline(intercept=0,slope=1)

  # compare several optimization runs at once
  ch.fin <- subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3))
  #ch.fin$scenario <- factor(ch.fin$scenario,levels=c('0% Drivers with Home Chargers','50% Drivers with Home Charging','100% Drivers with Home Charging'))
  ch.fin$scenario <- revalue(ch.fin$scenario,c('homeless'='0% Drivers with Home Chargers','half-homeless'='50% Drivers with Home Charging','no-homeless'='100% Drivers with Home Charging'))
  stat_sum_single <- function(fun, geom="point", ...) {
    stat_summary(fun.y=fun, geom=geom, size = 3, ...)
  }
  ggplot(ddply(ch.fin,.(scenario,level,penetration,seed),function(df) { data.frame(num.chargers=sum(df$value)) }),aes(x=scenario,y=num.chargers,colour=level))+geom_point() + facet_wrap(~penetration) + stat_summary(fun.y=mean,geom='point',size=3)+labs(x="Scenario",y="# Chargers",title="# Chargers Sited over Optimization Variations")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

################################################
# RANKINGS AND GOOGLE MAP
################################################
load(pp(pevi.shared,'data/DELHI/POLYGON/AggregatedTAZsCleaned.Rdata'))
agg.coords <- coordinates(agg.taz)
##write.csv(agg.taz@data,pp(pevi.shared,'data/DELHI/results/maps/agg-taz-data.csv'))
agg.taz.data <- read.csv(pp(pevi.shared,'data/DELHI/results/maps/agg-taz-groupings-and-existing.csv'))

plur.station <- function(num,rank){
  res <- ifelse(num>1,'s','')
  res <- ifelse(is.na(rank),res,pp(res,' (Priority: ',ifelse(rank==1,'first',ifelse(rank==2,'second',ifelse(rank==3,'third','fourth'))),')'))
  res
}
# export the TAZs for the base layer
#agg.taz$shp.id <- as.numeric(unlist(lapply(agg.taz@polygons,function(l){ slot(l,'ID') })))
#shp.to.kml(agg.taz,pp(pevi.shared,'data/DELHI/results/maps/tazs.kml'),'NTC Delhi TAZs','',borders="black",colors='#FFFFFF33',id.col="shp.id",name.col="name")

level.nums <- 1:4
level.names <- c('Level 1','Level 2','DC Fast','Battery Swap')

agg.data.rows <- match(agg.taz$agg.id,agg.taz.data$agg.id)
for(lev in level.nums){
  agg.taz@data[,pp('L',lev,'E')] <- agg.taz.data[agg.data.rows,pp('L',lev,'E')]
}

# Low zoom (most distant from earth)
agg.taz$low.zoom.group <- 'NCT of Delhi'

# Use k-means clustering for med zoom group
clust <- kmeans(agg.taz@data[,c('x','y')],5)
agg.taz$med.zoom.group <- pp('Group ',clust$cluster)

for(scen in scenarios){
  win <- data.table(subset(winners,scenario==scen),key=c('seed','penetration','iteration'))
  win[,rank:=(1:length(iteration))/length(iteration),by='seed']
  setkey(win,'taz','level','seed')
  best.ranks <- win[,list(rank=min(rank)),by=c('taz','level','seed')]
  best.ranks <- best.ranks[,list(rank=mean(rank)),by=c('taz','level')]
  setkey(best.ranks,'rank')
  best.ranks[,percentile:=(1:length(rank))/length(rank)]
  best.ranks[,quartile:=ifelse(percentile<=0.25,1,ifelse(percentile<=0.5,2,ifelse(percentile<=0.75,3,4)))]
  best.ranks$name <- agg.taz$name[match(best.ranks$taz,agg.taz$agg.id)]
  # the ranking analysis continues below
  
  level.nums <- sort(unique(win$level))
  num.levels <- length(level.nums)

  # Now to calculate some average buildouts. First, let's average the final buildout between all seeds.
  # The resulting data frame should have for each row a unique penetration/taz/level combination, and the
  # remaining columns will be the total for each scenario.

  ch.fin <- melt(subset(ch.fin.unshaped,scenario==scen),measure.vars=pp('L',0:4),variable_name="level")
  mean.ch.fin <- ddply(ch.fin,.(penetration,TAZ,level,scenario),function(df){
    data.frame(num.chargers = round(mean(df$value,na.rm=TRUE)))
  })

  setkey(best.ranks,'taz','level')
  for(lev in level.nums){
    lev.scenario <- subset(mean.ch.fin,penetration==0.02 & level==pp('L',lev),select=c('TAZ','num.chargers'))
    agg.taz@data[,pp('L',lev)] <- lev.scenario$num.chargers[match(agg.taz$agg.id,lev.scenario$TAZ)]
    agg.taz@data[,pp('L',lev,'.rank')] <- best.ranks[J(agg.taz.data$agg.id,lev),quartile]$quartile
  }

  agg.taz.data.for.join <- data.table(melt(agg.taz@data,id.vars=c('agg.id'),measure.vars=pp('L',level.nums)))
  agg.taz.data.for.join[,taz:=agg.id]
  agg.taz.data.for.join[,level:=as.numeric(substr(variable,2,2))]
  setkey(agg.taz.data.for.join,'taz','level')
  setkey(best.ranks,'taz','level')
  best.ranks <- agg.taz.data.for.join[best.ranks]
  setkey(best.ranks,'rank')
  write.csv(best.ranks,pp(pevi.shared,'data/DELHI/results/charger-priorities-',scen,'.csv'))

  # prep info for separating the icons
  angs <- head(seq(0,2*pi,length.out=num.levels+1),-1)
  sins <- -sin(angs)
  coss <- cos(angs)

  # For the charger files that wll get translated nto Google maps, we use mean.ch.fin
  # Each charger is defined by an array: 
  # [ NAME, LONG, LAT, DESCRIP, # CHARGERS, APPEAR ABOVE ZOOM, APPEAR BELOW ZOOM, EXISTING CHARGER?, CHARGER LEVEL ]
  charger.data.file <- pp(pevi.shared,'data/DELHI/results/maps/charger-data-',scen,'.yaml')
  cat('chargers:\n',file=charger.data.file)

  # start with the high zoom level tazs
  sep.radius <- 0.008
  for(i in 1:nrow(agg.taz@data)){
    for(lev in level.nums){
      sep.i <- which(lev == level.nums)
      for(is.existing in c(T,F)){
        existing.token <- ifelse(is.existing,'E','')
        existing.string <- ifelse(is.existing,'true','false')
        num.stations <- agg.taz@data[i,pp('L',lev,existing.token)]
        site.rank <- agg.taz@data[i,pp('L',lev,'.rank')]
        if(num.stations > 0) cat(sprintf(pp("  - ['%s',%f,%f,'<b>%s</b> <br/>%d ",ifelse(is.existing,'existing','proposed')," ",level.names[lev]," Station",plur.station(num.stations,site.rank),"',%d,%s,%s,%s,%d]\n"),agg.taz$name[i],agg.taz$x[i]+sep.radius*sins[sep.i],agg.taz$y[i]+sep.radius*coss[sep.i],agg.taz$name[i],num.stations,num.stations,'10','null',existing.string,lev),file=charger.data.file,append=T)
      }
    }
  }
  # now write medium zooms. To have medium zoom disappear as high zoom appears, make sure the maximum zoom level is 1 lower than high zoom's lowest level.
  sep.radius <- 0.04
  d_ply(agg.taz@data,.(med.zoom.group),function(df){
    lon <- mean(df$x)
    lat <- mean(df$y)
    for(lev in level.nums){
      sep.i <- which(lev == level.nums)
      for(is.existing in c(T,F)){
        existing.token <- ifelse(is.existing,'E','')
        existing.string <- ifelse(is.existing,'true','false')
        num.stations <- sum(df[,pp('L',lev,existing.token)])
        if(num.stations > 0) cat(sprintf(pp("  - ['%s',%f,%f,'<b>%s</b> <br/>%d ",ifelse(is.existing,'existing','proposed')," ",level.names[lev]," Station",plur.station(num.stations,site.rank),"',%d,%s,%s,%s,%d]\n"),df$med.zoom.group[1],lon+sep.radius*sins[sep.i],lat+sep.radius*coss[sep.i],df$med.zoom.group[1],num.stations,num.stations,'9','11',existing.string,lev),file=charger.data.file,append=T)
      }
    }
  })
  sep.radius <- 0.1
  d_ply(agg.taz@data,.(low.zoom.group),function(df){
    lon <- mean(df$x)
    lat <- mean(df$y)
    for(lev in level.nums){
      sep.i <- which(lev == level.nums)
      for(is.existing in c(T,F)){
        existing.token <- ifelse(is.existing,'E','')
        existing.string <- ifelse(is.existing,'true','false')
        num.stations <- sum(df[,pp('L',lev,existing.token)])
        if(num.stations > 0) cat(sprintf(pp("  - ['%s',%f,%f,'<b>%s</b> <br/>%d ",ifelse(is.existing,'existing','proposed')," ",level.names[lev]," Station",plur.station(num.stations,site.rank),"',%d,%s,%s,%s,%d]\n"),df$low.zoom.group[1],lon+sep.radius*sins[sep.i],lat+sep.radius*coss[sep.i],df$low.zoom.group[1],num.stations,num.stations,'null','10',existing.string,lev),file=charger.data.file,append=T)
      }
    }
  })
} # end for each scenario

