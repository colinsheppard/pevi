library(colinmisc)
Sys.setenv(NOAWT=1)
load.libraries(c('ggplot2','yaml','stringr','RNetLogo','maptools','reshape','colorRamps','caTools','grid','locfit'))
source(pp(pevi.home,'R/gis-functions.R'))

my.purp <- '#984ea3'
my.oran <- '#ff7f00'
my.red <- '#e41a1c'
my.blue <- '#377eb8'
my.green <- '#4daf4a'
charger.cols <- c(my.green,my.blue,my.purp,my.oran,my.red)

scenarios <- c('base')
scen.names <- yaml.load(readChar(pp(pevi.shared,'data/inputs/optim-new/scenarios.yaml'),file.info(pp(pevi.shared,'data/inputs/optim-new/scenarios.yaml'))$size))

refactor.scen <- function(x,base='Base'){
  x <- factor(x)
  ordering <- subset(data.frame(scen=names(scen.names),named=unlist(lapply(scen.names,function(x){ x[[1]] })),order=unlist(lapply(scen.names,function(x){ x[[2]] })),stringsAsFactors=F),scen%in%x)
  ordering$named[ordering$scen=="half-homeless"] <- base
  rename.vec <- ordering$named
  names(rename.vec) <- ordering$scen
  x <- revalue(x,rename.vec)
  x <- factor(x,ordering$named[order(ordering$order)])
  x
}
refactor.lev <- function(x){
  x <- factor(x)
  ordering <- subset(data.frame(scen=as.character(0:4),named=c('Residential','Level 1','Level 2','DC Fast','Battery Swapping'),order=1:5,stringsAsFactors=F),scen%in%x)
  rename.vec <- ordering$named
  names(rename.vec) <- ordering$scen
  x <- revalue(x,rename.vec)
  x <- factor(x,ordering$named[order(ordering$order)])
  x
}

chs <- data.frame()
opts <- data.frame()
build.increments <- list()
charger.types <- list()
done.list <- c()
for(optim.code in scenarios){
  source(pp(pevi.shared,'data/inputs/optim-new/upstate-',optim.code,'/params.R'))
  build.increments[[optim.code]] <- build.increment
  param.file <- pp(pevi.shared,'data/inputs/optim-new/upstate-',optim.code,'/params.txt')
  param.file.data <- read.table(param.file,sep='\t')
  param.file.data <- streval(pp('data.frame(',pp(apply(param.file.data,1,function(x){ pp(str_replace_all(x[1],'-','.'),'=',ifelse(length(grep('file|directory',x[1]))>0,pp('"',x[2],'"'),ifelse(x[2]%in%c('true','false'),x[2]=='true',x[2]))) }),collapse=","),',stringsAsFactors=F)'))
  charger.data <- read.table(pp(pevi.shared,param.file.data$charger.type.input.file),header=TRUE)
  names(charger.data) <- c('level',tail(names(charger.data),-1))
  charger.types[[optim.code]] <- charger.data
  for(seed in 1:5){
    hist.file <- pp(pevi.shared,'data/outputs/optim-new/upstate-',optim.code,'-seed',seed,'/charger-buildout-history.Rdata')
    final.evse.file <- pp(pevi.shared,'data/outputs/optim-new/upstate-',optim.code,'-seed',seed,'/upstate-',optim.code,'-seed',seed,'-pen2-final-infrastructure.txt')
    if(file.exists(hist.file)){
      load(hist.file)
      charger.buildout.history$scenario <- optim.code
      charger.buildout.history$seed     <- seed
      charger.buildout.history$obj      <- 'new'
      chs <- rbind(chs,charger.buildout.history)
      load(pp(pevi.shared,'data/outputs/optim-new/upstate-',optim.code,'-seed',seed,'/optimization-history.Rdata'))
      opt.history$scenario <- optim.code
      opt.history$seed     <- seed
      opts <- rbind(opts,opt.history)
      if(file.exists(final.evse.file))done.list <- c(done.list,pp(optim.code,'---',seed))
    }
  }
}
names(chs) <- c('TAZ',tail(names(chs),-1))
chs <- data.table(chs,key=c('scenario','seed','penetration','iter'))
chs.raw <- chs
opts.raw <- opts

#load(file=pp(pevi.shared,'data/inputs/compare/upstate-baseline-pain/mean-delay-and-strand.Rdata'))
#baseline.delay$scen[baseline.delay$scen=="All High"] <- "veh-high"
#baseline.delay$scen[baseline.delay$scen=="All Low"] <- "veh-low"
#baseline.delay <- subset(baseline.delay,!scen %in% c('veh-high','veh-low','no-homeless','homeless'))
#baseline.delay$scen[baseline.delay$scen=="opp-cost"] <- "opp-cost-high"
#baseline.delay$scen[baseline.delay$scen=="base"] <- "half-homeless"
#baseline.delay$penetration <- baseline.delay$penetration/100
#baseline.delay$key <- pp(baseline.delay$scen,"-",baseline.delay$penetration)
#opts <- opts.raw
#opts$key <- pp(opts$scenario,"-",opts$penetration)
#for(key in baseline.delay$key){
  #inds <- which(key == opts$key)
  #opts$mean.delay.cost[inds] <- opts$mean.delay.cost[inds] - baseline.delay$min.delay.cost[key == baseline.delay$key]
#}
#inds <- which(! opts$key %in% baseline.delay$key)
#opts$mean.delay.cost[inds] <- opts$mean.delay.cost[inds] - baseline.delay$min.delay.cost['half-homeless-0.01' == baseline.delay$key]

winners.raw <- ddply(opts,.(scenario,seed,penetration,iteration),function(df){
  df[which.min(df$obj),]
})
winners <- ddply(winners.raw,.(scenario,seed),function(df){
  build.increment <- unlist(build.increments[[df$scenario[1]]][,2:5])
  charger.type <- charger.types[[df$scenario[1]]] 
  cost.per.iter <- build.increment * charger.type$installed.cost[match(1:4,charger.type$level)]
  df$cost <- cost.per.iter[match(df$level,1:4)]
  df$num.added <- build.increment[match(df$level,1:4)]
  dt <- data.table(df,key=c('penetration','iteration'))
  df <- as.data.frame(dt)
  df$cum.cost <- dt[,list(cum.cost=cumsum(cost)),by='penetration']$cum.cost
  df
})
winners$delay <- winners$mean.delay.cost
winners <- ddply(winners,.(scenario,seed,penetration),function(df){
  data.frame(df,marginal.cost=c(NA,abs(diff(df$cum.cost*1e3) / diff(df$delay))))
})

# we need to find the right stopping point and get rid of the extraneous results
winners.tracking <- ddply(winners,.(scenario,penetration,seed),function(df){
  df$above.thresh <- F
  # drop all the data beyond the point at which the derivative estimate from locfit is greater than -1
  # note, -1000 here corresponds to a slope of -1 if the units of delay and cum.cost were the same
  tryCatch(fit <- locfit(as.formula('delay ~ lp(cum.cost, nn=0.5)'),data=df,deriv=1),error = function(e) e,finally=function(){})
  if(exists('fit')){
    #if(df$scenario[1]=='homeless'){
      #thresh <- -500
    #}else{
      #thresh <- -2000
    #}
    #first.to.drop <- which(predict(fit,newdata=df) > thresh)[1]
    #if(!is.na(first.to.drop)){
      #df$above.thresh[first.to.drop:nrow(df)] <- T
    #}
    df$above.thresh <- predict(fit,newdata=df)/1e3
  }
  df
})
chs <- chs.raw
winners <- ddply(winners,.(scenario,penetration,seed),function(df){
  df$above.thresh <- F
  # drop all the data beyond the point at which the derivative estimate from locfit is greater than -1
  # note, -1000 here corresponds to a slope of -1 if the units of delay and cum.cost were the same
  tryCatch(fit <- locfit(as.formula('delay ~ lp(cum.cost, nn=0.5)'),data=df,deriv=1),error = function(e) e,finally=function(){})
  if(exists('fit')){
    #if(df$scenario[1]=='homeless'){
      #thresh <- -500
    #}else if(df$scenario[1]=='half-homeless' & df$penetration[1]==0.01){
      #thresh <- -1500
    #}else if(df$scenario[1]=='veh-high' & df$penetration[1]==0.02){
      #thresh <- -5000
    #}else if(df$scenario[1]=='veh-low' & df$penetration[1]==0.02){
      #thresh <- -500
    #}else{
      thresh <- -1000
    #}
    first.to.drop <- which(predict(fit,newdata=df) > thresh)[1]
    if(!is.na(first.to.drop)){
      df$above.thresh[first.to.drop:nrow(df)] <- T
      chs <<- subset(chs,!(scenario==df$scenario[1] & penetration==df$penetration[1] & seed==df$seed[1] & iter >= df$iteration[first.to.drop]))
    }
  }
  if(all(!df$above.thresh)){
    chs <<- subset(chs,!(scenario==df$scenario[1] & penetration==df$penetration[1] & seed==df$seed[1]))
    df <- df[c(),]
  }
  df
})
winners <- subset(winners,!above.thresh)

ch.fin <- ddply(subset(chs,TAZ>0),.(scenario,seed,penetration),function(df){
  subset(df,iter==max(iter))
})
ch.fin.unshaped <- ch.fin

################################################
# ANALYSIS
################################################
if(F){
  wins <- winners
  wins$cum.cost <- wins$cum.cost*1000
  slopes <- ddply(wins,.(scenario,seed,penetration),function(df){
    # skip rows to simulate doubling build increment
    #start.row <- ifelse(nrow(df)%%2 == 0,2,1)
    #df <- df[seq(start.row,nrow(df),by=2),]
    data.frame(n=3:6,slope=sapply(3:6,function(x){ lm('delay ~ cum.cost',tail(df,x))$coefficients[2] }))
  })
  slopes <- ddply(slopes,.(scenario,penetration,n),function(df){
    data.frame(slope=mean(df$slope),n=df$n[1])
  })
  ggplot(slopes,aes(x=factor(penetration),y=slope,fill=factor(n))) + geom_bar(stat='identity',position='dodge') + facet_wrap(~scenario,scales='free_y') + labs(x="",y="",title="")+geom_abline(intercept=-10,slope=0)

  # pull out the value of the infrastructure
  val <- ddply(ddply(winners,.(penetration,seed),function(df){ data.frame(min.delay=min(df$mean.delay.cost),max.delay=max(df$mean.delay.cost))}),.(penetration),function(df){ data.frame(min.delay=mean(df$min.delay),max.delay= mean(df$max.delay))})
  val$saved <- (val$max.delay - val$min.delay)/1e6

  # make plots of Upstate with labels for the report
  library(gpclib); gpclibPermit()
  agg.taz.for.ggplot  <- fortify(agg.taz, region = "name")
  agg.taz.for.ggplot  <- merge(agg.taz.for.ggplot, agg.taz@data, by.x = "id", by.y = "name")
  agg.taz.for.ggplot$mapgroup <- NA 
  agg.taz.for.ggplot$mapgroup[substr(agg.taz.for.ggplot$id,1,3) == "SHA"] <- 'SHA'
  agg.taz.for.ggplot$mapgroup[substr(agg.taz.for.ggplot$id,1,3) == "RED"] <- 'RED'
  agg.taz.for.ggplot$mapgroup[substr(agg.taz.for.ggplot$id,1,3) == "COT" | substr(agg.taz.for.ggplot$id,1,3) == "AND"] <- 'COTAND'
  agg.taz.for.ggplot$mapgroup[substr(agg.taz.for.ggplot$id,4,4) != "_"] <- 'REST'

  dat.to.plot <- subset(agg.taz.for.ggplot,mapgroup=="REST")
  ggplot(agg.taz.for.ggplot,aes(x=long,y=lat,group=id,fill=mapgroup))+geom_polygon(colour='darkgrey')+geom_text(aes(label=id),data=ddply(dat.to.plot,.(id),function(df){ data.frame(long=mean(df$long),lat=mean(df$lat),mapgroup=NA) }),vjust=-0.5)+scale_fill_manual(values=c(my.blue,my.green,'lightgrey',my.oran))

  dat.to.plot <- subset(agg.taz.for.ggplot,mapgroup=="COTAND")
  ggplot(dat.to.plot,aes(x=long,y=lat,group=id))+geom_polygon(fill=my.blue,colour='darkgrey')+geom_text(aes(label=id),data=ddply(dat.to.plot,.(id),function(df){ data.frame(long=mean(df$long),lat=mean(df$lat)) }),vjust=-0.5)

  dat.to.plot <- subset(agg.taz.for.ggplot,mapgroup=="SHA")
  ggplot(dat.to.plot,aes(x=long,y=lat,group=id))+geom_polygon(fill=my.oran,colour='darkgrey')+geom_text(aes(label=id),data=ddply(dat.to.plot,.(id),function(df){ data.frame(long=mean(df$long),lat=mean(df$lat)) }),vjust=-0.5)

  dat.to.plot <- subset(agg.taz.for.ggplot,mapgroup=="RED")
  redding.labs <- ddply(dat.to.plot,.(id),function(df){ data.frame(long=mean(df$long),lat=mean(df$lat)) })
  redding.labs$id <- substr(redding.labs$id,5,nchar(redding.labs$id))
  redding.labs$lat[which(redding.labs$id=="Magnolia")] <-  redding.labs$lat[which(redding.labs$id=="Magnolia")] - 0.002
  redding.labs$long[which(redding.labs$id=="GoldHills")] <-  redding.labs$long[which(redding.labs$id=="GoldHills")] - 0.02
  redding.labs$long[which(redding.labs$id=="ForestHills")] <-  redding.labs$long[which(redding.labs$id=="ForestHills")] + 0.007
  redding.labs$lat[which(redding.labs$id=="ForestHills")] <-  redding.labs$lat[which(redding.labs$id=="ForestHills")] + 0.002
  redding.labs$long[which(redding.labs$id=="Enterprise")] <-  redding.labs$long[which(redding.labs$id=="Enterprise")] - 0.002
  redding.labs$long[which(redding.labs$id=="Pacheco")] <-  redding.labs$long[which(redding.labs$id=="Pacheco")] - 0.004
  redding.labs$long[which(redding.labs$id=="ParkMarina")] <-  redding.labs$long[which(redding.labs$id=="ParkMarina")] + 0.002
  redding.labs$lat[which(redding.labs$id=="MetzRoad")] <-  redding.labs$lat[which(redding.labs$id=="MetzRoad")] - 0.01
  redding.labs$lat[which(redding.labs$id=="LakeRedding")] <-  redding.labs$lat[which(redding.labs$id=="LakeRedding")] - 0.008
  redding.labs$long[which(redding.labs$id=="SportsComplex")] <-  redding.labs$long[which(redding.labs$id=="SportsComplex")] - 0.008
  redding.labs$id <- pp('RED_',redding.labs$id)
  ggplot(dat.to.plot,aes(x=long,y=lat,group=id))+geom_polygon(fill=my.green,colour='darkgrey')+geom_text(aes(label=id),data=redding.labs,vjust=-0.5,size=4)

}

################################################
# PLOTTING
################################################
if(F){
  num.ch.to.cap <- function(df){ 
    df$capacity <- df$num.chargers * c(1.5,6.6,50,400)[as.numeric(substr(df$level,2,2))]
    df
  }
  num.ch.to.cost <- function(df){ 
    scen <- df$scenario[1]
    df$installed.cost <- df$num.chargers * charger.types[[scen]]$installed.cost[match(df$level,pp('L',charger.types[[scen]]$level))] / 1e3
    df
  }
  
  # What are my sample sizes?
  write.csv(ddply(winners,.(scenario),function(df){ data.frame(num.seeds=length(unique(df$seed)))}),file=pp(pevi.shared,'data/DELHI/results//sample-size.csv'))

  ###############
  # Assess state of runs
  ###############
  winners.tracking$scenario.named <- refactor.scen(winners.tracking$scenario)
  winners.tracking$penetration.named <- pp(winners.tracking$penetration*100,"%")
  p <- ggplot(subset(winners.tracking,T),aes(x=cum.cost/1e3,y=delay/1e6,colour=above.thresh>-1)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",color="Scenario",shape="Scenario")+facet_wrap(scenario~seed~penetration.named) + scale_color_manual(values=charger.cols)

  ###############
  # Base Scenario
  ###############
  #winners$scenario.named <- refactor.scen(winners$scenario)
  #winners$penetration.named <- pp(winners$penetration*100,"%")
  #p <- gplot(subset(winners,seed==53 & scenario%in%c('half-homeless')),aes(x=cum.cost/1e3,y=delay/1e6,colour=factor(penetration.named))) + geom_point() + labs(x="Infrastructure Investment ($M)",y="PV of Driver Delay ($M)",title="",color="Fleet Penetration") + scale_color_manual(values=charger.cols)
  #ggsave(file=pp(pevi.shared,'data/DELHI/results//base/base-optimality-curves.pdf'),p,width=10,height=6)

  load(pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))
  load(pp(pevi.shared,'data/UPSTATE/od-converter.Rdata'))
  agg.taz$new.id <- od.converter$new.id[match(agg.taz$agg.id,od.converter$old.id)]
  agg.taz$county <- agg.taz$jurisdiction
  agg.taz$county[!agg.taz$county %in% c('Tehama','Siskiyou')] <- 'Shasta'
  ch.fin.unshaped$county <- agg.taz$county[match(ch.fin.unshaped$TAZ,agg.taz$new.id)]
  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,scenario %in% c('base')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',2:3) & !(seed==2 & penetration==.02)),.(scenario,penetration,county,level),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })

  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,scenario %in% c('base')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3)),.(scenario,level,penetration),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cost)
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cap)
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration <- pp(ch.fin$penetration*100,'%')
  ch.fin$scenario.named <- refactor.scen(ch.fin$scenario,base='50% Home Chargers')
  p <- ggplot(ch.fin,aes(x=penetration,y=capacity,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Capacity of Installed Chargers (kW)",title="Capacity of Chargers for Base Scenario",fill="Charger Level") +  theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results//base/base-charger-capacity.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=penetration,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="Chargers Sited for Base Scenario",fill="Charger Level") +  theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results//base/base-num-chargers.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=penetration,y=installed.cost,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Cost of Installed Chargers  ($M)",title="Charger Cost for Base Scenario",fill="Charger Level") + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results//base/base-charger-cost.pdf'),p,width=6,height=6)
  ch.fin$pen <- as.numeric(substr(ch.fin$penetration,1,nchar(ch.fin$penetration)-1))
  ch.fin.per.cap <- ddply(ch.fin,.(pen),function(df){
    data.frame(per.cap=sum(df$installed.cost)*1e6/(df$pen[1]*10e3))
  })
  p <- ggplot(ch.fin.per.cap,aes(x=pen,y=per.cap)) +  geom_point(colour=charger.cols[2]) + geom_line(colour=charger.cols[2]) + labs(x="Fleet Penetration (%)",y="Investment per Electric Vehicle ($)",title="Total Public Infrastructure Investment per Electric Vehicle") + scale_x_continuous(limits=c(0,max(ch.fin.per.cap$pen)*1.05)) + scale_y_continuous(limits=c(0,max(ch.fin.per.cap$per.cap)*1.05))
  ggsave(file=pp(pevi.shared,'data/DELHI/results//base/base-per-cap-cost.pdf'),p,width=6,height=6)

  # hours of delay per 1$M of total delay cost is 232.4047

  # Not a plot but save charger profiles for later runs
  ch.sav <- data.table(subset(ch.fin.unshaped,scenario=='half-homeless'),key=c('TAZ','penetration'))
  ch.sav <- ch.sav[,list(L0=round(mean(L0)),L1=round(mean(L1)),L2=round(mean(L2)),L3=round(mean(L3)),L4=round(mean(L4))),by=c('TAZ','penetration')]
  existing.ch <- read.table(file=pp(pevi.shared,'data/inputs/charger-input-file/delhi/delhi-existing-chargers-no-external.txt'),sep='\t',header=T)
  names(existing.ch) <- c('TAZ',pp('L',0:4))
  d_ply(ch.sav,.(penetration),function(df){
        pen <- df$penetration[1]
        df <- df[,c('TAZ',pp('L',0:4))]
        df <- rbind(df,subset(existing.ch,TAZ<0))
        names(df) <- c(';TAZ',pp('L',0:4))
        write.table(df,file=pp(pevi.shared,'data/inputs/charger-input-file/delhi/final-recommendations/delhi-final-rec-pen-',pen*100,'.txt'),sep='\t',row.names=F,quote=F)
  })
}

################################################
# RANKINGS AND GOOGLE MAP
################################################
load(pp(pevi.shared,'data/DELHI/POLYGON/AggregatedTAZsCleaned.Rdata'))
agg.coords <- coordinates(agg.taz)
##write.csv(agg.taz@data,pp(pevi.shared,'data/DELHI/results/maps/agg-taz-data.csv'))
agg.taz.data <- read.csv(pp(pevi.shared,'data/DELHI/results/maps/google-map-data/agg-taz-groupings-and-existing.csv'))

plur.station <- function(num,rank){
  res <- ifelse(num>1,'s','')
  res <- ifelse(is.na(rank),res,pp(res,' (Priority: ',ifelse(rank==1,'first',ifelse(rank==2,'second',ifelse(rank==3,'third','fourth'))),')'))
  res
}

round.to.five <- function(x){
  round(x/5)*5
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
clust <- kmeans(agg.taz@data[,c('x','y')],4)
cents <- data.table(clust$centers)
setkey(cents,'x','y')
dirs <- c('SW'=which.min(cents[1:2,y]),'NW'=which.max(cents[1:2,y]),'SE'=2 + which.min(cents[3:4,y]),'NE' = 2 + which.max(cents[1:2,y]))

agg.taz$med.zoom.group <- pp(names(dirs)[clust$cluster],' Delhi')

for(pen in c(0.005,0.01,0.02)){
  for(scen in scenarios){
    win <- data.table(subset(winners,scenario==scen),key=c('seed','penetration','iteration'))
    if(nrow(win)==0)next
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
      if(df$level[1]=='L1'){
        data.frame(num.chargers = round.to.five(mean(df$value,na.rm=TRUE)))
      }else{
        data.frame(num.chargers = round(mean(df$value,na.rm=TRUE)))
      }
    })

    setkey(best.ranks,'taz','level')
    for(lev in level.nums){
      lev.scenario <- subset(mean.ch.fin,penetration==pen & level==pp('L',lev),select=c('TAZ','num.chargers'))
      agg.taz@data[,pp('L',lev)] <- lev.scenario$num.chargers[match(agg.taz$agg.id,lev.scenario$TAZ)]
      agg.taz@data[is.na(agg.taz@data[,pp('L',lev)]),pp('L',lev)] <- 0
      agg.taz@data[,pp('L',lev,'.rank')] <- best.ranks[J(agg.taz.data$agg.id,lev),quartile]$quartile
    }

    # custom tweaks
    if(pen==0.01 & scen=="half-homeless"){
      agg.taz$L3[agg.taz$agg.id==8] <- 1
      agg.taz$L3.rank[agg.taz$agg.id==8] <- 4
    }

    agg.taz.data.for.join <- data.table(melt(agg.taz@data,id.vars=c('agg.id'),measure.vars=pp('L',level.nums)))
    agg.taz.data.for.join[,taz:=agg.id]
    agg.taz.data.for.join[,level:=as.numeric(substr(variable,2,2))]
    setkey(agg.taz.data.for.join,'taz','level')
    setkey(best.ranks,'taz','level')
    best.ranks <- agg.taz.data.for.join[best.ranks]
    setkey(best.ranks,'rank')
    write.csv(best.ranks,pp(pevi.shared,'data/DELHI/results/charger-priorities/charger-priorities-',scen,'-pen',pen*100,'.csv'))

    # prep info for separating the icons
    angs <- head(seq(0,2*pi,length.out=num.levels+1),-1)
    sins <- -sin(angs)
    coss <- cos(angs)

    # For the charger files that wll get translated nto Google maps, we use mean.ch.fin
    # Each charger is defined by an array: 
    # [ NAME, LONG, LAT, DESCRIP, # CHARGERS, APPEAR ABOVE ZOOM, APPEAR BELOW ZOOM, EXISTING CHARGER?, CHARGER LEVEL ]
    charger.data.file <- pp(pevi.shared,'data/DELHI/results/maps/google-map-data/charger-data-',scen,'-pen',pen*100,'.yaml')
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
} # end for each pen

