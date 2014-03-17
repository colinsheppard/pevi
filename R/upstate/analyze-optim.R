library(colinmisc)
Sys.setenv(NOAWT=1)
load.libraries(c('ggplot2','yaml','stringr','RNetLogo','maptools','reshape','colorRamps','caTools'))

#old.obj <- c('L2-10k','L2-12.5k','base','L2-20k','L3-30kW','no-L2','no-L3','no-phev-crit','opp-cost-10')
#old.obj <- c('base','no-L2','no-L3')
old.obj <- c()
new.obj <- c('new-obj','L2-10k','L2-20k','opp-cost-10','opp-cost-20')

chs <- data.frame()
opts <- data.frame()
for(optim.code in old.obj){
  for(seed in c(1:10)){
    hist.file <- pp(pevi.shared,'data/outputs/optim-new/old-obj/',optim.code,'-seed',seed,'/charger-buildout-history.Rdata')
    final.evse.file <- ifelse(optim.code=='no-L2' | optim.code=='no-L3',pp(pevi.shared,'data/outputs/optim-new/old-obj/',optim.code,'-seed',seed,'/',optim.code,'-seed',seed,'-pen0.5-final-infrastructure.txt'),pp(pevi.shared,'data/outputs/optim-new/old-obj/',optim.code,'-seed',seed,'/',optim.code,'-seed',seed,'-pen2-final-infrastructure.txt'))
    if(file.exists(final.evse.file)){
      load(hist.file)
      charger.buildout.history$scenario <- optim.code
      charger.buildout.history$seed     <- seed
      charger.buildout.history$obj      <- 'old'
      chs <- rbind(chs,charger.buildout.history)
      load(pp(pevi.shared,'data/outputs/optim-new/old-obj/',optim.code,'-seed',seed,'/optimization-history.Rdata'))
      opt.history$scenario <- optim.code
      opt.history$seed     <- seed
      opt.history$obj.type      <- 'old'
      opt.history$mean.delay.cost <- NA
      opt.history$mean.charger.cost <- NA
      opts <- rbind(opts,opt.history)
    }
  }
}
for(optim.code in new.obj){
  for(seed in c(1:10)){
    hist.file <- pp(pevi.shared,'data/outputs/optim-new/',optim.code,'-seed',seed,'/charger-buildout-history.Rdata')
    final.evse.file <- pp(pevi.shared,'data/outputs/optim-new/',optim.code,'-seed',seed,'/',optim.code,'-seed',seed,'-pen2-final-infrastructure.txt')
    if(file.exists(final.evse.file)){
      load(hist.file)
      charger.buildout.history$scenario <- optim.code
      charger.buildout.history$seed     <- seed
      charger.buildout.history$obj      <- 'new'
      chs <- rbind(chs,charger.buildout.history)
      load(pp(pevi.shared,'data/outputs/optim-new/',optim.code,'-seed',seed,'/optimization-history.Rdata'))
      opt.history$scenario <- optim.code
      opt.history$seed     <- seed
      opt.history$obj.type      <- 'new'
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
winners$cost <- c(15,75)[match(winners$level,c(2,3))]
winners$cost[winners$level==2 & winners$scenario=="L2-10k"] <- 10
winners$cost[winners$level==2 & winners$scenario=="L2-12.5k"] <- 12.5
winners$cost[winners$level==2 & winners$scenario=="L2-20k"] <- 20
winners$cost[winners$level==2 & winners$scenario=="L2-30kW"] <- 10
winners <- ddply(winners,.(scenario,seed),function(df){
  df <- as.data.frame(data.table(df,key=c('penetration','iteration')))
  data.frame(df,cum.cost=cumsum(df$cost))
})
# for old OBJ
old.inds <- winners$obj.type == 'old'
winners$delay[old.inds] <- winners$obj[old.inds] - winners$cum.cost[old.inds]*1000
# for new OBJ
new.inds <- winners$obj.type == 'new'
winners$delay[new.inds] <- winners$mean.delay.cost[new.inds] 

winners$penetration <- winners$penetration*100

#ggplot(subset(winners,seed==1),aes(x=cum.cost,y=obj,colour=factor(penetration))) + geom_point() + facet_wrap(~scenario,scales='free_y') + labs(x="Infrastructure Cost",y="Objective",title="")
winners$scenario <- factor(winners$scenario,levels=c('new-obj','L2-10k','L2-20k','opp-cost-10','opp-cost-20'))
winners$scenario <- revalue(winners$scenario,c('new-obj'='Base','L2-10k'='L2 Cost $10k','L2-20k'='L2 Cost $20k','opp-cost-10'='Opportunity Cost $10/hr','opp-cost-20'='Opportunity Cost $20/hr'))
ggplot(subset(winners,(scenario=="Base" & seed==4) | (scenario=="L2 Cost $10k" & seed==3) | (scenario=="L2 Cost $20k" & seed==1) | (scenario=="Opportunity Cost $10/hr" & seed==1)  | (scenario=="Opportunity Cost $20/hr" & seed==1)),aes(x=cum.cost/1e3,y=delay/1e6,colour=factor(scenario))) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",colour="Scenario")+facet_wrap(~penetration)
ggplot(subset(winners,(scenario=="Base" & seed==4)),aes(x=cum.cost/1e3,y=delay/1e6,colour=factor(penetration))) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",colour="Penetration (%)")

# can we blend the various seeds together?
winners <- data.table(winners,key=c('penetration','scenario','iteration'))
winners.blended <- winners[,list(cost=mean(cum.cost)/1e3,delay=mean(delay)/1e6),by=c('penetration','scenario','iteration')]
ggplot(winners.blended,aes(x=cost,y=delay,colour=factor(scenario))) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",colour="Scenario")+facet_wrap(~penetration)

# compare several optimization runs at once
ch.fin <- subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',2:3))
ch.fin$scenario <- factor(ch.fin$scenario,levels=c('base','no-L2','no-L3','new-obj','L2-by-two','L2-10k','L2-20k','opp-cost-10','opp-cost-20'))
stat_sum_single <- function(fun, geom="point", ...) {
  stat_summary(fun.y=fun, geom=geom, size = 3, ...)
}
ch.fin$scenario <- factor(ch.fin$scenario,levels=c('new-obj','L2-10k','L2-20k','opp-cost-10','opp-cost-20'))
ch.fin$scenario <- revalue(ch.fin$scenario,c('new-obj'='Base','L2-10k'='L2 Cost $10k','L2-20k'='L2 Cost $20k','opp-cost-10'='Opportunity Cost $10/hr','opp-cost-20'='Opportunity Cost $20/hr'))
ggplot(ddply(ch.fin,.(scenario,level,penetration,seed),function(df) { data.frame(num.chargers=sum(df$value)) }),aes(x=scenario,y=num.chargers,colour=level)) + geom_point() + facet_wrap(~penetration) + stat_summary(fun.y=mean,geom='point',shape='X',size=3)+labs(x="Scenario",y="# Chargers",title="# Chargers Sited over Several Optimization Variations")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

# the fraction of TAZs with chargers of each type 
ggplot(ddply(ch.fin,.(level,scenario,penetration,seed),function(df){ data.frame(frac.tazs=sum(df$value>0)/nrow(df))}),aes(x=scenario,y=frac.tazs,colour=level))+geom_point()+facet_wrap(~penetration)+labs(x="Scenario",y="Charger Coverage Fraction",title="Fraction of TAZs with Chargers")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
frac.tazs <- ddply(ch.fin,.(scenario,penetration,seed),function(df){ 
  df3 <- ddply(df,.(TAZ),function(df2){
    data.frame(value=sum(df2$value))
  })
  data.frame(frac.tazs=sum(df3$value>0)/nrow(df3))
})
ggplot(frac.tazs,aes(x=scenario,y=frac.tazs))+geom_point()+facet_wrap(~penetration)+labs(x="Scenario",y="Charger Coverage Fraction",title="Fraction of TAZs with Chargers")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

# hard coded comparison of scenarios
ch.fin <- cast(subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',2:3)),penetration + TAZ + seed + level ~ scenario)
#ggplot(ch.fin,aes(x=base,y=`build-by-two`,colour=level))+geom_point(position='jitter')+facet_wrap(~penetration)+ labs(x="# Chargers (build by one)",y="# Chargers (build by two)",title="# Chargers Sited for Each TAZ & Replicate")+geom_abline(intercept=0,slope=1,color='grey')
ggplot(ch.fin,aes(x=base,y=`fewer-drivers`,colour=level))+geom_point(position='jitter')+facet_wrap(~penetration)+ labs(x="# Chargers (all driver itins)",y="# Chargers (half driver itins)",title="# Chargers Sited for Each TAZ & Replicate")+geom_abline(intercept=0,slope=1,color='grey')
#ggplot(ddply(ch.fin,.(penetration,seed,level),function(df){ data.frame(base=sum(df$base),build.by.two=sum(df$`build-by-two`)) }),aes(x=base,y=build.by.two,colour=level))+geom_point(position='jitter')+facet_wrap(~penetration)+ labs(x="# Chargers (build by one)",y="# Chargers (build by two)",title="Total Chargers Sited for Each Replicate")+geom_abline(intercept=0,slope=1,color='grey')
ggplot(ddply(ch.fin,.(penetration,seed,level),function(df){ data.frame(base=sum(df$base),build.by.two=sum(df$`fewer-drivers`)) }),aes(x=base,y=build.by.two,colour=level))+geom_point(position='jitter')+facet_wrap(~penetration)+ labs(x="# Chargers (all driver itins)",y="# Chargers (half driver itins)",title="Total Chargers Sited for Each Replicate")+geom_abline(intercept=0,slope=1,color='grey')
# based on the following, I realized that in most cases, the issue was that the TAZ had a L3 charger and therefore didn't need any L2's
strange.tazs <- subset(ch.fin,`build-by-two`>1 & penetration==0.01 & seed==1 & base==0)$TAZ
subset(ch.fin.unshaped,penetration==0.01 & seed==1 & TAZ %in% strange.tazs)[order(subset(ch.fin.unshaped,penetration==0.01 & seed==1 & TAZ %in% strange.tazs)$TAZ),]


load(pp(pevi.shared,"data/outputs/optim-new/new-obj-seed1/build-result-history.Rdata")) #"base-detailed" not in pevi.shared
build.result.history <- ddply(build.result.history,.(penetration,iteration),function(df){ data.frame(df,obj.norm=df$obj/mean(df$obj))})
build.result.history.mean <- ddply(build.result.history,.(rep,level),function(df){ data.frame(obj.norm.mean=mean(df$obj.norm))})
build.result.history.mean <- build.result.history.mean[order(build.result.history.mean$obj.norm.mean),]

ggplot(build.result.history,aes(x=factor(rep),y=obj.norm))+geom_boxplot() + facet_wrap(~level) + stat_summary(fun.data = "mean_cl_boot", colour = "red")
ggplot(build.result.history,aes(x=obj.norm))+geom_histogram()
ggplot(build.result.history.mean,aes(x=factor(rep),y=obj.norm.mean,colour=factor(level)))+geom_point()

build.result.history.mean <- ddply(build.result.history,.(rep),function(df){ data.frame(obj.norm.mean=mean(df$obj.norm))})
build.result.history.mean$rep[build.result.history.mean$obj.norm.mean > quantile(build.result.history.mean$obj.norm.mean,c(0.25)) & build.result.history.mean$obj.norm.mean < quantile(build.result.history.mean$obj.norm.mean,c(0.75))]

########################
# GOOGLE MAP
########################

# export the TAZs for the base layer
shp.to.kml(agg.taz,pp(pevi.shared,'data/UPSTATE/results/maps/tazs.kml'),'Shasta TAZs','',borders="black",colors='#FFFFFF33',id.col="shp.id",name.col="name")


load(pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))
load(pp(pevi.shared,'data/UPSTATE/od-converter.Rdata'))
agg.taz$new.id <- od.converter$new.id[match(agg.taz$agg.id,od.converter$old.id)]
agg.coords <- coordinates(agg.taz)
#write.csv(agg.taz@data,pp(pevi.shared,'data/UPSTATE/results/maps/agg-taz-data.csv'))
agg.taz.data <- read.csv(pp(pevi.shared,'data/UPSTATE/results/maps/agg-taz-data.csv'))
agg.taz.data <- agg.taz.data[match(agg.taz$agg.id,agg.taz.data$agg.id),]
agg.taz.data$x <- agg.coords[,1]
agg.taz.data$y <- agg.coords[,2]

# Now to calculate some average buildouts. First, let's average the final buildout between all seeds.
# The resulting data frame should have for each row a unique penetration/taz/level combination, and the
# remaining columns will be the total for each scenario.

ch.fin <- subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',2:3))
mean.ch.fin <- ddply(ch.fin,.(penetration,TAZ,level,scenario),function(df){
	data.frame(num.chargers = round(mean(df$value,na.rm=TRUE)))
})

#l2.scenario <- subset(ch.fin,penetration==0.02 & seed==4 & level=='L2',select=c('penetration','TAZ','seed','level','new-obj'))
l2.scenario <- subset(mean.ch.fin,penetration==0.02 & level=='L2' & scenario=='new-obj',select=c('TAZ','num.chargers'))
agg.taz$L2 <- l2.scenario$num.chargers[match(agg.taz$new.id,l2.scenario$TAZ)]
#l3.scenario <- subset(ch.fin,penetration==0.02 & seed==4 & level=='L3',select=c('penetration','TAZ','seed','level','new-obj'))
l3.scenario <- subset(mean.ch.fin,penetration==0.02 & level=='L3' & scenario=='new-obj',select=c('TAZ','num.chargers'))
agg.taz$L3 <- l3.scenario$num.chargers[match(agg.taz$new.id,l3.scenario$TAZ)]

# For the charger files that wll get translated nto Google maps, we use mean.ch.fin
# Each charger is defined by an array: 
# [ NAME, LONG, LAT, DESCRIP, # CHARGERS, APPEAR ABOVE ZOOM, APPEAR BELOW ZOOM, EXISTING CHARGER?, L3 CHARGER? ]
charger.data.file <- pp(pevi.shared,'data/UPSTATE/results/maps/charger-data.yaml')
cat('chargers:\n',file=charger.data.file)

# start with the high zoom level tazs
for(i in 1:nrow(agg.taz@data)){
  appear.above.zoom <- ifelse(length(grep("_",agg.taz$name[i]))>0,'10','null')
  if(agg.taz$L2[i] > 0) cat(sprintf("  - ['%s',%f,%f,'<b>%s</b> <br/>%d proposed charging stations',%d,%s,%d,%s,%s]\n",agg.taz$name[i],agg.coords[i,1]-0.01,agg.coords[i,2],agg.taz$name[i],agg.taz$L2[i],agg.taz$L2[i],appear.above.zoom,15,'false','false'),file=charger.data.file,append=T)
  if(agg.taz$L3[i] > 0) cat(sprintf("  - ['%s',%f,%f,'<b>%s</b> <br/>%d proposed charging stations',%d,%s,%d,%s,%s]\n",agg.taz$name[i],agg.coords[i,1]+0.01,agg.coords[i,2],agg.taz$name[i],agg.taz$L3[i],agg.taz$L3[i],appear.above.zoom,15,'false','true'),file=charger.data.file,append=T)
}
# now write medium zooms
d_ply(agg.taz.data,.(med.zoom.group),function(df){
  lon <- mean(df$x)
  lat <- mean(df$y)
  l2 <- sum(df$L2)
  l3 <- sum(df$L3)
  if(agg.taz$L2[i] > 0) cat(sprintf("  - ['%s',%f,%f,'<b>%s</b> <br/>%d proposed charging stations',%d,%d,%d,%s,%s]\n",df$med.zoom.group[1],lon-0.01,lat,df$med.zoom.group[1],l2,l2,7,12,'false','false'),file=charger.data.file,append=T)
  if(agg.taz$L3[i] > 0) cat(sprintf("  - ['%s',%f,%f,'<b>%s</b> <br/>%d proposed charging stations',%d,%d,%d,%s,%s]\n",df$med.zoom.group[1],lon+0.01,lat,df$med.zoom.group[1],l3,l3,7,12,'false','true'),file=charger.data.file,append=T)
})
# low zoom
d_ply(agg.taz.data,.(low.zoom.group),function(df){
  lon <- mean(df$x)
  lat <- mean(df$y)
  l2 <- sum(df$L2)
  l3 <- sum(df$L3)
  if(agg.taz$L2[i] > 0) cat(sprintf("  - ['%s',%f,%f,'<b>%s</b> <br/>%d proposed charging stations',%d,%s,%d,%s,%s]\n",df$med.zoom.group[1],lon-0.01,lat,df$med.zoom.group[1],l2,l2,'null',7,'false','false'),file=charger.data.file,append=T)
  if(agg.taz$L3[i] > 0) cat(sprintf("  - ['%s',%f,%f,'<b>%s</b> <br/>%d proposed charging stations',%d,%s,%d,%s,%s]\n",df$med.zoom.group[1],lon+0.01,lat,df$med.zoom.group[1],l3,l3,'null',7,'false','true'),file=charger.data.file,append=T)
})
  
  


#path.to.google <- paste(base.path,'pev-shared/data/google-earth/',sep='')
#path.to.geatm <- paste(base.path,'pev-shared/data/GEATM-2020/',sep='')
#hard.code.coords <- read.csv(paste(path.to.google,"hard-coded-coords.csv",sep=''),stringsAsFactors=F)
#source(paste(pevi.home,"R/optim/buildout-functions.R",sep='')) 

# load aggregated tazs
#agg.taz <- readShapePoly(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights',sep=''),IDvar="ID")
#load(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights-fieldnames.Rdata',sep=''))
#names(agg.taz@data) <- c("SP_ID",taz.shp.fieldnames)

# load dist times
dist <- read.csv(file=paste(path.to.geatm,'taz-dist-time.csv',sep=''))
names(dist)[1] <- "from"

    #Prepares a null matrix to fill with values. MAKE SURE N.SEEDS IS ACTUALLY SET!
   	
write.csv(compare,file=paste(path.to.outputs.base,'/../analysis/compare-optim-scenarios.csv',sep=''))

# Load data already compiled
path.to.outputs.base <- paste(base.path,'pev-shared/data/outputs/sensitivity/',optim.codes[1],sep='')
compare <- read.csv(file=paste(path.to.outputs.base,'/../analysis/compare-optim-scenarios.csv',sep=''))


# Plotting Comparisons

compare$level <- factor(compare$level)
compare$scenario.named <- NA
compare$scenario.order <- NA

for(name.i in names(naming$`optim-code`)){
  compare$scenario.named[compare$scenario==name.i] <- naming$`optim-code`[[name.i]][[1]]
  compare$scenario.order[compare$scenario==name.i] <- naming$`optim-code`[[name.i]][[2]]
}
compare$scenario.named <- reorder(factor(compare$scenario.named),compare$scenario.order)
for(name.i in names(naming$tazs)){
  compare$name.order[compare$name==name.i] <- naming$tazs[[name.i]][[2]]
}
compare$name <- reorder(factor(compare$name),-compare$name.order)

# Summary of total L2/L3 chargers in 2% by scenario
ggplot(ddply(subset(compare,pen==.02),.(scenario.named,level),summarise,num.chargers=sum(mean)),aes(x=scenario.named,y=num.chargers,fill=level))+geom_bar(stat="identity",position="dodge")

# Compare spatial distributions
scen.combs <- combs(as.character(unique(compare$scenario)),2)

pdf(file=paste(path.to.outputs.base,'/../analysis/compare-tazs.pdf',sep=''),width=8,height=11)
for(scen.i in 1:nrow(scen.combs)){
  scen.a.code <- scen.combs[scen.i,1]
  scen.b.code <- scen.combs[scen.i,2]
  scen.a.name <- naming$`optim-code`[[scen.a.code]][[1]]
  scen.b.name <- naming$`optim-code`[[scen.b.code]][[1]]
  scen.a <- subset(compare,scenario==scen.a.code & pen==0.02)
  scen.b <- subset(compare,scenario==scen.b.code & pen==0.02)

  scen.a$b.named <- scen.b$scenario.named
  scen.a$diff <- scen.b$mean - scen.a$mean
  scen.a$diff.rank <- scen.a$mean.rank - scen.b$mean.rank

  p <- ggplot(scen.a,aes(x=name,y=diff,fill=level))+geom_bar(position="dodge",stat='identity')+coord_flip()+labs(title=paste("(",scen.b.name,") - (",scen.a.name,")",sep=''),y="Difference in Chargers",x="TAZ")
  print(p)
}
dev.off()
