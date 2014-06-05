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

num.ch.to.cap <- function(df){ 
  df$capacity <- df$num.chargers * c(1.5,6.6,50,400)[as.numeric(substr(df$level,2,2))]
  df
}
num.ch.to.cost <- function(df){ 
  scen <- df$scenario[1]
  df$installed.cost <- df$num.chargers * charger.types[[scen]]$installed.cost[match(df$level,pp('L',charger.types[[scen]]$level))] / 1e3
  df
}

# unfortunately, results from res-none are worthless
#scenarios <- c('base','res-none','homeless','half-homeless')
#scenarios <- c('homeless','half-homeless','no-homeless')
scenarios <- c('homeless','half-homeless','no-homeless','swap','veh-high','veh-low','opp-cost-high',pp('cheap-l',1:4),'congested')
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
  source(pp(pevi.shared,'data/inputs/optim-new/delhi-',optim.code,'/params.R'))
  build.increments[[optim.code]] <- build.increment
  param.file <- pp(pevi.shared,'data/inputs/optim-new/delhi-',optim.code,'/params.txt')
  param.file.data <- read.table(param.file,sep='\t')
  param.file.data <- streval(pp('data.frame(',pp(apply(param.file.data,1,function(x){ pp(str_replace_all(x[1],'-','.'),'=',ifelse(length(grep('file|directory',x[1]))>0,pp('"',x[2],'"'),ifelse(x[2]%in%c('true','false'),x[2]=='true',x[2]))) }),collapse=","),',stringsAsFactors=F)'))
  charger.data <- read.table(pp(pevi.shared,param.file.data$charger.type.input.file),header=TRUE)
  names(charger.data) <- c('level',tail(names(charger.data),-1))
  charger.types[[optim.code]] <- charger.data
  for(seed in c(52:62)){
  #for(seed in c(30:39)){
    if(seed < 30 & optim.code != 'swap')next
    hist.file <- pp(pevi.shared,'data/outputs/optim-new/delhi-',optim.code,'-seed',seed,'/charger-buildout-history.Rdata')
    final.evse.file <- pp(pevi.shared,'data/outputs/optim-new/delhi-',optim.code,'-seed',seed,'/delhi-',optim.code,'-seed',seed,'-pen2-final-infrastructure.txt')
    if(file.exists(hist.file)){
      load(hist.file)
      charger.buildout.history$scenario <- optim.code
      charger.buildout.history$seed     <- seed
      charger.buildout.history$obj      <- 'new'
      chs <- rbind(chs,charger.buildout.history)
      load(pp(pevi.shared,'data/outputs/optim-new/delhi-',optim.code,'-seed',seed,'/optimization-history.Rdata'))
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

load(file=pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/mean-delay-and-strand.Rdata'))
baseline.delay$scen[baseline.delay$scen=="All High"] <- "veh-high"
baseline.delay$scen[baseline.delay$scen=="All Low"] <- "veh-low"
baseline.delay <- subset(baseline.delay,!scen %in% c('veh-high','veh-low','no-homeless','homeless'))
baseline.delay$scen[baseline.delay$scen=="opp-cost"] <- "opp-cost-high"
baseline.delay$scen[baseline.delay$scen=="base"] <- "half-homeless"
baseline.delay$scen[baseline.delay$scen=="congested"] <- "congested"
baseline.delay$penetration <- baseline.delay$penetration/100
baseline.delay$key <- pp(baseline.delay$scen,"-",baseline.delay$penetration)
opts <- opts.raw
opts$key <- pp(opts$scenario,"-",opts$penetration)
for(key in baseline.delay$key){
  inds <- which(key == opts$key)
  opts$mean.delay.cost[inds] <- opts$mean.delay.cost[inds] - baseline.delay$min.delay.cost[key == baseline.delay$key]
}
inds <- which(! opts$key %in% baseline.delay$key)
opts$mean.delay.cost[inds] <- opts$mean.delay.cost[inds] - baseline.delay$min.delay.cost['half-homeless-0.01' == baseline.delay$key]

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
  if(df$seed[1]>=40){
    df$cum.cost <- dt[,list(cum.cost=cumsum(cost)),by='penetration']$cum.cost
  }else{
    df$cum.cost <- cumsum(df$cost)
  }
  df
})
winners$delay <- winners$mean.delay.cost
winners <- ddply(winners,.(scenario,seed,penetration),function(df){
  data.frame(df,marginal.cost=c(NA,abs(diff(df$cum.cost*1e3) / diff(df$delay))))
})

# for seeds >= 40 we need to find the right stopping point and get rid of the extraneous results
winners.tracking <- ddply(winners,.(scenario,penetration,seed),function(df){
  df$above.thresh <- F
  if(df$seed[1] >= 40){
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
  }
  df
})
chs <- chs.raw
winners <- ddply(winners,.(scenario,penetration,seed),function(df){
  df$above.thresh <- F
  if(df$seed[1] >= 40){
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
  }
  df
})
winners <- subset(winners,seed<40 | !above.thresh)

ch.fin <- ddply(subset(chs,TAZ>0),.(scenario,seed,penetration),function(df){
  subset(df,iter==max(iter))
})
ch.fin.unshaped <- ch.fin

# some custom deletions
#winners <- subset(winners,!(scenario=='half-homeless' & seed==43 & penetration==0.02))
#ch.fin.unshaped <- subset(ch.fin.unshaped,!(scenario=='half-homeless' & seed==43 & penetration==0.02))

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
}

################################################
# PLOTTING
################################################
if(F){
  make.dir(pp(pevi.shared,'data/DELHI/results/base'))
  make.dir(pp(pevi.shared,'data/DELHI/results/vehicle-composition'))
  make.dir(pp(pevi.shared,'data/DELHI/results/residential-capacity'))
  make.dir(pp(pevi.shared,'data/DELHI/results/battery-swapping'))
  make.dir(pp(pevi.shared,'data/DELHI/results/opportunity-cost'))
  make.dir(pp(pevi.shared,'data/DELHI/results/cheap-chargers'))
  make.dir(pp(pevi.shared,'data/DELHI/results/congestion'))

  
  # What are my sample sizes?
  write.csv(ddply(winners,.(scenario),function(df){ data.frame(num.seeds=length(unique(df$seed)))}),file=pp(pevi.shared,'data/DELHI/results/sample-size.csv'))

  ###############
  # Assess state of runs
  ###############
  winners.tracking$scenario.named <- refactor.scen(winners.tracking$scenario)
  winners.tracking$penetration.named <- pp(winners.tracking$penetration*100,"%")
  p <- ggplot(subset(winners.tracking,T),aes(x=cum.cost/1e3,y=delay/1e6,colour=above.thresh>-10)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",color="Scenario",shape="Scenario")+facet_wrap(scenario~seed~penetration.named) + scale_color_manual(values=charger.cols)

  ###############
  # Base Scenario
  ###############
  winners$scenario.named <- refactor.scen(winners$scenario)
  winners$penetration.named <- pp(winners$penetration*100,"%")
  scen.to.plot <- subset(winners,seed==73 & scenario%in%c('half-homeless'))
  p <- ggplot(scen.to.plot,aes(x=cum.cost/1e3,y=delay/1e6,colour=factor(penetration.named))) + geom_point() + labs(x="Infrastructure Investment ($M)",y="PV of Driver Delay ($M)",title="",color="Fleet Penetration") + scale_color_manual(values=charger.cols) + scale_y_continuous(limits=c(0,max(scen.to.plot$delay/1e6)))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/base/base-optimality-curves.pdf'),p,width=10,height=6)

  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,scenario %in% c('half-homeless')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3)),.(scenario,level,penetration,seed),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cost)
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cap)
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration <- pp(ch.fin$penetration*100,'%')
  ch.fin$scenario.named <- refactor.scen(ch.fin$scenario,base='50% Home Chargers')
#  p <- ggplot(ch.fin,aes(x=penetration,y=capacity,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Capacity of Installed Chargers (kW)",title="Capacity of Chargers for Base Scenario",fill="Charger Level") +  theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))  
#  ggsave(file=pp(pevi.home,'../plots/delhi-analysis/base/base-charger-capacity.pdf'),p,width=6,height=6)
#  p <- ggplot(ch.fin,aes(x=penetration,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="Chargers Sited for Base Scenario",fill="Charger Level") +  theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
#  ggsave(file=pp(pevi.home,'../plots/delhi-analysis/base/base-num-chargers.pdf'),p,width=6,height=6)
#  p <- ggplot(ch.fin,aes(x=penetration,y=installed.cost,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Cost of Installed Chargers  ($M)",title="Charger Cost for Base Scenario",fill="Charger Level") + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
#  ggsave(file=pp(pevi.home,'../plots/delhi-analysis/base/base-charger-cost.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=penetration,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers",title="Number of Chargers",fill="Charger Level") +  theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/base/base-num-chargers.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=penetration,y=capacity,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Capacity (kW)",title="Capacity of Chargers",fill="Charger Level") +  theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/base/base-charger-capacity.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=penetration,y=installed.cost,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Cost ($M)",title="Cost of Chargers",fill="Charger Level") + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/base/base-charger-cost.pdf'),p,width=6,height=6)
  ch.fin$pen <- as.numeric(substr(ch.fin$penetration,1,nchar(ch.fin$penetration)-1))
  ch.fin.per.cap <- ddply(ch.fin,.(pen),function(df){
    data.frame(per.cap=sum(df$installed.cost)*1e6/(df$pen[1]*10e3))
  })
  p <- ggplot(ch.fin.per.cap,aes(x=pen,y=per.cap)) +  geom_point(colour=charger.cols[2]) + geom_line(colour=charger.cols[2]) + labs(x="Fleet Penetration (%)",y="Investment per Electric Vehicle ($)",title="Total Public Infrastructure Investment per Electric Vehicle") + scale_x_continuous(limits=c(0,max(ch.fin.per.cap$pen)*1.05)) + scale_y_continuous(limits=c(0,max(ch.fin.per.cap$per.cap)*1.05))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/base/base-per-cap-cost.pdf'),p,width=6,height=6)

  # hours of delay per 1$M of total delay cost is 232.4047

  ###############
  # Vehicle Type
  ###############
  winners$scenario.named <- refactor.scen(winners$scenario,base='Low/Med/High Capacity Vehicles')
  winners$penetration.named <- pp(winners$penetration*100,"%")
  p <- ggplot(subset(winners,scenario%in%c('veh-high','veh-low','half-homeless') & penetration==.01),aes(x=cum.cost/1e3,y=delay/1e6,shape=scenario.named,colour=scenario.named)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",color="Scenario",shape="Scenario") + scale_color_manual(values=charger.cols)+theme(legend.position=c(0,1), legend.justification = c(-1, 1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/vehicle-composition/vehicle-type-optimality-curves.pdf'),p,width=10,height=6)

  winners <- ddply(winners,.(scenario,penetration,seed),function(df){
    df$delay.frac <- df$delay/max(df$delay)
    df
  })
  p <- ggplot(subset(winners,scenario%in%c('veh-high','veh-low','half-homeless') & penetration==.01),aes(x=cum.cost/1e3,y=delay.frac,shape=scenario.named,colour=scenario.named)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",color="Scenario",shape="Scenario") + scale_color_manual(values=charger.cols)+theme(legend.position=c(0,1), legend.justification = c(-5, 1))

  equi.delay <- 20e6 # horizontal transect of the above plot where we will make comparable num.charger plots
  win.sub <- ddply(winners,.(scenario,penetration,seed),function(df){
    df <- df[order(df$iteratio),]
    df$tmp <- df$num.added
    df$tmp[df$level!=1] <- 0
    df$cum.l1 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=2] <- 0
    df$cum.l2 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=3] <- 0
    df$cum.l3 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=4] <- 0
    df$cum.l4 <- cumsum(df$tmp)
    df
  })
  win.sub.m <- melt(win.sub,id.vars=c('scenario','penetration','seed','cum.cost','delay','scenario.named','penetration.named'),variable_name='level.str',measure.vars=c('cum.l1','cum.l2','cum.l3','cum.l4'))
  win.sub.m$level <- (1:4)[match(win.sub.m$level.str,pp('cum.l',1:4))]
  num.ch.at.transects <- ddply(subset(win.sub.m,abs(delay-equi.delay)<1e6),.(scenario,penetration,level),function(df){
    data.frame(num.chargers=round(mean(df$value)),tot.cost=mean(df$cum.cost),delay.fin=mean(df$delay),scenario.named=df$scenario.named[1],penetration.named=df$penetration.named[1])
  })
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario,level),function(df){
    df <- df[order(df$penetration),]
    df$num.chargers <- cumsum(df$num.chargers)
    df
  })
  num.ch.at.transects$level.named <- refactor.lev(num.ch.at.transects$level)
  num.ch.at.transects$level <- pp('L',num.ch.at.transects$level)
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario),num.ch.to.cost)
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario),num.ch.to.cap)
  p <- ggplot(subset(num.ch.at.transects,penetration==0.01 & level!="L4" & scenario%in%c('veh-high','veh-low','half-homeless')),aes(x=factor(scenario.named),y=num.chargers,fill=level.named)) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers",title=pp("Number of Chargers to Reduce PV of Delay to $",equi.delay/1e6,"M at 1% Penetration"),fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/vehicle-composition/vehicle-type-num-chargers-at-',equi.delay/1e6,'M-delay.pdf'),p,width=6,height=6)
  p <- ggplot(subset(num.ch.at.transects,penetration==0.01 & level!="L4" & scenario%in%c('veh-high','veh-low','half-homeless')),aes(x=factor(scenario.named),y=installed.cost,fill=level.named)) + geom_bar(stat='identity') + labs(x="",y="Cost ($M)",title="Cost of Chargers",fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/vehicle-composition/vehicle-type-charger-cost-at-',equi.delay/1e6,'M-delay.pdf'),p,width=6,height=6)
  p <- ggplot(subset(num.ch.at.transects,penetration==0.01 & level!="L4" & scenario%in%c('veh-high','veh-low','half-homeless')),aes(x=factor(scenario.named),y=capacity,fill=level.named)) + geom_bar(stat='identity') + labs(x="",y="Capacity (kW)",title="Capacity of Chargers",fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/vehicle-composition/vehicle-type-charger-capacity-at-',equi.delay/1e6,'M-delay.pdf'),p,width=6,height=6)

  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,scenario %in% c('veh-high','veh-low','half-homeless')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3)),.(scenario,level,penetration,seed),function(df) { data.frame(num.chargers=sum(df$value)) })
  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,scenario %in% c('veh-high','veh-low','half-homeless')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3)),.(scenario,level,penetration),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cost)
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cap)
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration <- pp(ch.fin$penetration*100,'%')
  ch.fin$scenario.named <- refactor.scen(ch.fin$scenario,base='50% Home Chargers')
  p <- ggplot(subset(ch.fin,penetration=="1%"),aes(x=scenario.named,y=capacity,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Charging Capacity (kW)",title="Charging Capacity for Three Vehicle Composition Scenarios",fill="Charger Level") + facet_wrap(~penetration) + theme(axis.text.x = element_text(angle = 35, hjust = 1,colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/vehicle-composition/vehicle-type-charger-capacity.pdf'),p,width=6,height=6)
  p <- ggplot(subset(ch.fin,penetration=="1%"),aes(x=scenario.named,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="Chargers Sited for Three Vehicle Composition Scenarios",fill="Charger Level") + facet_wrap(~penetration) + theme(axis.text.x = element_text(angle = 35, hjust = 1,colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/vehicle-composition/vehicle-type-num-chargers.pdf'),p,width=6,height=6)
  p <- ggplot(subset(ch.fin,penetration=="1%"),aes(x=scenario.named,y=installed.cost,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Cost of Installed Chargers  ($M)",title="Charger Cost for Three Vehicle Composition Scenarios",fill="Charger Level") + facet_wrap(~penetration) + theme(axis.text.x = element_text(angle = 35, hjust = 1,colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/vehicle-composition/vehicle-type-charger-cost.pdf'),p,width=6,height=6)

  ######################
  # Residential Capacity
  ######################
  winners$scenario.named <- refactor.scen(winners$scenario,base='50% Home Chargers')
  winners$penetration.named <- pp(winners$penetration*100,"%")
  p <- ggplot(subset(winners,penetration==.01 & scenario%in%c('homeless','no-homeless','half-homeless')),aes(x=cum.cost/1e3,y=delay/1e6,shape=scenario.named,colour=scenario.named)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",shape="Scenario",color="Scenario")+facet_wrap(~penetration.named) + scale_color_manual(values=charger.cols)+theme(legend.position=c(0,1), legend.justification = c(0, 1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/residential-capacity/residential-capacity-optim-curves.pdf'),p,width=10,height=6)

  equi.delay <- 5e6 # horizontal transect of the above plot where we will make comparable num.charger plots
  win.sub <- ddply(subset(winners,penetration == .01 & scenario%in%c('no-homeless','half-homeless','homeless')),.(scenario,penetration,seed),function(df){
    df <- df[order(df$iteratio),]
    df$tmp <- df$num.added
    df$tmp[df$level!=1] <- 0
    df$cum.l1 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=2] <- 0
    df$cum.l2 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=3] <- 0
    df$cum.l3 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=4] <- 0
    df$cum.l4 <- cumsum(df$tmp)
    df
  })
  win.sub.m <- melt(win.sub,id.vars=c('scenario','penetration','seed','cum.cost','delay','scenario.named','penetration.named'),variable_name='level.str',measure.vars=c('cum.l1','cum.l2','cum.l3','cum.l4'))
  win.sub.m$level <- (1:4)[match(win.sub.m$level.str,pp('cum.l',1:4))]
  num.ch.at.transects <- ddply(subset(win.sub.m,abs(delay-equi.delay)<2e6),.(scenario,penetration,level),function(df){
    data.frame(num.chargers=round(mean(df$value)),tot.cost=mean(df$cum.cost),delay.fin=mean(df$delay),scenario.named=df$scenario.named[1],penetration.named=df$penetration.named[1])
  })
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario,level),function(df){
    df <- df[order(df$penetration),]
    df$num.chargers <- cumsum(df$num.chargers)
    df
  })
  num.ch.at.transects$level.named <- refactor.lev(num.ch.at.transects$level)
  num.ch.at.transects$level <- pp('L',num.ch.at.transects$level)
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario),num.ch.to.cost)
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario),num.ch.to.cap)
  p <- ggplot(subset(num.ch.at.transects,level!="L4"),aes(x=factor(scenario.named),y=num.chargers,fill=level.named)) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title=pp('Number of Chargers Sited to Reduce PV of Delay to $',equi.delay/1e6,'M at 1% Penetration'),fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/residential-capacity/residential-capacity-num-chargers-at-',equi.delay/1e6,'M-delay.pdf'),p,width=6,height=6)
  p <- ggplot(subset(num.ch.at.transects,penetration==0.01 & level!="L4"),aes(x=factor(scenario.named),y=installed.cost,fill=level.named)) + geom_bar(stat='identity') + labs(x="",y="Cost ($M)",title="Cost of Chargers",fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/residential-capacity/residential-capacity-charger-cost-at-',equi.delay/1e6,'M-delay.pdf'),p,width=6,height=6)
  p <- ggplot(subset(num.ch.at.transects,level!="L4"),aes(x=factor(scenario.named),y=capacity,fill=level.named)) + geom_bar(stat='identity') + labs(x="",y="Capacity (kW)",title="Capacity of Chargers",fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/residential-capacity/residential-capacity-charger-capacity-at-',equi.delay/1e6,'M-delay.pdf'),p,width=6,height=6)

  # how about installed capacity
  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,scenario %in% c('homeless','no-homeless','half-homeless')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3)),.(scenario,level,penetration,seed),function(df) { data.frame(num.chargers=sum(df$value)) })
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cost)
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cap)
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration <- pp(ch.fin$penetration*100,'%')
  ch.fin$scenario.named <- refactor.scen(ch.fin$scenario,base='50% Home Chargers')
  p <- ggplot(ch.fin,aes(x=scenario.named,y=num.chargers,colour=factor(level))) + geom_point() + facet_wrap(~penetration)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=capacity,colour=factor(level))) + geom_point() + facet_wrap(~penetration)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=installed.cost,colour=factor(level))) + geom_point()

  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,penetration==.01 & scenario %in% c('homeless','no-homeless','half-homeless')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3)),.(scenario,level,penetration),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cost)
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cap)
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration <- pp(ch.fin$penetration*100,'%')
  ch.fin$scenario.named <- refactor.scen(ch.fin$scenario,base='50% Home Chargers')
  p <- ggplot(ch.fin,aes(x=scenario.named,y=capacity,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Charging Capacity (kW)",title="Charging Capacity for Three Residential Capacity Scenarios",fill="Charger Level")  + theme(axis.text.x = element_text(angle = 35, hjust = 1,colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/residential-capacity/residential-capacity-charger-capacity.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="Chargers Sited for Three Residential Capacity Scenarios",fill="Charger Level") + theme(axis.text.x = element_text(angle = 35, hjust = 1,colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/residential-capacity/residential-capacity-num-chargers.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=installed.cost,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Cost of Installed Chargers  ($M)",title="Charger Cost for Three Residential Capacity Scenarios",fill="Charger Level") + theme(axis.text.x = element_text(angle = 35, hjust = 1,colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/residential-capacity/residential-capacity-charger-cost.pdf'),p,width=6,height=6)

  ######################
  # Battery Swapping
  ######################
  cbPalette <- charger.cols
  winners$scenario.named <- refactor.scen(winners$scenario)
  winners$penetration.named <- pp(winners$penetration*100,"%")
  p <- ggplot(subset(winners,scenario%in%c('swap','half-homeless')),aes(x=cum.cost/1e3,y=delay/1e6,shape=scenario.named,colour=scenario.named)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",color="Scenario",shape="Scenario")+facet_wrap(~penetration.named) + scale_color_manual(values=cbPalette[c(2,5)])+theme(legend.position=c(1,0), legend.justification = c(1,0))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/battery-swapping/swapping-optim-curves.pdf'),p,width=10,height=6)
  # marginal cost of swapping
  p <- ggplot(subset(winners,marginal.cost < 100 & scenario%in%c('swap','half-homeless')),aes(x=cum.cost/1e3,y=1/marginal.cost,shape=scenario.named,colour=scenario.named)) + geom_point(alpha=0.5) + labs(x="Infrastructure Cost ($M)",y="Marginal Return on Investment ($ saved / $ spent)",title="",color="Scenario",shape="Scenario")+facet_wrap(~penetration.named) + scale_color_manual(values=cbPalette[c(2,5)])+geom_smooth(se=F,method='loess',size=1.5)+scale_y_log10(labels=c(0.01,0.1,1,10,100,1000),breaks=c(0.01,0.1,1,10,100,1000))+ theme(panel.grid.minor = element_line(colour = NA),legend.position=c(0,1), legend.justification = c(0, 1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/battery-swapping/marginal-roi.pdf'),p,width=10,height=6)

  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,scenario %in% c('swap','half-homeless')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:4)),.(scenario,level,penetration),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cost)
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration <- pp(ch.fin$penetration*100,'%')
  ch.fin$scenario.named <- refactor.scen(ch.fin$scenario)
  cbPalette <- tail(charger.cols,-1)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="Chargers Sited with/without Battery Swapping",fill="Charger Level") + facet_wrap(~penetration) + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,0.5),"cm")) + scale_fill_manual(values=cbPalette)
  ggsave(file=pp(pevi.shared,'data/DELHI/results/battery-swapping/swapping-num-chargers.pdf'),p,width=10,height=6)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=installed.cost,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Cost ($M)",title="Cost of Chargers Sited with/without Battery Swapping",fill="Charger Level") + facet_wrap(~penetration) + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,0.5),"cm")) + scale_fill_manual(values=cbPalette)
  ggsave(file=pp(pevi.shared,'data/DELHI/results/battery-swapping/swapping-charger-cost.pdf'),p,width=10,height=6)

  seeds.in.both <- ddply(subset(winners,scenario%in%c('swap','half-homeless')),.(scenario),function(df){ data.frame(seed=unique(df$seed))})$seed
  delay.reduced <- ddply(subset(winners,scenario%in%c('swap','half-homeless') & seed %in% seeds.in.both[duplicated(seeds.in.both)]),.(penetration,seed),function(df){
    data.frame(reduced=(tail(subset(df,scenario=="half-homeless")$delay,1) - tail(subset(df,scenario=="swap")$delay,1))/1e6,cost=(tail(subset(df,scenario=="swap")$cum.cost,1) - tail(subset(df,scenario=="half-homeless")$cum.cost,1))/1e3)
  })
  delay.reduced <- ddply(delay.reduced,.(penetration),function(df){ data.frame(reduced=mean(df$reduced),cost=mean(df$cost)) })
  delay.reduced$penetration.named <- pp(delay.reduced$penetration*100,'%')
  delay.reduced <- melt(delay.reduced,id.vars='penetration.named',measure.vars=c('reduced','cost'))
  delay.reduced$variable <- revalue(delay.reduced$variable,c('reduced'='Value of Reduced Delay','cost'='Additional Cost')) 
  p <- ggplot(delay.reduced,aes(x=penetration.named,y=value,fill=variable)) + geom_bar(stat='identity',position='dodge') + labs(x="Penetration",y="Value ($M)",title="Value vs Cost of Using Battery Swap Stations to Reduce Driver Delay",fill="")+scale_fill_manual(values=charger.cols)
  ggsave(file=pp(pevi.shared,'data/DELHI/results/battery-swapping/value-vs-cost-to-reduce-delay.pdf'),p,width=10,height=6)

  equi.cost <- c(1.75,2,2)*1e3 # transects of the above plot where we will make comparable num.charger plots
  names(equi.cost) <- c('0.5%','1%','2%')
  winners$equi.cost <- equi.cost[winners$penetration.named]
  win.sub <- ddply(winners,.(scenario,penetration,seed),function(df){
    df <- df[order(df$iteratio),]
    df$tmp <- df$num.added
    df$tmp[df$level!=1] <- 0
    df$cum.l1 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=2] <- 0
    df$cum.l2 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=3] <- 0
    df$cum.l3 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=4] <- 0
    df$cum.l4 <- cumsum(df$tmp)
    df
  })
  win.sub.m <- melt(win.sub,id.vars=c('scenario','penetration','seed','cum.cost','equi.cost','delay','scenario.named','penetration.named'),variable_name='level.str',measure.vars=c('cum.l1','cum.l2','cum.l3','cum.l4'))
  win.sub.m$level <- (1:4)[match(win.sub.m$level.str,pp('cum.l',1:4))]
  num.ch.at.transects <- ddply(subset(win.sub.m,abs(cum.cost-equi.cost)<50),.(scenario,penetration,level),function(df){
    data.frame(num.chargers=mean(df$value),tot.cost=mean(df$cum.cost),delay.fin=mean(df$delay),scenario.named=df$scenario.named[1],penetration.named=df$penetration.named[1])
  })
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario,level),function(df){
    df <- df[order(df$penetration),]
    df$num.chargers <- cumsum(df$num.chargers)
    df
  })
  num.ch.at.transects$level.named <- refactor.lev(num.ch.at.transects$level)
  p <- ggplot(subset(num.ch.at.transects,penetration==0.005 & scenario%in%c('swap','half-homeless')),aes(x=factor(scenario.named),y=num.chargers,fill=level.named)) + geom_bar(stat='identity',position='dodge') + facet_wrap(~level.named,scales='free_y') + labs(x="",y="Number of Chargers Sited",title="Number of Chargers Sited for $1M at 0.5% Penetration for Three Vehicle Composition Scenarios",fill='Charger Level') + scale_fill_manual(values=cbPalette)+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/battery-swapping/num-chargers-at-1M.pdf'),p,width=10,height=6)

  #ch.fin <- ddply(subset(ch.fin.unshaped,scenario=='swap'),.(TAZ,penetration),function(df){ data.frame(num.L4=mean(df$L4)) })
  #load(pp(pevi.shared,'data/inputs/compare/delhi-battery-swap/logs.Rdata'))
  #results <- logs[['results']]
  #results$swap <- F
  #results$swap[grep('with-swap',results$infrastructure.scenario)] <- T
  #ggplot(results,aes(x=swap,y=total.delay.cost))+geom_point()+facet_wrap(~penetration)

  ######################
  # Opportunity Cost
  ######################
  winners$scenario.named <- refactor.scen(winners$scenario,base='Base')
  winners$penetration.named <- pp(winners$penetration*100,"%")
  p <- ggplot(subset(winners,penetration==.01 & scenario%in%c('congested','half-homeless')),aes(x=cum.cost/1e3,y=delay/1e6,shape=scenario.named,colour=scenario.named)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",color="Scenario",shape="Scenario")+facet_wrap(~penetration.named) + scale_color_manual(values=charger.cols)+theme(legend.position=c(0,1), legend.justification = c(0,1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/opportunity-cost/opp-cost-optim-curves.pdf'),p,width=10,height=6)
  
  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,penetration == 0.01 & scenario %in% c('opp-cost-high','half-homeless')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3)),.(scenario,level,penetration),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cost)
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cap)
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration <- pp(ch.fin$penetration*100,'%')
  ch.fin$scenario.named <- refactor.scen(ch.fin$scenario)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers",title="Number of Chargers",fill="Charger Level") + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,0.5),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/opportunity-cost/opp-cost-num-chargers.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=capacity,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Capacity (kW)",title="Capacity of Chargers",fill="Charger Level") + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,0.5),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/opportunity-cost/opp-cost-charger-capacity.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=installed.cost,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Cost ($M)",title="Cost of Chargers",fill="Charger Level") + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,0.5),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/opportunity-cost/opp-cost-charger-cost.pdf'),p,width=6,height=6)

  equi.cost <- c(0.5,1.25,2)*1e3 # transects of the above plot where we will make comparable num.charger plots
  names(equi.cost) <- c('0.5%','1%','2%')
  winners$equi.cost <- equi.cost[winners$penetration.named]
  win.sub <- ddply(subset(winners,scenario%in%c('opp-cost-high','half-homeless')),.(scenario,seed),function(df){
    #df <- df[order(df$iteration),]
    df$tmp <- df$num.added
    df$tmp[df$level!=1] <- 0
    df$cum.l1 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=2] <- 0
    df$cum.l2 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=3] <- 0
    df$cum.l3 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=4] <- 0
    df$cum.l4 <- cumsum(df$tmp)
    df
  })
  win.sub.m <- melt(win.sub,id.vars=c('scenario','penetration','seed','cum.cost','equi.cost','delay','scenario.named','penetration.named'),variable_name='level.str',measure.vars=c('cum.l1','cum.l2','cum.l3','cum.l4'))
  win.sub.m$level <- (1:4)[match(win.sub.m$level.str,pp('cum.l',1:4))]
  num.ch.at.transects <- ddply(subset(win.sub.m,abs(cum.cost-equi.cost)<50),.(scenario,penetration,level),function(df){
    data.frame(num.chargers=mean(df$value),tot.cost=mean(df$cum.cost),delay.fin=mean(df$delay),scenario.named=df$scenario.named[1],penetration.named=df$penetration.named[1])
  })
  #num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario,level),function(df){
    #df <- df[order(df$penetration),]
    #df$num.chargers <- cumsum(df$num.chargers)
    #df
  #})
  num.ch.at.transects$level.named <- refactor.lev(num.ch.at.transects$level)
  p <- ggplot(subset(num.ch.at.transects,level<4),aes(x=factor(scenario.named),y=num.chargers,fill=level.named)) + geom_bar(stat='identity',position='dodge') + facet_grid(level.named~ penetration.named,scales='free_y') + labs(x="",y="Number of Chargers Sited",title="",fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(colour='black'))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/opportunity-cost/opp-cost-chargers-at-fixed-investments.pdf'),p,width=10,height=6)

  ######################
  # Congestion
  ######################
  winners$scenario.named <- refactor.scen(winners$scenario,base='Base')
  winners$penetration.named <- pp(winners$penetration*100,"%")
  p <- ggplot(subset(winners,penetration==.01 & scenario%in%c('congested','half-homeless')),aes(x=cum.cost/1e3,y=delay/1e6,shape=scenario.named,colour=scenario.named)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",color="Scenario",shape="Scenario")+facet_wrap(~penetration.named) + scale_color_manual(values=charger.cols)+theme(legend.position=c(0,1), legend.justification = c(0,1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/congestion/congestion-optim-curves.pdf'),p,width=10,height=6)
  
  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,penetration == 0.01 & scenario %in% c('congested','half-homeless')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3)),.(scenario,level,penetration),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cost)
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cap)
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration <- pp(ch.fin$penetration*100,'%')
  ch.fin$scenario.named <- refactor.scen(ch.fin$scenario)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers",title="Number of Chargers",fill="Charger Level") + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,0.5),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/congestion/congestion-num-chargers.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=capacity,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Capacity (kW)",title="Capacity of Chargers",fill="Charger Level") + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,0.5),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/congestion/congestion-charger-capacity.pdf'),p,width=6,height=6)
  p <- ggplot(ch.fin,aes(x=scenario.named,y=installed.cost,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Cost ($M)",title="Cost of Chargers",fill="Charger Level") + theme(axis.text.x = element_text(colour='black'),plot.margin=unit(c(0.5,0.2,0.2,0.5),"cm")) + scale_fill_manual(values=tail(charger.cols,-1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/congestion/congestion-charger-cost.pdf'),p,width=6,height=6)

  equi.delay <- 40e6 # horizontal transect of the above plot where we will make comparable num.charger plots
  win.sub <- ddply(subset(winners,penetration == .01 & scenario%in%c('congested','half-homeless')),.(scenario,penetration,seed),function(df){
    df <- df[order(df$iteratio),]
    df$tmp <- df$num.added
    df$tmp[df$level!=1] <- 0
    df$cum.l1 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=2] <- 0
    df$cum.l2 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=3] <- 0
    df$cum.l3 <- cumsum(df$tmp)
    df$tmp <- df$num.added
    df$tmp[df$level!=4] <- 0
    df$cum.l4 <- cumsum(df$tmp)
    df
  })
  win.sub.m <- melt(win.sub,id.vars=c('scenario','penetration','seed','cum.cost','delay','scenario.named','penetration.named'),variable_name='level.str',measure.vars=c('cum.l1','cum.l2','cum.l3','cum.l4'))
  win.sub.m$level <- (1:4)[match(win.sub.m$level.str,pp('cum.l',1:4))]
  num.ch.at.transects <- ddply(subset(win.sub.m,abs(delay-equi.delay)<2e6),.(scenario,penetration,level),function(df){
    data.frame(num.chargers=round(mean(df$value)),tot.cost=mean(df$cum.cost),delay.fin=mean(df$delay),scenario.named=df$scenario.named[1],penetration.named=df$penetration.named[1])
  })
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario,level),function(df){
    df <- df[order(df$penetration),]
    df$num.chargers <- cumsum(df$num.chargers)
    df
  })
  num.ch.at.transects$level.named <- refactor.lev(num.ch.at.transects$level)
  num.ch.at.transects$level <- pp('L',num.ch.at.transects$level)
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario),num.ch.to.cost)
  num.ch.at.transects <- ddply(num.ch.at.transects,.(scenario),num.ch.to.cap)
  p <- ggplot(subset(num.ch.at.transects,level!="L4"),aes(x=factor(scenario.named),y=num.chargers,fill=level.named)) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title=pp('Number of Chargers Sited to Reduce PV of Delay to $',equi.delay/1e6,'M at 1% Penetration'),fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/congestion/congestion-num-chargers-at-',equi.delay/1e6,'M-delay.pdf'),p,width=6,height=6)
  p <- ggplot(subset(num.ch.at.transects,penetration==0.01 & level!="L4"),aes(x=factor(scenario.named),y=installed.cost,fill=level.named)) + geom_bar(stat='identity') + labs(x="",y="Cost ($M)",title="Cost of Chargers",fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/congestion/congestion-charger-cost-at-',equi.delay/1e6,'M-delay.pdf'),p,width=6,height=6)
  p <- ggplot(subset(num.ch.at.transects,level!="L4"),aes(x=factor(scenario.named),y=capacity,fill=level.named)) + geom_bar(stat='identity') + labs(x="",y="Capacity (kW)",title="Capacity of Chargers",fill='Charger Level') + scale_fill_manual(values=tail(charger.cols,-1))+ theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm"))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/congestion/congestion-charger-capacity-at-',equi.delay/1e6,'M-delay.pdf'),p,width=6,height=6)


  ######################
  # Cheap Chargers
  ######################
  cbPalette <- charger.cols
  winners$scenario.named <- refactor.scen(winners$scenario)
  winners$penetration.named <- pp(winners$penetration*100,"%")
  p <- ggplot(subset(winners,penetration == 0.01 & scenario%in%c(pp('cheap-l',1:3),'half-homeless')),aes(x=cum.cost/1e3,y=delay/1e6,shape=scenario.named,colour=scenario.named)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",color="Scenario",shape="Scenario")+facet_wrap(~penetration.named) + scale_color_manual(values=cbPalette)+theme(legend.position=c(0,1), legend.justification = c(-10,1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/cheap-chargers/cheap-chargers-optim-curves.pdf'),p,width=10,height=6)
  p <- ggplot(subset(winners,penetration == 0.01 & scenario%in%c(pp('cheap-l',4),'swap')),aes(x=cum.cost/1e3,y=delay/1e6,shape=scenario.named,colour=scenario.named)) + geom_point() + labs(x="Infrastructure Cost ($M)",y="PV of Driver Delay ($M)",title="",color="Scenario",shape="Scenario")+facet_wrap(~penetration.named) + scale_color_manual(values=cbPalette)+theme(legend.position=c(0,1), legend.justification = c(-10,1))
  ggsave(file=pp(pevi.shared,'data/DELHI/results/cheap-chargers/cheap-chargers-swapping-optim-curves.pdf'),p,width=10,height=6)

  ch.fin <- ddply(subset(melt(subset(ch.fin.unshaped,scenario %in% c(pp('cheap-l',1:4),'half-homeless','swap')),measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:4)),.(scenario,level,penetration),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin <- ddply(ch.fin,.(scenario),num.ch.to.cost)
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration.named <- pp(ch.fin$penetration*100,'%')
  ch.fin$scenario.named <- refactor.scen(ch.fin$scenario)
  ch.fin$scenario.named <- factor(ch.fin$scenario.named,)
  cbPalette <- tail(charger.cols,-1)
  p <- ggplot(subset(ch.fin,!scenario%in%c('swap','cheap-l4')),aes(x=scenario.named,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="",fill="Charger Level") + facet_wrap(~penetration.named) + theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=cbPalette)
  ggsave(file=pp(pevi.shared,'data/DELHI/results/cheap-chargers/cheap-chargers-num-chargers.pdf'),p,width=10,height=6)
  p <- ggplot(subset(ch.fin,!scenario%in%c('swap','cheap-l4')),aes(x=scenario.named,y=installed.cost,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Cost of Chargers Sited ($M)",title="",fill="Charger Level") + facet_wrap(~penetration.named) + theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=cbPalette)
  ggsave(file=pp(pevi.shared,'data/DELHI/results/cheap-chargers/cheap-chargers-charger-cost.pdf'),p,width=10,height=6)
  p <- ggplot(subset(ch.fin,!scenario%in%c('swap','cheap-l4') & level!="Battery Swapping"),aes(x=scenario.named,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="",fill="Charger Level") + facet_grid(level~ penetration.named,scales="free_y") + theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=cbPalette)
  ggsave(file=pp(pevi.shared,'data/DELHI/results/cheap-chargers/cheap-chargers-num-chargers-zoomed.pdf'),p,width=10,height=6)
  p <- ggplot(subset(ch.fin,scenario%in%c('swap','cheap-l4')),aes(x=scenario.named,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="",fill="Charger Level") + facet_wrap(~penetration.named) + theme(axis.text.x = element_text(angle = 25, hjust = 1, colour='black'),plot.margin=unit(c(0.5,0.2,0.2,1),"cm")) + scale_fill_manual(values=cbPalette)
  ggsave(file=pp(pevi.shared,'data/DELHI/results/cheap-chargers/cheap-chargers-swapping-num-chargers.pdf'),p,width=10,height=6)
  p <- ggplot(subset(ch.fin,scenario%in%c('swap','cheap-l4') & penetration==0.02),aes(x=scenario.named,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="",fill="Charger Level") + facet_wrap(~level,scales='free_y') + theme(axis.text.x = element_text(colour='black')) + scale_fill_manual(values=cbPalette)
  ggsave(file=pp(pevi.shared,'data/DELHI/results/cheap-chargers/cheap-chargers-swapping-num-chargers-zoomed-pen2.pdf'),p,width=10,height=6)

  ######################
  # Old Plots
  ######################

  # look at the # chargers in each taz
  ch.fin <- subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:3))
  ggplot(subset(ch.fin,scenario=='homeless'),aes(x=factor(TAZ),y=value,fill=level)) + geom_bar(stat='identity') + facet_grid(penetration~seed) + labs(x="",y="",title="")

  # compare seeds against each other for each TAZ / level
  ch.fin <- cast(subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),scenario=='homeless' & level%in%pp('L',1:3)),TAZ + level + penetration ~ seed)
  ggplot(ch.fin,aes(x=`21`,y=`22`,colour=level)) + geom_point() + facet_wrap(~penetration) + labs(x="",y="",title="")+geom_abline(intercept=0,slope=1)

  # compare several optimization runs at once
  ch.fin.unshaped$scenario <- refactor.scen(ch.fin.unshaped$scenario,base='50% Home Chargers')
  ch.fin <- subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:4))
  stat_sum_single <- function(fun, geom="point", ...) {
    stat_summary(fun.y=fun, geom=geom, size = 3, ...)
  }
  ch.fin$value[ch.fin$level=='L1'] <- ch.fin$value[ch.fin$level=='L1']/1e2
  ggplot(ddply(ch.fin,.(scenario,level,penetration,seed),function(df) { data.frame(num.chargers=sum(df$value)) }),aes(x=scenario,y=num.chargers,colour=level))+geom_point() + facet_wrap(~penetration) + stat_summary(fun.y=mean,geom='point',size=3)+labs(x="Scenario",y="# Chargers",title="# Chargers Sited over Optimization Variations")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # same as above but just focus on mean value
  ch.fin <- ddply(subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:4)),.(scenario,level,penetration),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin$num.chargers[ch.fin$level=='L1'] <- ch.fin$num.chargers[ch.fin$level=='L1']/1e1
  ch.fin <- subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:4))
  stat_sum_single <- function(fun, geom="point", ...) {
    stat_summary(fun.y=fun, geom=geom, size = 3, ...)
  }
  ch.fin$value[ch.fin$level=='L1'] <- ch.fin$value[ch.fin$level=='L1']/1e2
  ggplot(ddply(ch.fin,.(scenario,level,penetration,seed),function(df) { data.frame(num.chargers=sum(df$value)) }),aes(x=scenario,y=num.chargers,colour=level))+geom_point() + facet_wrap(~penetration) + stat_summary(fun.y=mean,geom='point',size=3)+labs(x="Scenario",y="# Chargers",title="# Chargers Sited over Optimization Variations")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # same as above but just focus on mean value
  ch.fin <- ddply(subset(melt(ch.fin.unshaped,measure.vars=pp('L',0:4),variable_name="level"),level%in%pp('L',1:4)),.(scenario,level,penetration),function(df) { data.frame(num.chargers=sum(df$value)/length(unique(df$seed))) })
  ch.fin$level <- revalue(ch.fin$level,c('L1'='Level 1','L2'='Level 2','L3'='DC Fast','L4'='Battery Swapping'))
  ch.fin$penetration <- pp(ch.fin$penetration*100,'%')
  cbPalette <- tail(charger.cols,-1)
  ggplot(ch.fin,aes(x=scenario,y=num.chargers,fill=factor(level))) + geom_bar(stat='identity') + labs(x="",y="Number of Chargers Sited",title="Chargers Sited for Various Optimization Scenarios",fill="Charger Level") + facet_wrap(~penetration) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_fill_manual(values=cbPalette)


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
    write.csv(best.ranks,pp(pevi.shared,'data/DELHI/results/maps/charger-priorities/charger-priorities-',scen,'-pen',pen*100,'.csv'))

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

