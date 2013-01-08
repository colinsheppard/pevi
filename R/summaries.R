library(colinmisc)
load.libraries(c('ggplot2','plyr'))

path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'
path.to.outputs <- '~/Dropbox/serc/pev-colin/pevi/outputs/'

#dr <- read.csv(paste(path.to.outputs,"driver-summary-out.csv",sep='')) #,stringsAsFactors=F
#ggplot(dr,aes(x=vehicle.type,y=value))+geom_boxplot()+facet_wrap(~metric,scales='free_y')

#dr.summed <- ddply(dr,.(vehicle.type,metric),function(df){ data.frame(value=sum(df$value)) })
#ggplot(dr.summed,aes(x=vehicle.type,y=value))+geom_bar(stat='identity')+facet_wrap(~metric,scales='free_y')

ch <- read.csv(paste(path.to.outputs,"charging-out.csv",sep='')) #,stringsAsFactors=F
ch <- ddply(ch,.(driver),function(df){ data.frame(df,event.num=1:nrow(df))})

ggplot(ch,aes(x=time))+geom_histogram(binwidth=1)+facet_grid(vehicle.type ~ charger.level)
ggplot(ch,aes(x=duration))+geom_histogram(binwidth=0.25)+facet_grid(vehicle.type ~ charger.level)
ggplot(ch,aes(x=energy))+geom_histogram(binwidth=1)+facet_grid(vehicle.type ~ charger.level)
ggplot(ch,aes(x=time,y=duration,colour=factor(charger.level)))+geom_point()+facet_wrap(~vehicle.type)
ggplot(ch,aes(x=time,y=begin.soc,colour=factor(charger.level)))+geom_point()+facet_wrap(~vehicle.type)
ggplot(ch,aes(x=time,y=begin.soc,colour=factor(event.num)))+geom_point()+facet_grid(charger.level~vehicle.type)
ggplot(ch,aes(x=time,y=begin.soc,colour=factor(charger.level),shape=factor(charger.level),size=2))+geom_point()+facet_wrap(~vehicle.type)
ggplot(ch,aes(x=factor(event.num)))+geom_histogram()+facet_grid(~vehicle.type)

ggplot(ch,aes(x=time,y=end.soc,colour=factor(event.num),shape=factor(charger.level)))+geom_point()+facet_grid(after.end.charge~vehicle.type)

# SEGMENTS
ggplot(ch,aes(x=time,xend=time+duration,y=begin.soc,yend=end.soc,colour=energy))+geom_segment()+facet_grid(charger.level~vehicle.type)

# DRIVERS WHO DONT CHARGE UNTIL FULL
ggplot(subset(ch,end.soc<0.999),aes(x=time,xend=time+duration,y=begin.soc,yend=end.soc,colour=factor(charger.level),shape=factor(charger.level),size=2))+geom_segment()+facet_wrap(~vehicle.type)

# DUTY FACTOR OF INDIVIDUAL CHARGERS
chs <- ddply(ch,.(charger.id),function(df){
  if(df$charger.level[1]==0){
    data.frame(charger.level=0,duty.factor=df$duration/(df$time+df$duration))
  }else{
    data.frame(charger.level=df$charger.level[1],duty.factor=sum(df$duration)/max(df$time+df$duration))
  }
})
ggplot(chs,aes(x=duty.factor*100))+geom_histogram(binwidth=1)+facet_wrap(~charger.level,scales="free_y")
