
# HOW MANY DRIVERS ARE DELAYED
p <- ggplot(results,aes(x=infrastructure.scenario.named,y=frac.drivers.delayed))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# UNSCHEDULED TRIPS / DRIVER
p <- ggplot(results,aes(x=infrastructure.scenario.named,y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# GASOLINE USED
p <- ggplot(results,aes(x=infrastructure.scenario.named,y=gasoline.used))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# DUTY FACTOR
p <- ggplot(results,aes(x=infrastructure.scenario.named,y=mean.duty.factor))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# CHARGER-SEARCH-DISTANCE
p <- ggplot(results,aes(x=factor(charger.search.distance),y=total.delay))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.stranded/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# ELECTRIC-FUEL-CONSUMPTION
p <- ggplot(results,aes(x=factor(electric.fuel.consumption),y=total.delay))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.stranded/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
# Look at both fuel consumption params for one scenario
p <- ggplot(subset(results,vehicle.scenario==1 & infrastructure.scenario==1),aes(x=factor(electric.fuel.consumption.range),y=total.delay))+geom_boxplot(aes(fill=factor(electric.fuel.consumption.sd)))+facet_wrap(~penetration)

# ELECTRIC-FUEL-CONSUMPTION-MEAN
ggplot(results,aes(x=vehicle.scenario.named,y=frac.drivers.delayed))+geom_boxplot(aes(fill=factor(penetration)))+facet_wrap(~infrastructure.scenario.named)
ggplot(results,aes(x=vehicle.scenario.named,y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_wrap(~infrastructure.scenario.named,scales="free_y")
ggplot(results,aes(x=vehicle.scenario.named,y=num.stranded))+geom_boxplot(aes(fill=factor(penetration)))+facet_wrap(~infrastructure.scenario.named,scales="free_y")

# PROBABILITY-OF-UNNEEDED-CHARGE
p <- ggplot(results,aes(x=factor(probability.of.unneeded.charge),y=total.delay))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(probability.of.unneeded.charge),y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(probability.of.unneeded.charge),y=num.stranded/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(probability.of.unneeded.charge),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(probability.of.unneeded.charge),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# TIME-OPPORTUNITY-COST
p <- ggplot(results,aes(x=factor(time.opportunity.cost),y=total.delay))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.stranded/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# WAIT-TIME-MEAN
p <- ggplot(results,aes(x=factor(wait.time.mean),y=total.delay))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.stranded/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)


# NUM DENAILS
p <- ggplot(results,aes(x=infrastructure.scenario.named,y=num.denials))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# ALL METRICS, JUST ONE VEHICLE SCENARIO
p <- ggplot(melt(subset(results,vehicle.scenario.named=="BEV/PHEV (90/10)"),id.vars=c('infrastructure.scenario.named','penetration'),measure.vars=c('mean.delay','frac.drivers.delayed','num.unscheduled.trips','energy.charged','driver.expenses','infrastructure.cost','gasoline.used','miles.driven','num.denials','num.stranded','mean.duty.factor','frac.denied')),aes(x=infrastructure.scenario.named,y=value))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~variable,scales="free_y")
p <- ggplot(melt(results,id.vars=c('infrastructure.scenario.named','penetration'),measure.vars=c('mean.delay','frac.drivers.delayed','num.unscheduled.trips','energy.charged','driver.expenses','infrastructure.cost','gasoline.used','miles.driven','num.denials','num.stranded','mean.duty.factor','frac.denied')),aes(x=infrastructure.scenario.named,y=value))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~variable,scales="free_y")
p <- ggplot(melt(subset(results,infrastructure.scenario==2),id.vars=c('vehicle.scenario.named','penetration'),measure.vars=c('mean.delay','frac.drivers.delayed','num.unscheduled.trips','energy.charged','driver.expenses','infrastructure.cost','gasoline.used','miles.driven','num.denials','num.stranded','mean.duty.factor','frac.denied')),aes(x=vehicle.scenario.named,y=value))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_wrap(~variable,scales="free_y")

# KEY METRICS vs PARAM
ggplot(melt(results,id.vars=c('infrastructure.scenario.named','penetration','wait.time.mean'),measure.vars=c('frac.drivers.delayed','num.unscheduled.trips','num.stranded','frac.stranded.by.delay','frac.denied')),aes(x=factor(wait.time.mean),y=value))+geom_boxplot(aes(fill=as.factor(penetration)))+facet_grid(variable~infrastructure.scenario.named,scales="free_y")

# CHARGE-SAFETY-FACTOR
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=total.delay))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named)
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=num.stranded/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(charge.safety.factor),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# WILLING TO ROAM TIME THRESHOLD 
p <- ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=total.delay/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=frac.denied))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=num.stranded/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=num.unscheduled.trips/num.drivers))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)
p <- ggplot(results,aes(x=factor(willing.to.roam.time.threshold),y=mean.duty.factor))+geom_boxplot(aes(fill=factor(penetration)))+facet_grid(infrastructure.scenario.named~vehicle.scenario.named) + scale_y_log10()
ggsave(paste(path.to.inputs,"plots/num-drivers-delayed.pdf",sep=''),p,width=15,height=11)

# ANALYZE VARIANCE
stat.cvs <- ddply(results,.(penetration,vehicle.scenario.named,infrastructure.scenario.named),function(df){ data.frame(
  frac.drivers.delayed.cv=sd(df$frac.drivers.delayed,na.rm=T)/mean(df$frac.drivers.delayed,na.rm=T),
  mean.delay.cv=sd(df$mean.delay,na.rm=T)/mean(df$mean.delay,na.rm=T),
  frac.denials.cv=sd(df$num.denials/df$num.drivers,na.rm=T)/mean(df$num.denials/df$num.drivers,na.rm=T),
  frac.denied.cv=sd(df$frac.denied,na.rm=T)/mean(df$frac.denied,na.rm=T),
  mean.duty.factor.cv=sd(df$mean.duty.factor,na.rm=T)/mean(df$mean.duty.factor,na.rm=T),
  frac.stranded.cv=sd(df$num.stranded/df$num.drivers,na.rm=T)/mean(df$num.stranded/df$num.drivers,na.rm=T))})
ggplot( melt(stat.cvs,id.vars=c('penetration','vehicle.scenario.named','infrastructure.scenario.named')),aes(x=infrastructure.scenario.named,y=value,colour=as.factor(penetration)))+geom_point()+facet_grid(vehicle.scenario.named~variable)
ggplot( melt(subset(stat.cvs,vehicle.scenario.named=="BEV/PHEV (90/10)"),id.vars=c('penetration','vehicle.scenario.named','infrastructure.scenario.named')),aes(x=infrastructure.scenario.named,y=value,colour=as.factor(penetration)))+geom_point()+facet_wrap(variable~)

