
load(pp(pevi.shared,"data/inputs/compare/upstate-ghg/logs.Rdata"))

my.purp <- '#984ea3'
my.oran <- '#ff7f00'
my.red <- '#e41a1c'
my.blue <- '#377eb8'
my.green <- '#4daf4a'
my.grey <- '#808080'

weekend.factor <- 0.5
weekday.to.year <- 260 + 105*weekend.factor
frac.ldt <- 0.01 # how many light duty trucks should we assume

# emissions rates in 2020
county.fracs <- c('sha'=0.6172152,'teh'=0.2321632,'sis'=0.1506216)
grid.intensity <- data.frame(util=c('rei','pge','pac'),rate=c(0.172,0.140,0.46),frac=c(county.fracs['sha']*0.5,county.fracs['sha']*0.5+county.fracs['teh'],county.fracs['sis']))
grid.rate <- weighted.mean(grid.intensity$rate,grid.intensity$frac)
write.csv(grid.rate,file=pp(pevi.shared,"data/UPSTATE/ghg/grid-carbon-intensity.csv"))

# upstream emissions from GREET in kg CO2e/gal
upstream <- c('gas'=1.319,'dsl'=0.784) 

# EMT
emt.to.plot <- ddply(logs[['results']],.(penetration,infrastructure.scenario.named),function(df){ data.frame(emt=mean(df$electric.miles.driven),num=mean(df$num.drivers)) })
emt.to.plot$with.chargers <- factor(ifelse(emt.to.plot$infrastructure.scenario.named=="Existing Chargers","Without Public EVSE","With Public EVSE"),c("Without Public EVSE","With Public EVSE"))
emt.to.plot$emt.per.cap <- emt.to.plot$emt / emt.to.plot$num
emt.to.plot$co2 <- emt.to.plot$emt * 0.33 / 1e3 * grid.rate * weekday.to.year
emt.to.plot$co2.per.cap <- emt.to.plot$emt.per.cap * 0.33 / 1e3 * grid.rate * weekday.to.year
emt.to.plot <- rbind(subset(emt.to.plot,with.chargers!="With Public EVSE"),ddply(subset(emt.to.plot,with.chargers=="With Public EVSE"),.(penetration),function(df){
  df$emt <- df$emt - subset(emt.to.plot,with.chargers=="Without Public EVSE" & penetration==df$penetration[1])$emt
  df
}))
emt.to.plot$infrastructure.scenario.named <- revalue(emt.to.plot$infrastructure.scenario.named,c('Existing Chargers'='veh','Chargers for 0.5%'='veh.evse','Chargers for 1%'='veh.evse','Chargers for 2%'='veh.evse'))
write.csv(emt.to.plot,file=pp(pevi.shared,"data/UPSTATE/ghg/emt-from-pevi.csv"))

# strandings
mean.results <- ddply(logs[['results']],.(penetration,infrastructure.scenario.named),function(df){ 
  colMeans(df[,sapply(df,is.numeric)])
})
p <- ggplot(ddply(mean.results,.(penetration),function(df){ data.frame(num.drivers=df$num.drivers[1],infrastructure.scenario.named=df$infrastructure.scenario.named[1],new.strandings=diff(df$num.stranded))}),aes(x=factor(penetration),y=new.strandings)) + geom_bar(stat='identity',position='dodge',fill=my.blue) + labs(x="Light Duty Fleet Penetration (%)",y="# BEV Drivers Who Avoid Standings Due to EVSE",title="") + scale_fill_manual(values=c(my.blue,my.grey))
ggsave(file=pp(pevi.shared,'data/UPSTATE/results/ghg/avoided-strandings.pdf'),p,width=6,height=6)

num.pev <- ddply(subset(logs[['results']],infrastructure.scenario.named!="Existing Chargers"),.(penetration),function(df){ data.frame(num=mean(df$num.drivers),miles.driven=mean(df$miles.driven),gas.miles.driven=mean(df$miles.driven - df$electric.miles.driven),vmt=mean(df$miles.driven)/mean(df$num.drivers)) })
write.csv(num.pev,file=pp(pevi.shared,"data/UPSTATE/ghg/vmt-from-pevi.csv"))

emt.from.evse <- subset(emt.to.plot,with.chargers=="With Public EVSE")
emm.replacement <- read.csv(pp(pevi.shared,"data/UPSTATE/ghg/lda-lta1-2018-emissions.csv"),stringsAsFactors=F)

emm <- subset(read.csv(pp(pevi.shared,"data/UPSTATE/ghg/emissions-factors.csv"),skip=7),Region%in%c('Shasta','Tehama','Siskiyou'))
emm$Fuel_Gas[is.na(emm$Fuel_Gas)]<-0
emm$Fuel_Dsl[is.na(emm$Fuel_Dsl)]<-0

# summarize fleet characteristics
fleet <- rbind(ddply(subset(emm,Veh_Class%in%c('LDA','LDT1')),.(Region,Veh_Class,Fuel),function(df){ data.frame(pop=sum(df$Population),vmt=sum(df$VMT))}),data.frame(Veh_Class="Other",ddply(subset(emm,!Veh_Class%in%c('LDA','LDT1')),.(Region,Fuel),function(df){ data.frame(pop=sum(df$Population),vmt=sum(df$VMT))})))
fleet$pop <- fleet$pop/1e3
fleet$vmt <- fleet$vmt/1e6
fleet <- melt(fleet,measure.vars=c('pop','vmt'))
fleet$class.fuel <- revalue(factor(pp(fleet$Veh_Class,'-',fleet$Fuel)),c('LDA-GAS'='Light Duty Gasoline Passenger Cars','LDA-DSL'='Light Duty Diesel Passenger Cars','LDT1-GAS'='Light Duty Gasoline Trucks (0-3750lbs)','LDT1-DSL'='Light Duty Diesel Trucks (0-3750lbs)'))
fleet$Veh_Class <- revalue(fleet$Veh_Class,c('LDA'='Light Duty Passenger Cars','LDT1'='Light Duty Trucks (0-3750lbs)'))
p <- ggplot(subset(fleet,variable=="pop"),aes(x=Region,y=value,fill=Veh_Class)) + geom_bar(stat='identity') + labs(x="",y="Vehicle Population (1000's of vehicles)",fill="Vehicle Class") + scale_fill_manual(values=c(my.blue,my.green,my.grey,my.purp))
ggsave(file=pp(pevi.shared,'data/UPSTATE/results/ghg/veh-pop.pdf'),p,width=8,height=6)

frac.veh <- ddply(emm,.(Region),function(df){ data.frame(num.veh=sum(df$Population),frac.veh=sum(df$Population)/sum(emm$Population),vmt=sum(df$VMT)/sum(df$Population)) })
# the following is used to compensate for the fact that we have almost double the VMT in the PEVI model than EMFAC
pevi.to.emfac.ratio <- weighted.mean(frac.veh$vmt,frac.veh$num.veh) / mean(num.pev$vmt)
num.pev$gas.miles.driven.scaled <- num.pev$gas.miles.driven * pevi.to.emfac.ratio
emm$penetration <- NA
emm$scenario <- 'No PEVs'
emm.orig <- emm
for(credit in c('veh.evse','veh')){
  for(pen in c(0.5,1,2)){
    tmp <- emm.orig
    tmp$penetration <- pen 
    tmp$scenario <- credit
    num <- subset(num.pev,penetration==pen)$num
    if(credit=="veh.evse"){
      gas.miles <- subset(num.pev,penetration==pen)$gas.miles.driven.scaled
    }else{
      gas.miles <- subset(num.pev,penetration==pen)$gas.miles.driven.scaled + emt.from.evse$emt[emt.from.evse$penetration==pen] * pevi.to.emfac.ratio
    }
    for(county in c('Shasta','Tehama','Siskiyou')){
      frac.v <- subset(frac.veh,Region==county)$frac.veh
      num.lda <- num * (1-frac.ldt) * frac.v
      num.ldt <- num * frac.ldt * frac.v

      # light duty autos
      tmp.row <- which(tmp$Veh_Class=="LDA" & tmp$Fuel=="GAS" & tmp$Region==county)
      pop.before <- tmp$Population[tmp.row]
      tmp$Population[tmp.row] <- pop.before - num.lda
      tmp$VMT[tmp.row] <- tmp$VMT[tmp.row] * tmp$Population[tmp.row] / pop.before
      tmp$Trips[tmp.row] <- tmp$Trips[tmp.row] * tmp$Population[tmp.row] / pop.before
      tmp$Fuel_Gas[tmp.row] <- tmp$Fuel_Gas[tmp.row] * tmp$Population[tmp.row] / pop.before
      tmp$Fuel_Dsl[tmp.row] <- tmp$Fuel_Dsl[tmp.row] * tmp$Population[tmp.row] / pop.before

      # light duty trucks
      tmp.row <- which(tmp$Veh_Class=="LDT1" & tmp$Fuel=="GAS" & tmp$Region==county)
      pop.before <- tmp$Population[tmp.row]
      tmp$Population[tmp.row] <- pop.before - num.ldt
      tmp$VMT[tmp.row] <- tmp$VMT[tmp.row] * tmp$Population[tmp.row] / pop.before
      tmp$Trips[tmp.row] <- tmp$Trips[tmp.row] * tmp$Population[tmp.row] / pop.before
      tmp$Fuel_Gas[tmp.row] <- tmp$Fuel_Gas[tmp.row] * tmp$Population[tmp.row] / pop.before
      tmp$Fuel_Dsl[tmp.row] <- tmp$Fuel_Dsl[tmp.row] * tmp$Population[tmp.row] / pop.before

      replacements <- subset(emm.replacement,Region==county)
      replacements$Population[replacements$Veh_Class=="LDA"] <- num.lda
      replacements$VMT[replacements$Veh_Class=="LDA"] <- gas.miles*(1-frac.ldt)*frac.v
      replacements$Fuel_Gas[replacements$Veh_Class=="LDA"] <- gas.miles/50*(1-frac.ldt)*frac.v/1000
      replacements$Population[replacements$Veh_Class=="LDT1"] <- num.ldt
      replacements$VMT[replacements$Veh_Class=="LDT1"] <- gas.miles*frac.ldt*frac.v
      replacements$Fuel_Gas[replacements$Veh_Class=="LDT1"] <- gas.miles/50*frac.ldt*frac.v/1000
      replacements$penetration <- pen
      replacements$scenario <- credit
      tmp <- rbind.fill(tmp,replacements)
    }
    emm <- rbind.fill(emm,tmp)
  }
}
emm$co2.upstream <- (emm$Fuel_Gas * upstream['gas'] + emm$Fuel_Dsl * upstream['dsl'])*1e6 # the 1M here is to convert 1000 gal/day to gal/day and kg/gal to g/gal for a final value in g
scens.1 <- ddply(subset(emm,Veh_Class%in%c('LDA','LDT1')),.(Region,penetration,scenario),function(df){ 
  data.frame(
    num.veh=sum(df$Population),
    vmt=sum(df$VMT)/sum(df$Population),
    co2.regs=sum(df$VMT * df$CO2_RUNEX.Pavley.I.LCFS + df$Population * (df$CO2_IDLEX.Pavley.I.LCFS + df$CO2_STREX.Pavley.I.LCFS) + df$co2.upstream,na.rm = T)*weekday.to.year/1e6
  )
})
scens.2 <- ddply(subset(scens.1,scenario!="No PEVs"),.(Region,penetration),function(df){
  df$co2.regs <- subset(scens.1,Region==df$Region[1] & scenario=="No PEVs")$co2.regs - df$co2.regs - subset(frac.veh,Region==df$Region[1])$frac.veh * subset(emt.to.plot,penetration==df$penetration[1] & infrastructure.scenario.named==df$scenario[1])$co2 * pevi.to.emfac.ratio
  df
})
scens <- rbind(subset(scens.2,scenario=="veh"),ddply(subset(scens.2,scenario=="veh.evse"),.(Region,penetration),function(df){
  df$co2.regs <- df$co2.regs - subset(scens.2,penetration==df$penetration[1] & Region==df$Region[1] & scenario=="veh")$co2.regs
  df
}))
scens$scenario <- factor(unlist(revalue(scens$scenario,list(veh='Without Public EVSE',veh.evse='With Public EVSE'))))

p <- ggplot(scens,aes(x=factor(penetration),y=co2.regs,fill=scenario)) + geom_bar(stat='identity') + labs(x="Light Duty Fleet Penetration (%)",y=expression(paste("Metric Tons ",CO[2],"e")),title="Avoided GHG Emissions from PEV Adoption with/without Public EVSE") + scale_fill_manual(values=c(my.blue,my.grey,my.green,my.purp)) + facet_wrap(~Region)
ggsave(file=pp(pevi.shared,'data/UPSTATE/results/ghg/ghg-avoided.pdf'),p,width=10,height=6)

p <- ggplot(emt.to.plot,aes(x=factor(penetration),y=pevi.to.emfac.ratio*emt/1e3,fill=with.chargers)) + geom_bar(stat='identity') + labs(x="Light Duty Fleet Penetration (%)",y="1000's Electric Miles Traveled per Day",title="",fill="") + scale_fill_manual(values=c(my.grey,my.blue))
ggsave(file=pp(pevi.shared,'data/UPSTATE/results/ghg/emt.pdf'),p,width=8,height=6)

# Compare the results to total light duty
ddply(scens,.(penetration),function(df){
  data.frame(percent.of.light.duty=sum(df$co2.regs)/sum(subset(scens.1,is.na(penetration))$co2.regs)*100)
})

# load results from the excel based analysis (the final version of this work)

for(fleet.scen in c('Total Vehicle Fleet','Light Duty Fleet')){
  if(fleet.scen=='Light Duty Fleet'){
    scens <- read.csv(pp(pevi.shared,'data/UPSTATE/ghg/GHG-Emissions-Analysis-Results.csv'))
  }else{
    scens <- read.csv(pp(pevi.shared,'data/UPSTATE/ghg/GHG-Emissions-Analysis-Results-WHOLE-FLEET.csv'))
  }
  no.pevs <- subset(scens,Scenario=="No PEVs")
  for(scen in grep("No PEVs",unique(scens$Scenario),value=T,invert=T)){
    no.pevs$Scenario <- scen
    scens <- rbind(scens,no.pevs)
  }
  scens <- subset(scens,Scenario!="No PEVs")
  #scens <- subset(scens,Scenario=="PEVS w/ EVSE")
  scens$TOT <- scens$BEV + scens$GAS + scens$PHEV + scens$DSL
  scens <- ddply(scens,.(County,Scenario),function(df){
                 df$p.diff <- pp(roundC((1 - df$TOT / subset(df,Penetration==0)$TOT) * 100,2),'%')
                 df
  })
  scens.m <- melt(scens[,1:(ncol(scens)-2)],id.vars=c('Scenario','Penetration','County'))
  scens.m$variable <- factor(scens.m$variable,rev(c('BEV','PHEV','DSL','GAS')))
  scens.m$value <- scens.m$value/1e3
  p <- ggplot(scens.m,aes(x=factor(Penetration),y=value,fill=variable,order=variable)) + geom_bar(stat='identity',position='stack') + labs(x="Total Vehicle Fleet Penetration (%)",y=expression(paste("1000's Metric Tons ",CO[2],"e")),title=pp("Upstate ",fleet.scen," GHG Emissions with EVSE"),fill="Vehicle Type")+scale_fill_manual(values=c(my.grey,my.blue,my.purp,my.green)) + geom_text(aes(label=p.diff,y=1.03*max(scens$TOT)/1e3,fill=NA,order=NA),data=scens)
  #p <- p + facet_wrap(~ County) 
  p <- p + facet_grid(Scenario ~ County) 
  ggsave(file=pp(pevi.shared,'data/UPSTATE/results/ghg/emissions-',fleet.scen,'.pdf'),p,width=10,height=8)
}
