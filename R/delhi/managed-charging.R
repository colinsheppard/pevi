load.libraries('reshape2')
exp.name <- 'delhi-smart-charging-demand'
path.to.inputs <- pp(pevi.shared,'data/inputs/compare/',exp.name,'/')
path.to.outputs <- pp(pevi.shared,'data/DELHI/results/managed-charging')
load(paste(path.to.inputs,'logs.Rdata',sep=''))

driver.schedules <- data.table(read.csv(pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/no-homeless/driver-schedule-pen5-rep1-4day-20150212.txt'),sep='\t'))
driver.schedules.half <- data.table(read.csv(pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/half-homeless/driver-schedule-pen5-rep1-4day-20150212.txt'),sep='\t'))
driver.schedules[,':='(driver=X.driver,X.driver=NULL)]
driver.schedules.half[,':='(driver=X.driver,X.driver=NULL)]
setkey(driver.schedules,driver,depart)
setkey(driver.schedules.half,driver,depart)
driver.home <- driver.schedules[match(unique(driver.schedules[,driver]),driver.schedules[,driver]),home,by=driver]
driver.home.half <- driver.schedules.half[match(unique(driver.schedules.half[,driver]),driver.schedules.half[,driver]),home,by=driver]
driver.home[,itin.scenario:='no-homeless']
driver.home.half[,itin.scenario:='half-homeless']
driver.home <- rbindlist(list(driver.home,driver.home.half))

# params used in the model run and needed here
vehs <- data.table(read.csv(pp(pevi.shared,'/data/inputs/vehicle-type-input-file/vehicle-types-scen-delhi-smart.txt'),sep='\t'))
vehs[,':='(name=X.name,X.name=NULL)]
chs <- data.table(read.csv(pp(pevi.shared,'/data/inputs/charger-type-input-file/charger-types-scen-delhi.txt'),sep='\t'))
chs[,':='(level=X.level,X.level=NULL)]

fact.safety <- 1.1
batt.caps <- array(vehs$battery.capacity,dimnames=list(vehs$name))
charger.power <- array(chs$charge.rate,dimnames=list(pp('L',chs$level)))

# summarize / list what's there
#for(factor.col in grep('named',names(logs[['results']]),value=T)){
  #print(factor.col)
  #print(unique(logs[['results']][,factor.col]))
#}
#print(unique(logs[['results']]$replicate))
#print(unique(logs[['results']]$penetration))

# for this analysis, pull just pen 2% and rep 1 from the mix
pain <- data.table(logs[['pain']],key='driver')
pain[,':='(time=time-6)]
stranded.drivers <- unique(pain[pain.type=='stranded']$driver)
trips <- data.table(subset(logs[['trip']], !driver%in%stranded.drivers))
trips[,':='(time=time-6,end.time=end.time-6)]
#tazs <- data.table(subset(read.csv(pp(path.to.inputs,'tazs-out.csv')), time >= 5.99999 & time <= 36),key='taz')
#tazs[,':='(time=time-6)]
charging <- data.table(subset(logs[['charging']], !driver%in%stranded.drivers))
charging[,':='(time=time-6)]

# what soc is needed for each trip
setkey(trips,itin.scenario,driver)
trips[,soc.needed:=elec.used*fact.safety/batt.caps[vehicle.type]]

# Calculate the delta.soc for each trip. The first trip will have a delta.soc of 0. subsequent trips are the soc.needed - end.soc from previous trip.
trips[,':='(delta.soc=0,total.trips=length(distance)),by=c('itin.scenario','driver')]

# For any driver with more than 1 trip, delta.soc = soc.needed - end.soc from previous trip.
trips[total.trips>1,delta.soc:=as.vector(c(0,soc.needed[2:length(distance)]-end.soc[1:(length(distance)-1)])),by=c('itin.scenario','driver')]
# A negative delta.soc means they had more charge from the previous trip than they needed, so we set those delta.socs to 0.
trips[delta.soc<0,delta.soc:=0]

needy.trips <- trips[delta.soc>0]

setkey(needy.trips,itin.scenario,driver)
setkey(charging,itin.scenario,driver)

ch.cands <- charging[needy.trips,allow.cartesian=T]

ch.cands[,trip.charge.time.diff:=i.time-time]
ch.cands[,trip.charge.time.diff:=ifelse(trip.charge.time.diff<0,Inf,trip.charge.time.diff)]
ch.cands[,most.recent:=rank(trip.charge.time.diff),by=c('itin.scenario','driver')]
needy.charges <- ch.cands[most.recent==1]

needy.charges[,end.needy.time:=time + soc.needed * batt.caps[vehicle.type] / charger.power[pp('L',charger.level)]]
needy.charges[,row:=1:length(time),by=itin.scenario]

non.curtailable.charge <- ddply(needy.charges,.(itin.scenario,vehicle.type,row),function(df){ 
      pow.hrs <- unlist(floor(df['time'])):unlist(floor(df['end.needy.time']))
      if(length(pow.hrs) > 1){
        fractional.pows <- charger.power[pp('L',df['charger.level'])] * unlist(c(ceiling(df['time']) - df['time'],df['end.needy.time'] - floor(df['end.needy.time'])))
        pow <- c(fractional.pows[1],rep(charger.power[pp('L',df['charger.level'])],length(pow.hrs)-2),fractional.pows[2])
      }else{
        pow <- charger.power[pp('L',df['charger.level'])] * (df['end.needy.time']-df['time'])
      }
      data.frame(hour=pow.hrs,power=unlist(pow))
})
non.curtailable.profile <- data.table(ddply(non.curtailable.charge,.(itin.scenario,vehicle.type,hour),function(df){ data.frame(power=sum(df$power)) }),key=c('itin.scenario','hour'))

charging[,end.time:=time+duration]
charging[,row:=1:length(time),by=c('itin.scenario','vehicle.type')]

setkey(charging,itin.scenario,driver)
setkey(driver.home,itin.scenario,driver)
charging <- driver.home[charging]
charging[,at.home:=home==location]
charging[is.na(at.home),at.home:=F] # NA indicates no home charger for that driver

all.charge <- ddply(charging,.(itin.scenario,vehicle.type,row),function(df){ 
      pow.hrs <- unlist(floor(df['time'])):unlist(floor(df['end.time']))
      if(length(pow.hrs) > 1){
        fractional.pows <- charger.power[pp('L',df['charger.level'])] * unlist(c(ceiling(df['time']) - df['time'],df['end.time'] - floor(df['end.time'])))
        pow <- c(fractional.pows[1],rep(charger.power[pp('L',df['charger.level'])],length(pow.hrs)-2),fractional.pows[2])
      }else{
        pow <- charger.power[pp('L',df['charger.level'])] * (df['end.time']-df['time'])
      }
      data.frame(hour=pow.hrs,power=unlist(pow),at.home=df['at.home'])
})
all.charge <- data.table(all.charge)

all.profile <- data.table(ddply(all.charge,.(itin.scenario,vehicle.type,hour,at.home),function(df){ data.frame(all.power=sum(df$power,na.rm=T)) }),key=c('itin.scenario','hour'))

# first make a plot that dissaggregates by at.home then simplify
all.profile[,itin.scen.named:=ifelse(itin.scenario=='half-homeless','50% Home','100% Home')]
ggplot(all.profile[hour>=6 & hour<=30],aes(x=hour,y=all.power/1e3,colour=at.home,fill=vehicle.type))+geom_bar(stat='identity')+facet_wrap(~itin.scen.named)+labs(y="Charging Demand (MW)")
trips[,itin.scen.named:=ifelse(itin.scenario=='half-homeless','50% Home','100% Home')]
ggplot(trips[time>=6 & time<=30],aes(x=end.time,fill=vehicle.type))+geom_histogram(binwidth=1)+facet_wrap(~itin.scen.named)+labs(x='Arrival Time')

## !!!!!!!!!!!!!!!
## TODO reaggregate all.profile here
## !!!!!!!!!!!!!!!

setkey(non.curtailable.profile,itin.scenario,vehicle.type,hour)
setkey(all.profile,itin.scenario,vehicle.type,hour)
profile <- non.curtailable.profile[all.profile]
profile[,frac.shiftable:=(all.power-power)/all.power]
profile[,frac.shiftable:=ifelse(is.na(frac.shiftable),1,frac.shiftable)]
profile <- profile[hour>6 & hour<24*2+31] 
profile[hour>=73,hour:=hour-72]

ggplot(profile,aes(x=hour,y=all.power,colour=itin.scenario))+geom_line()
ggplot(profile,aes(x=hour,y=all.power,shape= vehicle.type,colour=itin.scenario))+geom_point()

setkey(profile,itin.scenario,vehicle.type,hour)
profile[,power:=NULL]
profile[,power:=all.power]
profile[,all.power:=NULL]

# scale the power up to something in the ballpark
#tot.energy <- 50*750e3*.033 # 50 km/day, 750k drivers, 33Wh/km 
#profile[,power:=power*tot.energy/sum(power)/1000] # now in MW

# now convert to Plexos format
itin.scen <- 'no-homeless'
veh.type <- 'two-wheel'
for(itin.scen in u(profile$itin.scenario)){
  for(veh.type in u(profile$vehicle.type)){
    ann.profile <- data.frame(hour=1:8760,power.shiftable=NA,power=NA)

    pow <- profile[J(itin.scen,veh.type)]$power / 1e3 # convert to MW
    pow.shiftable <- pow * profile[J(itin.scen,veh.type)]$frac.shiftable # convert to MW
    ann.profile$power.shiftable <- c(rep(pow.shiftable,365/3),pow.shiftable[1:48])
    ann.profile$power <- c(rep(pow,365/3),pow[1:48])

    weekend.inds <- c()
    for(i in 1:52){
      weekend.inds <- c(weekend.inds,(1:48)+(i-1)*(24*7)) 
    }
    weekend.inds <- weekend.inds + 72 # the 72 accounts for 1/1/2025 being a Wednesday

    # Based on PM measurements in Delhi:
    #http://www.academia.edu/9093533/WEEKDAY_WEEKEND_DIFFERENCES_IN_AIR_QUALITY_PARAMETERS_IN_DELHI_INDIA
    ann.profile$power[weekend.inds] <- ann.profile$power[weekend.inds] * 0.87
    ann.profile$power.shiftable[weekend.inds] <- ann.profile$power.shiftable[weekend.inds] * 0.87

    ann.profile$hour  <- rep(1:24,365)
    ann.profile$day   <- rep(1:365,each=24)

    plexos.format <- acast(melt(ann.profile,id.vars=c('hour','day')),day ~ hour ~ variable)

    itin.scen.named <- ifelse(itin.scen=='half-homeless','half-home-charging','full-home-charging')
    #write.csv(plexos.format[,,2],file=pp(path.to.outputs,'/pevi-load-for-plexos-',itin.scen.named,'-',veh.type,'.csv'))
    #write.csv(plexos.format[,,1],file=pp(path.to.outputs,'/pevi-shiftable-load-for-plexos-',itin.scen.named,'-',veh.type,'.csv'))
  }
}

setkey(charging,itin.scenario,vehicle.type)
#write.csv(charging[,list(avg.charge.duration=mean(duration),median.charge.duration=median(duration),sd.charge.duration=sd(duration),min.charge.duration=min(duration),max.charge.duration=max(duration)),by=c('itin.scenario','vehicle.type')],file=pp(path.to.outputs,'/charging-stats-by-vehicle-type.csv'))
ggplot(charging,aes(x=duration,y=..density..))+geom_histogram()+facet_grid(itin.scenario~vehicle.type,scales='free_x')


# Analzing results to make sure they keep with the RITES survey data

setkey(trips,driver,vehicle.type)
journey.dists <- trips[,list(dist=sum(distance)),by=c('driver','vehicle.type')]
ggplot(journey.dists,aes(x=dist))+geom_histogram()+facet_wrap(~vehicle.type)+labs(x="Distance (km)",y="Count",title=pp("Travel Distances from PEVI output (n=",nrow(journey.dists),")"))+scale_x_continuous(limits=c(0,100))
sum(journey.dists$dist>41.3)/nrow(journey.dists) # 8.5% of journey dists are greater than range of two-wheeler

# key totals
sum(trips$distance)   # 11.25M km
sum(trips$elec.used)  #  415 MWh
sum(charging$energy)  # 1246 MWh

# do all drivers charge?

all(u(charging$driver) %in% u(trips$driver)) # true
all(u(trips$driver) %in% u(charging$driver)) # false
sum(!u(trips$driver) %in% u(charging$driver)) # 94k or 12% of all drivers
length(u(trips$driver)) # 767k
setkey(pain,pain.type); pain[,list(length(driver)),by='pain.type']
          #pain.type     V1
#1:            delay 138550
#2:           denial 112076
#3:         stranded  63760
#4: unscheduled-trip  70098

setkey(trips,driver,vehicle.type)


# Look at energy gained/lost based solely on beginning/ending SOC
charging[,battery.cap:=batt.caps[vehicle.type]]
setkey(charging,driver)
socs <- charging[,list(begin.soc=begin.soc[1],end.soc=tail(end.soc,1),type=vehicle.type[1]),by='driver']
socs[,battery.cap:=batt.caps[type]]
socs[,net.energy.diff:=(end.soc - begin.soc)*battery.cap]
length(u(socs$driver)) # 673k
mean(socs$net.energy.diff) # 1.5 kWh per driver
sum(socs$net.energy.diff)/1e3
# total net increase in charge for drivers that *do* charge is 1026 MWh

non.charging.drivers <- u(trips$driver)[!u(trips$driver) %in% u(charging$driver)]
socs2 <- trips[driver %in% non.charging.drivers,list(begin.soc=begin.soc[1],end.soc=tail(end.soc,1),type=vehicle.type[1]),by='driver']
socs2[,battery.cap:=batt.caps[type]]
socs2[,net.energy.diff:=(end.soc - begin.soc)*battery.cap]
length(u(socs2$driver)) # 94k
mean(socs2$net.energy.diff) # -0.25 kWh per driver
sum(socs2$net.energy.diff)/1e3
# total net increase in charge for drivers that *do NOT* charge is -23 MWh

# total charge delivered to drivers who plug-in at beginning of model t = -6
length(u(charging[time==-6]$driver)) # 249k, 32.5%
sum(charging[time==-6]$energy)/1e3 # 385 MWh

# net gain in energy after that first charge acheived: 1026 - 23 - 385 = 618 MWh


