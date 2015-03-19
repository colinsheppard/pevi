load.libraries('reshape2')
exp.name <- 'delhi-smart-charging-demand'
path.to.inputs <- pp(pevi.shared,'data/inputs/compare/',exp.name,'/')
path.to.outputs <- pp(pevi.shared,'data/DELHI/results/managed-charging')
load(paste(path.to.inputs,'logs.Rdata',sep=''))

driver.schedules <- data.table(read.csv(pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/no-homeless/driver-schedule-pen5-rep1-20150212.txt'),sep='\t'))
driver.schedules[,':='(driver=X.X.driver,X.X.driver=NULL)]
setkey(driver.schedules,driver,depart)
driver.home <- driver.schedules[match(unique(driver.schedules[,driver]),driver.schedules[,driver]),home,by=driver]

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
setkey(trips,driver)
trips[,soc.needed:=elec.used*fact.safety/batt.caps[vehicle.type]]

# Calculate the delta.soc for each trip. The first trip will have a delta.soc of 0. subsequent trips are the soc.needed - end.soc from previous trip.
trips[,':='(delta.soc=0,total.trips=length(distance)),by=driver]

# For any driver with more than 1 trip, delta.soc = soc.needed - end.soc from previous trip.
trips[total.trips>1,delta.soc:=as.vector(c(0,soc.needed[2:length(distance)]-end.soc[1:(length(distance)-1)])),by=driver]
# A negative delta.soc means they had more charge from the previous trip than they needed, so we set those delta.socs to 0.
trips[delta.soc<0,delta.soc:=0]

needy.trips <- trips[delta.soc>0]

setkey(needy.trips,driver)
setkey(charging,driver)

ch.cands <- charging[needy.trips]

ch.cands[,trip.charge.time.diff:=i.time-time]
ch.cands[,trip.charge.time.diff:=ifelse(trip.charge.time.diff<0,Inf,trip.charge.time.diff)]
ch.cands[,most.recent:=rank(trip.charge.time.diff),by='driver']
needy.charges <- ch.cands[most.recent==1]

needy.charges[,end.needy.time:=time + soc.needed * batt.caps[vehicle.type] / charger.power[pp('L',charger.level)]]
needy.charges[,row:=1:length(time)]

non.curtailable.charge <- ddply(needy.charges,.(row),function(df){ 
      pow.hrs <- unlist(floor(df['time'])):unlist(floor(df['end.needy.time']))
      if(length(pow.hrs) > 1){
        fractional.pows <- charger.power[pp('L',df['charger.level'])] * unlist(c(ceiling(df['time']) - df['time'],df['end.needy.time'] - floor(df['end.needy.time'])))
        pow <- c(fractional.pows[1],rep(charger.power[pp('L',df['charger.level'])],length(pow.hrs)-2),fractional.pows[2])
      }else{
        pow <- charger.power[pp('L',df['charger.level'])] * (df['end.needy.time']-df['time'])
      }
      data.frame(hour=pow.hrs,power=unlist(pow))
})
non.curtailable.profile <- data.table(ddply(non.curtailable.charge,.(hour),function(df){ data.frame(power=sum(df$power)) }),key='hour')

charging[,end.time:=time+duration]
charging[,row:=1:length(time)]

all.charge <- ddply(charging,.(row),function(df){ 
      pow.hrs <- unlist(floor(df['time'])):unlist(floor(df['end.time']))
      if(length(pow.hrs) > 1){
        fractional.pows <- charger.power[pp('L',df['charger.level'])] * unlist(c(ceiling(df['time']) - df['time'],df['end.time'] - floor(df['end.time'])))
        pow <- c(fractional.pows[1],rep(charger.power[pp('L',df['charger.level'])],length(pow.hrs)-2),fractional.pows[2])
      }else{
        pow <- charger.power[pp('L',df['charger.level'])] * (df['end.time']-df['time'])
      }
      data.frame(hour=pow.hrs,power=unlist(pow))
})

all.profile <- data.table(ddply(all.charge,.(hour),function(df){ data.frame(all.power=sum(df$power)) }),key='hour')

profile <- non.curtailable.profile[all.profile]
profile[,frac.shiftable:=(all.power-power)/all.power]
profile[,frac.shiftable:=ifelse(is.na(frac.shiftable),1,frac.shiftable)]
profile <- profile[hour>3 & hour<28] 
profile[hour>23,hour:=hour-24]
setkey(profile,hour)
profile[,power:=NULL]
profile[,power:=all.power]
profile[,all.power:=NULL]

# scale the power up to something in the ballpark
#tot.energy <- 50*750e3*.033 # 50 km/day, 750k drivers, 33Wh/km 
#profile[,power:=power*tot.energy/sum(power)/1000] # now in MW

# now convert to Plexos format
ann.profile <- data.frame(hour=1:8760,frac.shiftable=NA,power=NA)

ann.profile$frac.shiftable <- rep(profile$frac.shiftable,365)
ann.profile$power <- rep(profile$power,365)

weekend.inds <- c()
for(i in 1:52){
  weekend.inds <- c(weekend.inds,(1:48)+(i-1)*(24*7))
}
# Based on PM measurements in Delhi:
#http://www.academia.edu/9093533/WEEKDAY_WEEKEND_DIFFERENCES_IN_AIR_QUALITY_PARAMETERS_IN_DELHI_INDIA
ann.profile$power[weekend.inds] <- ann.profile$power[weekend.inds] * 0.87

ann.profile$hour  <- rep(1:24,365)
ann.profile$day   <- rep(1:365,each=24)

plexos.format <- acast(melt(ann.profile,id.vars=c('hour','day')),day ~ hour ~ variable)

write.csv(plexos.format[,,2],file=pp(path.to.outputs,'/plexos-load.csv'))
write.csv(plexos.format[,,1],file=pp(path.to.outputs,'/plexos-frac-shiftable.csv'))


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


