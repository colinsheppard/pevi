
exp.name <- 'smart-charging-demand'
path.to.inputs <- pp(pevi.shared,'data/inputs/compare/',exp.name,'/')
path.to.outputs <- pp(pevi.shared,'data/DELHI/results/managed-charging')

driver.schedules <- data.table(read.csv(pp(pevi.shared,'/data/inputs/compare/smart-charging-demand/driver-schedule-pen2-rep10-20140129.csv')))
setkey(driver.schedules,driver,depart)
driver.home <- driver.schedules[match(unique(driver.schedules[,driver]),driver.schedules[,driver]),home,by=driver]

# params used in the model run and needed here
kwh.per.mile <- 0.35
fact.safety <- 1.1
batt.caps <- array(c(25,13.3),dimnames=list(c('leaf','volt')))
charger.power <- array(c(6.6,6.6,50),dimnames=list(c('L0','L2','L3')))

# summarize / list what's there
#for(factor.col in grep('named',names(logs[['results']]),value=T)){
  #print(factor.col)
  #print(unique(logs[['results']][,factor.col]))
#}
#print(unique(logs[['results']]$replicate))
#print(unique(logs[['results']]$penetration))

# for this analysis, pull just pen 2% and rep 1 from the mix
pain <- data.table(read.csv(pp(path.to.inputs,'pain-out.csv')),key='driver')
pain[,':='(time=time-6)]
stranded.drivers <- unique(pain[pain.type=='stranded']$driver)
trips <- data.table(subset(read.csv(pp(path.to.inputs,'trip-out.csv')), !driver%in%stranded.drivers))
trips[,':='(time=time-6,end.time=end.time-6)]
#tazs <- data.table(subset(read.csv(pp(path.to.inputs,'tazs-out.csv')), time >= 5.99999 & time <= 36),key='taz')
#tazs[,':='(time=time-6)]
charging <- data.table(subset(read.csv(pp(path.to.inputs,'charging-out.csv')), !driver%in%stranded.drivers))
charging[,':='(time=time-6)]

trips <- trips[vehicle.type=='leaf']
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
tot.energy <- 50*750e3*.033 # 50 km/day, 750k drivers, 33Wh/km 
profile[,power:=power*tot.energy/sum(power)/1000] # now in MW

# now convert to Plexos format
ann.profile <- data.frame(hour=1:8760,frac.shiftable=NA,power=NA)

ann.profile$frac.shiftable <- rep(profile$frac.shiftable,365)
ann.profile$power <- rep(profile$power,365)

weekend.inds <- c()
for(i in 1:52){
  if(i%%2==0){
    weekend.inds <- c(weekend.inds,(25:48)+(i-2)*(24*7))
  }else{
    weekend.inds <- c(weekend.inds,(1:24)+(i-1)*(24*7))
  }
}
# Based on PM measurements in Delhi:
#http://www.academia.edu/9093533/WEEKDAY_WEEKEND_DIFFERENCES_IN_AIR_QUALITY_PARAMETERS_IN_DELHI_INDIA
ann.profile$power[weekend.inds] <- ann.profile$power[weekend.inds] * 0.87

ann.profile$hour  <- rep(1:24,365)
ann.profile$day   <- rep(1:365,each=24)

plexos.format <- cast(melt(ann.profile,id.vars=c('hour','day')),day ~ hour ~ variable)

write.csv(plexos.format[,,2],file=pp(path.to.outputs,'/plexos-load.csv'))
write.csv(plexos.format[,,1],file=pp(path.to.outputs,'/plexos-frac-shiftable.csv'))


