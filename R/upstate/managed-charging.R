
exp.name <- 'smart-charging-demand'
path.to.inputs <- pp(pevi.shared,'data/inputs/compare/',exp.name,'/')
path.to.outputs <- pp(pevi.shared,'data/UPSTATE/results/managed-charging')

load(file=paste(path.to.inputs,'logs.Rdata',sep=''),verbose=T)

# params used in the model run and needed here
kwh.per.mile <- 0.35
fact.safety <- 1.1
batt.caps <- array(c(25,13.3),dimnames=list(c('leaf','volt')))
charger.power <- 6.6

# summarize / list what's there
for(factor.col in grep('named',names(logs[['results']]),value=T)){
  print(factor.col)
  print(unique(logs[['results']][,factor.col]))
}
print(unique(logs[['results']]$replicate))
print(unique(logs[['results']]$penetration))

# for this analysis, pull just pen 2% and rep 1 from the mix
trips <- data.table(subset(logs[['trip']],penetration==2 & replicate==1))
trips[,':='(driver.input.file=NULL,charger.input.file=NULL,time=time-6)]
tazs <- data.table(subset(logs[['tazs']],penetration==2 & replicate==1 & taz>=1 & time >= 5.99999 & time <= 30),key='taz')
tazs[,':='(driver.input.file=NULL,charger.input.file=NULL,time=time-6)]
charging <- data.table(subset(logs[['charging']],penetration==2 & replicate==1))
charging[,':='(driver.input.file=NULL,charger.input.file=NULL,time=time-6)]

# time.step defines the discritization used to bin charger availability (in hours)
time.step <- 5/60
time.steps <- seq(0,24,by=time.step)



# make the charger availability matrix
avail <- array(NA,c(length(unique(tazs$taz)),length(time.steps),2),list(as.character(unique(tazs$taz)),as.character(1:length(time.steps)),c('2','3')))
num.ch <- array(NA,c(length(unique(tazs$taz)),2),list(as.character(unique(tazs$taz)),c('2','3')))
for(level in 2:3){
  for(this.taz in unique(tazs$taz)){
    avail[this.taz,,as.character(level)] <- streval(pp('tazs[J(',this.taz,'),]$num.L',level)) # This will get us a fresh slate that we can fill up our own way.
#    avail[this.taz,,as.character(level)] <- streval(pp('tazs[J(',this.taz,'),]$num.avail.L',level))
    num.ch[this.taz,as.character(level)] <- streval(pp('tazs[J(',this.taz,')][1]$num.L',level))
  }
}

# Step 1: Determine the amount of charging from level 3 charger
# Step 2: Eliminate that range from the subsequent distances
# Step 3: Determine what the deltaT would be for each trip. This value will be 0 for the first trip of the day.

### I hate nested for loops - is there a way we can remove any of these?

for(level.3.driver in unique(charging[charger.level==3,driver])) {
  for (row in nrow(charging[charger.level==3&driver==level.3.driver,])) {
    #### We should get the driver's actual fuel economy, in either the need-to-charge or the wait-time logs. For now, just assume it is 0.35 kWh/mile.
    # Convert the level 3 charge delivered into VMT
    vmt <- charging[charger.level==3&driver==level.3.driver,energy][row] / kwh.per.mile
    charge.start <- charging[charger.level==3&driver==level.3.driver,time][row]
    
    # Subtract vmt from trip distances from subsequent trips
    while (vmt>0&nrow(trips[driver==level.3.driver&time>charge.start&distance>0,])>0) {
      min.time <- min(trips[driver==level.3.driver&time>charge.start&distance>0,time])
      if(trips[driver==level.3.driver&time>charge.start&distance>0,distance][1]<vmt) {
        # Subtract trip from VMT and set trip to 0
        vmt <- vmt - trips[driver==level.3.driver&time>charge.start&distance>0&time==min.time,distance]
        trips[driver==level.3.driver&time>charge.start&distance>0&time==min.time,distance:=0]
      } else {
        # Subtract vmt from trip and set vmt to 0
        trips[driver==level.3.driver&time>charge.start&distance>0&time==min.time,distance:=distance - vmt]
        vmt <- 0
      }
    } # end while loop
  } # end for loop - row
} # end for loop - level.3.driver

# what soc is needed for each trip
trips[,soc.needed:=distance*kwh.per.mile*fact.safety/batt.caps[vehicle.type]]
trips[vehicle.type=='volt',soc.needed:=0]

# Calculate the deltaT for each trip. The first trip will have a deltaT of 0. subsequent trips are the soc.needed - end.soc from previous trip.
trips[,':='(deltaT=0,total.trips=length(distance))]

# For any driver with at least 1 trip, deltaT = soc.needed - end.soc from previous trip.
trips[total.trips>1,deltaT:=as.vector(c(0,soc.needed[2:length(distance)]-end.soc[1:(length(distance)-1)])),by=driver]
# A negative deltaT means they had more charge from the previous trip than they needed, so we set those deltaTs to 0.
trips[deltaT<0,deltaT:=0]

# grab pricing data

# for now fake it, this is price on 5 minute increments for 24 hours
price <- c(8,rep(c(8,9,7,8,9,10,11,12,14,13,12,16,17,20,24,28,32,29,25,21,18,16,10,9),each=round(1/time.step)))

# cost of energy by driver
setkey(charging,driver)


# for development
energy <- charging[J(12)]$energy
time <- charging[J(12)]$time
duration <- charging[J(12)]$duration
energy <- charging[J(5266)]$energy
time <- charging[J(5266)]$time
duration <- charging[J(5266)]$duration
time.to.index <- function(t){
  round(t*(1/time.step))+1
}
sum(unlist(lapply(alply(cbind(time,duration),1,function(x){ seq(time.to.index(x[1]),time.to.index(sum(x))) }),function(ll){ sum(price[ll],na.rm=T)/(1/time.step) })))

cost.of.energy <- charging[,list(cost=sum(unlist(lapply(alply(cbind(time,duration),1,function(x){ seq(time.to.index(x[1]),time.to.index(sum(x))) }),function(ll){ sum(price[ll],na.rm=T)/(1/time.step) })))),by='driver']

# I think we decided that ranking drivers was unecessary, so long as we handle all driver's needed charging first.

# Driver's initial charge assumed to be begin.soc at start of first trip. Do not assign any charging BEFORE that point.

# Use deltaT values to determine if pre-trip charge is needed. Find the time window we can work in, this will be bounded by trips schedule. Each deltaT is attached to a given trip; window is for the last arrival to the current departure time 
# and level 3 charging (can't use those; they have already been eliminated). The charging window will be an array of indices corresponding to the 5 minute intervals, use the pricing array to sort. Go down the price-sorted array - is a chaerger avaialble? Great. Schedule it, reduce the 
# remaining deltaT. Continue until we are done with deltaT. This will be the same process with the accounting charging, except that we lose the scedule constraints.

# Start a for loop for all drivers who have a positive deltaT somewhere in their day

# Test with driver 262 - multiple deltaTs

for (this.driver in unique(trips[deltaT>0&origin>0,driver])) {
print(pp('this driver is ',this.driver))
  while (length(trips[deltaT>0&driver==this.driver&origin>0,deltaT])>0) {
    charging.period.end <- trips[deltaT>0&driver==this.driver&origin>0,time][1]
    charging.period.begin <- trips[driver==this.driver&origin>0&time<charging.period.end,time][nrow(trips[driver==this.driver&origin>0&time<charging.period.end,])]
    charging.period.taz <- trips[deltaT>0&driver==this.driver&origin>0,origin][1]
    
    # Determine the array of 5 minute time windows that falls within the charging period.
    charge.period.indices <- match(time.steps[time.steps>charging.period.begin&time.steps<charging.period.end],time.steps)
    
    # Determine which periods have a charger available. Remember, only level 2.
    charge.period.indices <- charge.period.indices[avail[charging.period.taz,charge.period.indices,'2']>0]
    
    DelT <- trips[deltaT>0&driver==this.driver&origin>0,deltaT][1]
	print(pp('DelT is ',DelT))
	# Determine energy price for each charge period index. Rank.
	# Take top ranked period. Make charger unavailable. Subtract charger power*charge period length (5/60) from deltaT until deltaT is 0.
	while(DelT > 0) {
	  # Refresh indicies, rank again
	  charge.period.indices <- charge.period.indices[avail[charging.period.taz,charge.period.indices,'2']>0]
	  ranked.charge.periods <- data.table(charge.period=charge.period.indices,cost=price[charge.period.indices])
      setkey(ranked.charge.periods,cost)
	  avail[charging.period.taz,ranked.charge.periods[1,charge.period],'2'] <- 0
	  trips[driver==this.driver&origin>0&time==charging.period.end,deltaT:=deltaT-charger.power*time.step/batt.caps[vehicle.type]]
	  DelT <- trips[driver==this.driver&origin>0&time==charging.period.end,deltaT][1]
	} #end while DelT loop
  } #end while length loop
} # end for loop this.driver

# generator charging profiles
setkey(tazs,time)
prof<-tazs[,list(L0.kw=6.6*sum(num.L0-num.avail.L0),L2.kw=6.6*sum(num.L2-num.avail.L2),L3.kw=50*sum(num.L3-num.avail.L3)),by='time']
prof[,hour:=floor(time)]
setkey(prof,hour)
prof<-prof[,list(load=sum(L0.kw+L3.kw+L2.kw)),by='hour']

# Now come up with plots to show major events (driver, charging events, trip,s etc)

write.csv(prof,file=pp(path.to.outputs,'load-profile.csv'))



