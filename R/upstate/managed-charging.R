
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
pain <- data.table(subset(logs[['pain']],penetration==2 & replicate==1),key='driver')
pain[,':='(time=time-6)]
stranded.drivers <- unique(pain[pain.type=='stranded']$driver)
trips <- data.table(subset(logs[['trip']],penetration==2 & replicate==1 & !driver%in%stranded.drivers))
trips[,':='(driver.input.file=NULL,charger.input.file=NULL,time=time-6,end.time=end.time-6)]
tazs <- data.table(subset(logs[['tazs']],penetration==2 & replicate==1 & time >= 5.99999 & time <= 36),key='taz')
tazs[,':='(driver.input.file=NULL,charger.input.file=NULL,time=time-6)]
charging <- data.table(subset(logs[['charging']],penetration==2 & replicate==1 & !driver%in%stranded.drivers))
charging[,':='(driver.input.file=NULL,charger.input.file=NULL,time=time-6)]

# time.step defines the discritization used to bin charger availability (in hours)
time.step <- 5/60
time.steps <- seq(0,30,by=time.step)
soc.per.time.step <- array(charger.power*time.step/batt.caps,dimnames=list(c('leaf','volt')))

# Step 1: Determine the amount of charging from level 3 charger
# Step 2: Eliminate that range from the subsequent distances
# Step 3: Determine what the delta.soc would be for each trip. This value will be 0 for the first trip of the day.

### I hate nested for loops - is there a way we can remove any of these?

setkey(charging,charger.level,driver)
for(level.3.driver in unique(charging[J(3)]$driver)) {
  for(row in 1:nrow(charging[J(3,level.3.driver)])){
    #### We should get the driver's actual fuel economy, in either the need-to-charge or the wait-time logs. For now, just assume it is 0.35 kWh/mile.
    # Convert the level 3 charge delivered into VMT
    vmt <- charging[J(3,level.3.driver)]$energy[row] / kwh.per.mile
    charge.start <- charging[J(3,level.3.driver)]$time[row]
    
    # Subtract vmt from trip distances from subsequent trips
    trip.inds <- which(trips$driver==level.3.driver & trips$time>charge.start & trips$distance>0)
    setkey(trips,time)
    while (vmt>0&length(trip.inds)>0) {
      if(trips[trip.inds,distance][1]<vmt) {
        # Subtract trip from VMT and set trip to 0
        vmt <- vmt - trips[trip.inds,distance][1]
        trips[trip.inds[1],distance:=0]
      } else {
        # Subtract vmt from trip and set vmt to 0
        trips[trip.inds[1],distance:=distance - vmt]
        vmt <- 0
      }
      trip.inds <- trip.inds[-1]
    } # end while loop
  } # end for loop - row
} # end for loop - level.3.driver

# what soc is needed for each trip
trips[,soc.needed:=distance*kwh.per.mile*fact.safety/batt.caps[vehicle.type]]
trips[vehicle.type=='volt',soc.needed:=0]

# Calculate the delta.soc for each trip. The first trip will have a delta.soc of 0. subsequent trips are the soc.needed - end.soc from previous trip.
setkey(trips,driver)
trips[,':='(delta.soc=0,total.trips=length(distance)),by=driver]

# For any driver with more than 1 trip, delta.soc = soc.needed - end.soc from previous trip.
trips[total.trips>1,delta.soc:=as.vector(c(0,soc.needed[2:length(distance)]-end.soc[1:(length(distance)-1)])),by=driver]
# A negative delta.soc means they had more charge from the previous trip than they needed, so we set those delta.socs to 0.
trips[delta.soc<0,delta.soc:=0]

# grab pricing data

# for now fake it, this is price on 5 minute increments for 30 hours
price <- c(8,rep(c(8,9,7,8,9,10,11,12,14,13,12,16,17,20,24,28,32,29,25,21,18,16,10,9,8,9,7,8,9,10),each=round(1/time.step)))
names(price) <- roundC(time.steps,2)

# cost of energy by driver
setkey(charging,driver)
time.to.index <- function(t){
  round(t*(1/time.step))+1
}
cost.of.energy <- charging[,list(cost=sum(unlist(lapply(alply(cbind(time,duration),1,function(x){ seq(time.to.index(x[1]),time.to.index(sum(x))) }),function(ll){ sum(price[ll],na.rm=T)/(1/time.step) })))),by='driver']

# for development
#energy <- charging[J(12)]$energy
#time <- charging[J(12)]$time
#duration <- charging[J(12)]$duration
#energy <- charging[J(5266)]$energy
#time <- charging[J(5266)]$time
#duration <- charging[J(5266)]$duration
#sum(unlist(lapply(alply(cbind(time,duration),1,function(x){ seq(time.to.index(x[1]),time.to.index(sum(x))) }),function(ll){ sum(price[ll],na.rm=T)/(1/time.step) })))

# I think we decided that ranking drivers was unecessary, so long as we handle all driver's needed charging first.

# Driver's initial charge assumed to be begin.soc at start of first trip. Do not assign any charging BEFORE that point.

# Use delta.soc values to determine if pre-trip charge is needed. Find the time window we can work in, this will be bounded by trips schedule. Each delta.soc is attached to a given trip; window is for the last arrival to the current departure time 
# and level 3 charging (can't use those; they have already been eliminated). The charging window will be an array of indices corresponding to the 5 minute intervals, use the pricing array to sort. Go down the price-sorted array - is a chaerger avaialble? Great. Schedule it, reduce the 
# remaining delta.soc. Continue until we are done with delta.soc. This will be the same process with the accounting charging, except that we lose the scedule constraints.

# Start a for loop for all drivers who have a positive delta.soc somewhere in their day
setkey(trips,driver)

# Test with driver 262 - multiple delta.socs
this.driver <- 262

# make the charger availability matrix, all Level 2!!!
avail <- array(NA,c(length(unique(tazs$taz)),length(time.steps)),list(as.character(unique(tazs$taz)),roundC(time.steps,2)))
num.ch <- array(NA,length(unique(tazs$taz)),list(as.character(unique(tazs$taz))))
for(this.taz in unique(tazs$taz)){
  avail[as.character(this.taz),] <- tazs[J(this.taz)]$num.L2 # Initialize all chargers as "available" which will then get used up as the alg proceeds
  num.ch[as.character(this.taz)] <- tazs[J(this.taz)][1]$num.L2
}

# Ordering the drivers matters, some drivers have less flexibility and need to go first.  delta.soc is correlated with
# degree of flexibility (higher soc -> less flexible) but not a certain predictor.  For now, let's start with that and
# then custom prioritize the drivers with problems. 
trips[,loop.order:=-delta.soc]
trips[J(c(338)),loop.order:=-1]

# Loop through drivers in reverse order of delta.soc which prioritizes drivers with a lot of charge to git
for (this.driver in unique(trips[delta.soc>0&origin>0]$driver[order(trips[delta.soc>0&origin>0]$loop.order)])) {
  print(pp('driver ',this.driver))
  this.vehicle.type <- trips[J(this.driver)]$vehicle.type[1]
  this.driver.trips <- trips[J(this.driver)]
  for(trip.i in which(this.driver.trips$delta.soc>0)) {
    charging.period.end <- this.driver.trips[trip.i,time]
    charging.period.begin <- tail(this.driver.trips[time<charging.period.end,time],1)
    charging.period.taz <- as.character(this.driver.trips[trip.i,origin])
    
    # Determine the array of 5 minute time windows that falls within the charging period.
    charge.period.indices <- which(time.steps>charging.period.begin&time.steps<charging.period.end)

    ##### TODO
    # Exclude intervals when this driver was in a Level 3 charge event
    ##### /TODO
    
    # Reduce the window of charging based on charger availability.
    charge.period.indices <- charge.period.indices[avail[charging.period.taz,charge.period.indices]>0]
    
    # Determine energy price for each charge period index. Rank. Take top ranked periods.
    # Make chargers unavailable.
    num.time.periods.needed <- floor(this.driver.trips[trip.i,delta.soc]/soc.per.time.step[this.vehicle.type])
    if(num.time.periods.needed > length(charge.period.indices))stop("Error: the charging window isn't big enough for this driver to charge!")
    time.periods.to.use <- order(price[charge.period.indices])[1:num.time.periods.needed]
    avail[charging.period.taz,charge.period.indices[time.periods.to.use]] <- avail[charging.period.taz,charge.period.indices[time.periods.to.use]] - 1
    if(any(avail<0))stop("Bad avail, bad avail")
  } #end for each trip with delta.soc to distribute 
} # end for loop this.driver

# generator charging profiles
setkey(tazs,time)
prof<-tazs[,list(L0.kw=6.6*sum(num.L0-num.avail.L0),L2.kw=6.6*sum(num.L2-num.avail.L2),L3.kw=50*sum(num.L3-num.avail.L3)),by='time']
prof[,hour:=floor(time)]
setkey(prof,hour)
prof<-prof[,list(load=sum(L0.kw+L3.kw+L2.kw)),by='hour']

# Now come up with plots to show major events (driver, charging events, trip,s etc)

write.csv(prof,file=pp(path.to.outputs,'load-profile.csv'))



