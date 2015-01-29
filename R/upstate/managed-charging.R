
exp.name <- 'upstate-charging-demand'
path.to.inputs <- pp(pevi.shared,'data/inputs/compare/',exp.name,'/')
path.to.outputs <- pp(pevi.shared,'data/UPSTATE/results/managed-charging')

load(file=paste(path.to.inputs,'logs.Rdata',sep=''),verbose=T)

# params used in the model run and needed here
kwh.per.mile <- 0.35
fact.safety <- 1.1
batt.caps <- array(c(25,13.3),dimnames=list(c('leaf','volt')))

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
    avail[this.taz,,as.character(level)] <- streval(pp('tazs[J(',this.taz,'),]$num.avail.L',level))
    num.ch[this.taz,as.character(level)] <- streval(pp('tazs[J(',this.taz,')][1]$num.L',level))
  }
}

# what soc is needed for each trip
trips[,soc.needed:=distance*kwh.per.mile*fact.safety/batt.caps[vehicle.type]]
trips[vehicle.type=='volt',soc.needed:=0]

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

# generator charging profiles
setkey(tazs,time)
prof<-tazs[,list(L0.kw=6.6*sum(num.L0-num.avail.L0),L2.kw=6.6*sum(num.L2-num.avail.L2),L3.kw=50*sum(num.L3-num.avail.L3)),by='time']
prof[,hour:=floor(time)]
setkey(prof,hour)
prof<-prof[,list(load=sum(L0.kw+L3.kw+L2.kw)),by='hour']

write.csv(prof,file=pp(path.to.outputs,'load-profile.csv'))
