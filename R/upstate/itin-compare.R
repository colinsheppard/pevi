load.libraries(c('data.table'))
gpclibPermit()
registerDoMC(num.cpu)

make.plots  <- T

path.to.outputs <- pp(pevi.shared,'~/data/inputs/driver-input-file/upstate-uncombined/')
path.to.plots <- pp(pevi.nondrop,'itin-plots/')

pev.pens <- c(0.005,0.01,0.02,0.04)
replicate <- 1
source(pp(pevi.home,'R/upstate/itin-functions.R',sep=''))

# load od.agg.all, agg.taz.data, time.distance
load(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-agg-tricounty.Rdata'))
load(pp(pevi.shared,'data/UPSTATE/od.converter.Rdata'))
time.distance[,':='(from=od.converter$old.id[match(from,od.converter$new.id)],to=od.converter$old.id[match(to,od.converter$new.id)])]
# make sure time.distance does not have dups
setkey(time.distance,'from','to')
time.distance <- unique(time.distance)
time.distance$miles.int <- round(time.distance$miles)
time.distance[,ft:=pp(from,' ',to)]

num.replicates <- 80
time.distance$ft <- pp(time.distance$from,' ',time.distance$to)
date.code <- '20140129'
pev.penetration <- 0.005
pev.pen.char <- roundC(pev.penetration,3)
load(file=pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined-schedule-replicates-',date.code,'.Rdata',sep=''))

# melt the reps into one data.table
schedule.reps <- data.table(ldply(schedule.reps[['0.005']],function(df){ df }))
schedule.reps[,':='(rep=`.id`,`.id`=NULL)]
schedule.reps[,':='(ft=pp(abs(from),' ',abs(to)))]

setkey(time.distance,'ft')
setkey(schedule.reps,'ft')

schedule.reps <- time.distance[schedule.reps]
schedule.reps.up <- schedule.reps

# now load the comparable data from the Humboldt model

do.or.load(pp(pevi.shared,'/data/inputs/driver-input-file/humboldt-uncombined-schedule-replicates-20130219.Rdata'),function(){
  schedule.reps <- data.frame()
  for(i in 1:num.replicates){
    tmp <- read.table(pp(pevi.shared,'/data/inputs/driver-input-file/humboldt-combined/driver-schedule-pen0.5-rep',i,'-20130219.txt'),header=T)
    tmp$rep <- i
    schedule.reps <- rbind(schedule.reps,tmp)
  }
  list('schedule.reps'=schedule.reps)
})

schedule.reps.hum <- data.table(schedule.reps)
schedule.reps.hum[,':='(driver=X.driver,X.driver=NULL)]
hum.dt <- data.table(read.csv(file=pp(pevi.shared,'data/GEATM-2020/taz-dist-time.csv',sep='')))
hum.dt[,':='(from=X.from,X.from=NULL)]
setkey(hum.dt,'from','to')
setkey(schedule.reps.hum,'from','to')

schedule.reps.hum <- hum.dt[schedule.reps.hum]

schedule.reps.hum[,':='(model='Humboldt',arrive=depart+time,hours=time)]
schedule.reps.up$model <- 'Upstate'

schedule.reps <- rbind(schedule.reps.hum[,list(model,rep,driver,from,to,depart,arrive,miles,hours)],schedule.reps.up[,list(model,rep,driver,from,to,depart,arrive,miles,hours)])

setkey(schedule.reps,'model','rep','driver','depart')
schedule.reps[,dwell:=c(NA,tail(depart,-1) - head(arrive,-1)),by=c('model','rep','driver')]

ggplot(schedule.reps,aes(x=miles,y=..density..)) + geom_histogram() + geom_vline(data=data.frame(model=c('Upstate','Humboldt'),x=c(schedule.reps[J('Upstate'),mean(miles)]$V1, schedule.reps[J('Humboldt'),mean(miles)]$V1)),aes(xintercept=x),col='red') + facet_wrap(~model)+ labs(x="Trip Distance (miles)",y="Density",title="Trip Distances by Model") 
ggplot(schedule.reps,aes(x=dwell,y=..density..)) + geom_histogram() + geom_vline(data=data.frame(model=c('Upstate','Humboldt'),x=c(schedule.reps[J('Upstate'),mean(dwell,na.rm=T)]$V1, schedule.reps[J('Humboldt'),mean(dwell,na.rm=T)]$V1)),aes(xintercept=x),col='red') + facet_wrap(~model)+ labs(x="Dwell Time (hours)",y="Density",title="Dwell Times by Model")

ambition <- schedule.reps[,list(tot.miles=sum(miles),tot.dwell=sum(dwell,na.rm=T),max.trip=max(miles)),by=c('model','rep','driver')]
setkey(ambition,'model')

ggplot(ambition,aes(x=tot.miles,y=..density..)) + geom_histogram() + geom_vline(data=data.frame(model=c('Upstate','Humboldt'),x=c(ambition[J('Upstate'),mean(tot.miles,na.rm=T)]$V1, ambition[J('Humboldt'),mean(tot.miles,na.rm=T)]$V1)),aes(xintercept=x),col='red') + facet_wrap(~model)+ labs(x="Daily Travel Distance (miles)",y="Density",title="Total per Driver Daily Travel Distance")

ggplot(ambition,aes(x=tot.miles,y=tot.dwell,colour=max.trip)) + geom_point(alpha=0.5) + facet_wrap(~model) + labs(x="Total Distance (miles)",y="Total Dwell Time (hours)",title="Total per Driver Daily Dwell Time vs Travel Distance",colour='Maximum Trip Distance')

load(file=pp(pevi.shared,'data/CHTS/nssr-subset.Rdata'))
summary(nssr.place$tripdistance)
setkey(nssr.place,'sampn','perno')
by.per <- nssr.place[,list(tot.dist=sum(tripdistance)),by=c('sampn','perno')]
summary(by.per$tot.dist)

load(file=pp(pevi.shared,'data/UPSTATE/itin-generation/data-preprocessed.Rdata'))
summary(rur.tours$TOT_MILS)
setkey(rur.tours,'journey.id')
by.per <- rur.tours[,list(tot.dist=sum(TOT_MILS)),by=c('journey.id')]
summary(by.per$tot.dist)


# Humboldt 
    #model               dwell      
 #Length:168585      Min.   :-1.20  
 #Class :character   1st Qu.: 1.18  
 #Mode  :character   Median : 2.42  
                    #Mean   : 3.54  
                    #3rd Qu.: 5.08  
                    #Max.   :20.54  
                    #NA's   :62714  
    #model               miles         
 #Length:168585      Min.   :  0.1273  
 #Class :character   1st Qu.:  1.5500  
 #Mode  :character   Median :  3.5000  
                    #Mean   :  7.3253  
                    #3rd Qu.:  8.4000  
                    #Max.   :146.7600  
    #model             tot.miles       
 #Length:62714       Min.   :  0.1273  
 #Class :character   1st Qu.:  5.2800  
 #Mode  :character   Median : 10.9100  
                    #Mean   : 19.6915  
                    #3rd Qu.: 25.1600  
                    #Max.   :305.6700

# Upstate
    #model               dwell       
 #Length:450865      Min.   : 0.00   
 #Class :character   1st Qu.: 0.97   
 #Mode  :character   Median : 1.61   
                    #Mean   : 2.50   
                    #3rd Qu.: 3.21   
                    #Max.   :22.34   
                    #NA's   :106759  

    #model               miles          
 #Length:450865      Min.   :  0.00019  
 #Class :character   1st Qu.:  1.30000  
 #Mode  :character   Median :  4.20000  
                    #Mean   : 14.89251  
                    #3rd Qu.: 11.30000  
                    #Max.   :178.00000  
    #model             tot.miles       
 #Length:106759      Min.   :  0.0002  
 #Class :character   1st Qu.: 15.0004  
 #Mode  :character   Median : 29.0000  
                    #Mean   : 62.8941  
                    #3rd Qu.: 86.2002  
                    #Max.   :651.8000  

# NSSR
#> summary(nssr.place$tripdistance)
   #Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #0.000   1.429   3.470  11.120  10.080 670.100     124 
#> summary(by.per$tot.dist)
   #Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  #0.000   7.778  19.830  42.050  46.580 924.800     102 

# NHTS
#summary(rur.tours$TOT_MILS)
    #Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
   #0.111    4.000   10.000   17.860   20.000 3620.000 
#> summary(by.per$tot.dist)
    #Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
   #0.111   16.000   32.000   51.700   60.000 3645.000 

