library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','RPostgreSQL','colorRamps','reshape'))
gpclibPermit()

path.to.leaf   <- '~/Dropbox/serc/pev-colin/data/leaf-performance/'
path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'
path.to.geatm  <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))

# get route.ordered and route.hists from distance-time analysis
load(file=paste(path.to.pevi,'inputs/routing-corrected.Rdata',sep=''))
disttime <- read.csv(file=paste(path.to.geatm,'taz-dist-time.csv',sep=''))
taz <- readShapePoly(paste(path.to.pevi,'inputs/development/aggregated-taz',sep=''))
load(paste(path.to.pevi,'inputs/development/aggregated-taz-fieldnames.Rdata',sep=''))
names(taz@data) <- c('row',agg.taz.shp.fieldnames)

# setup bins for gradient and speed
x.bins <- c(-0.6,seq(-0.05,0.05,by=0.01),0.6)
x.labs <- c("[-0.6,-0.05)","[-0.05,-0.04)","[-0.04,-0.03)","[-0.03,-0.02)","[-0.02,-0.01)","[-0.01,0.00)","[0.00,0.01)","[0.01,0.02)","[0.02,0.03)","[0.03,0.04)","[0.04,0.05)","[0.05,0.6)","NA")
y.bins <- c(0,25,40,50,60,70)
y.labs <- c("[0,25)","[25,40)","[40,50)","[50,60)","[60,70)","NA")

#"Alderpoint"
#"ARC_Giuntoli"
#"ARC_Greenview"
#"ARC_HSU"
#"ARC_North"
#"ARC_Plaza"
#"ARC_South"
#"ARC_SunnyBrae"
#"ARC_Westwood"
#"Bayside"
#"Bayview"
#"BlueLake"
#"Bottoms"
#"Bridgeville"
#"CollegeRedwoods"
#"Cutten"
#"EKA_E_Central"
#"EKA_Harris"
#"EKA_Harrison"
#"EKA_HendersonCenter"
#"EKA_N_Broadway"
#"EKA_NE101"
#"EKA_NW101"
#"EKA_S_Broadway"
#"EKA_Slough"
#"EKA_W_Central"
#"EKA_Waterfront"
#"Ferndale"
#"Fieldbrook"
#"FieldsLanding"
#"FOR_Central"
#"FOR_East"
#"FOR_North"
#"FOR_South"
#"Garberville"
#"HoopaKlamath"
#"Hydesville"
#"Loleta"
#"LostCoast"
#"MCK_Central"
#"MCK_North"
#"MCK_South"
#"Myrtletown"
#"Orick"
#"Redway"
#"RioDell"
#"Samoa"
#"Scotia"
#"ShelterCove"
#"Trinidad"
#"WestEnd"
#"WillowCreek"

# notes: the 40-49 mph speed bin has very little density at steep slopes unless you go to
# Shelter Cover or Alderpoint, so those can be safely ignored.  Fortun

taz.id <- 27 # EKA_Waterfront

planned.routes <- list( 
          list(from="EKA_Waterfront",to="WillowCreek"),
          list(from="WillowCreek",to="EKA_Waterfront"),
          list(from="EKA_Waterfront",to="LostCoast"),
          list(from="LostCoast",to="EKA_Waterfront"),
          list(from="EKA_Waterfront",to="Hydesville"),
          list(from="Hydesville",to="EKA_Waterfront"),
          list(from="EKA_Waterfront",to="Orick"),
          list(from="Orick",to="EKA_Waterfront"),
          list(from="EKA_Waterfront",to="CollegeRedwoods"),
          list(from="CollegeRedwoods",to="ARC_SunnyBrae"),
          list(from="ARC_SunnyBrae",to="EKA_Waterfront"),
          list())


tot.miles <- route.hists[,,,planned.routes[[1]]$from,planned.routes[[1]]$to]
for(i in 2:(length(planned.routes)-1)){
  tot.miles <- tot.miles + route.hists[,,,planned.routes[[i]]$from,planned.routes[[i]]$to]
}

tot.miles.begin <- tot.miles


ch <- read.csv(paste(path.to.leaf,'data/charging-event.csv',sep=''))
ch$hour <- (ch$minute + ch$second/60)/60

plot(ch$hour,ch$soc,type='l',main="GHD Leaf Charging Event - October 2012",xlab="Hour",ylab="State of Charge (%)",ylim=c(0,100)) abline(lm('soc ~ hour',ch),lty=2,col='red') text(-0.25,35,paste('r-squared: ',roundC(summary(lm('soc ~ hour',ch))$r.squared,3),sep=''),pos=4)
abline(h=max(ch$soc))
text(-0.25,max(ch$soc)-5,paste('max soc: ',roundC(max(ch$soc),2),sep=''),pos=4)

# load data and translate lat,lon to actually start at GHD
ghd <- c(-124.163904,40.803678)*100

dbuser<-"pev"
dbpassword<-""
dbname<-"pev"
dbhost<-"localhost"
con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	

dr <- data.frame(read.csv(paste(path.to.leaf,'data/bear-river-ridge-high.csv',sep='')),route="Bear River Ridge")
cr <- data.frame(read.csv(paste(path.to.leaf,'data/cr-sunny-brae-high.csv',sep='')),route="CR / Sunny Brae")
hy <- data.frame(read.csv(paste(path.to.leaf,'data/hydesville-high.csv',sep='')),route="Hydesville")
wc <- data.frame(read.csv(paste(path.to.leaf,'data/willow-creek-high.csv',sep='')),route="Willow Creek")

dr <- rbind(dr,cr,hy,wc)

names(dr) <- c("minutes","seconds","long","lat","elev","speed","soc","battery.volts","battery.amps","route")

# get rid of rows with bad values for lat/long
dr <- dr[-which(dr$lat == 0 | dr$long == 0),]

dr$lat <- as.integer(dr$lat/100) + (dr$lat - as.integer(dr$lat/100)*100)/60
dr$long <- as.integer(dr$long/100) + (dr$long - as.integer(dr$long/100)*100)/60
dr$hour <- (dr$minutes + dr$seconds/60)/60
dr$delta.t <- c(0,diff(dr$hour))
dr$int.sec <- as.integer(dr$hour*3600) - as.integer(dr$hour*3600) %% 15

dr <- ddply(dr,.(route,int.sec),function(df){ 
  data.frame(
    hour=df$hour[1],
    long=weighted.mean(df$long,df$delta.t,na.rm=T),
    lat=weighted.mean(df$lat,df$delta.t,na.rm=T),
    elev=weighted.mean(df$elev,df$delta.t,na.rm=T),
    speed=weighted.mean(df$speed,df$delta.t,na.rm=T),
    soc=weighted.mean(df$soc,df$delta.t,na.rm=T),
    battery.volts=weighted.mean(df$battery.volts,df$delta.t,na.rm=T),
    battery.amps=weighted.mean(df$battery.amps,df$delta.t,na.rm=T),
    delta.t=df$hour[nrow(df)]-df$hour[1])
})
dr <- subset(dr,delta.t*3600>=4.99)
dr$wh <- dr$battery.volts * dr$battery.amps * dr$delta.t

dr <- ddply(dr,.(route),function(df){ 
  df <- df[order(df$hour),]
  df$dist <- NA
  for(i in which(!is.na(df$lat[1:(nrow(df)-1)]) & !is.na(df$long[1:(nrow(df)-1)]) & !is.na(df$lat[2:nrow(df)]) & !is.na(df$long[2:nrow(df)]))){
    if(i==1){
      df$dist[i] <- 0
      next
    }
    # 26941 is California Zone I state plane projection in meters (then converted to miles using 0.00062137119)
    df$dist[i] <- as.numeric(0.00062137119 * dbGetQuery(con,paste("SELECT ST_Distance(
        ST_Transform(ST_GeomFromText('POINT(",df$long[i-1]," ",df$lat[i-1],")',4326),26941),
        ST_Transform(ST_GeomFromText('POINT(",df$long[i]," ",df$lat[i],")', 4326),26941)
      )",sep='')))
  }
  df$speed2 <- c(0,df$dist[2:nrow(df)] / diff(df$hour))
  df$cum.dist <- c(0,cumsum(df$dist[2:nrow(df)]))
  df
})
# nrow(subset(dr,dist==0))/nrow(dr)  #  3.9% of rows have 0 distance
# sum(abs(subset(dr,dist==0)$wh))/sum(abs(dr$wh))  # only 0.3% of energy is in rows with 0 distance 
# get rid of rows with 0 distance
dr <- dr[-which(dr$dist==0),]

dr$speed[dr$route=="Willow Creek"] <- dr$speed2[dr$route=="Willow Creek"]
dr$speed[dr$route=="Willow Creek" & dr$speed>60] <- 60

dr <- ddply(dr,.(route),function(df){
  df <- df[order(df$hour),]
  df$gradient <- c(0,diff(df$elev)/(df$dist[2:nrow(df)]*1609.344))
  df
})
# calculate performance for each row
dr$perf <- dr$wh / 1000 / dr$dist
dr <- na.omit(dr)

# look into unrealistic gradients (for 15 sec data):
# ggplot(subset(dr,abs(gradient)<0.25),aes(x=speed,y=gradient*100,size=dist,colour=dist))+geom_point()+facet_wrap(~route)
# nrow(subset(dr,abs(gradient)>0.25))/nrow(dr)  # 7.7% of rows have gradient > 0.25
# sum(abs(subset(dr,abs(gradient)>0.25)$wh))/sum(abs(dr$wh))  # only 0.6% of energy is in rows with gradient over 0.15
# throw out the unrealistic gradients
dr <- dr[abs(dr$gradient) < 0.25,]

# look into throwing out speed
# nrow(subset(dr,speed<5))/nrow(dr)  # 4.5% of rows have speed < 5
# sum(abs(subset(dr,abs(gradient)>0.25)$wh))/sum(abs(dr$wh))  # only 0.4% of energy is in rows with speed < 5
dr <- dr[dr$speed > 5,]

# look into throwing out speed
# nrow(subset(dr,speed<5))/nrow(dr)  # 0.7% of rows have perf > 2
# sum(abs(subset(dr,abs(gradient)>0.25)$wh))/sum(abs(dr$wh))  # only 0.6% of energy is in rows with perf over 2
dr <- dr[dr$perf < 2,]

intervs <- findInterval(dr$gradient,x.bins)
intervs[is.na(intervs)] <- length(x.labs)
dr$gradient.binned <- factor(x.labs[intervs],levels=x.labs)
intervs <- findInterval(dr$speed,y.bins)
intervs[is.na(intervs)] <- length(y.labs)
dr$speed.binned    <- factor(y.labs[intervs],levels=y.labs)
dr.hists <- cast(melt(dr,id.vars=c('route','gradient.binned','speed.binned'),measure.vars=c('dist')),gradient.binned ~ speed.binned ~ variable ~ route,fun.aggregate=sum)

save(dr,dr.hists,file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))
write.csv(dr,paste(path.to.leaf,'data/leaf-trips-cleaned.csv',sep=''))

load(file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))
print(weighted.mean(dr$perf,dr$dist))

ggplot(dr,aes(x=gradient*100,y=perf,size=dist,colour=dist))+geom_point()+facet_wrap(~speed.binned)
ggplot(subset(dr,abs(perf)<1),aes(x=speed,y=perf))+geom_point()+facet_wrap(~gradient.binned)

print(summary(lm('perf ~ gradient',subset(dr,abs(perf)<1))))
print(summary(lm('perf ~ gradient + speed',subset(dr,abs(perf)<1))))
print(summary(lm('perf ~ gradient*speed',subset(dr,abs(perf)<1))))
print(summary(lm('perf ~ gradient*speed - speed',subset(dr,abs(perf)<1))))
print(summary(lm('perf ~ gradient*speed + I(gradient^2) + I(speed^2)',subset(dr,abs(perf)<1))))
print(summary(lm('perf ~ gradient*speed + I(speed^2)',subset(dr,abs(perf)<1))))

# apply a couple models to the data and look at the observed performance
fit <- lm('perf ~ gradient',subset(dr,abs(perf)<1))
fit2 <- lm('perf ~ gradient*speed',subset(dr,abs(perf)<1))
route.ordered$perf1 <- predict(fit,newdata=data.frame(gradient=route.ordered$gradient,speed=route.ordered$ab_speed,speed.binned=route.ordered$speed.binned))
route.ordered$perf2 <- predict(fit2,newdata=data.frame(gradient=route.ordered$gradient,speed=route.ordered$ab_speed,speed.binned=route.ordered$speed.binned))
perf <- ddply(route.ordered,.(from_taz,to_taz),function(df){ data.frame(perf1=weighted.mean(df$perf1,df$length),perf2=weighted.mean(df$perf2,df$length)) })
names(perf) <- c('from','to','perf1','perf2')

# choose perf 2 and scale by perf at 0 gradient and weighted mean of speed (55.8mph)
perf$perf <- perf$perf2 / predict(fit2,newdata=data.frame(gradient=0,speed=weighted.mean(route.ordered$ab_speed,route.ordered$length)))

# finally, there is one route (Lost Coast -> Ferndale) with negative performance, but this might break the model, since it's so close to 0, make it zero
perf$perf[perf$perf<0] <- 0

save(dr,dr.hists,perf,file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))
