library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','RPostgreSQL','colorRamps','reshape'))
gpclibPermit()

path.to.leaf   <- '~/Dropbox/serc/pev-colin/data/leaf-performance/'
path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))

# get route.ordered and route.hists from distance-time analysis
load(file=paste(path.to.pevi,'inputs/routing-corrected.Rdata',sep=''))
disttime <- read.csv(file=paste(path.to.geatm,'taz-dist-time.csv',sep=''))
taz <- readShapePoly(paste(path.to.pevi,'inputs/development/aggregated-taz',sep=''))
load(paste(path.to.pevi,'inputs/development/aggregated-taz-fieldnames.Rdata',sep=''))
names(taz@data) <- c('row',agg.taz.shp.fieldnames)

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

dr <- read.csv(paste(path.to.leaf,'data/bear-river-ridge.csv',sep=''))
dr$route <- "Bear River Ridge"

# bear river ridge, shift so that the turn around point of the journey is at intersection of mattole and brr rd. or -124.283656 40.509905 which happens at minute 32
dr$X.Longitude. <- dr$X.Longitude. + (-12428.3656 + 12416.73)
dr$X.Latitude. <- dr$X.Latitude. + (4050.9905 - 4030.592)
# scale lat/lon data thinks GHD is at -124.2146, 40.68633
ll <- c(-124.2867,40.50985)*100 # mattole and BRR rd
scale.x <- (ghd[1] - ll[1])/(-12421.46 - ll[1])
scale.y <- (ghd[2] - ll[2])/(4068.633 - ll[2])
dr$X.Longitude. <- (dr$X.Longitude. - ll[1]) * scale.x + ll[1]
dr$X.Latitude. <- (dr$X.Latitude. - ll[2]) * scale.y + ll[2]

cr <- data.frame(read.csv(paste(path.to.leaf,'data/cr-sunny-brae.csv',sep='')),route="CR / Sunny Brae")
cr$X.Longitude. <- cr$X.Longitude. + 12409.85 + ghd[1]
cr$X.Latitude. <- cr$X.Latitude. - 4048.228 + ghd[2]
# scale lat/lon data thinks samoa & 101 -124.1153, 40.83946  actually at -124.083115, 40.863439
scale.x <- (-12408.3115 - ghd[1])/(-12411.53 - ghd[1])
scale.y <- (4086.3439 - ghd[2])/(4083.946 - ghd[2])
cr$X.Longitude. <- (cr$X.Longitude. - ghd[1]) * scale.x + ghd[1]
cr$X.Latitude. <- (cr$X.Latitude. - ghd[2]) * scale.y + ghd[2]


hy <- data.frame(read.csv(paste(path.to.leaf,'data/hydesville.csv',sep='')),route="Hydesville")
hy$X.Longitude. <- hy$X.Longitude. + 12409.85 + ghd[1]
hy$X.Latitude. <- hy$X.Latitude. - 4048.228 + ghd[2]
# scale lat/lon data thinks lower right (carlotta) is at -124.2146, 40.68633 actually at -124.040724, 40.534442
scale.x <- (-12404.0724 - ghd[1])/(-12421.46 - ghd[1])
scale.y <- (4053.4442 - ghd[2])/(4068.633 - ghd[2])
hy$X.Longitude. <- (hy$X.Longitude. - ghd[1]) * scale.x + ghd[1]
hy$X.Latitude. <- (hy$X.Latitude. - ghd[2]) * scale.y + ghd[2]

# willow creek high point according to g-earth
# 40.897982, -123.774316, elev= 2820
# willow creek high point according to logger
# 40.53846, -123.4643
# the second half of trip was adjusted in original data, by adding -0.4003 to the long for those rows after minute 22, second 36.231
wc <- data.frame(read.csv(paste(path.to.leaf,'data/willow-creek.csv',sep='')),route="Willow Creek")
wc$X.Longitude.[(which(wc$X.Time.Stamp==36.231 & wc$X.Session.Time == 22)+1):nrow(wc)] <- wc$X.Longitude.[(which(wc$X.Time.Stamp==36.231 & wc$X.Session.Time == 22) + 1):nrow(wc)] - 40.03
wc$X.Longitude. <- wc$X.Longitude. + 12409.85 + ghd[1]
wc$X.Latitude. <- wc$X.Latitude. - 4048.228 + ghd[2]
# intersection of 299 & 96, should be at -123.631308, 40.939521, data says -123.8446, 40.88502
scale.x <- (-12363.1308 - ghd[1])/(-12384.46 - ghd[1])
scale.y <- (4093.9521 - ghd[2])/(4088.502 - ghd[2])
wc$X.Longitude. <- (wc$X.Longitude. - ghd[1]) * scale.x + ghd[1]
wc$X.Latitude. <- (wc$X.Latitude. - ghd[2]) * scale.y + ghd[2]



dr <- rbind(dr,cr,hy,wc)

names(dr) <- c("seconds","minutes","long","lat","elev","vel","soc","battery.volts","motor.amps","battery.amps","route")
dr$lat <- as.integer(dr$lat/100) + (dr$lat - as.integer(dr$lat/100)*100)/60
dr$long <- as.integer(dr$long/100) + (dr$long - as.integer(dr$long/100)*100)/60
dr$hour <- (dr$minutes + dr$seconds/60)/60
dr <- ddply(dr,.(route),function(df){ 
  df <- df[order(df$hour),]
  data.frame(df,dist=c(NA,(2*asin(sqrt((sin((diff(df$lat))/2))^2 + cos(df$lat[1:(nrow(df)-1)])*cos(df$lat[2:nrow(df)])*(sin((diff(df$long))/2))^2))) * 111.32 * 0.6214) ) })
dr$dist[which(dr$dist>10)] <- NA
dr$vel2[2:nrow(dr)] <- dr$dist[2:nrow(dr)] / diff(dr$hour)

dr <- ddply(dr,.(route),function(df){ 
  df <- df[order(df$hour),]
  df$dist3 <- NA
  for(i in 2:nrow(df)){
    # 26941 is California Zone I state plane projection in meters (then converted to miles using 0.00062137119)
    df$dist3[i] <- as.numeric(0.00062137119 * dbGetQuery(con,paste("SELECT ST_Distance(
        ST_Transform(ST_GeomFromText('POINT(",df$long[i-1]," ",df$lat[i-1],")',4326),26941),
        ST_Transform(ST_GeomFromText('POINT(",df$long[i]," ",df$lat[i],")', 4326),26941)
      )",sep='')))
  }
  df$vel3[2:nrow(df)] <- df$dist3[2:nrow(df)] / diff(df$hour)
  df
})
#dr$dist3[dr$dist3>200] <- NA
#dr$vel3[dr$vel3>75] <- NA
dr$vel[dr$route=="Willow Creek"] <- dr$vel3[dr$route=="Willow Creek"]
dr$vel[dr$route=="Willow Creek" & dr$vel>60] <- 60

save(dr,file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))
load(file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))

dr <- ddply(dr,.(route),function(df){
  df <- df[order(df$hour),]
  df$gradient <- c(NA,diff(df$elev)/(df$dist3[2:nrow(df)]*1609.344))
  df
})
dr$gradient[abs(dr$gradient)> 0.1] <- NA

x.bins <- c(-0.6,seq(-0.05,0.05,by=0.01),0.6)
x.labs <- c("[-0.6,-0.05)","[-0.05,-0.04)","[-0.04,-0.03)","[-0.03,-0.02)","[-0.02,-0.01)","[-0.01,0.00)","[0.00,0.01)","[0.01,0.02)","[0.02,0.03)","[0.03,0.04)","[0.04,0.05)","[0.05,0.6)","NA")
y.bins <- c(0,25,40,50,60,70)
y.labs <- c("[0,25)","[25,40)","[40,50)","[50,60)","[60,70)","NA")
intervs <- findInterval(dr$gradient,x.bins)
intervs[is.na(intervs)] <- length(x.labs)
dr$gradient.binned <- factor(x.labs[intervs],levels=x.labs)
intervs <- findInterval(dr$vel,y.bins)
intervs[is.na(intervs)] <- length(y.labs)
dr$speed.binned    <- factor(y.labs[intervs],levels=y.labs)
dr.hists <- cast(melt(dr,id.vars=c('route','gradient.binned','speed.binned'),measure.vars=c('dist3')),gradient.binned ~ speed.binned ~ variable ~ route,fun.aggregate=sum)
save(dr,dr.hists,file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))

load(file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))
# Sum of hists

               #speed.binned
#gradient.binned    [0,25)   [25,40)   [40,50)    [50,60)     [60,70)         NA
  #[-0.6,-0.05)  6.5405895 11.548172 0.3773506 0.00000000 0.008677182 0.01054585
  #[-0.05,-0.04) 0.8921868  1.486686 0.2664096 0.00000000 0.000000000 0.00000000
  #[-0.04,-0.03) 0.5504298  2.214045 0.2056079 0.03580833 0.009239853 0.00000000
  #[-0.03,-0.02) 1.4049544  3.641098 0.1855021 0.00000000 0.000000000 0.01039613
  #[-0.02,-0.01) 1.7433457  5.082657 0.4368757 0.02966753 0.037256449 0.03143676
  #[-0.01,0.00)  2.3902644  9.583997 1.1727119 0.01590520 0.018628173 0.03426136
  #[0.00,0.01)   2.6762964 12.698368 1.1979470 0.40813766 0.244778508 0.53073808
  #[0.01,0.02)   1.7347942  5.519907 0.3815027 0.02854215 0.000000000 0.01223307
  #[0.02,0.03)   1.0933193  3.766353 0.3272974 0.01655843 0.008683466 0.02402879
  #[0.03,0.04)   1.3648474  2.844847 0.1505375 0.00000000 0.000000000 0.01160917
  #[0.04,0.05)   0.4708181  1.618442 0.0180372 0.02138794 0.000000000 0.03104598
  #[0.05,0.6)    6.4056909 12.179532 0.7079687 0.08894877 0.018204674 0.06532273
  #NA            1.8005173  1.377210 0.2172528 0.03271469 0.018628229         NA

ggplot(dr,aes(x=vel,y=gradient*100,size=dist3,colour=dist3))+geom_point(position='jitter',height=0,width=2)+facet_wrap(~route,scales="free_y")

# calculate performance for each row
dr$battery.energy <- dr$soc / 100 * 24000
dr <- ddply(dr,.(route),function(df){
  df <- df[order(df$hour),]
  df$delta.t[2:nrow(df)] <- diff(df$hour)
  df$delta.e[2:nrow(df)] <- -diff(df$battery.energy)
  df
})
dr$wh <- dr$battery.volts * dr$battery.amps * dr$delta.t
dr$perf <- dr$wh / 1000 / dr$dist3
dr$perf[abs(dr$perf)==Inf] <- NA
dr$perf2 <- dr$delta.e / 1000 / dr$dist3
dr$perf2[abs(dr$perf2)>10] <- NA
dr$perf2[which(dr$perf2==0)] <- dr$perf[which(dr$perf2==0)]

# performance vs gradient, colored by speed, note we omit very high performance as they probably include stops
ggplot(subset(dr,perf<5),aes(x=gradient*100,y=perf,size=dist3,colour=dist3))+geom_point()+facet_wrap(~route)
ggplot(subset(dr,perf<5),aes(x=gradient*100,y=perf,size=dist3,colour=dist3))+geom_point()
ggplot(subset(dr,perf<2),aes(x=gradient*100,y=perf,size=dist3,colour=dist3))+geom_point()+facet_wrap(~speed.binned)
ggplot(subset(dr,perf<10),aes(x=gradient*100,y=perf))+geom_point()+geom_point(colour='red',aes(x=gradient*100,y=perf2))+facet_wrap(~route)

ggplot(subset(dr,abs(perf2)<5),aes(x=gradient*100,y=perf2,size=dist3,colour=dist3))+geom_point()+facet_wrap(~speed.binned)

# make 2-minute averages
dr <- ddply(dr,.(route),function(df){
  df <- df[order(df$hour),]
  df$two.min <- as.integer(df$hour*3600) - as.integer(df$hour*3600) %% 2
  df$cum.dist <- c(0,cumsum(df$dist3[2:nrow(df)]))
  df
})
perf.two <- ddply(dr,.(route,two.min),function(df){ data.frame(gradient=weighted.mean(df$gradient,df$dist3,na.rm=T),perf.two=weighted.mean(df$perf,df$dist3,na.rm=T))})
# make 5-minute averages
dr <- ddply(dr,.(route),function(df){
  df <- df[order(df$hour),]
  df$five.min <- as.integer(df$hour*3600) - as.integer(df$hour*3600) %% 5
  df
})
perf.five <- ddply(dr,.(route,five.min),function(df){ data.frame(gradient=weighted.mean(df$gradient,df$dist3,na.rm=T),perf.five=weighted.mean(df$perf,df$dist3,na.rm=T))})

# make 15-minute averages
dr <- ddply(dr,.(route),function(df){
  df <- df[order(df$hour),]
  df$fift.min <- as.integer(df$hour*3600) - as.integer(df$hour*3600) %% 15
  df
})
perf.fift <- ddply(dr,.(route,fift.min),function(df){ data.frame(gradient=weighted.mean(df$gradient,df$dist3,na.rm=T),perf.fift=weighted.mean(df$perf,df$dist3,na.rm=T))})
ggplot(subset(perf.two,perf.two<5),aes(x=gradient*100,y=perf.two))+geom_point()+facet_wrap(~route)
ggplot(subset(perf.five,perf.five<5),aes(x=gradient*100,y=perf.five))+geom_point()+facet_wrap(~route)
ggplot(subset(perf.fift,perf.fift<5),aes(x=gradient*100,y=perf.fift))+geom_point()+facet_wrap(~route)

ggplot(dr,aes(x=cum.dist,y=elev))+geom_point()+facet_wrap(~route,scales="free_y")
ggplot(dr,aes(x=long,y=lat))+geom_point()+facet_wrap(~route,scales="free")

