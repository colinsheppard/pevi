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

plot(ch$hour,ch$soc,type='l',main="GHD Leaf Charging Event - October 2012",xlab="Hour",ylab="State of Charge (%)",ylim=c(0,100))
abline(lm('soc ~ hour',ch),lty=2,col='red')
text(-0.25,35,paste('r-squared: ',roundC(summary(lm('soc ~ hour',ch))$r.squared,3),sep=''),pos=4)
abline(h=max(ch$soc))
text(-0.25,max(ch$soc)-5,paste('max soc: ',roundC(max(ch$soc),2),sep=''),pos=4)


dbuser<-"pev"
dbpassword<-""
dbname<-"pev"
dbhost<-"localhost"
con <- dbConnect(PostgreSQL(), user=dbuser, password=dbpassword, dbname=dbname, host=dbhost)	

dr <- read.csv(paste(path.to.leaf,'data/bear-river-ridge.csv',sep=''))
dr$route <- "Bear River Ridge"
dr <- rbind(dr,data.frame(read.csv(paste(path.to.leaf,'data/cr-sunny-brae.csv',sep='')),route="CR / Sunny Brae"))
dr <- rbind(dr,data.frame(read.csv(paste(path.to.leaf,'data/hydesville.csv',sep='')),route="Hydesville"))
dr <- rbind(dr,data.frame(read.csv(paste(path.to.leaf,'data/willow-creek-trip.csv',sep='')),route="Willow Creek"))
names(dr) <- c("seconds","minutes","long","lat","elev","vel","soc","battery.volts","motor.amps","route")
dr$lat <- dr$lat/100
dr$long <- dr$long/100
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
  df
})
dr$vel3[2:nrow(dr)] <- dr$dist3[2:nrow(dr)] / diff(dr$hour)

# translate lat,lon to actually start at GHD
ghd <- c(-124.163904,40.803678)
dr2 <- ddply(subset(dr,route %in% c("CR / Sunny Brae","Hydesville","Willow Creek")),.(route),function(df){ 
  df <- df[order(df$hour),]
  df$long <- df$long + ghd[1] - -124.0985
  df$lat  <- df$lat  + ghd[2] - 40.48228
  df
})


# willow creek high point according to g-earth
# 40.897982, -123.774316, elev= 2820
# willow creek high point according to logger
# 40.53846, -123.4643
# the second half of trip was adjusted in original data, by adding -0.4003 to the long for those rows after minute 22, second 36.231

# bear river ridge, shifted in original data so that the turn around point of the journey is at intersection of mattole and brr rd. or -124.283656 40.509905 which happens at minute 23

dr <- rbind(subset(dr,route=="Bear River Ridge"),dr2)

save(dr,file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))

dr <- ddply(dr,.(route),function(df){ 
  df <- df[order(df$hour),]
  df$gradient <- c(NA,diff(df$elev)/(diff(df$dist3)*1609.344))
  df
})
dr$gradient[abs(dr$gradient)> 0.6] <- NA

x.bins <- c(-0.6,seq(-0.05,0.05,by=0.01),0.6)
x.labs <- c("[-0.6,-0.05)","[-0.05,-0.04)","[-0.04,-0.03)","[-0.03,-0.02)","[-0.02,-0.01)","[-0.01,0.00)","[0.00,0.01)","[0.01,0.02)","[0.02,0.03)","[0.03,0.04)","[0.04,0.05)","[0.05,0.6)","NA")
y.bins <- c(0,25,40,50,60,70)
y.labs <- c("[0,25)","[25,40)","[40,50)","[50,60)","[60,70)","NA")
intervs <- findInterval(dr$gradient,x.bins)
intervs[is.na(intervs)] <- length(x.labs)
dr$gradient.binned <- factor(x.labs[intervs],levels=x.labs)
intervs <- findInterval(dr$vel3,y.bins)
intervs[is.na(intervs)] <- length(y.labs)
dr$speed.binned    <- factor(y.labs[intervs],levels=y.labs)
dr.hists <- cast(melt(dr,id.vars=c('route','gradient.binned','speed.binned'),measure.vars=c('dist3')),gradient.binned ~ speed.binned ~ variable ~ route,fun.aggregate=sum)
save(dr,dr.hists,file=paste(path.to.leaf,'data/all-trips-cleaned.Rdata',sep=''))

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

