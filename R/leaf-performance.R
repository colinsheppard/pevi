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
