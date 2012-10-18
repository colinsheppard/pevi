library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','RPostgreSQL','colorRamps','reshape'))
gpclibPermit()

path.to.leaf   <- '~/Dropbox/serc/pev-colin/data/leaf-performance/'
path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))

# get route.ordered and route.hists from distance-time analysis
load(file=paste(path.to.pevi,'inputs/routing.Rdata',sep=''))

ch <- read.csv(paste(path.to.leaf,'data/charging-event.csv',sep=''))
ch$hour <- (ch$minute + ch$second/60)/60

plot(ch$hour,ch$soc,type='l',main="GHD Leaf Charging Event - October 2012",xlab="Hour",ylab="State of Charge (%)",ylim=c(0,100))
abline(lm('soc ~ hour',ch),lty=2,col='red')
text(-0.25,35,paste('r-squared: ',roundC(summary(lm('soc ~ hour',ch))$r.squared,3),sep=''),pos=4)
abline(h=max(ch$soc))
text(-0.25,max(ch$soc)-5,paste('max soc: ',roundC(max(ch$soc),2),sep=''),pos=4)
