library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','RPostgreSQL'))
gpclibPermit()

path.to.geatm  <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
path.to.plots  <- '~/Dropbox/serc/pev-colin/plots/'
path.to.pevi   <- '~/Dropbox/serc/pev-colin/pevi/'

taz <- readShapePoly(paste(path.to.pevi,'inputs/development/aggregated-taz',sep=''))
load(paste(path.to.pevi,'inputs/development/aggregated-taz-fieldnames.Rdata',sep=''))
names(taz@data) <- c('row',agg.taz.shp.fieldnames)

plot(taz)
plot(taz[21,],col='red',add=T) # ferndale
plot(taz)
plot(taz[25,],col='red',add=T) # fortuna - central
plot(taz)
plot(taz[25,],col='red',add=T) # fortuna - central
plot(taz)
plot(taz[25,],col='red',add=T) # fortuna - central
plot(taz)
plot(taz[25,],col='red',add=T) # fortuna - central

map.color <- function (x,c.map){
   c.map[round((length(c.map)-1)*(x-min(x))/diff(range(x))+1,0)]
}
num.chargers <- as.integer(runif(nrow(taz@data),0,30))

plot(taz,col=map.color(num.chargers,blue2red(30)))
text(coordinates(taz)[,1],coordinates(taz)[,2],labels=num.chargers)


