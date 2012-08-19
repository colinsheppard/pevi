library(colinmisc)
load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice'))
gpclibPermit()

path.to.geatm <- '~/Dropbox/serc/pev-colin/data/GEATM-2020/'
path.to.humveh <- '~/Dropbox/serc/pev-colin/data/Vehicle-Registration/'

taz <- readShapePoly(paste(path.to.geatm,'Shape_Files/taz-LATLON.shp',sep=''))
zips    <- readShapePoly(paste(path.to.geatm,'../CA-ZIPS/tl_2010_06_zcta510.shp',sep=''))
zips@data$INTPTLAT10 <- as.numeric(as.character(zips@data$INTPTLAT10))
zips@data$INTPTLON10 <- as.numeric(as.character(zips@data$INTPTLON10))
hum.zips<- which(zips@data$INTPTLAT10 < 42 & zips@data$INTPTLAT10 > 39.5 & zips@data$INTPTLON10 < -123 & zips@data$INTPTLON10 > -124.5 )
plot(zips[hum.zips,])
plot(taz,col='#0000ff22',add=T)
# finding two overlapping polygons
taz.id <- which(taz@data$ID==519)
zip.id <- which(zips@data$ZCTA5CE10==95549)
plot(taz[taz.id,],col='#ee229922',add=T)
plot(zips[zip.id,],col='#3399ff22',add=T)


# area.of.union
# this gives you the area of the union of any number of SpatialPolygonsDataFrame objects including multiple rows within
# make sure that both objects are in the same projection before attempting
#
# arguments must be an object of class "SpatialPolygonsDataFrame" or a list of such objects 
# each data frame can have multiple rows and the union over all rows of all data frames is taken
area.of.union <- function(spatial.polys.list){
  if(inherits(spatial.polys.list,"SpatialPolygonsDataFrame")){
    spatial.polys.list <- list(spatial.polys.list)
  }
  first.union <- T
  for(spatial.polys.list.i in 1:length(spatial.polys.list)){
    for(row.i in nrow(spatial.polys.list[[spatial.polys.list.i]]@data)){
      polys <- slot(spatial.polys.list[[spatial.polys.list.i]][row.i,], "polygons")  
      for(polys.i in 1:length(polys)){
        polys.polys <- slot(polys[[polys.i]], "Polygons")
        for(polys.polys.i in 1:length(polys.polys)){
          pp.coords <- slot(polys.polys[[polys.polys.i]], "coords")
          if(first.union){
            result.gpc.poly <- as(pp.coords, "gpc.poly")
            first.union <- F
          }else{
            result.gpc.poly <- gpclib:::union(result.gpc.poly,as(pp.coords, "gpc.poly"))
          }
        }
      }
    }
  }
  area.poly(result.gpc.poly)
}

if(!file.exists(paste(path.to.geatm,"zip-fraction-in-taz-matrix.Rdata",sep=''))){
  # get the data frame ready for storage of the results which will hold the fraction of each zip (the columns) by area that 
  # are in each TAZ (the rows), where the 'null' column contains the fraction of the TAZ with no associated zip code
  zips.in.taz <- as.data.frame(matrix(0,nrow(taz@data),length(hum.zips)+1))
  names(zips.in.taz) <- c('null',as.character(zips[hum.zips,]@data$ZCTA5CE10))
  row.names(zips.in.taz) <- as.character(sort(taz@data$NEWTAZ))

  # precalculate the area of each zip polygon to reduce redundancy
  zip.areas <- list()
  for(zip.i in names(zips.in.taz)[2:ncol(zips.in.taz)]){
    zip.id <- which(zips@data$ZCTA5CE10 == zip.i)
    zip.areas[[zip.i]] <- area.of.union(zips[zip.id,])
  }

  for(taz.i in row.names(zips.in.taz)){
    print(taz.i)
    taz.id <- which(taz@data$NEWTAZ == taz.i)
    taz.area <- area.of.union(taz[taz.id,])
    cum.frac <- 0
    for(zip.i in names(zips.in.taz)[2:ncol(zips.in.taz)]){
      if(cum.frac >= 1)next
      zip.id <- which(zips@data$ZCTA5CE10 == zip.i)
      zips.in.taz[taz.i,zip.i] <- (taz.area + zip.areas[[zip.i]] - area.of.union(list(taz[taz.id,],zips[zip.id,]))) / taz.area
      cum.frac <- cum.frac + zips.in.taz[taz.i,zip.i]
    }
  }
  zips.in.taz[,'null'] <- apply(zips.in.taz[,2:ncol(zips.in.taz)],1,function(x){ 1-sum(x) }) 
  # the first 10 rows are the boundary TAZs which should have 1 for null and zero for all other zips
  zips.in.taz[1:10,'null'] <- 1
  zips.in.taz[1:10,2:ncol(zips.in.taz)] <- 0
  # remove 0 columns which represent zip codes outside of humboldt
  zips.in.taz <- zips.in.taz[,-which(apply(zips.in.taz,2,sum)<=0)]
  save(zips.in.taz,file=paste(path.to.geatm,"zip-fraction-in-taz-matrix.Rdata",sep=''))
}else{
  load(paste(path.to.geatm,"zip-fraction-in-taz-matrix.Rdata",sep=''))
}

# now load up the data providing the fraction of EV's and Hybrids in Humboldt by zipcode and year from 2003-2011
load(paste(path.to.humveh,'frac-ev-hybrid-by-zip-year.Rdata'))
# simplify -- toss out distinction on fuel type
frac.by.zip.year <- ddply(frac.ev.hybrid.by.zip.year,.(zip,year),function(df){ data.frame(frac=sum(df$frac)) })
# now use these data to estimate 2011 penetration via a linear fit, include r-squared in the results for inspection
frac.est <- ddply(frac.by.zip.year,.(zip),function(df){ data.frame(frac=predict(lm('frac ~ year',df),newdata=data.frame(year=2011)),r.squared=summary(lm('frac ~ year',df))$r.squared)})

# now dump results from PO Box only zip codes 95502 95518 95534 into their surrounding zips that actually have geographic extension
frac.est$frac[frac.est$zip==95521] <- sum(frac.est$frac[frac.est$zip%in%c(95521,95518)])
frac.est$frac[frac.est$zip==95503] <- sum(frac.est$frac[frac.est$zip%in%c(95503,95534)])
frac.est$frac[frac.est$zip==95501] <- sum(frac.est$frac[frac.est$zip%in%c(95501,95502)])
frac.est <- frac.est[!frac.est$zip %in% c(95502,95518,95534),]

# for zip codes that weren't in the polk data (mostly border zips outside of humboldt with tiny fractions inside TAZs), we add rows to frac.est make them NA
frac.est <- rbind(frac.est,data.frame(zip=names(zips.in.taz)[! names(zips.in.taz) %in% frac.est$zip],frac=NA,r.squared=NA))

# now apply these estimates to the matrix




#tracts  <- readShapePoly(paste(path.to.geatm,'../CA-CENSUS-TRACTS/tl_2010_06_tract10.shp',sep=''))
#hum.tracts <- which(tracts@data$COUNTYFP10=='023')
#plot(tracts[hum.tracts,])

# tried to read in as lat-long but file has non-conformant data
#taz <- readShapePoly('GEATM-Data/Shape_Files/taz.shp',proj4string=CRS("+proj=longlat"))
#summary(taz)
#attributes(taz)

#plot(taz)

taz.coords <- coordinates(taz)

newtaznum <- 50

taz.clusterd <- kmeans(taz.coords,newtaznum)

agg.taz <- unionSpatialPolygons(taz,taz.clusterd$cluster)
centroid.coordinates <- coordinates(agg.taz)

dev.new()
plot(agg.taz)
points(coordinates(agg.taz))
textxy(centroid.coordinates[,1],centroid.coordinates[,2],1:25,cx=1)
#zoomplot( locator(2) )

taz@data$cluster <- taz.clusterd$cluster

aggregate.data <- function(df)
{ 
 return( colSums(df[,c('AREA','ACRES','SHAPE_AREA')]) ) 
}

agg.taz.data <- ddply(taz@data,.(cluster),aggregate.data)

writePolyShape(SpatialPolygonsDataFrame(agg.taz,data=agg.taz.data),paste('taz-aggregated.shp',sep=''))

#read.table or read.csv?
#ODtable.24hr.old <- read.csv('GEATM-Data/OD_Tables/OD Table SUM2.csv', header=TRUE, row.names=1)

#doesn't work ODtable.24hr.old <- matrix(scan('GEATM-Data/OD_Tables/OD Table SUM2.csv', n=774*774, sep="'"),nrow=774,ncol=774, byrow = TRUE)
#ODtable.AM.old <- read.csv('GEATM-Data/OD_Tables/OD AM SUM.txt', header=TRUE)
#ODtable.PM.old <- read.csv('GEATM-Data/OD_Tables/OD PM SUM.txt', header=TRUE)

ODtable.24hr.old <- as.matrix(read.csv('GEATM-Data/OD_Tables/OD Table SUM2.csv',header=FALSE))
aggmat <- function(old,ntc,num)
{
	tmp <- mat.or.vec(775,num)
	new <- mat.or.vec(num,num)
	
	for (zn in 1:num) # for each new zone
	{	# store old zones in oldind
		oldind <- which(ntc %in% zn) + 21 
		# plus 20 for the zones skipped in the original data set and plus 1 for the taz number column
		tmp[,zn] <- rowSums(old[,oldind], dims=1) # sum the rows corresponding to old zones)
	}
	for (zn in 1:num) # for each new zone
	{	# store old zones in oldind
		oldind <- which(ntc %in% zn) + 21
		new[zn,] <- colSums(tmp[which(ntc %in% zn)+21,])
	}
	new
}

ODtable.24hr.new <- aggmat(ODtable.24hr.old,taz$cluster,newtaznum)

# read in all columns of OD by type data
#OD.cols <- c(rep('numeric',12)) # I think numeric is the default and this is not needed
# read.table or read.csv or scan
#OD.24hr.old <- read.table('GEATM-Data/OD_Tables/OD by type 24 hr.txt', header=FALSE, sep=",",
					colClasses=OD.cols)
OD.24hr.old <- read.csv('GEATM-Data/OD_Tables/OD by type 24 hr.txt', header=FALSE)
OD.AM.old <- read.csv('GEATM-Data/OD_Tables/OD by type AM.txt', header=FALSE)
OD.PM.old <- read.csv('GEATM-Data/OD_Tables/OD by type PM.txt', header=FALSE)

names(OD.24hr.old) <- c('fromtaz','totaz','HBW','HBSHOP','HBELEM','HBUNIV','HBRO','NHB','IX','XI','EE','Sum24hr')

# started to change this but didn't complete
condense <- function(old,ntc,num) # still needs verified
{
	new <- array(0,dim=c(num,12))
	rowindex <- 1
	for (zn in 1:num) # for each new zone
	{
		oldind <- which(ntc %in% zn) # store old zones in oldind 
			for (zi in 1:length(oldind)) # for each old zone
			{	
				new[rowindex,1] <- c(1:num) # assign from zone to first column
				# for fromtaz data, sum 3rd column of old and put in 2nd col of new
				new[zn,2] <-  new [zn,2] + sum(old[3][old[1]==oldind[zi]])
				# for totaz data, sum 3rd column of old and put in 3nd col of new 
				new[zn,3] <-  new [zn,3] + sum(old[3][old[2]==oldind[zi]])
			}
	}
	new
}

 # condense the old taz data to the total sum to (column 2) and from (column 3) new tazs
OD.24hr.new <- collapse(OD.24hr.old,taz$cluster,newtaznum)

dev.new()
#plot(agg.taz,col=color.scale(OD.24hr.new[,3],color.spec="rgb"))
# we could also try to use spplot here
# trellis.par.set(sp.theme())
# spplot(agg.taz,colorkey=list(space="bottom"), scales=list(draw=TRUE))

Eureka <- GetMap(center=c(40.8022222,-124.1625), zoom =14, destfile = "Eureka_z14.png")
EkaImg <- readPNG('Eureka_z14.png')
dev.new()
plot(1:2, type='n', main="Eureka zoom = 14", xlab="x", ylab="y")
lim <- par()
rasterImage(EkaImg, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])


