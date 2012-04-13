library(maptools)
library(plotrix)
library(stats)
library(gpclib)
library(plyr)
library(png)
library(RgoogleMaps)
library(lattice)

TAZ <- readShapePoly('GEATM-Data/Shape_Files/TAZ.shp')
# tried to read in as lat-long but file has non-conformant data
#TAZ <- readShapePoly('GEATM-Data/Shape_Files/TAZ.shp',proj4string=CRS("+proj=longlat"))
#summary(TAZ)
#attributes(TAZ)

#plot(TAZ)

TAZ.coords <- coordinates(TAZ)

newTAZnum <- 25

TAZ.clusterd <- kmeans(TAZ.coords,newTAZnum)

agg.TAZ <- unionSpatialPolygons(TAZ,TAZ.clusterd$cluster)
centroid.coordinates <- coordinates(agg.TAZ)

dev.new()
plot(agg.TAZ)
points(coordinates(agg.TAZ))
textxy(centroid.coordinates[,1],centroid.coordinates[,2],1:25,cx=1)
#zoomplot( locator(2) )

TAZ@data$cluster <- TAZ.clusterd$cluster

aggregate.data <- function(df)
{ 
 return( colSums(df[,c('AREA','ACRES','SHAPE_AREA')]) ) 
}

agg.TAZ.data <- ddply(TAZ@data,.(cluster),aggregate.data)

writePolyShape(SpatialPolygonsDataFrame(agg.TAZ,data=agg.TAZ.data),paste('TAZ-aggregated.shp',sep=''))

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
		# plus 20 for the zones skipped in the original data set and plus 1 for the TAZ number column
		tmp[,zn] <- rowSums(old[,oldind], dims=1) # sum the rows corresponding to old zones)
	}
	for (zn in 1:num) # for each new zone
	{	# store old zones in oldind
		oldind <- which(ntc %in% zn) + 21
		new[zn,] <- colSums(tmp[which(ntc %in% zn)+21,])
	}
	new
}

ODtable.24hr.new <- aggmat(ODtable.24hr.old,TAZ$cluster,newTAZnum)

# read in all columns of OD by type data
#OD.cols <- c(rep('numeric',12)) # I think numeric is the default and this is not needed
# read.table or read.csv or scan
#OD.24hr.old <- read.table('GEATM-Data/OD_Tables/OD by type 24 hr.txt', header=FALSE, sep=",",
					colClasses=OD.cols)
OD.24hr.old <- read.csv('GEATM-Data/OD_Tables/OD by type 24 hr.txt', header=FALSE)
OD.AM.old <- read.csv('GEATM-Data/OD_Tables/OD by type AM.txt', header=FALSE)
OD.PM.old <- read.csv('GEATM-Data/OD_Tables/OD by type PM.txt', header=FALSE)

names(OD.24hr.old) <- c('fromTAZ','toTAZ','HBW','HBSHOP','HBELEM','HBUNIV','HBRO','NHB','IX','XI','EE','Sum24hr')

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
				# for fromTAZ data, sum 3rd column of old and put in 2nd col of new
				new[zn,2] <-  new [zn,2] + sum(old[3][old[1]==oldind[zi]])
				# for toTAZ data, sum 3rd column of old and put in 3nd col of new 
				new[zn,3] <-  new [zn,3] + sum(old[3][old[2]==oldind[zi]])
			}
	}
	new
}

 # condense the old TAZ data to the total sum to (column 2) and from (column 3) new TAZs
OD.24hr.new <- collapse(OD.24hr.old,TAZ$cluster,newTAZnum)

dev.new()
#plot(agg.TAZ,col=color.scale(OD.24hr.new[,3],color.spec="rgb"))
# we could also try to use spplot here
# trellis.par.set(sp.theme())
# spplot(agg.TAZ,colorkey=list(space="bottom"), scales=list(draw=TRUE))

Eureka <- GetMap(center=c(40.8022222,-124.1625), zoom =14, destfile = "Eureka_z14.png")
EkaImg <- readPNG('Eureka_z14.png')
dev.new()
plot(1:2, type='n', main="Eureka zoom = 14", xlab="x", ylab="y")
lim <- par()
rasterImage(EkaImg, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])


