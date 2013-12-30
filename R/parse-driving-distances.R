
path.to.upstate.data <- pp(pevi.shared,'data/UPSTATE/driving-distances/')

origin <- vector()
destination <- vector()
time <- vector()
distance <- vector()


for(i in 1:length(list.files(path.to.upstate.data))) {
	parsed.xml <- xmlTreeParse(pp(path.to.upstate.data,list.files(path.to.upstate.data)[i]))
	ref.origin <- xmlRoot(parsed.xml)[[2]][[1]]
	time.dist.xml <- xmlRoot(parsed.xml)[['row']]
	for(j in 1:xmlSize(time.dist.xml)) {
		origin <- rbind(origin,ref.origin)
		destination <- rbind(destination,xmlRoot(parsed.xml)[[j+2]][[1]])
		time <- rbind(time,time.dist.xml[[j]][[2]][[2]][[1]])
		distance <- rbind(distance,time.dist.xml[[j]][[3]][[2]][[1]])
	}
}

time.distance <- data.frame(unlist(origin),unlist(destination),unlist(time),unlist(distance))

###########################################################################
#  Convert google rep of time and distance into doubles
###########################################################################
taz.time.distance$hours <- ldply(str_split(as.character(taz.time.distance$time)," hour"),function(v){ 
  if(length(v)>1){
    hour <- as.numeric(v[1])
    min.str <- v[2]
  }else{
    hour <- 0
    min.str <- v[1]
  }
  if(substr(min.str,1,1)=="s")min.str <- substr(min.str,3,nchar(min.str))
  data.frame(hours=hour + as.numeric(str_split(min.str," min")[[1]][1])/60)
})$hours
taz.time.distance$miles <- ldply(str_split(as.character(taz.time.distance$distance)," mi"),function(v){
  if(length(v)>1){
    dist <- as.numeric(v[1])
  }else{
    dist <- as.numeric(str_split(v[1]," ft")[[1]][1])/5280
  }
  data.frame(dist=dist)
})$dist

###########################################################################
#  Rename TAZs to be consistent across other data inputs
###########################################################################
to.replace <- data.frame(orig=c('Proberta','Platina Center','RED-ForestHills','COTTON_West','COTTON_East','SHALKE_BearMountain','SHALKE_Downtown','SHALKE_ShastaDam','RED_MunicipalAirport','RoundMountainMontgomeryCreekOakRun','FallRiverMcArthur','LakeheadLakeshoreOBrien','Mt. Shasta','BurneyHatCreekCassel'),
  replacement=c('Gerber','Platina','RED_ForestHills','COT_West','COT_East','SHA_BearMountain','SHA_Downtown','SHA_ShastaDam','RED_Airport','RoundMountain','FallRiver','Lakehead','Shasta','Burney'),stringsAsFactors=F)
for(i in 1:nrow(to.replace)){
  taz.time.distance$destination.taz[taz.time.distance$destination.taz==to.replace$orig[i]] <- to.replace$replacement[i]
  taz.time.distance$origin.taz[taz.time.distance$origin.taz==to.replace$orig[i]] <- to.replace$replacement[i]
}
save(taz.time.distance,file=pp(pevi.shared,'data/UPSTATE/driving-distances/taz_time_distance.Rdata'))

# Andy's renaming stuff

update.time.distance <- data.frame(unlist(origin[,6]),unlist(destination[,6]),unlist(time[,6]),unlist(distance[,6]))

update.time.distance$origin.taz <- ''
update.time.distance$destination.taz <- ''

for(i in 1:nrow(taz.time.distance)) {
	if(update.time.distance$origin[i] %in% taz.time.distance$origin) {
		update.time.distance$origin.taz[i] <- taz.time.distance$origin.taz[match(update.time.distance$origin[i],taz.time.distance$origin)]
	}
	if(update.time.distance$destination[i] %in% taz.time.distance$destination.address) {
		update.time.distance$destination.taz[i] <- as.character(taz.time.distance$destination.taz[match(update.time.distance$destination[i],taz.time.distance$destination.address)])
	}
}

for(i in 1:nrow(update.time.distance)) {
# Fix the origin addresses first
	if(update.time.distance$destination[i]=='2600-2998 Balls Ferry Road, Anderson, CA 96007, USA') {
		 update.time.distance$destination.taz[i] <- 'AND_Cascade'
	}
	if(update.time.distance$destination[i]=='945-1001 Highway 99 W, Corning, CA 96021, USA') {
 					 update.time.distance$destination.taz[i] <- 'Corning'
	}
	if(update.time.distance$destination[i]=='3571-3591 Park Drive, Cottonwood, CA 96022, USA') {
			 update.time.distance$destination.taz[i] <- 'COTTON_West'
	}
	if(update.time.distance$destination[i]=='Railroad Avenue, Dorris, CA 96023, USA') {
			 update.time.distance$destination.taz[i] <- 'Dorris'
	}
	if(update.time.distance$destination[i]=='327-337 Collier Way, Etna, CA 96027, USA') {
			 update.time.distance$destination.taz[i] <- 'Etna'
	}
	if(update.time.distance$destination[i]=='11819 Main Street, Fort Jones, CA 96032, USA') {
			 update.time.distance$destination.taz[i] <- 'Fort Jones'
	}
	if(update.time.distance$destination[i]=='7918 Railroad Avenue, Los Molinos, CA 96055, USA') {
			 update.time.distance$destination.taz[i] <- 'Los Molinos'
	}
	if(update.time.distance$destination[i]=='11816-11826 Railroad Avenue East, Macdoel, CA 96058, USA') {
			 update.time.distance$destination.taz[i] <- 'Macdoel'
	}
	if(update.time.distance$destination[i]=='480 California 89, McCloud, CA 96057, USA') {
			 update.time.distance$destination.taz[i] <- 'McCloud'
	}
	if(update.time.distance$destination[i]=='4558-4650 California 36, Platina, CA 96076, USA') {
			 update.time.distance$destination.taz[i] <- 'Platina'
	}
	if(update.time.distance$destination[i]=='1001-1199 Churn Creek Road, Redding, CA 96003, USA') {
			 update.time.distance$destination.taz[i] <- 'RED_BoulderCreek'
	}
	if(update.time.distance$destination[i]=='4465 Airport Road, Redding, CA 96002, USA') {
			 update.time.distance$destination.taz[i] <- 'RED_CloverCreek'
	}
	if(update.time.distance$destination[i]=='11537 Ridgewood Road, Redding, CA 96003, USA') {
			 update.time.distance$destination.taz[i] <- 'RED_GoldHills'
	}
	if(update.time.distance$destination[i]=='7223 Churn Creek Road, Redding, CA 96002, USA') {
			 update.time.distance$destination.taz[i] <- 'RED_Pacheco'
	}
	if(update.time.distance$destination[i]=='9874 Old Oregon Trail, Redding, California 96003, USA') {
			 update.time.distance$destination.taz[i] <- 'RED_SportsComplex'
	}
	if(update.time.distance$destination[i]=='3001-3031 Shasta View Drive, Redding, CA 96002, USA') {
			 update.time.distance$destination.taz[i] <- 'RED_ForestHills'
	}
 	if(update.time.distance$destination[i]=='344 Black Canyon Road, Shasta Lake, CA 96019, USA') {
 			 update.time.distance$destination.taz[i] <- 'SHALKE_ShastaDam'
	}
	if(update.time.distance$destination[i]=='30659-30667 Whitmore Road, Whitmore, CA 96096, USA') {
			 update.time.distance$destination.taz[i] <- 'Whitmore'
	}	
}

