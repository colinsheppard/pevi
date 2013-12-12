
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
