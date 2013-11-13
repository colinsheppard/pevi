
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
