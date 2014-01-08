
path.to.upstate.data <- pp(pevi.shared,'data/UPSTATE/driving-distances/')

origin <- c()
destination <- c()
time <- c()
orig.name <- c()
distance <- c()

the.files <- grep('Rdata',list.files(path.to.upstate.data,'.xml'),value=T,invert=T)
for(the.file in the.files){
  taz.name <- strsplit(strsplit(the.file,'.xml')[[1]],'-2')[[1]]
	parsed.xml <- xmlTreeParse(pp(path.to.upstate.data,the.file))
	ref.origin <- unlist(xmlRoot(parsed.xml)[[2]][[1]])['value']
	time.dist.xml <- xmlRoot(parsed.xml)[['row']]
	for(j in 1:xmlSize(time.dist.xml)) {
		origin <- c(origin,ref.origin)
		destination <- c(destination,unlist(xmlRoot(parsed.xml)[[j+2]][[1]])['value'])
		time <- c(time,unlist(time.dist.xml[[j]][[2]][[2]][[1]])['value'])
    orig.name <- c(orig.name,taz.name)
		distance <- c(distance,unlist(time.dist.xml[[j]][[3]][[2]][[1]])['value'])
	}
}

time.distance <- data.frame(origin,destination,time,distance,orig.name,stringsAsFactors=F)

###########################################################################
#  Convert google rep of time and distance into doubles
###########################################################################
time.distance$hours <- ldply(str_split(as.character(time.distance$time)," hour"),function(v){ 
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
time.distance$miles <- ldply(str_split(as.character(time.distance$distance)," mi"),function(v){
  if(length(v)>1){
    dist <- as.numeric(v[1])
  }else{
    dist <- as.numeric(str_split(v[1]," ft")[[1]][1])/5280
  }
  data.frame(dist=dist)
})$dist

###########################################################################
#  Get the naming to all be based on orig.name
###########################################################################
time.distance$dest <- time.distance$orig.name[match(time.distance$destination,time.distance$origin)]
# couple of unique cases
time.distance$dest[time.distance$destination=="3030 Shasta View Drive, Redding, CA 96002, USA"] <- "RED_ForestHills"
time.distance$dest[time.distance$destination=="22663 Flores Avenue, Red Bluff, CA 96080, USA"] <- "Gerber"
time.distance$dest[time.distance$destination=="12113-12119 Railroad Avenue East, Macdoel, CA 96058, USA"] <- "Macdoel"
time.distance$dest[time.distance$destination=="116 East First Street, Dorris, CA 96023, USA"] <- "Dorris"
time.distance$dest[time.distance$destination=="356 Lawndale Court, McCloud, CA 96057, USA"] <- "McCloud"
time.distance$dest[time.distance$destination=="Corning Road & Interstate 5, Corning, CA 96021, USA"] <- "Corning"
time.distance$dest[time.distance$destination=="Browning Street, Redding, CA 96003, USA"] <- "RED_BoulderCreek"
time.distance$dest[time.distance$destination=="112 Kingsview Court, Redding, CA 96003, USA"] <- "RED_LakeRedding"
time.distance$dest[time.distance$destination=="2301 Duncan Lane, Redding, CA 96002, USA"] <- "RED_Pacheco"
time.distance$dest[time.distance$destination=="1930 North Street, Anderson, CA 96007, USA"] <- "AND_Cascade"
time.distance$dest[time.distance$destination=="4450-4498 California 151, Shasta Lake, CA 96019, USA"] <- "SHA_Downtown"
time.distance$dest[time.distance$destination=="15915 Dragon Fly Drive, Oak Run, CA 96069, USA"] <- "RoundMountain"
time.distance$dest[time.distance$destination=="5001 Bechelli Lane, Redding, CA 96002, USA"] <- "RED_LaytonOaks"
time.distance$dest[time.distance$destination=="1898 Benton Drive, Redding, CA 96003, USA"] <- "RED_LakeRedding"
time.distance$dest[time.distance$destination=="1915-1941 Rancho Road, Redding, CA 96002, USA"] <- "RED_ChurnCreek"
time.distance$dest[time.distance$destination=="21114 Golden Trail West, Redding, CA 96003, USA"] <- "SHA_BearMountain"
dest.to.exclude <- c('50976-51798 Klamath River Highway, Klamath National Forest, Seiad Valley, CA 96086, USA',
                      '22542-23316 Klamath River Highway, Klamath National Forest, Klamath River, CA 96050, USA',
                      '4043-5285 Central Avenue, Weed, CA 96094, USA',
                      'Forest Route 46N50, Klamath National Forest, Klamath River, CA 96050, USA',
                      '2401-2517 Klamath River Highway, Yreka, CA 96097, USA',
                      'Seiad Oaks Road, Klamath National Forest, Seiad Valley, CA 96086, USA',
                      'Mill Road, Klamath National Forest, Happy Camp, CA 96039, USA',
                      '1200 Cherry Maple Road, Klamath National Forest, Horse Creek, CA 96050, USA',
                      '4001-5285 Center Street, Weed, CA 96094, USA',
                      '3001 California 273, Anderson, CA 96007, USA')
time.distance <- subset(time.distance,!destination %in% dest.to.exclude)
time.distance$orig <- time.distance$orig.name
time.distance <- time.distance[,c('orig','dest','hours','miles')]
time.distance <- rbind(time.distance,data.frame(orig='RED_MetzRoad',dest='AND_Cascade',hours=0.15,miles=4.6))

time.distance <- unique(data.table(time.distance,key=c('orig','dest')))

save(time.distance,file=pp(pevi.shared,'data/UPSTATE/driving-distances/time.distance.Rdata'))

