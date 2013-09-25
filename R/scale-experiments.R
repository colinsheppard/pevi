Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
library(colinmisc)
load.libraries(c('ggplot2','yaml','RNetLogo','plyr','reshape','gtools','stringr'))

#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/Raskolnikovbot3001/Dropbox/'

exp.name <- "scale-experiment"

path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/sensitivity/',exp.name,'/',sep='')

taz <- read.table(pp(path.to.inputs,"/taz-52.txt"),header=T,stringsAsFactors=F)
names(taz) <- c('from','to','miles','time','enroute','perf')
enroutes <- sapply(taz$enroute,function(s){ na.omit(as.numeric(str_split(s,",")[[1]]))})

chargers <- read.table(pp(path.to.inputs,"/chargers-52.txt"),header=T,stringsAsFactors=F)

for(n in c(104,208,416)){
  new.taz <- data.frame(permutations(n,2,repeats.allowed=T))
  names(new.taz) <- c('from','to')
  new.taz$miles <- taz$miles 
  new.taz$time  <- taz$time
  new.taz$enroute <- taz$enroute
  scale.enroute <- n/52
  for(i in 53:(n*n)){
    taz.i <- i%%52+1
    new.taz$enroute[i] <- pp(sample(max(1,(new.taz$from[i] - scale.enroute * length(enroutes[[taz.i]]))):min(n,(new.taz$from[i] + scale.enroute * length(enroutes[[taz.i]]))), scale.enroute * length(enroutes[[taz.i]])),collapse=",")
  }
  # make sure we don't have enroute for intra-TAZ travel
  for(i in 1:n){
    new.taz$enroute[(i-1)*n+i] <- ""
  }
  new.taz$perf  <- taz$perf
  write.table(new.taz,file=pp(path.to.inputs,"/taz-",n,".txt"),row.names=F,sep="\t")
}

for(n in c(104,208,416)){
  new.chargers <- data.frame(taz=1:n)
  new.chargers$L0 <- 1
  new.chargers$L1 <- chargers$L1
  new.chargers$L2 <- chargers$L2
  new.chargers$L3 <- chargers$L3
  names(new.chargers) <- c(";TAZ","L0","L1","L2","L3")

  write.table(new.chargers,file=pp(path.to.inputs,"/chargers-",n,".txt"),row.names=F,sep="\t",quote=F)
}

# analysis of scaling results
prof <- read.csv("~/Dropbox/serc/pev-colin/pev-shared/data/inputs/sensitivity/scale-experiment/scale_experiment_profiling_results.csv",header=F,stringsAsFactors=F)
prof.names <- prof[,1]
prof <- data.frame(t(prof[,2:ncol(prof)]))
names(prof) <- prof.names
prof.melted <- melt(prof,c("Drivers","TAZs"))

# plot all at once
ggplot(prof.melted,aes(x=Drivers,y=value/1000,colour=variable))+geom_line()+facet_wrap(~TAZs)

# find the 20 most expensive procedures in the highest scale-out case
top.20 <- as.character(subset(prof.melted,Drivers==384000 & TAZs==52)$variable[order(subset(prof.melted,Drivers==384000 & TAZs==52)$value)][1:20])

# plot just top 20 
ggplot(subset(prof.melted,variable %in% top.20),aes(x=Drivers,y=value/1000,colour=variable))+geom_line()+facet_wrap(~TAZs)
