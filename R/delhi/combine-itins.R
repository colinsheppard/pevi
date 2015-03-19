
ds <- list()

for(rep in c(1,2,3,4)){
  ds[[rep]] <- read.table(pp(pevi.shared,'/data/inputs/driver-input-file/delhi-uncombined/driver-schedule-pen5-rep',rep,'-20150212.txt'),header=T,sep='\t')
}

# we need to use rep 4 to get 1-3 up to ~750k drivers
pool <- unique(ds[[4]]$X.driver)
used.drivers <- c()
for(rep in 1:3){
  # 759k is our 5% pen # of drivers and we multiply by 1.08 to adjust for irreduscible strandings in the model
  num.needed <- 1.08 * 759e3 - length(unique(ds[[rep]]$X.driver))
  to.use <- sample(pool,num.needed)
  itins.to.use <- subset(ds[[4]],X.driver%in%to.use)
  itins.to.use$X.driver <- itins.to.use$X.driver + max(ds[[rep]]$X.driver)
  ds[[rep]] <- rbind(ds[[rep]],itins.to.use)
  pool <- pool[!pool %in% to.use]

  if(rep>1){
    rep.j <- rep-1
    itins.to.use <- ds[[rep.j]]
    itins.to.use$X.driver <- itins.to.use$X.driver + max(ds[[rep]]$X.driver)
    ds[[rep]] <- rbind(ds[[rep]],itins.to.use)
  }
}

for(rep in 1:3){
  names(ds[[rep]]) <- c(';driver','from','to','depart','home')
  write.table(ds[[rep]],pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/no-homeless/driver-schedule-pen',(rep*5),'-rep1-20150212.txt'),sep='\t',row.names=F)
}


for(rep in 1:3){
  tmp <- ds[[rep]]
  for(new.day in 1:3){
    tmp$depart <- tmp$depart + 24
    ds[[rep]] <- rbind(ds[[rep]],tmp)
  }
  names(ds[[rep]]) <- c('driver','from','to','depart','home')
  to.write <- data.table(ds[[rep]])
  setkey(to.write,driver,depart)
  names(to.write) <- c(';driver','from','to','depart','home')
  write.table(to.write,pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/no-homeless/driver-schedule-pen',(rep*5),'-rep1-4day-20150212.txt'),sep='\t',row.names=F,quote=F)
}

ds <- list()
for(rep in 1:3){
  ds[[rep]] <- read.table(pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/half-homeless/driver-schedule-pen5-rep',rep,'-20150212.txt'),header=T,sep='\t')
  tmp <- ds[[rep]]
  for(new.day in 1:3){
    tmp$depart <- tmp$depart + 24
    ds[[rep]] <- rbind(ds[[rep]],tmp)
  }
  names(ds[[rep]]) <- c('driver','from','to','depart','home')
  to.write <- data.table(ds[[rep]])
  setkey(to.write,driver,depart)
  names(to.write) <- c(';driver','from','to','depart','home')
  write.table(to.write,pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/half-homeless/driver-schedule-pen',(rep*5),'-rep1-4day-20150212.txt'),sep='\t',row.names=F,quote=F)
}
