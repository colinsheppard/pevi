
ds <- list()

for(rep in c(1,2,3,4)){
  ds[[rep]] <- read.table(pp(pevi.shared,'/data/inputs/driver-input-file/delhi-uncombined/driver-schedule-pen5-rep',rep,'-20150212.txt'),header=T,sep='\t')
}

# we need to use rep 4 to get 1-3 up to ~750k drivers

pool <- unique(ds[[4]]$X.driver)
used.drivers <- c()
for(rep in 3:1){
  # 759k is our 5% pen # of drivers and we multiply by 1.08 to adjust for irreduscible strandings in the model
  num.needed <- 1.08 * 759e3 - length(unique(ds[[rep]]$X.driver))
  to.use <- sample(pool,num.needed)
  itins.to.use <- subset(ds[[4]],X.driver%in%to.use)
  itins.to.use$X.driver <- itins.to.use$X.driver + max(ds[[rep]]$X.driver)
  ds[[rep]] <- rbind(ds[[rep]],itins.to.use)
  pool <- pool[!pool %in% to.use]

  if(rep>1){
    for(rep.j in 1:(rep-1)){
      itins.to.use <- ds[[rep.j]]
      itins.to.use$X.driver <- itins.to.use$X.driver + max(ds[[rep]]$X.driver)
      ds[[rep]] <- rbind(ds[[rep]],itins.to.use)
    }
  }
  write.table(ds[[rep]],pp(pevi.shared,'/data/inputs/driver-input-file/delhi-combined/half-homeless/driver-schedule-pen',(rep*5),'-rep1-20150212.txt'),sep='\t',row.names=F)
}

