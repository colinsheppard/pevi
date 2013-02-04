library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools','doMC','reshape','maptools'))

path.to.outputs <- '~/Dropbox/serc/pev-colin/data/scheduler-optim/'
load(paste(path.to.outputs,'image-20130129.Rdata',sep=''))

pev.penetration <- as.numeric(commandArgs(trailingOnly=T)[1])

source(paste(path.to.pevi,'R/create-schedule.R',sep=''))

registerDoMC(num.processors)

pev.pen.char <- roundC(pev.penetration,3)
schedule.reps <- list()
schedule.reps[[pev.pen.char]] <- list()
for(replicate in 1:num.replicates){
  print(paste('Penetration ',pev.penetration,' replicate ',replicate,sep=''))
  schedule.reps[[pev.pen.char]][[as.character(replicate)]] <- create.schedule(pev.penetration,1)
  write.table(schedule.reps[[pev.pen.char]][[as.character(replicate)]][,c('driver','from','to','depart','home')],file=paste(path.to.shared.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130129.txt",sep=''),sep='\t',row.names=F,quote=F)
  save(schedule.reps,file=paste(path.to.outputs,'schedule-replicates-pen',pev.penetration*100,'-20130129.Rdata',sep=''))
}
save(schedule.reps,file=paste(path.to.outputs,'schedule-replicates-pen',pev.penetration*100,'-20130129.Rdata',sep=''))
