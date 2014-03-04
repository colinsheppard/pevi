# A handy for loop to combine 0.5% penetration itineraries into half as many 1% itineraries.
# To combine 1% and 2% files, adjust "pev.penetration". If working with more than 80 files, adjust the i limits.

#path.to.uncombined.inputs <- pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined/')
#path.to.combined.inputs <- pp(pevi.shared,'data/inputs/driver-input-file/upstate-combined/')

load(pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined-schedule-replicates-20140217.Rdata'))
path.to.combined.inputs <- pp(pevi.shared,'data/inputs/driver-input-file/delhi-combined/')
path.to.combined.homeless.inputs <- pp(pevi.shared,'data/inputs/driver-input-file/delhi-combined/homeless/')

pens <- c(0.005,0.01,0.02)

for (pen in pens){
  final.end <- ifelse(pen==0.005,80,80/(pen/0.005))
  for (final.replicate in 1:final.end){
    rep.1 <- 2*final.replicate-1
    rep.2 <- 2*final.replicate
    if(pen==0.005){
      sched <- schedule.reps[['0.005']][[final.replicate]]
      next.sched <- data.frame()
    }else{
      sched <- schedule.reps[['0.005']][[rep.1]]
      next.sched <- schedule.reps[['0.005']][[rep.2]]
      next.sched$driver <- next.sched$driver + max(sched$driver)
    }
    new.sched <- data.table(rbind(sched,next.sched,deparse.level = 0),key='driver')
    new.sched <- as.data.frame(new.sched[,list(from=rep(from,2),to=rep(to,2),depart=c(depart,depart+24),home=rep(home,2)),by='driver'])
    #new.sched <- rbind(sched,next.sched,deparse.level = 0)
    #new.sched <- ddply(new.sched,.(driver),function(df){ 
                   #df2 <- df 
                   #df2$depart <- df2$depart + 24
                   #rbind(df,df2)
                  #})
    names(new.sched) <- c(';driver',tail(names(new.sched),-1))
    write.table(new.sched[,c(';driver','from','to','depart','home')],file=paste(path.to.combined.inputs,"driver-schedule-pen",pen*100,"-rep",final.replicate,"-20140217.txt",sep=''),sep='\t',row.names=F,quote=F)
    new.sched$home <- 0
    write.table(new.sched[,c(';driver','from','to','depart','home')],file=paste(path.to.combined.homeless.inputs,"driver-schedule-pen",pen*100,"-rep",final.replicate,"-20140217.txt",sep=''),sep='\t',row.names=F,quote=F)
  }
}
