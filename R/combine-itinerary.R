# A handy for loop to combine 0.5% penetration itineraries into half as many 1% itineraries.
# To combine 1% and 2% files, adjust "pev.penetration". If working with more than 80 files, adjust the i limits.

#path.to.uncombined.inputs <- pp(pevi.shared,'data/inputs/driver-input-file/upstate-uncombined/')
#path.to.combined.inputs <- pp(pevi.shared,'data/inputs/driver-input-file/upstate-combined/')

load(pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined-schedule-replicates-20140217.Rdata'))
load(pp(pevi.shared,'data/inputs/driver-input-file/delhi-uncombined-schedule-replicates-20140422.Rdata'))

# ANDY make sure to change file paths to a "congested" directory

path.to.combined.inputs <- pp(pevi.shared,'data/inputs/driver-input-file/delhi-combined/congested/doubled/')
path.to.combined.homeless.inputs <- pp(pevi.shared,'data/inputs/driver-input-file/delhi-combined//congested/homeless/')
path.to.combined.half.homeless.inputs <- pp(pevi.shared,'data/inputs/driver-input-file/delhi-combined/congested/half-homeless/')

pens <- c(0.005,0.01,0.02)
schedule.reps[['0.01']] <- list()

for (pen in pens){
  prev.pen <- ifelse(pen==0.01,'0.005','0.01')
  #pen <- pens[1]
  final.end <- ifelse(pen==0.005,20,20/(pen/0.005))
  for (final.replicate in 1:final.end){
    #final.replicate <- 1
    rep.1 <- 2*final.replicate-1
    rep.2 <- 2*final.replicate
    if(pen==0.005){
      sched <- schedule.reps[['0.005']][[final.replicate]]
      sched$depart <- sched$depart - 6
      next.sched <- data.frame()
    }else{
      sched <- schedule.reps[[prev.pen]][[rep.1]]
      next.sched <- schedule.reps[[prev.pen]][[rep.2]]
      next.sched$driver <- next.sched$driver + max(sched$driver)
    }
    new.sched <- data.table(rbind(sched,next.sched,deparse.level = 0),key='driver')
    if(pen==0.01){
      schedule.reps[['0.01']][[final.replicate]] <- new.sched
    }
    new.sched <- as.data.frame(new.sched[,list(from=rep(from,2),to=rep(to,2),depart=c(depart,depart+24),home=rep(home,2)),by='driver'])
    # fix inconsistency in drivers who don't end up the first day where they start
    new.sched$day <- ifelse(new.sched$depart<30,1,2)
    new.sched <- ddply(new.sched,.(driver),function(df){
      df$from[which(df$day==2)[1]] <- tail(subset(df,day==1),1)$to
      df
    })
    quad.sched <- as.data.frame(data.table(new.sched,key='driver')[,list(from=rep(from,2),to=rep(to,2),depart=c(depart,depart+48),home=rep(home,2)),by='driver'])
    quad.sched$day <- ifelse(quad.sched$depart<30,1,ifelse(quad.sched$depart<54,2,ifelse(quad.sched$depart<78,3,4)))
    quad.sched <- ddply(quad.sched,.(driver),function(df){
      df$from[which(df$day==2)[1]] <- tail(subset(df,day==1),1)$to
      df$from[which(df$day==3)[1]] <- tail(subset(df,day==2),1)$to
      df$from[which(df$day==4)[1]] <- tail(subset(df,day==3),1)$to
      df
    })
    new.sched <- new.sched[,1:(ncol(new.sched)-1)]
    quad.sched <- quad.sched[,1:(ncol(quad.sched)-1)]
    for(path.to.combined in pp(c(path.to.combined.inputs,path.to.combined.half.homeless.inputs,path.to.combined.homeless.inputs),'consistent/')){
      #path.to.combined <- path.to.combined.inputs
      make.dir(path.to.combined)
      if(path.to.combined == pp(path.to.combined.half.homeless.inputs,'consistent/')){
        names(new.sched) <- c('driver',tail(names(new.sched),-1))
        evics <- data.frame(driver=unique(new.sched$driver),evict=sample(c(T,F),length(unique(new.sched$driver)),replace=T))
        evics.sched <- evics$evict[match(new.sched$driver,evics$driver)]
        new.sched$home[evics.sched] <- 0
      }else if(path.to.combined == pp(path.to.combined.homeless.inputs,'consistent/')){
        new.sched$home <- 0
      }
      names(new.sched) <- c(';driver',tail(names(new.sched),-1))
      write.table(new.sched[,c(';driver','from','to','depart','home')],file=pp(path.to.combined,"driver-schedule-pen",pen*100,"-rep",final.replicate,"-20140422.txt"),sep='\t',row.names=F,quote=F)
      print(pp(path.to.combined,"driver-schedule-pen",pen*100,"-rep",final.replicate,"-20140217.txt"))
      print(h(new.sched))
      print(sum(new.sched$home==0))
    }
    for(path.to.combined in pp(c(path.to.combined.inputs,path.to.combined.half.homeless.inputs,path.to.combined.homeless.inputs),'quadrupled/')){
      #path.to.combined <- path.to.combined.inputs
      make.dir(path.to.combined)
      if(path.to.combined == pp(path.to.combined.half.homeless.inputs,'quadrupled/')){
        names(quad.sched) <- c('driver',tail(names(quad.sched),-1))
        evics <- data.frame(driver=unique(quad.sched$driver),evict=sample(c(T,F),length(unique(quad.sched$driver)),replace=T))
        evics.sched <- evics$evict[match(quad.sched$driver,evics$driver)]
        quad.sched$home[evics.sched] <- 0
      }else if(path.to.combined == pp(path.to.combined.homeless.inputs,'quadrupled/')){
        quad.sched$home <- 0
      }
      names(quad.sched) <- c(';driver',tail(names(quad.sched),-1))
      write.table(quad.sched[,c(';driver','from','to','depart','home')],file=pp(path.to.combined,"driver-schedule-pen",pen*100,"-rep",final.replicate,"-20140422.txt"),sep='\t',row.names=F,quote=F)
      print(pp(path.to.combined,"driver-schedule-pen",pen*100,"-rep",final.replicate,"-20140422.txt"))
      print(h(quad.sched))
      print(sum(quad.sched$home==0))
    }
  }
}

# analyze incidence of teleportation
# result: 0.67% of drivers teleport
cons <- ddply(new.sched,.(driver),function(df){
  end.pt <- tail(subset(df,day==1),1)$to
  st.pt <- head(subset(df,day==1),2)$from
  data.frame(consistent=end.pt==st.pt)
})
