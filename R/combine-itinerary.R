# A handy for loop to combine 0.5% penetration itineraries into half as many 1% itineraries.
# To combine 1% and 2% files, adjust "pev.penetration".

path.to.combined.inputs <- paste(base.path,'pev-shared/data/inputs/driver-input-file/combined/',sep='')

pev.penetration <- 0.005
replicate <- 1
final.replicate <- 1

for (i in 1:40){
	if(file.exists(paste(path.to.combined.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130219.txt",sep=''))==FALSE) {break}
	next.replicate <- replicate + 1
	sched <- read.table(file=paste(path.to.combined.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",replicate,"-20130219.txt",sep=''),sep='\t',header=T)
	next.sched <- read.table(file=paste(path.to.combined.inputs,"driver-schedule-pen",pev.penetration*100,"-rep",next.replicate,"-20130219.txt",sep=''),sep='\t',header=T)
	next.sched[,1] <- next.sched[,1] + max(sched[,1])
	new.sched <- rbind(sched,next.sched,deparse.level = 0)
	names(new.sched) <- c(';driver','from','to','depart','home')
	write.table(new.sched,file=paste(path.to.combined.inputs,"driver-schedule-pen",pev.penetration*200,"-rep",final.replicate,"-20130219.txt",sep=''),sep='\t',row.names=F,quote=F)
	replicate <- replicate + 2
	final.replicate <- final.replicate + 1
}