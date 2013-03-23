library(colinmisc)
Sys.setenv(NOAWT=1)
load.libraries(c('ggplot2','yaml','stringr','RNetLogo','maptools','reshape','colorRamps'))

#base.path <- '/Users/wave/Dropbox/HSU/'
base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/Raskolnikovbot3001/Dropbox/'

#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-75-25'
#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-25-75'
#optim.code <- 'min-cost-constrained-by-frac-stranded-50-50'
optim.code <- 'linked2-50-50'
#optim.code <- 'thresh-1-linked-min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'thresh-2-linked-min-cost-constrained-by-frac-stranded-50-50'
optim.code.date <- paste(optim.code,"-",format(Sys.time(), "%Y%m%d"),sep='')

link.pens <- str_detect(optim.code,"linked")

path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/buildout/',optim.code,'/',sep='')
path.to.outputs <- paste(base.path,'pev-shared/data/outputs/buildout/',optim.code,'/',sep='')
path.to.google <- paste(base.path,'pev-shared/data/google-earth/',sep='')
path.to.geatm <- paste(base.path,'pev-shared/data/GEATM-2020/',sep='')

make.dir(paste(path.to.outputs,"plots",sep=''))
make.dir(paste(path.to.outputs,"plots/",optim.code.date,sep=''))
make.dir(paste(path.to.google,"buildout",sep=''))
make.dir(paste(path.to.inputs,"../../charger-input-file/",optim.code,sep=''))

hard.code.coords <- read.csv(paste(path.to.google,"hard-coded-coords.csv",sep=''),stringsAsFactors=F)
source(paste(path.to.pevi,'R/gis-functions.R',sep=''))
source(paste(path.to.pevi,"R/optim/buildout-functions.R",sep='')) 

# load aggregated tazs
agg.taz <- readShapePoly(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights',sep=''),IDvar="ID")
load(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights-fieldnames.Rdata',sep=''))
names(agg.taz@data) <- c("SP_ID",taz.shp.fieldnames)

# load dist times
dist <- read.csv(file=paste(path.to.geatm,'taz-dist-time.csv',sep=''))
names(dist)[1] <- "from"

results <- list()

#Make build.res a list, differentiated by seed.
build.res <- list()

n.seeds <- 19

#pev.penetration <- 0.005
for(pev.penetration in c(0.005,0.01,0.02,0.04)){

	#Prepares a null matrix to fill with values			
	results.matrix <- matrix(,104,n.seeds)
	colnames(results.matrix) <- paste("seed",1:n.seeds,sep='')
	
	for(seed in 1:n.seeds){
		path.to.outputs <- path.to.outputs <- paste(base.path,'pev-shared/data/outputs/buildout/',optim.code,'-seed',seed,'/',sep='')
		build.files <- data.frame(file=list.files(path.to.outputs,paste('^buildout-pen',pev.penetration*100,'.*csv',sep='')),stringsAsFactors=F)
		if(nrow(build.files)==0)next
		build.files$iter <- as.numeric(sapply(strsplit(build.files$file,'-iter',fixed=T),function(x){ strsplit(x[2],'-',fixed=T)[[1]][1] }))
		build.files <- build.files[order(build.files$iter),]
		build.file.iter <- nrow(build.files)
	
		if(!link.pens | pev.penetration==0.005){
			build.res[[seed]] <- data.frame(iter=rep(build.files$iter,each=105),taz=rep(c(1:52,1:52,0),nrow(build.files)),level=c(rep(2,52),rep(3,52),0),cost=NA,pain=NA,chargers=NA,marg.cost.of.abatement=NA,pen=pev.penetration)
			all.build.rows <- 1:nrow(build.res[[seed]])
		}else{
			build.res[[seed]] <- rbind(build.res[[seed]],data.frame(iter=rep(build.files$iter,each=105),taz=rep(c(1:52,1:52,0),nrow(build.files)),level=c(rep(2,52),rep(3,52),0),cost=NA,pain=NA,chargers=NA,marg.cost.of.abatement=NA,first=NA,name=NA,pen=pev.penetration))
			all.build.rows <- which(build.res[[seed]]$iter %in% build.files$iter)
		}
		
		for(build.i in 1:build.file.iter){
			build.res[[seed]][build.res[[seed]]$iter==build.files$iter[build.i],c('cost','pain','chargers','marg.cost.of.abatement')] <- read.csv(paste(path.to.outputs,build.files$file[build.i],sep=''))[,c('cost','pain','chargers','marg.cost.of.abatement')]
			# write the charger distribution to the inputs directory for use in future model runs
			# write.charger.file(build.res[[seed]]$chargers[build.res[[seed]]$iter==build.files$iter[build.i]][1:104],filepath=paste(path.to.inputs,"../../charger-input-file/",optim.code,"/chargers-iter",build.files$iter[build.i],"-pen",pev.penetration*100,".txt",sep=''))
		}
	
		build.res[[seed]]$name <- agg.taz$name[match(build.res[[seed]]$taz,agg.taz$id)]
	
		first <- ddply(build.res[[seed]],.(taz),function(df){ data.frame(first=ifelse(sum(df$chargers)>0,subset(df,chargers>ifelse(df$taz%in%c(6,23,27),1,0))$iter[1],9999)) })
		build.res[[seed]]$first <- first$first[match(build.res[[seed]]$taz,first$taz)]
    build.res[[seed]]$name <- reorder(build.res[[seed]]$name,build.res[[seed]]$first)
				
    #if(!link.pens){
      results.matrix[,paste('seed',seed,sep='')] <- NA
      n.iter <- max(build.res[[seed]]$iter)
      agg.taz$L2 <- NA
      agg.taz$L3 <- NA
      for(taz.i in 1:52){
        results.matrix[taz.i,c(paste('seed',seed,sep=''))] <- build.res[[seed]]$chargers[build.res[[seed]]$taz == taz.i & build.res[[seed]]$iter==n.iter & build.res[[seed]]$level==2]
        results.matrix[taz.i+52,c(paste('seed',seed,sep=''))] <- build.res[[seed]]$chargers[build.res[[seed]]$taz == taz.i & build.res[[seed]]$iter==n.iter & build.res[[seed]]$level==3]
        agg.taz$L2[which(agg.taz$id==taz.i)] <- build.res[[seed]]$chargers[build.res[[seed]]$taz == taz.i & build.res[[seed]]$iter==n.iter & build.res[[seed]]$level==2]
        agg.taz$L3[which(agg.taz$id==taz.i)] <- build.res[[seed]]$chargers[build.res[[seed]]$taz == taz.i & build.res[[seed]]$iter==n.iter & build.res[[seed]]$level==3]
      }
      #c.map <- paste(map.color(agg.taz@data$weighted.demand,blue2red(50)),'7F',sep='')
      #chargers.to.kml(agg.taz,paste(path.to.google,'buildout/',optim.code,'-pen',100*pev.penetration,'-seed',seed,'.kml',sep=''),paste('Buildout Pen ',100*pev.penetration,'% Optimization: ',optim.code,sep=''),'Color denotes total chargers in each TAZ with L3 counting for 2 chargers (click to get actual # chargers).','black',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','L2','L3','weighted.demand','frac.homes'))
      #to.csv <- agg.taz@data[,c('id','name','L2','L3')]
      #write.csv(to.csv,file=paste(path.to.google,'buildout/',optim.code,'-pen',100*pev.penetration,'-seed-',seed,'.csv',sep=''),row.names=F)
    #}
	
		if(!link.pens){
			# facet by name
			#p <- ggplot(subset(build.res,level>0),aes(x=iter,y=chargers,fill=factor(level),width=1))+geom_bar(stat='identity')+facet_wrap(~name)+opts(title=paste("Loading Order, ",pev.penetration*100,"% Penetration",sep='')) + labs(x="Nth Charger Added", y="Cumulative Number of Chargers in Zone",fill="Charger Level") 
			#ggsave(paste(path.to.outputs,"plots/",optim.code.date,"/loading-order-pen",pev.penetration*100,".pdf",sep=''),p,width=15,height=11)
			# plot the optimality curve
			#p <- ggplot(subset(build.res,taz==0),aes(x=cost,y=pain*100))+geom_point()+scale_x_continuous(limits=c(0,max(subset(build.res,taz==0)$cost)))+scale_y_continuous(limits=c(0,max(subset(build.res,taz==0)$pain*100)))
			#ggsave(paste(path.to.outputs,"plots/",optim.code.date,"/optim-curve-pen",pev.penetration*100,".pdf",sep=''),p,width=11,height=11)
		}
	}	# end loop for(seed in 1:n.seeds)
  # make sure we only process columns with numeric values, no NA's
  numeric.cols <- !is.na(results.matrix[1,])
	results[[as.character(pev.penetration)]] <- results.matrix[,numeric.cols]
	cov.data <- cor(t(results[[as.character(pev.penetration)]]))
  reorder.inds <- match(agg.taz$id,agg.taz$order)
  reorder.inds <- c(reorder.inds,(53:104)[reorder.inds])
  cov.data <- cov.data[reorder.inds,reorder.inds]
	write.csv(cov.data,file=paste(base.path,'pev-shared/data/outputs/buildout/covariance-data/','cov-pen',100*pev.penetration,'.csv',sep=''),row.names=T)
	write.csv(data.frame(name=agg.taz$name,mean=apply(results[[as.character(pev.penetration)]],1,mean,na.rm=T)[reorder.inds],sd=apply(results[[as.character(pev.penetration)]],1,sd,na.rm=T)[reorder.inds]),file=paste(base.path,'pev-shared/data/outputs/buildout/covariance-data/mean-sd-pen',100*pev.penetration,'.csv',sep=''),row.names=T)
  mean.results <- apply(results[[as.character(pev.penetration)]],1,mean,na.rm=T)[reorder.inds]
  
  agg.taz$L2 <- NA
  agg.taz$L3 <- NA
  agg.taz$L2 <- round(mean.results[1:52],digits = 0)
  agg.taz$L3 <- round(mean.results[53:104])
  
  #c.map <- paste(map.color(agg.taz@data$weighted.demand,blue2red(50)),'7F',sep='')
  #chargers.to.kml(agg.taz,paste(path.to.google,'buildout/',optim.code,'-pen',100*pev.penetration,'-mean.kml',sep=''),paste('Buildout Pen ',100*pev.penetration,'% Optimization: ',optim.code,sep=''),'Color denotes total chargers in each TAZ with L3 counting for 2 chargers (click to get actual # chargers).','black',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','L2','L3','weighted.demand','frac.homes'))
  #to.csv <- agg.taz@data[,c('id','name','L2','L3')]  

  # summarize the ranking of each taz in terms of order of first acquisition of a charger, only do it for the last loop or pen4
  rankings <- data.frame(matrix(9999,52,n.seeds))
  names(rankings) <- paste("seed",1:n.seeds,sep='')
  for(seed in 1:n.seeds){
    winners <- subset(build.res[[seed]],iter==build.res[[seed]]$iter[nrow(build.res[[seed]])] & level > 0)[,c('taz','first')]
    rankings[winners$taz,seed] <- winners$first
  }
  rankings[is.na(rankings)] <- 9999
  rankings <- apply(rankings,2,function(x){ 
                    max.real <- max(x[x<9999])
                    x[x==9999] <- max.real+1
                    x/(max.real+1) })
  mean.rank <- order(apply(rankings,1,function(x){ mean(x,na.rm=T) }))
  write.csv(data.frame(name=agg.taz$name,mean.rank=match(agg.taz$id,mean.rank)),file=paste(base.path,'pev-shared/data/outputs/buildout/covariance-data/mean-rankings-',100*pev.penetration,'.csv',sep=''),row.names=T)
  #write.csv(data.frame(name=agg.taz$name[match(mean.rank,agg.taz$id)]),file=paste(base.path,'pev-shared/data/outputs/buildout/covariance-data/mean-rankings-',100*pev.penetration,'.csv',sep=''),row.names=T)
}


  
  
  


build.res[[seed]]$name <- agg.taz$name[match(build.res[[seed]]$taz,agg.taz$id)]
first <- ddply(build.res[[seed]],.(taz),function(df){ data.frame(first=ifelse(sum(df$chargers)>0,subset(df,chargers>0)$iter[1],9999)) })
build.res[[seed]]$first <- first$first[match(build.res[[seed]]$taz,first$taz)]
build.res[[seed]]$name <- reorder(build.res[[seed]]$name,build.res[[seed]]$first)


pen.transitions <- ddply(build.res,.(pen),function(df){ df$iter[1] })
p <- ggplot(subset(build.res,level>0),aes(x=iter,y=chargers,fill=factor(level),width=1))+geom_bar(stat='identity')+geom_vline(data=pen.transitions,aes(xintercept=V1),colour='grey')+facet_wrap(~name)+opts(title=paste("Loading Order, Linked Penetrations",sep='')) + labs(x="Nth Charger Added", y="Cumulative Number of Chargers in Zone",fill="Charger Level") 
#ggsave(paste(path.to.outputs,"plots/",optim.code.date,"/loading-order-linked-pens.pdf",sep=''),p,width=15,height=11)


n.iter <- max(build.res[[seed]]$iter)
agg.taz$L2 <- NA
agg.taz$L3 <- NA
for(taz.i in 1:52){
  agg.taz$L2[which(agg.taz$id==taz.i)] <- build.res[[seed]]$chargers[build.res[[seed]]$taz == taz.i & build.res[[seed]]$iter==n.iter & build.res[[seed]]$level==2]
  agg.taz$L3[which(agg.taz$id==taz.i)] <- build.res[[seed]]$chargers[build.res[[seed]]$taz == taz.i & build.res[[seed]]$iter==n.iter & build.res[[seed]]$level==3]
}
chargers.to.kml(agg.taz,paste(path.to.google,'buildout/',optim.code,'.kml',sep=''),paste('Linked Buildout: ',optim.code,sep=''),'Color denotes total chargers in each TAZ with L3 counting for 2 chargers (click to get actual # chargers).','red',1.5,'blue',id.col='ID',name.col='name',description.cols=c('id','name','L2','L3','weighted.demand','frac.homes'))
to.csv <- agg.taz@data[,c('id','name','L2','L3')]
to.csv$cost <- 8 * to.csv$L2 + 50 * to.csv$L3
to.csv$power <- 6.6 * to.csv$L2 + 30 * to.csv$L3
l2.l3.tazs <- agg.taz$id[which(agg.taz@data$L2>0 | agg.taz@data$L3>0)]
for(taz.id in agg.taz$id){ 
  to.csv$min.dist.to[which(to.csv$id==taz.id)] <- min(subset(dist,from==taz.id & to%in%l2.l3.tazs[l2.l3.tazs != taz.id])$miles) 
}
to.csv$l2.per.trip <- agg.taz$weighted.demand*pev.penetration / agg.taz$L2
to.csv$l3.per.trip <- agg.taz$weighted.demand*pev.penetration / agg.taz$L3
to.csv$l2.per.trip[to.csv$l2.per.trip == Inf] <- NA
to.csv$l3.per.trip[to.csv$l3.per.trip == Inf] <- NA
#write.csv(to.csv,file=paste(path.to.google,'buildout/',optim.code,'.csv',sep=''),row.names=F)
