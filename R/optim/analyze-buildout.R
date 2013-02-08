library(colinmisc)
Sys.setenv(NOAWT=1)
load.libraries(c('ggplot2','yaml','stringr','RNetLogo','maptools','reshape','colorRamps'))

base.path <- '/Users/wave/Dropbox/HSU/'
#base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/Raskolnikovbot3001/Dropbox/'

#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-75-25'
#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-25-75'
optim.code <- 'min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'thresh-1-linked-min-cost-constrained-by-frac-stranded-50-50'
optim.code.date <- paste(optim.code,"-",format(Sys.time(), "%Y%m%d"),sep='')

link.pens <- str_detect(optim.code,"linked")

path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.inputs <- paste(base.path,'pev-shared/data/inputs/buildout/',optim.code,'/',sep='')
path.to.outputs <- paste(base.path,'pev-shared/data/outputs/buildout/',optim.code,'/',sep='')
path.to.google <- paste(base.path,'pev-shared/data/google-earth/',sep='')

make.dir(paste(path.to.outputs,"plots",sep=''))
make.dir(paste(path.to.outputs,"plots/",optim.code.date,sep=''))
make.dir(paste(path.to.google,"buildout",sep=''))

nl.path <- "/Applications/NetLogo\ 5.0.3"
model.path <- paste(path.to.pevi,"netlogo/PEVI-nolog.nlogo",sep='')

source(paste(path.to.pevi,'R/gis-functions.R',sep=''))
source(paste(path.to.pevi,"R/optim/buildout-functions.R",sep='')) 

# load aggregated tazs
agg.taz <- readShapePoly(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights',sep=''))
load(paste(path.to.google,'aggregated-taz-with-weights/aggregated-taz-with-weights-fieldnames.Rdata',sep=''))
names(agg.taz@data) <- c('row',taz.shp.fieldnames)
agg.taz@data$ID <- unlist(lapply(agg.taz@polygons,function(x){slot(x,'ID')}))


#pev.penetration <- 0.005
for(pev.penetration in c(0.005,0.01,0.02,0.04)){
  build.files <- data.frame(file=list.files(path.to.outputs,paste('^buildout-pen',pev.penetration*100,'.*csv',sep='')),stringsAsFactors=F)
  if(nrow(build.files)==0)next
  build.files$iter <- as.numeric(sapply(strsplit(build.files$file,'-iter',fixed=T),function(x){ strsplit(x[2],'-',fixed=T)[[1]][1] }))
  build.files <- build.files[order(build.files$iter),]
  n.iter <- nrow(build.files)

  if(!link.pens | pev.penetration==0.005){
    build.res <- data.frame(iter=rep(build.files$iter,each=105),taz=rep(c(1:52,1:52,0),nrow(build.files)),level=c(rep(2,52),rep(3,52),0),cost=NA,pain=NA,chargers=NA,marg.cost.of.abatement=NA,pen=pev.penetration)
    all.build.rows <- 1:nrow(build.res)
  }else{
    build.res <- rbind(build.res,data.frame(iter=rep(build.files$iter,each=105),taz=rep(c(1:52,1:52,0),nrow(build.files)),level=c(rep(2,52),rep(3,52),0),cost=NA,pain=NA,chargers=NA,marg.cost.of.abatement=NA,first=NA,name=NA,pen=pev.penetration))
    all.build.rows <- which(build.res$iter %in% build.files$iter)
  }
  
  for(build.i in 1:n.iter){
    build.res[build.res$iter==build.files$iter[build.i],c('cost','pain','chargers','marg.cost.of.abatement')] <- read.csv(paste(path.to.outputs,build.files$file[build.i],sep=''))[,c('cost','pain','chargers','marg.cost.of.abatement')]
  }

  build.res$name <- agg.taz$name[match(build.res$taz,agg.taz$id)]

  # plot every 5th iteration
  #ggplot(subset(build.res,level>0 & iter%%5==0),aes(x=taz,y=chargers,fill=factor(level)))+geom_bar(stat='identity')+facet_wrap(~iter)

  first <- ddply(build.res,.(taz),function(df){ data.frame(first=ifelse(sum(df$chargers)>0,subset(df,chargers>0)$iter[1],9999)) })
  build.res$first <- first$first[match(build.res$taz,first$taz)]
  build.res$name <- reorder(build.res$name,build.res$first)

  if(!link.pens){
    agg.taz$L2 <- NA
    agg.taz$L3 <- NA
    for(taz.i in 1:52){
      agg.taz$L2[which(agg.taz$id==taz.i)] <- build.res$chargers[build.res$taz == taz.i & build.res$iter==n.iter & build.res$level==2]
      agg.taz$L3[which(agg.taz$id==taz.i)] <- build.res$chargers[build.res$taz == taz.i & build.res$iter==n.iter & build.res$level==3]
    }
    chargers.to.kml(agg.taz,paste(path.to.google,'buildout/',optim.code,'-pen',100*pev.penetration,'.kml',sep=''),paste('Buildout Pen ',100*pev.penetration,'% Optimization: ',optim.code,sep=''),'Color denotes total chargers in each TAZ with L3 counting for 2 chargers (click to get actual # chargers).','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','L2','L3','weighted.demand','frac.homes'))
    to.csv <- agg.taz@data[,c('id','name','L2','L3')]
    to.csv$cost <- 8 * to.csv$L2 + 50 * to.csv$L3
    to.csv$power <- 6.6 * to.csv$L2 + 30 * to.csv$L3
    write.csv(to.csv,file=paste(path.to.google,'buildout/',optim.code,'-pen',100*pev.penetration,'.csv',sep=''),row.names=F)
  }

  if(!link.pens){
    # facet by name
    p <- ggplot(subset(build.res,level>0),aes(x=iter,y=chargers,fill=factor(level),width=1))+geom_bar(stat='identity')+facet_wrap(~name)+opts(title=paste("Loading Order, ",pev.penetration*100,"% Penetration",sep='')) + labs(x="Nth Charger Added", y="Cumulative Number of Chargers in Zone",fill="Charger Level") 
    ggsave(paste(path.to.outputs,"plots/",optim.code.date,"/loading-order-pen",pev.penetration*100,".pdf",sep=''),p,width=15,height=11)
  }
}

build.res$name <- agg.taz$name[match(build.res$taz,agg.taz$id)]
first <- ddply(build.res,.(taz),function(df){ data.frame(first=ifelse(sum(df$chargers)>0,subset(df,chargers>0)$iter[1],9999)) })
build.res$first <- first$first[match(build.res$taz,first$taz)]
build.res$name <- reorder(build.res$name,build.res$first)


pen.transitions <- ddply(build.res,.(pen),function(df){ df$iter[1] })
p <- ggplot(subset(build.res,level>0),aes(x=iter,y=chargers,fill=factor(level),width=1))+geom_bar(stat='identity')+geom_vline(data=pen.transitions,aes(xintercept=V1),colour='grey')+facet_wrap(~name)+opts(title=paste("Loading Order, Linked Penetrations",sep='')) + labs(x="Nth Charger Added", y="Cumulative Number of Chargers in Zone",fill="Charger Level") 
ggsave(paste(path.to.outputs,"plots/",optim.code.date,"/loading-order-linked-pens.pdf",sep=''),p,width=15,height=11)


n.iter <- max(build.res$iter)
agg.taz$L2 <- NA
agg.taz$L3 <- NA
for(taz.i in 1:52){
  agg.taz$L2[which(agg.taz$id==taz.i)] <- build.res$chargers[build.res$taz == taz.i & build.res$iter==n.iter & build.res$level==2]
  agg.taz$L3[which(agg.taz$id==taz.i)] <- build.res$chargers[build.res$taz == taz.i & build.res$iter==n.iter & build.res$level==3]
}
chargers.to.kml(agg.taz,paste(path.to.google,'buildout/',optim.code,'.kml',sep=''),paste('Linked Buildout: ',optim.code,sep=''),'Color denotes total chargers in each TAZ with L3 counting for 2 chargers (click to get actual # chargers).','red',1.5,c.map,id.col='ID',name.col='name',description.cols=c('id','name','L2','L3','weighted.demand','frac.homes'))
to.csv <- agg.taz@data[,c('id','name','L2','L3')]
to.csv$cost <- 8 * to.csv$L2 + 50 * to.csv$L3
to.csv$power <- 6.6 * to.csv$L2 + 30 * to.csv$L3
write.csv(to.csv,file=paste(path.to.google,'buildout/',optim.code,'.csv',sep=''),row.names=F)
