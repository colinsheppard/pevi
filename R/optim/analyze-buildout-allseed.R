library(colinmisc)
Sys.setenv(NOAWT=1)
load.libraries(c('ggplot2','yaml','stringr','RNetLogo','maptools','reshape','colorRamps','caTools'))

#base.path <- '/Users/wave/Dropbox/HSU/'
base.path <- '/Users/critter/Dropbox/serc/pev-colin/'
#base.path <- '/Users/sheppardc/Dropbox/serc/pev-colin/'
#base.path <- '/Users/Raskolnikovbot3001/Dropbox/'

path.to.pevi <- paste(base.path,'pevi/',sep='')
path.to.google <- paste(base.path,'pev-shared/data/google-earth/',sep='')
path.to.geatm <- paste(base.path,'pev-shared/data/GEATM-2020/',sep='')
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

#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-75-25'
#optim.code <- 'linked-min-cost-constrained-by-frac-stranded-25-75'
#optim.code <- 'min-cost-constrained-by-frac-stranded-50-50'
optim.code <- 'linked2-50-50'
#optim.code <- 'thresh-1-linked-min-cost-constrained-by-frac-stranded-50-50'
#optim.code <- 'thresh-2-linked-min-cost-constrained-by-frac-stranded-50-50'

optim.codes <- c('linked2-50-50','linked2-battery-1.25','linked2-battery-1.5','linked2-battery-2.0','linked2-cost-1.5','linked2-cost-2.0','linked2-type','linked2-base-fixed')

path.to.inputs <- paste(base.path,'pev-shared/data/inputs/buildout/',optim.codes[1],'-seed1/',sep='')
naming <- yaml.load(readChar(paste(path.to.inputs,'../naming.yaml',sep=''),file.info(paste(path.to.inputs,'../naming.yaml',sep=''))$size))

if(exists('compare'))rm('compare')
for(optim.code in optim.codes){

  optim.code.date <- paste(optim.code,"-",format(Sys.time(), "%Y%m%d"),sep='')

  link.pens <- str_detect(optim.code,"linked")

  path.to.inputs <- paste(base.path,'pev-shared/data/inputs/buildout/',optim.code,'-seed1/',sep='')
  #path.to.outputs <- paste(base.path,'pev-shared/data/outputs/buildout/',optim.code,'/',sep='')
  path.to.outputs.base <- paste(base.path,'pev-shared/data/outputs/sensitivity/',optim.code,sep='')

  make.dir(path.to.outputs.base)
  make.dir(paste(path.to.outputs.base,"/",optim.code.date,sep=''))
  make.dir(paste(path.to.google,"buildout",sep=''))
  make.dir(paste(path.to.inputs,"../../charger-input-file/",optim.code,sep=''))

  results <- list()

  #Make build.res a list, differentiated by seed.
  build.res <- list()

  n.seeds <- max(as.numeric(unlist(lapply(strsplit(grep(optim.code,list.files(paste(path.to.outputs.base,"-seed1/..",sep='')),value=T),"-seed"),function(x){ ifelse(length(x)==2,x[2],NA)}))),na.rm=T)

  #pev.penetration <- 0.005
  for(pev.penetration in c(0.005,0.01,0.02,0.04)){

    #Prepares a null matrix to fill with values			
    results.matrix <- matrix(,104,n.seeds)
    colnames(results.matrix) <- paste("seed",1:n.seeds,sep='')
    
    if(pev.penetration==0.005) {target.year <- 2014}
    if(pev.penetration==0.01) {target.year <- 2017}
    if(pev.penetration==0.02) {target.year <- 2023}
    if(pev.penetration==0.04) {target.year <- 'After 2026'}
    
    charger.descrip.text <- paste('This google earth layer presents the number of recommended plug-in electric vehicle (PEV) chargers for each each travel analysis zone if PEV adoption reaches ',pev.penetration*100,'% penetration into the Humboldt County vehicle fleet.  Each icon contains two numbers indicating the number of Level 2 and the number of Level 3 chargers to be sited in that zone (separated by a slash).  Level 2 chargers can provide a full charge to a Nissan Leaf in 5-6 hours, Level 3 chargers would take less than 1 hour.   The map contains an additional level of detail for the municipalities of Eureka, Arcata, McKinleyville, and Fortuna, zoom in to reveal this detail.',sep='')
    driver.descrip.text <- paste('This google earth layer presents the number of expected plug-in electric vehicle (PEV) drivers living in each travel analysis zone as simulated in the North Coast PEV modeling analysis.  The map contains an additional level of detail for the municipalities of Eureka, Arcata, McKinleyville, and Fortuna, zoom in to reveal this detail.',sep='')
    
    for(seed in 1:n.seeds){
      path.to.charger.input.file <- paste(path.to.inputs,"../../charger-input-file/",optim.code,"/seed",seed,sep='')
      make.dir(path.to.charger.input.file)
      path.to.outputs <- paste(path.to.outputs.base,'-seed',seed,'/',sep='')
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
        write.charger.file(build.res[[seed]]$chargers[build.res[[seed]]$iter==build.files$iter[build.i]][1:104],filepath=paste(path.to.charger.input.file,"/chargers-iter",build.files$iter[build.i],"-pen",pev.penetration*100,".txt",sep=''))
      }
    
      build.res[[seed]]$name <- agg.taz$name[match(build.res[[seed]]$taz,agg.taz$id)]
    
      first <- ddply(build.res[[seed]],.(taz),function(df){ data.frame(first=ifelse(sum(df$chargers)>0,subset(df,chargers>ifelse(df$taz%in%c(6,23,27),1,0))$iter[1],9999)) })
      build.res[[seed]]$first <- first$first[match(build.res[[seed]]$taz,first$taz)]
      build.res[[seed]]$name <- reorder(build.res[[seed]]$name,build.res[[seed]]$first)
          
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
    
    }	# end loop for(seed in 1:n.seeds)

    # make sure we only process columns with numeric values, no NA's
    numeric.cols <- !is.na(results.matrix[1,])
    results[[as.character(pev.penetration)]] <- results.matrix[,numeric.cols]
    reorder.inds <- match(agg.taz$id,agg.taz$order)
    reorder.inds <- c(reorder.inds,(53:104)[reorder.inds])
    #cov.data <- cor(t(results[[as.character(pev.penetration)]]))
    #cov.data <- cov.data[reorder.inds,reorder.inds]
    mean.results <- apply(results[[as.character(pev.penetration)]],1,mean,na.rm=T)[reorder.inds]
    sd.results <- apply(results[[as.character(pev.penetration)]],1,sd,na.rm=T)[reorder.inds]
    
    agg.taz$L2 <- NA
    agg.taz$L3 <- NA
    agg.taz$L2 <- round(mean.results[1:52],digits = 0)
    agg.taz$L3 <- round(mean.results[53:104])
    
    c.map <- paste(map.color(agg.taz@data$weighted.demand,blue2red(50)),'7F',sep='')
    #demand.to.kml(agg.taz,150e3 * pev.penetration,paste(path.to.google,'buildout/',optim.code,'-pen',100*pev.penetration,'-demand-publish.kml',sep=''),paste('PEV Drivers at ',100*pev.penetration,'% Penetration (Target Year ',target.year,')',sep=''),driver.descrip.text,'black',1.5,c.map,id.col='ID',name.col='name',description.cols=c('weighted.demand'))
    chargers.to.kml(agg.taz,paste(path.to.outputs.base,'/',optim.code.date,"/",optim.code,'-mean-pen',100*pev.penetration,'.kml',sep=''),paste('Charger Deployment for ',100*pev.penetration,'% PEV Penetration (Target Year ',target.year,')',sep=''),charger.descrip.text,'black',1.5,'#0000FF7F',id.col='ID',name.col='name',description.cols=c('L2','L3','weighted.demand'),open.kml=F)

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
    mean.data <- data.frame(scenario=optim.code,pen=pev.penetration,name=agg.taz$name,level=c(rep(2,52),rep(3,52)),mean=mean.results,sd=sd.results,mean.rank=match(agg.taz$id,mean.rank))
    if(!exists('compare')){
      compare <- mean.data
    }else{
      compare <- rbind(compare,mean.data)
    } 
    write.csv(mean.data,file=paste(path.to.outputs.base,'/',optim.code.date,'/mean-sd-rank-pen',100*pev.penetration,'.csv',sep=''),row.names=T)
  }
}
write.csv(compare,file=paste(path.to.outputs.base,'/../analysis/compare-optim-scenarios.csv',sep=''))

# Load data already compiled
path.to.outputs.base <- paste(base.path,'pev-shared/data/outputs/sensitivity/',optim.codes[1],sep='')
compare <- read.csv(file=paste(path.to.outputs.base,'/../analysis/compare-optim-scenarios.csv',sep=''))


# Plotting Comparisons

compare$level <- factor(compare$level)
compare$scenario.named <- NA
compare$scenario.order <- NA

for(name.i in names(naming$`optim-code`)){
  compare$scenario.named[compare$scenario==name.i] <- naming$`optim-code`[[name.i]][[1]]
  compare$scenario.order[compare$scenario==name.i] <- naming$`optim-code`[[name.i]][[2]]
}
compare$scenario.named <- reorder(factor(compare$scenario.named),compare$scenario.order)
for(name.i in names(naming$tazs)){
  compare$name.order[compare$name==name.i] <- naming$tazs[[name.i]][[2]]
}
compare$name <- reorder(factor(compare$name),-compare$name.order)

# Summary of total L2/L3 chargers in 2% by scenario
ggplot(ddply(subset(compare,pen==.02),.(scenario.named,level),summarise,num.chargers=sum(mean)),aes(x=scenario.named,y=num.chargers,fill=level))+geom_bar(stat="identity",position="dodge")

# Compare spatial distributions
scen.combs <- combs(as.character(unique(compare$scenario)),2)

pdf(file=paste(path.to.outputs.base,'/../analysis/compare-tazs.pdf',sep=''),width=8,height=11)
for(scen.i in 1:nrow(scen.combs)){
  scen.a.code <- scen.combs[scen.i,1]
  scen.b.code <- scen.combs[scen.i,2]
  scen.a.name <- naming$`optim-code`[[scen.a.code]][[1]]
  scen.b.name <- naming$`optim-code`[[scen.b.code]][[1]]
  scen.a <- subset(compare,scenario==scen.a.code & pen==0.02)
  scen.b <- subset(compare,scenario==scen.b.code & pen==0.02)

  scen.a$b.named <- scen.b$scenario.named
  scen.a$diff <- scen.b$mean - scen.a$mean
  scen.a$diff.rank <- scen.a$mean.rank - scen.b$mean.rank

  p <- ggplot(scen.a,aes(x=name,y=diff,fill=level))+geom_bar(position="dodge",stat='identity')+coord_flip()+labs(title=paste("(",scen.b.name,") - (",scen.a.name,")",sep=''),y="Difference in Chargers",x="TAZ")
  print(p)
}
dev.off()
