
evaluate.fitness <- function(ptx){
  if(!exists('cl')){
    print('starting new cluster')
    cl <- makeSOCKcluster(rep("localhost",num.cpu))
  }
  numrows <- nrow(ptx)
  breaks <- seq(1,numrows,by=ceiling(numrows/length(cl)))
  break.pairs <- list()
  for(i in 1:(length(breaks)-1)){
    break.pairs[[i]] <- c(breaks[i],breaks[i+1]-1)
  }
  break.pairs[[i+1]] <- c(breaks[i+1],numrows)

  results<-clusterEvalQ(cl,rm(list=ls()))
  clusterExport(cl,c( 'run.pevi.batch','pev.penetration','path.to.inputs','path.to.outputs','nl.path','model.path',
                      'write.charger.file','reporters','logfiles','results','fitness.params','path.to.pevi','vary.tab'))
  clusterEvalQ(cl,Sys.setenv(NOAWT=1))
  clusterEvalQ(cl,library('RNetLogo'))
  clusterEvalQ(cl,library('colinmisc'))
  rm('results')
  results<-clusterApply(cl,break.pairs,fun='run.pevi.batch',ptx=ptx)
  results<-unlist(results)

  return(results)
}

stop.criteria <- function(fit,gen.num){
  if(all(fit==Inf))return(F)
  # true if all deviations of fitnesses from the min are less than threshold OR if we've hit max iterations
  return(all((fit-min(fit))/abs(min(fit))<stop.params$diff.from.best.threshold) | gen.num>=de.params$max.iter)
}

# combine the objective and constrainst into the "objective" function called within the model
if(exists('objective'))rm('objective')
objective <- function(){
  constr <- 0
  for(constraint.name in constraint.names){
    constr <- constr + streval(paste(constraint.name,"()",sep=''))
  }
  return( streval(paste(objective.name,"()",sep='')) + constr )
}

run.pevi.batch <- function(break.pair,ptx){
  ll<-break.pair[1]
  ul<-break.pair[2]
  break.pair.code <- paste("node ",paste(break.pair,collapse=","),":",sep='')
  batch.results <- array(NA,length(ll:ul))
  i <- 1

  tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
  NLLoadModel(model.path)
  for(cmd in paste('set log-',logfiles,' false',sep='')){ NLCommand(cmd) }

  for(ptx.i in ll:ul){
    write.charger.file(ptx[ptx.i,],ptx.i)
    for(results.i in 1:nrow(results)){
      NLCommand('clear-all-and-initialize')
      NLCommand(paste('set parameter-file "',path.to.inputs,'params.txt"',sep=''))
      NLCommand(paste('set model-directory "',path.to.pevi,'netlogo/"',sep=''))
      NLCommand('read-parameter-file')
      for(param in names(vary.tab)){
        if(is.character(vary.tab[1,param])){
          NLCommand(paste('set ',param,' "',vary.tab[results.i,param],'"',sep=''))
        }else{
          NLCommand(paste('set ',param,' ',vary.tab[results.i,param],'',sep=''))
        }
      }
      NLCommand('setup')
      NLCommand('dynamic-scheduler:go-until schedule 500')
      results[results.i,names(reporters)] <- tryCatch(NLDoReport(1,"",reporter = paste("(sentence",paste(reporters,collapse=' '),")"),as.data.frame=T,df.col.names=names(reporters)),error=function(e){ NA })
    }
    batch.results[i] <- streval(paste(fitness.params[['aggregation.function']],"(c(",paste(results[,fitness.params[['summary.stat']]],collapse=","),"),na.rm=T)",sep=''))
    i <- i+1
  }
  cat(paste('batch results: ',paste(batch.results,collpase=","),sep=''),file=paste(path.to.outputs,"/logfile.txt",sep=''),append=T,fill=T,labels=break.pair.code) 
  return(batch.results)
}

write.charger.file <- function(num.chargers,ptx.i){
  chargers <- data.frame(TAZ=1:52,L1=num.chargers[1:52],L2=num.chargers[1:52],L3=num.chargers[53:104])
  names(chargers) <- c(";TAZ","L1","L2","L3")
  write.table(chargers,file=paste(path.to.inputs,'chargers-ptx',ptx.i,'.txt',sep=''),sep="\t",row.names=F,quote=F)
}

### plot.ptx(ptx,dimensions=1:2)
# this will create an animation of the particles over time projected onto the 2 specified dimensions
plot.ptx <- function(ptx,num.gens=1,dimensions=1:2,decision.vars=decision.vars,quantize.range=1){
  #fit.range <- round(10*range(ptx[,'fitness',],na.rm=T))
  fits <- as.numeric(ptx[,'fitness',])
  fits <- fits[fits<Inf]
  fit.range <- round(quantize.range * range(fits,na.rm=T))
  cols <- diverge_hcl(diff(fit.range)+1)
  library(animation)
  oopt = ani.options(interval = 0.5, nmax = num.gens, ani.dev = png, withprompt=F,
    ani.type = "png",
    title = paste("DE Evolution Results"),
    description  = paste("Projection onto dimensions:",paste(apply(decision.vars[dimensions,c('mod.name','param.name')],1,paste,collapse="."),collapse=", "))
  )
  #xdim <- seq(50,100,by=0.1)
  #ydim <- seq(50,100,by=0.1)
  #ack <- array(NA,c(length(xdim),length(ydim)))
  #for(i in 1:length(xdim)){
    #for(j in 1:length(ydim)){
      #ack[i,j] <- ackley(c(xdim[i]-75,ydim[j]-75))
    #}
  #}
  ani.start()
  for(gen in 1:num.gens){
    #contour(xdim,ydim,ack,col="grey",
      #xlim=as.numeric(decision.vars[dimensions[1],c('lbound','ubound')]),
      #ylim=as.numeric(decision.vars[dimensions[2],c('lbound','ubound')]),
      #xlab=paste(decision.vars[1,c('mod.name','param.name')],collapse="."),
      #ylab=paste(decision.vars[2,c('mod.name','param.name')],collapse="."),
      #main=paste("Gen:",gen))
    col.ind <- round(quantize.range * ptx[,'fitness',gen])
    col.ind <- col.ind - round(min(fits,na.rm=T)) + 1
    col.ind[col.ind==Inf] <- length(cols)
    
    plot(ptx[,dimensions[1],gen],ptx[,dimensions[2],gen],
      col=cols[col.ind],
      bg=cols[col.ind],pch=23,
      xlim=as.numeric(decision.vars[dimensions[1],c('lbound','ubound')]),
      ylim=as.numeric(decision.vars[dimensions[2],c('lbound','ubound')]),
      xlab=paste(decision.vars[1,c('mod.name','param.name')],collapse="."),
      ylab=paste(decision.vars[2,c('mod.name','param.name')],collapse="."),
      main=paste("Gen:",gen))
    #points(75,75,pch="X",col="red")
  }
  ani.stop()
}
plot.ptx.alldim <- function(ptx,num.gens=1,decision.vars=decision.vars,quantize.range=1,width=1500,height=1500,no.ani=F,pt.cex=1){
  #fit.range <- round(10*range(ptx[,'fitness',],na.rm=T))
  fits <- as.numeric(ptx[,'fitness',])
  fits <- fits[fits<Inf]
  fit.range <- round(quantize.range * range(fits,na.rm=T))
  if(log(diff(fit.range),base=10)>2){ # implies more than 100 levels of color, let's increase the spacing
    quantize.range <- 10^(-(floor(log(diff(fit.range),base=10))-1))
    fit.range <- round(quantize.range * range(fits,na.rm=T))
  }
  cols <- diverge_hcl(diff(fit.range)+2)
  cols[length(cols)] <- "green"
  library(animation)
  oopt = ani.options(interval = 0.5, nmax = num.gens, ani.dev = png, withprompt=F,
    ani.type = "png",
    title = paste("DE Evolution Results"),
    description  = "",
    ani.width=width, ani.height=height
  )
  n <- nrow(decision.vars)
  layout.matrix <- matrix(1:n^2,n,byrow=T)
  saved.inf <- array(NA,c(dim(ptx)[1]*dim(ptx)[3],dim(ptx)[2]-1))
  
  if(!no.ani){
    ani.start()
    gen.start <- 1
  }else{
    gen.start <- num.gens
  }
  for(gen in gen.start:num.gens){
    col.ind <- round(quantize.range * ptx[,'fitness',gen])
    col.ind <- col.ind - fit.range[1] + 1
    n.inf <- sum(col.ind==Inf)
    if(n.inf>0){
      n.inf.saved <- sum(!is.na(saved.inf[,1]))
      saved.inf[(n.inf.saved+1):(n.inf.saved+n.inf),] <- ptx[col.ind==Inf,1:(dim(ptx)[2]-1),gen]
    }
    col.ind[col.ind==Inf] <- length(cols)
  
    nf<-layout(layout.matrix)
    for(dim.i in 1:n){
      if(dim.i > 1){
        for(skip in 1:(dim.i-1)){
          if(dim.i==n & skip==1){
            plot.new()
            text(0.5,0.5,gen,font=2,cex=2)      
          }else if(dim.i==n & skip==2){
            fit.to.plot <- ptx[,'fitness',gen]
            fit.to.plot[fit.to.plot==Inf] <- NA
            hist(ptx[,'fitness',gen],main="Fitness (Inf removed)")
          }else{
            plot.new()
          }
        }
      }
      dim.name <- paste(decision.vars[dim.i,'name'],collapse=".")
      brks <- seq(decision.vars[dim.i,'lbound'],decision.vars[dim.i,'ubound'],length.out=25)
      par(mar=c(3.1,4.1,3.1,3.1))
      #print(ptx[,dim.i,gen])
      hist(ptx[,dim.i,gen],xlab="",main=dim.name,breaks=brks)
      mtext(dim.name,side=4,font=2,cex=0.8)
      if(dim.i==n)next
      for(dim.i2 in (dim.i+1):n){
        par(mar=c(2.1,2.1,1.1,1.1))
        plot(ptx[,dim.i2,gen],ptx[,dim.i,gen],
          col=cols[col.ind],
          bg=cols[col.ind],pch=23,font.lab=2,cex.lab=1.3,
          cex=pt.cex,
          xlim=as.numeric(decision.vars[dim.i2,c('lbound','ubound')]),
          ylim=as.numeric(decision.vars[dim.i,c('lbound','ubound')]),
          main="")
        points(na.omit(saved.inf[,dim.i2]),na.omit(saved.inf[,dim.i]),col="lightgreen",pch='.',cex=3)
      }
    }
  }
  if(!no.ani){
    ani.stop()
  }
}
