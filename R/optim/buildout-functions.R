
evaluate.fitness <- function(){
  if(!exists('cl')){
    stop('no cluster started')
  }
  #clusterEvalQ(cl,rm(list=ls()))
  clusterExport(cl,c( 'run.buildout.batch','run.buildout.batch.one.itin','pev.penetration','path.to.inputs','optim.code','nl.path','path.to.outputs','seed','param.file','taz.charger.combos','charger.file','write.charger.file','reporters','pevi.home','vary.tab','streval','try.nl','debug.reporters','pevi.shared','build.increment','nl.obj','reference.charger.cost','reference.delay.cost','add.charger','args'))
  if(exists('batch.results'))rm('batch.results')
  if(length(vary.tab$`driver-input-file`)==1){
    taz.charger.combos.inds <- which(taz.charger.combos$include)
    batch.results<-clusterApplyLB(cl,taz.charger.combos.inds,fun='run.buildout.batch.one.itin')
  }else{
    batch.results<-clusterApplyLB(cl,vary.tab$`driver-input-file`,fun='run.buildout.batch')
  }
  return(ldply(batch.results,function(ll){ ll }))
}

init.netlogo <- function(){
  tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
  NLLoadModel(model.path)
  for(cmd in paste('set log-',logfiles,' false',sep='')){ NLCommand(cmd) }
}
quit.netlogo <- function(){
  #	Quit the NetLogo instance
	NLQuit()
}

add.charger <- function(new.taz,new.level){
  NLCommand(paste('add-charger',new.taz,new.level,build.increment[pp('l',new.level)]))
}

run.buildout.batch <- function(driver.input.file){

  rep <- as.numeric(strsplit(strsplit(driver.input.file,'rep')[[1]][2],'-')[[1]][1])

  NLCommand('clear-all-and-initialize') # Clear out the old file

  # Set to fixed-seed if applicable.
  if(!is.na(seed)){
    NLCommand(paste('set starting-seed',seed))      
    NLCommand('set fix-seed TRUE')
  } else {
    NLCommand('set fix-seed FALSE')
  }
      
  # The params file pathways are all assumed to be based from pev-shared. We set a param-base variable in NetLogo
  # to make this happen. The outputs folder is unchanged.
  NLCommand(pp('set param-file-base "',pevi.shared,'"'))
  NLCommand(paste('set parameter-file "',param.file,'"',sep=''))
  NLCommand('read-parameter-file')
      
  if(is.character(driver.input.file)){
    NLCommand(pp('set driver-input-file "',driver.input.file,'"'))
  }else{
    NLCommand(pp('set driver-input-file ',driver.input.file,''))
  }
  # set the charger input file
  NLCommand(pp('set charger-input-file "',charger.file,'"'))

  # set the reference charger and delay costs
  NLCommand(pp('set reference-charger-cost ',reference.charger.cost))
  NLCommand(pp('set reference-delay-cost ',reference.delay.cost))

  # Now we start a batch-run, and iterate through potential infrastructures.
  NLCommand('setup-in-batch-mode')
              
  #	Iterate through every taz/charger combo
  input.i.result <- ddply(subset(taz.charger.combos,include),.(taz,level),function(df) {
      #	Add the candidate charger, then run the model.
      NLCommand(paste('add-charger',df$taz,df$level,build.increment[pp('l',df$level)]))
      NLCommand('time:go-until 500')
          
      if(args$externaltazs){
        total.charger.cost <-  tryCatch(NLReport('total-charger-cost-including-external'),error=function(e){ NA })
      }else{
        total.charger.cost <-  tryCatch(NLReport('total-charger-cost'),error=function(e){ NA })
      }
      total.delay.cost <-  tryCatch(NLReport('total-delay-cost'),error=function(e){ NA })
      objective <-  tryCatch(NLReport(nl.obj),error=function(e){ NA })

      #	Reset for the next run, and delete the charger we added.
      NLCommand('setup-in-batch-mode')	
      NLCommand(paste('remove-charger',df$taz,df$level,build.increment[pp('l',df$level)]))
      data.frame(obj = objective, total.charger.cost = total.charger.cost, total.delay.cost = total.delay.cost)
  }) # end infrastructure testing - charger type count
	
  return(data.frame(input.i.result,rep=rep))
}

run.buildout.batch.one.itin <- function(taz.charger.combos.inds){
  driver.input.file <- vary.tab$`driver-input-file`[1]
  rep <- as.numeric(strsplit(strsplit(driver.input.file,'rep')[[1]][2],'-')[[1]][1])

  # set the reference charger and delay costs
  NLCommand(pp('set reference-charger-cost ',reference.charger.cost))
  NLCommand(pp('set reference-delay-cost ',reference.delay.cost))

  # Now we start a batch-run, and iterate through potential infrastructures.
  NLCommand('setup-in-batch-mode')
              
  #	Iterate through every taz/charger combo
  input.i.result <- ddply(taz.charger.combos[taz.charger.combos.inds,],.(taz,level),function(df) {
      #	Add the candidate charger, then run the model.
      NLCommand(paste('add-charger',df$taz,df$level,build.increment[pp('l',df$level)]))
      NLCommand('time:go-until 500')
          
      if(args$externaltazs){
        total.charger.cost <-  tryCatch(NLReport('total-charger-cost-including-external'),error=function(e){ NA })
      }else{
        total.charger.cost <-  tryCatch(NLReport('total-charger-cost'),error=function(e){ NA })
      }
      total.delay.cost <-  tryCatch(NLReport('total-delay-cost'),error=function(e){ NA })
      objective <-  tryCatch(NLReport(nl.obj),error=function(e){ NA })

      #	Reset for the next run, and delete the charger we added.
      NLCommand('setup-in-batch-mode')	
      NLCommand(paste('remove-charger',df$taz,df$level,build.increment[pp('l',df$level)]))
      data.frame(obj = objective, total.charger.cost = total.charger.cost, total.delay.cost = total.delay.cost)
  }) # end infrastructure testing - charger type count
  
  return(data.frame(input.i.result,rep=rep))
}

evaluate.baseline <- function(){
  if(!exists('cl')){
    stop('no cluster started')
  }
  #clusterEvalQ(cl,rm(list=ls()))
  clusterExport(cl,c( 'run.baseline.batch','run.baseline.batch.one.itin','pev.penetration','path.to.inputs','optim.code','nl.path','path.to.outputs','seed','param.file','taz.charger.combos','charger.file','write.charger.file','reporters','pevi.home','vary.tab','streval','try.nl','debug.reporters','pevi.shared','build.increment','add.charger','args'))
  if(exists('batch.results'))rm('batch.results')
  if(length(vary.tab$`driver-input-file`)==1){
    # note this does redundant runs, but we need every node to do the setup actions, but we only keep one result
    batch.results<-clusterCall(cl,fun='run.baseline.batch.one.itin')
    batch.results <- batch.results[1]
  }else{
    batch.results<-clusterApplyLB(cl,vary.tab$`driver-input-file`,fun='run.baseline.batch')
  }
  build.result<-batch.results[[1]]
  if(length(batch.results)>1){
    for(i in 2:length(batch.results)){
      build.result <- rbind(build.result,batch.results[[i]])
    }
  }
  return(build.result)
}

run.baseline.batch.one.itin <- function(){
  driver.input.file <- vary.tab$`driver-input-file`
  rep <- as.numeric(strsplit(strsplit(driver.input.file,'rep')[[1]][2],'-')[[1]][1])

  NLCommand('clear-all-and-initialize') # Clear out the old file

  # Set to fixed-seed if applicable.
  if(!is.na(seed)){
    NLCommand(paste('set starting-seed',seed))      
    NLCommand('set fix-seed TRUE')
  } else {
    NLCommand('set fix-seed FALSE')
  }
      
  # The params file pathways are all assumed to be based from pev-shared. We set a param-base variable in NetLogo
  # to make this happen. The outputs folder is unchanged.
  NLCommand(pp('set param-file-base "',pevi.shared,'"'))
  NLCommand(paste('set parameter-file "',param.file,'"',sep=''))
  NLCommand('read-parameter-file')
      
  # If a parameter change exists in vary.tab, we go through and read in the new parameters.
  # Given how we are parallel programming batch-mode, I can't think of a way to do this without
  # the blanket assumption that only driver input files are in vary.tab, as names aren't preserved
  # when we break apart vary.tab for processing.
      
  if(is.character(driver.input.file)){
    NLCommand(pp('set driver-input-file "',driver.input.file,'"'))
  }else{
    NLCommand(pp('set driver-input-file ',driver.input.file,''))
  }

  # set the charger input file
  NLCommand(pp('set charger-input-file "',charger.file,'"'))

  # Now we start a batch-run, and iterate through potential infrastructures.
  NLCommand('setup-in-batch-mode')
              
  NLCommand('time:go-until 500')
          
  if(args$externaltazs){
    total.charger.cost <-  tryCatch(NLReport('total-charger-cost-including-external'),error=function(e){ NA })
  }else{
    total.charger.cost <-  tryCatch(NLReport('total-charger-cost'),error=function(e){ NA })
  }
  total.delay.cost <-  tryCatch(NLReport('total-delay-cost'),error=function(e){ NA })

  data.frame(total.charger.cost = total.charger.cost, total.delay.cost = total.delay.cost, rep=rep)
}
run.baseline.batch <- function(driver.input.file){

  rep <- as.numeric(strsplit(strsplit(driver.input.file,'rep')[[1]][2],'-')[[1]][1])

  NLCommand('clear-all-and-initialize') # Clear out the old file

  # Set to fixed-seed if applicable.
  if(!is.na(seed)){
    NLCommand(paste('set starting-seed',seed))      
    NLCommand('set fix-seed TRUE')
  } else {
    NLCommand('set fix-seed FALSE')
  }
      
  # The params file pathways are all assumed to be based from pev-shared. We set a param-base variable in NetLogo
  # to make this happen. The outputs folder is unchanged.
  NLCommand(pp('set param-file-base "',pevi.shared,'"'))
  NLCommand(paste('set parameter-file "',param.file,'"',sep=''))
  NLCommand('read-parameter-file')
      
  # If a parameter change exists in vary.tab, we go through and read in the new parameters.
  # Given how we are parallel programming batch-mode, I can't think of a way to do this without
  # the blanket assumption that only driver input files are in vary.tab, as names aren't preserved
  # when we break apart vary.tab for processing.
      
  if(is.character(driver.input.file)){
    NLCommand(pp('set driver-input-file "',driver.input.file,'"'))
  }else{
    NLCommand(pp('set driver-input-file ',driver.input.file,''))
  }

  # set the charger input file
  NLCommand(pp('set charger-input-file "',charger.file,'"'))

  # Now we start a batch-run, and iterate through potential infrastructures.
  NLCommand('setup-in-batch-mode')
              
  NLCommand('time:go-until 500')
          
  if(args$externaltazs){
    total.charger.cost <-  tryCatch(NLReport('total-charger-cost-including-external'),error=function(e){ NA })
  }else{
    total.charger.cost <-  tryCatch(NLReport('total-charger-cost'),error=function(e){ NA })
  }
  total.delay.cost <-  tryCatch(NLReport('total-delay-cost'),error=function(e){ NA })

  data.frame(total.charger.cost = total.charger.cost, total.delay.cost = total.delay.cost, rep=rep)
}

try.nl <- function(cmd,break.pair.code=""){
  err <- tryCatch(NLCommand(cmd),error=function(err){ paste("NLCommand('",cmd,"')",sep='') })
  if(!is.null(err)){
    cat(paste('error: ',err,sep=''),file=paste(path.to.outputs,"/logfile.txt",sep=''),append=T,fill=T,labels=break.pair.code)
    cat(paste('error reporters: ',paste(names(debug.reporters),tryCatch(NLDoReport(1,"",reporter = paste("(sentence",paste(debug.reporters,collapse=' '),")"),as.data.frame=T,df.col.names=names(debug.reporters)),error=function(e){ NA }),collapse=","),sep=''),file=paste(path.to.outputs,"/logfile.txt",sep=''),append=T,fill=T,labels=break.pair.code)
  }
  return(err)
}

write.charger.file <- function(num.chargers,alt.i=0,filepath=NA){
  if(is.na(filepath))filepath <- paste(path.to.inputs,'chargers-alt-',alt.i,'.txt',sep='')
  chargers <- data.frame(TAZ=1:52,L0=rep(1,52),L1=num.chargers[1:52],L2=num.chargers[1:52],L3=num.chargers[53:104])
  names(chargers) <- c(";TAZ","L0","L1","L2","L3")
  write.table(chargers,file=filepath,sep="\t",row.names=F,quote=F)
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
