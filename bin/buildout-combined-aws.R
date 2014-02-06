#!/usr/bin/Rscript --no-save --no-restore
##############################################################################################################################################
# Script to conduct the buildout optimization
##############################################################################################################################################

##############################################################################################################################################
# LOAD LIBRARIES NEED BY THIS SCRIPT
load.libraries(c('optparse','yaml','RNetLogo','plyr','reshape','stringr','doMC'),quietly=T)

##############################################################################################################################################
# COMMAND LINE OPTIONS 
option_list <- list(
  make_option(c("-d", "--experimentdir"), type="character", default='.', help="Path to the directory containing the files needed to run the optimization (params.txt, vary.yaml, paths.yaml) [\"%default\"]"),
  make_option(c("-b", "--buildincrement"), type="numeric", default=1, help="Number of chargers we wish to build in one TAZ each iteration [\"%default\"]")
)
if(interactive()){
  setwd(pp(pevi.shared,'data/inputs/optim-new/'))
  args<-c()
  args <- parse_args(OptionParser(option_list = option_list,usage = "shp2kml.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "shp2kml.R [options]"),positional_arguments=F)
}

if(substr(args$experimentdir,1,1)!="/")args$experimentdir <- pp(getwd(),"/",args$experimentdir)
if(substr(args$experimentdir,nchar(args$experimentdir),nchar(args$experimentdir)) != "/")args$experimentdir <- pp(args$experimentdir,"/")
build.increment <- args$buildincrement

Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
registerDoMC(num.cpu)
source(pp(args$experimentdir,'params.R'))
source(paste(pevi.home,"R/optim/buildout-functions.R",sep=''))
source(paste(pevi.home,"R/reporters-loggers.R",sep=''))

# The params file will need to be set in R, so we can edit it mid-run. 
# Otherwise, batch setup is impossible.

# NL does not recognize tilde expansion. KEEP THIS IN MIND.
param.file <- pp(args$experimentdir,'params.txt')
param.file.data <- read.table(param.file,sep='\t')
charger.file <- pp(pevi.shared,param.file.data[grep('charger-input-file',param.file.data$V1),2])

# For the current optimization, we need to know the installation costs of the chargers. Assuming that we won't want to vary
# those costs within an optimization run, we read them in here. If we do want to vary those costs, this would need to be 
# moved to the driver-file for loop.

charger.info <- read.table(pp(pevi.shared,param.file.data[grep('charger-type-input-file',param.file.data$V1),2]),row.names=NULL,header=T,sep='\t')
names(charger.info) <- c('level','charge.rate','energy.price','installed.cost')

for(seed in seeds){
#seed <- 1 
  optim.code <- paste(optim.scenario,'-seed',seed,sep='')
  print(optim.code)

  # setup the paths
  path.to.inputs <- paste(args$experimentdir,optim.code,'/',sep='')
  make.dir(path.to.inputs)
  system(paste("cp ",args$experimentdir,'params.txt ',path.to.inputs,sep=''))
  system(paste("cp ",args$experimentdir,'vary.yaml ',path.to.inputs,sep=''))
  path.to.outputs <- paste(path.to.outputs.base,optim.code,'/',sep='')
  make.dir(path.to.outputs)

  source(pp(pevi.home,"R/reporters-loggers.R",sep=''))

  # read the parameters and values to vary in the experiment
  vary <- yaml.load(readChar(pp(path.to.inputs,'vary.yaml',sep=''),file.info(pp(path.to.inputs,'/vary.yaml',sep=''))$size))
  for(file.param in names(vary)[grep("-file",names(vary))]){
    vary[[file.param]] <- pp(path.to.vary.files,vary[[file.param]],sep='')
  }

  # setup the data frame containing all combinations of those parameter values
  vary.tab.original <- expand.grid(vary,stringsAsFactors=F)
#  vary.tab.original$row <- 1:nrow(vary.tab.original)

	init.charger.buildout <- read.table(charger.file,header=T,sep='\t')
	taz.charger.combos <- expand.grid(subset(init.charger.buildout,X.TAZ>0)$X.TAZ,subset(charger.info,level != 0)$level)
	names(taz.charger.combos) <- c('taz','level')

	if(!exists('cl')){
  	print('starting new cluster')
		cl <- makeCluster(c(rep(list(list(host="localhost")),num.cpu)),type="SOCK")
  	clusterEvalQ(cl,options(java.parameters="-Xmx2048m"))
		clusterEvalQ(cl,Sys.setenv(NOAWT=1))
	  clusterEvalQ(cl,library('RNetLogo'))
	}
	
	# The clusters previously devided work based upon the number of taz/charger combinations. To take advantage of 
	# batch mode, we can't do that; we need to divide work based on input files. Instead of break.pairs describing 
	# the results matrix, "break.pairs" (or somthing like it) should divide up vary.tab
	
	model.path <- paste(pevi.home,"netlogo/PEVI-nolog.nlogo",sep='')

  for(pev.penetration in pev.penetrations){
  #pev.penetration <- pev.penetrations[1]
    print(paste("pen",pev.penetration))
    
    current.obj <- Inf
    
    # Note that the expectation is that all pev penetrations beyond the first are even multiples of the first
    if(pev.penetration == pev.penetrations[1]){
      vary.tab <- vary.tab.original
    }else{
      pen.ratio <- pev.penetration/pev.penetrations[1]
    	new.vary <- vary
    	new.vary$'driver-input-file' <- new.vary$'driver-input-file'[1:round(length(vary$'driver-input-file')/pen.ratio)]
    	vary.tab <- expand.grid(new.vary,stringsAsFactors=F)
    }
    vary.tab$`driver-input-file` <- str_replace(vary.tab$`driver-input-file`,"penXXX",paste("pen",pev.penetration*100,sep=""))
		
    #	Read in the starting infrastructure. If we want linked buildout, put in an if statement to only set at the start.
		charger.buildout <- init.charger.buildout
		names(charger.buildout) <- c(';TAZ','L0','L1','L2','L3','L4')
    #	num.charger.type <- ncol(charger.buildout)-3
		
		begin.build.i <- 1

    # Start for loop for overall penetration level optimization
		for(build.i in begin.build.i:250){
		 #build.i <- 1 
			print(paste('build.i = ',build.i))
			
			build.result <- data.frame()
			
      #	Next is the loop through driver files. the snow parallelization happens here.
			build.result <- evaluate.fitness(build.result)
			
      #	We've run every combination of tazs/chargers for each driver file. Now we asses which charger to place
      #	by averaging the objective function results across all replicates.
			taz.charger.obj <- ddply(build.result,.(taz,level),function(df){
				obj.average <- mean(df$objective)
				data.frame(obj = obj.average)
			})
						
      #	Winner determined by lowest objective function (currently cost)
			winner <- which.min(taz.charger.obj$obj)
			print(paste('winner =',winner))
			
      # If our objective value has reached a minimum, we're done.
			if(current.obj > taz.charger.obj$obj[winner]) {
				current.obj <- taz.charger.obj$obj[winner]
			} else {
				current.obj <- Inf
				break
			}
			
      #	Now update our infrastructure file for the next round
			charger.buildout[taz.charger.obj$taz[winner],grep(taz.charger.obj$level[winner],names(charger.buildout))] <- (charger.buildout[taz.charger.obj$taz[winner],grep(taz.charger.obj$level[winner],names(charger.buildout))] + build.increment)
			write.table(charger.buildout,charger.file,quote=FALSE,sep='\t',row.names=FALSE)
						
    } # end iteration loop
    
    # Write out our final infrastructure to a new file.
		final.output.filepath <- pp(path.to.outputs,optim.code,'-pen',pev.penetration*100,'-final-infrastructure.txt')
    write.table(charger.buildout,final.output.filepath,quote=FALSE,sep='\t',row.names=FALSE)    
    
  } # end penetration-level loop
  
  #	Reset the initial charger file.
	write.table(init.charger.buildout,charger.file,quote=FALSE,sep='\t',row.names=FALSE)    
    
} # end seed loop
