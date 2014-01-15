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
  make_option(c("-d", "--experimentdir"), type="character", default='.', help="Path to the directory containing the files needed to run the optimization (params.txt, vary.yaml, paths.yaml) [\"%default\"]")
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

Sys.setenv(NOAWT=1)
options(java.parameters="-Xmx2048m")
registerDoMC(num.cpu)
source(pp(args$experimentdir,'params.R'))

# The params file will need to be set in R, so we can edit it mid-run. 
# Otherwise, batch setup is impossible.

# NL does not recognize tilde expansion. KEEP THI IN MIND.
param.file <- pp(args$experimentdir,'params.txt')
param.file.data <- read.table(param.file,sep='\t')
charger.file <- pp(pevi.shared,param.file.data[grep('charger-input-file',param.file.data$V1),2])

# For the current optimization, we need to know the installation costs of the chargers. Assuming that we won't want to vary
# those costs within an optimization run, we read them in here. If we do want to vary those costs, this would need to be 
# moved to the driver-file for loop.

charger.info <- read.table(pp(pevi.shared,param.file.data[grep('charger-type-input-file',param.file.data$V1),2]),row.names=NULL,header=T,sep='\t')
names(charger.info) <- c('level','charge.rate','energy.price','installed.cost')

#for(seed in seeds){
seed <- 1 
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
  vary.tab.original$row <- 1:nrow(vary.tab.original)

	init.charger.buildout <- read.table(charger.file,header=T,sep='\t')
	current.obj <- 0
	taz.charger.combos <- expand.grid(init.charger.buildout$X.TAZ,subset(charger.info,level != 0)$level)
	names(taz.charger.combos) <- c('taz','level')   

  #for(pev.penetration in pev.penetrations){
  pev.penetration <- pev.penetrations[1]
    print(paste("pen",pev.penetration))
    
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
		
    #	Read in the starting infrastructure. If we want linked buildout, out in an if statement to only set at the start.
		charger.buildout <- init.charger.buildout
		names(charger.buildout) <- c(';TAZ','L0','L1','L2','L3','L4')
    #	num.charger.type <- ncol(charger.buildout)-3
		
		begin.build.i <- 1

    # Start for loop for overall penetration level optimization
		#for(build.i in begin.build.i:250){
		 build.i <- 1 
			print(paste('build.i = ',build.i))

      #	Next is the loop through driver files. We'll want to do a ddply here, which means we'll need a way to
      #	switch parameter files without hacking into another param file, as well as a new NL instance for each file.
      #	Here is where we are going to use parallel=T to take advantage of extra cores.
	
			build.result <- ddply(vary.tab,.(row),function(vary.row) {  # parallel processing applied at this level
			#vary.row <- vary.tab[1,]
														
        # start NL
    		tryCatch(NLStart(nl.path, gui=F),error=function(err){ NA })
    		model.path <- paste(pevi.home,"netlogo/PEVI-nolog.nlogo",sep='')
    		NLLoadModel(model.path)
    		for(cmd in paste('set log-',logfiles,' false',sep='')){ NLCommand(cmd) }

        #	We can setup a batch-run for each iteration (each new input file).
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
      	for(param in grep("row",names(vary.tab),value=T,invert=T)){
          if(is.character(vary.tab[1,param])){
            NLCommand(pp('set ',param,' "',vary.row[[param]],'"'))
          }else{
            NLCommand(pp('set ',param,' ',vary.row[[param]],''))
          }
        }

        # set the charger input file
        NLCommand(pp('set charger-input-file "',charger.file,'"'))

        # Now we start a batch-run, and iterate through potential infrastructures.
				NLCommand('setup-in-batch-mode')
				
        #	Do a dry run (adding no chargers) to set an initial objective function value
				NLCommand('time:go-until 500')
				current.obj <<- NLReport('objective-function')
				
				NLCommand('setup-in-batch-mode')
				
        #	Iterate through every taz/charger combo
				input.i.result <- ddply(taz.charger.combos,.(taz),function(df) {
					charger.results <- ddply(df,.(level),function(df1) {
						
            #	Add the candidate charger, then run the model.
						NLCommand(paste('add-charger',df1$taz,df1$level))
						NLCommand(pp('print "taz',df1$taz,'level ',df1$level,'"'))
						NLCommand('time:go-until 500')
						
						objective <-  tryCatch(NLReport('objective-function'),error=function(e){ NA })

            #	Set the counter + 1, reset for the next run, and delete the charger we added.
						NLCommand('setup-in-batch-mode')	
						NLCommand(paste('remove-charger',df1$taz,df1$level))
						data.frame(obj = objective)
					}) # end infrastructure testing - charger type count
					data.frame(level = charger.results$level,
										 obj = charger.results$obj)
				}) #end infrastructure testing - taz count
				
        #	Quit our NetLogo instance
				NLQuit()
				
				data.frame(taz = input.i.result$taz,
									 level = input.i.result$level,
									 obj = input.i.result$obj)
			},.parallel=(num.cpu>1),.progress=ifelse(num.cpu>1,'none','text')) # end build.result ldply
			
      #	We've run every combination of tazs/chargers for each driver file. Now we asses which charger to place
      #	by averaging the objective function results across all replicates.
			taz.charger.obj <- ddply(build.result,.(taz,level),function(df){
				obj.average <- mean(df$obj)
				data.frame(obj = obj.average)
			})
			
			print(taz.charger.obj)
			
      #	Winner determined by lowest objective function (currently cost)
			winner <- which.min(taz.charger.obj$obj)
			print(paste('winner =',winner))

      #	How will adding the winning charger improve the objective?
			charger.objective.benefit <- (current.obj-taz.charger.obj$obj[winner]) 

      #	Items from taz.charger.obj can't be multiplied without factor errors. We handle that by assigning them to variables here.
			new.charger.cost.unadjusted <- charger.info$installed.cost[match(taz.charger.obj$level[winner],charger.info$level)]

      # How will adding the winning harm the objective?
			charger.objective.cost <- new.charger.cost.unadjusted * 1000
			
      # If the decrease in our objective function benefit is less than the objective function cost of installing the next charger, we're done.
			if(charger.objective.benefit > charger.objective.cost) {
				current.obj <- taz.charger.obj$obj[winner]
			} else {
				current.obj <- 0
				break
			}
			
      #	Now update our infrastructure file for the next round
			charger.buildout[taz.charger.obj$taz[winner],grep(taz.charger.obj$level[winner],names(charger.buildout))] <- charger.buildout[taz.charger.obj$taz[winner],grep(taz.charger.obj$level[winner],names(charger.buildout))] + 1
			write.table(charger.buildout,charger.file,quote=FALSE,sep='\t',row.names=FALSE)
						
    } # end iteration loop
    
    # Write out our final infrastructure to a new file.
		final.output.filepath <- pp(path.to.outputs,optim.code,'-pen',pev.penetration*100,'-final-infrastructure.txt')
    write.table(charger.buildout,final.output.filepath,quote=FALSE,sep='\t',row.names=FALSE)    
    
  } # end penetration-level loop
  
  #	Reset the initial charger file.
	write.table(init.charger.buildout,charger.file,header=T,sep='\t',row.names=FALSE)    
    
} # end seed loop
