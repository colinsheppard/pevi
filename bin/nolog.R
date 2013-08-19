#!/usr/bin/Rscript --vanilla 
##############################################################################################################################################
# Script to comment out lines of a file flagged with ";;;LOG"
##############################################################################################################################################

##############################################################################################################################################
# BOILERPLATE LIBRARY SETUP
if(length(grep("colinmisc",installed.packages()))==0){
  if(length(grep("devtools",installed.packages()))==0)install.packages('devtools',repos='http://cran.cnr.Berkeley.edu')
  library(devtools)
  install_github('colinmisc','colinsheppard')
}
library(colinmisc,quietly=T)

##############################################################################################################################################
# LOAD LIBRARIES NEED BY THIS SCRIPT
load.libraries(c('optparse','stringr'))

##############################################################################################################################################
# COMMAND LINE OPTIONS 
option_list <- list(
  make_option(c("-v", "--verbose"), action="store_true", default=F, help="Print extra output"),
  make_option(c("-o", "--outfile"), type="character", default='nolog.out', help="Output file to write the commented code [\"%default\"]",metavar="out-file")
)
if(interactive()){
  setwd('~/Dropbox/serc/pev-colin/pevi/netlogo')
  args<-c('-o','PEVI-nolog.nlogo','PEVI.nlogo')
  args <- parse_args(OptionParser(option_list = option_list,usage = "nolog.R [options] INFILE"),positional_arguments=T,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "nolog.R [options] INFILE"),positional_arguments=T)
}
opts <- data.frame(args$options,stringsAsFactors=F)
infile <- args$args

dat <- scan(file=infile,what="character",sep="\n",quiet=T,blank.lines.skip=F)
log.rows <- grep(";;;LOG",dat)
dat[log.rows] <- pp(";;;",dat[log.rows])

write(dat,file=opts$outfile)
