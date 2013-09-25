#!/usr/bin/Rscript --vanilla 
##############################################################################################################################################
# Script to process traffic count data from Caltrans 
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
load.libraries(c('optparse','stringr','yaml'))

##############################################################################################################################################
# COMMAND LINE OPTIONS 
option_list <- list(
  make_option(c("-c", "--config"), default="PEVI_HOME/pevi.conf", help="Path to PEVI config file [default %default]")
)
if(interactive()){
  setwd('~/Dropbox/serc/pev-colin/pevi/R')
  args<-c()
  args <- parse_args(OptionParser(option_list = option_list,usage = "count-data.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "count-data.R [options]"),positional_arguments=F)
}
opts <- data.frame(args,stringsAsFactors=F)

# First test to see if the path has already been set
r.settings.path <- pp(Sys.getenv("R_HOME"),"/etc/Renviron")
r.settings <- readLines(r.settings.path)
pevi.i <- grep("PEVI_HOME",r.settings)

if(length(pevi.i)==0){
  # Nothing there so add it
  cat(pp("PEVI_HOME='",opts$pevi,"'"),file=r.settings.path,append=T)
}else if(length(pevi.i)==1){
  # One there, replace it
  r.settings[pevi.i] <- pp("PEVI_HOME='",opts$pevi,"'")
  writeLines(r.settings,con=r.settings.path)
}else{
  # Multiple there, replace first one and delete the rest 
  r.settings <- r.settings[-pevi.i[2:length(pevi.i)]]
  r.settings[pevi.i[1]] <- pp("PEVI_HOME='",opts$pevi,"'")
  writeLines(r.settings,con=r.settings.path)
}
