#!/usr/bin/Rscript --vanilla 
##############################################################################################################################################
# Script to configure PEVI modeling environment
##############################################################################################################################################

options(warn=-1)

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
  make_option(c("-p", "--pevi"), default="~/Dropbox/pevi", help="Path to PEVI repository [default %default]"),
  make_option(c("-s", "--shared"), default="~/Dropbox/pev-shared", help="Path to pev-shared directory [default %default]"),
  make_option(c("-n", "--nondrop"), default="~/Documents/pevi", help="Path to non-dropbox directory for pevi [default %default]")
)
if(interactive()){
  setwd('~/Dropbox/serc/pev-colin/pevi/R')
  args<-c('-p','/Users/sheppardc/Dropbox/serc/pev-colin/pevi/','-s','/Users/sheppardc/Dropbox/serc/pev-colin/pev-shared/','-n','/Users/sheppardc/Documents/pev/')
  args <- parse_args(OptionParser(option_list = option_list,usage = "configure-pevi.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "configure-pevi.R [options]"),positional_arguments=F)
}
opts <- data.frame(args,stringsAsFactors=F)

# Mapping of argument name to environment name
map <- list('pevi'='PEVI_HOME','shared'='PEVI_SHARED','nondrop'='PEVI_NONDROP')

# First test to see if the path has already been set
r.settings.path <- pp(Sys.getenv("R_HOME"),"/etc/Renviron")

for(arg.name in names(map)){
  make.dir(opts[arg.name])
  r.settings <- readLines(r.settings.path)
  pevi.i <- grep(map[[arg.name]],r.settings)

  if(length(pevi.i)==0){
    # Nothing there so add it
    cat(pp(map[[arg.name]],"='",opts[arg.name],"'\n"),file=r.settings.path,append=T)
  }else if(length(pevi.i)==1){
    # One there, replace it
    r.settings[pevi.i] <- pp(map[[arg.name]],"='",opts[arg.name],"'")
    writeLines(r.settings,con=r.settings.path)
  }else{
    # Multiple there, replace first one and delete the rest 
    r.settings <- r.settings[-pevi.i[2:length(pevi.i)]]
    r.settings[pevi.i[1]] <- pp(map[[arg.name]],"='",opts[arg.name],"'")
    writeLines(r.settings,con=r.settings.path)
  }
}
