#!/usr/bin/Rscript --no-save --no-restore
##############################################################################################################################################
# Script to configure PEVI modeling environment
##############################################################################################################################################

options(warn=-1)
options(repos=structure(c(CRAN="http://cran.cnr.Berkeley.edu")))

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
  args<-c('-p','/Users/sheppardc/Dropbox/serc/pev-colin/pevi/','-s','/Users/sheppardc/Dropbox/serc/pev-colin/pev-shared/','-n','/Users/sheppardc/Documents/pev/','colinmisc')
  args <- parse_args(OptionParser(option_list = option_list,usage = "configure-pevi.R [options] AUTO_LOADED_R_LIBRARIES"),positional_arguments=T,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "configure-pevi.R [options] AUTO_LOADED_R_LIBRARIES"),positional_arguments=T)
}
opts <- data.frame(args$options,stringsAsFactors=F)
libs <- args$args

# Mapping of argument name to environment name
map <- list('pevi'='pevi.home','shared'='pevi.shared','nondrop'='pevi.nondrop')

# Extra lines to be added (or replaced if #TAG-* finds a match
extra.lines <- c("#TAG-LOAD-PEVI-CONF","pevi.conf <- pp(pevi.home,'pevi-conf.R')","if(file.exists(pevi.conf)){","  source(pevi.conf)","}else{","  warning(pp('Missing configuration file: ',pevi.conf))","}")

# First test to see if the path has already been set
r.profile.path <- pp(Sys.getenv("HOME"),"/.Rprofile")

for(arg.name in names(map)){
  make.dir(as.character(opts[arg.name]))
  if(!file.exists(r.profile.path))writeLines("",r.profile.path)
  r.profile <- readLines(r.profile.path)
  pevi.i <- grep(map[[arg.name]],r.profile)

  if(length(pevi.i)==0){
    # Nothing there so add it
    cat(pp(map[[arg.name]]," <- '",opts[arg.name],"'\n"),file=r.profile.path,append=T)
  }else if(length(pevi.i)==1){
    # One there, replace it
    r.profile[pevi.i] <- pp(map[[arg.name]]," <- '",opts[arg.name],"'")
    writeLines(r.profile,con=r.profile.path)
  }else{
    # Multiple there, replace first one and delete the rest 
    r.profile <- r.profile[-pevi.i[2:length(pevi.i)]]
    r.profile[pevi.i[1]] <- pp(map[[arg.name]]," <- '",opts[arg.name],"'")
    writeLines(r.profile,con=r.profile.path)
  }
}

avail.packs <- available.packages()
for(lib in libs){
  if(length(grep(lib,installed.packages()))==0){
    if(length(grep(lib,avail.packs[,1]))==0){
      if(length(grep("devtools",installed.packages()))==0)install.packages('devtools',repos='http://cran.cnr.Berkeley.edu')
      library(devtools)
      install_github(lib,'colinsheppard')
    }else{
      install.packages(lib)
    }
  }

  r.profile <- readLines(r.profile.path)
  pevi.i <- grep(lib,r.profile)

  if(length(pevi.i)==0){
    # Nothing there so add it
    cat(pp('library(',lib,',quietly=T)',"\n"),file=r.profile.path,append=T)
  }else if(length(pevi.i)==1){
    # One there, replace it
    r.profile[pevi.i] <- pp('library(',lib,')',"\n") 
    writeLines(r.profile,con=r.profile.path)
  }else{
    # Multiple there, replace first one and delete the rest 
    r.profile <- r.profile[-pevi.i[2:length(pevi.i)]]
    r.profile[pevi.i[1]] <- pp('library(',lib,')',"\n")
    writeLines(r.profile,con=r.profile.path)
  }
}


rm('r.profile')
for(extra.line in extra.lines){
  if(substr(extra.line,1,4) == "#TAG"){
    if(!exists('r.profile'))r.profile <- readLines(r.profile.path)
    pevi.i <- grep(extra.line,r.profile)

    if(length(pevi.i)>=1){
      r.profile <- r.profile[-((pevi.i[1]):length(r.profile))]
    }
    r.profile <- c(r.profile,extra.line)
  }else{
    r.profile <- c(r.profile,extra.line)
  }
}
writeLines(r.profile,con=r.profile.path)

my.cat("The file ~/.Rprofile has been added or modified\n\n")
my.cat(pp("Please also setup the configuration file ",opts$pevi,"pevi-conf.R (use a copy of pevi-conf-TEMPLATE.R)."))
