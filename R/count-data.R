#!/usr/bin/Rscript --no-save --no-restore
##############################################################################################################################################
# Script to process traffic count data from Caltrans 
##############################################################################################################################################

##############################################################################################################################################
# LOAD LIBRARIES NEED BY THIS SCRIPT
load.libraries(c('optparse','stringr','yaml','plyr','ggplot2','reshape','maptools','data.table'))

##############################################################################################################################################
# COMMAND LINE OPTIONS 
option_list <- list(
  make_option(c("-c", "--config"), default="PEVI_HOME/pevi.conf", help="Path to PEVI config file [default %default]"),
  make_option(c("-y", "--years"), default="2011,2012", help="Comma-separated years of raw data to process [default %default]"),
  make_option(c("-p", "--path"), default="data/UPSTATE", help="Path to Caltrans data relative to pevi.shared [default %default]")
)
if(interactive()){
  setwd(pp(pevi.home))
  args<-c('-y',pp(2004:2012,collapse=","))
  args <- parse_args(OptionParser(option_list = option_list,usage = "count-data.R [options]"),positional_arguments=F,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "count-data.R [options]"),positional_arguments=F)
}
opts <- data.frame(args,stringsAsFactors=F)
years <- as.numeric(str_split(opts$years,",")[[1]])


##############################################################################################################################################
# WHAT ROUTES SHOULD WE FOCUS ON
rts <- c(96,89,5,263,3,97,5,299,273,44,89,36,151,99,172,32)
rts <- rts[!duplicated(rts)]
counties <- c('SHA','SIS','TEH')

##############################################################################################################################################
# SCRIPT CONTENT 
##############################################################################################################################################

cnts <- data.frame()
for(yr in years){
  file.path <- pp(pevi.shared,opts$path,"/",yr,"aadt-raw.csv")
  if(!file.exists(file.path))next
  tmp <- read.csv(file.path,stringsAsFactors=F)
  names(tmp) <- str_replace(str_replace(str_replace(names(tmp),"CO$","County"),"Dist$","District"),"Ahead..AADT","Ahead.AADT")
  tmp <- subset(tmp,District==2 & Route %in% rts & County %in% counties)
  tmp$Postmile <- as.numeric(tmp$Postmile)
  tmp$Ahead.AADT <- as.numeric(str_replace_all(tmp$Ahead.AADT,",",""))
  tmp$Back.AADT <- as.numeric(str_replace_all(tmp$Back.AADT,",",""))
  
  tmp.m <- melt(tmp,id.vars=c("County","Route","Postmile"),measure.vars=c('Ahead.AADT','Back.AADT'))
  tmp.m$year <- yr
  cnts <- rbind(cnts,tmp.m)
  #ggplot(tmp.m,aes(x=Postmile,y=value,colour=variable))+geom_bar(stat="identity",position='dodge')+facet_wrap(County~Route)
}
save(cnts,file=pp(pevi.shared,opts$path,"/screenline-counts.Rdata"))

##############################################################################################################################################
# LOAD Road Network

rds <- readShapeLines(pp(pevi.shared,opts$path,'/shapefiles/Upstate_Roads_WGS84',sep=''))
mm <- readShapePoints(pp(pevi.shared,opts$path,'/shapefiles/MileMarkers',sep=''))
parse.mm <- str_split(mm$Name,"-")
mm$county <- sapply(parse.mm,function(l){ l[1] })
mm$route  <- sapply(parse.mm,function(l){ l[2] })
mm$post.mile <- sapply(parse.mm,function(l){ l[3] })

##############################################################################################################################################
# Look at Routes in SIS and TEH near border to SHA
ggplot(subset(cnts,Route%in%c(5,89,99,299)),aes(x=year,y=value,colour=Postmile,shape=variable))+geom_point()+facet_grid(Route~County)


