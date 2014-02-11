######################################################################################################
# OD Processing
# 
# The Delhi model requires some processing of the OD data provided by RITES 
######################################################################################################

load.libraries(c('maptools','gpclib','plyr','stringr','ggplot2','doMC','reshape','data.table'))
gpclibPermit()
registerDoMC(num.cpu)

###################################################################################################################################################
# LOAD OD MATRICES
###################################################################################################################################################

modes <- sapply(str_split(list.files(pp(pevi.shared,'data/DELHI/tdfs-data/od-by-mode'),".csv"),".csv"),function(x){ x[1] })
mode.names <- str_replace(modes,"-",".")

# Take the pa matrix and distribute it by time of day according to the od distribution
do.or.load(pp(pevi.shared,'data/DELHI/tdfs-data/od-by-mode/od-raw.Rdata'),function(modes){
  od <- data.frame()
  # Load the OD data and melt
  for(mode in modes){
    tmp <- read.table(pp(pevi.shared,'data/DELHI/tdfs-data/od-by-mode/',mode,'.csv'),header=T, row.names=1,sep=',')
    # assuming the data have totals in the last row/column, drop these
    tmp <- tmp[1:(nrow(tmp)-1),1:(ncol(tmp)-1)]
    tmp$o <- pp('X',rownames(tmp))
    tmp <- melt(tmp,id.vars='o',variable_name='d')
    tmp$o <- as.numeric(substr(tmp$o,2,nchar(tmp$o)))
    tmp$d <- as.numeric(substr(tmp$d,2,nchar(as.character(tmp$d))))
    tmp$mode <- mode

    # external OD matrices have zeros for the internal trips which are useless here
    if(substr(mode,1,2)=="x-")tmp <- subset(tmp,o>n.taz | d>n.taz)
  
    if(nrow(od)==0){
      od <- tmp
      n.taz <- max(tmp$o)
    }else{
      od <- rbind(od,tmp)
    }
  }
  od$mode <- factor(od$mode)
  od <- data.table(od,key=c('mode','o','d'))
  od[,':='(trips=value,value=NULL)]
  list('od'=od)
},modes)


do.or.load(pp(pevi.shared,'data/DELHI/tdfs-data/od-by-mode/od-agg.Rdata'),function(){
  # Load taz.data which has the mapping from original TAZ to aggregated TAZ
  load(pp(pevi.shared,'data/DELHI/taz-aggregation-mapping.Rdata'))

  od$o.agg <- taz.data$agg.id[match(od$o,taz.data$ZONE_NO)]
  od$d.agg <- taz.data$agg.id[match(od$d,taz.data$ZONE_NO)]

  setkey(od,'mode','o.agg','d.agg')
  od.agg <- od[,list(trips=sum(trips)),by=c('mode','o.agg','d.agg')]
  od.agg[,':='(o=o.agg,d=d.agg,o.agg=NULL,d.agg=NULL)]
  list('od.agg'=od.agg)
})

