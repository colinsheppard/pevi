library(colinmisc)
load.libraries(c('sas7bdat','plyr','ggplot2','gtools'))

path.to.hevi <- '~/Dropbox/serc/pev-colin/hevi/'

sch <- read.table(paste(path.to.hevi,"R/p1r1.txt",sep=''),sep="\t",header=T)
sch$pen <- 1

for(pen in 2:5){
  sch <- rbind(sch,data.frame(read.table(paste(path.to.hevi,"R/p",pen,"r1.txt",sep=''),sep="\t",header=T),pen=pen))
}

ddply(sch,.(pen),function(df){ length(unique(df$X.driver))/nrow(df) })
ddply(sch,.(pen),nrow)

