s<-read.csv(pp(pevi.shared,'/delhi/tdfs-data/hh-survey.csv'),stringsAsFactors=F,row.names=NULL)

s$start.hour <- as.numeric(substr((s$ST_TIME),1,nchar((s$ST_TIME))-2)) + as.numeric(substr((s$ST_TIME),nchar((s$ST_TIME))-1,nchar((s$ST_TIME))))/60
s$end.hour <- as.numeric(substr((s$END_TIME),1,nchar((s$END_TIME))-2)) + as.numeric(substr((s$END_TIME),nchar((s$END_TIME))-1,nchar((s$END_TIME))))/60
s$trip.dur <- s$end.hour - s$start.hour
inds <- which(s$trip.dur<0)
s$trip.dur[inds] <- s$trip.dur[inds]+24
s$L1_MODE[s$L1_MODE==999] <- NA
s$L1_MODE[s$L1_DISTENC==999] <- NA
s$L1_MODE[s$L1_TRVTIME==999] <- NA
s$L2_MODE[s$L2_MODE==999] <- NA
s$L2_MODE[s$L2_DISTENC==999] <- NA
s$L2_MODE[s$L2_TRVTIME==999] <- NA
s$L3_MODE[s$L3_MODE==999] <- NA
s$L3_MODE[s$L3_DISTENC==999] <- NA
s$L3_MODE[s$L3_TRVTIME==999] <- NA
s$tot.travel.time <- (ifelse(is.na(s$L1_TRVTIME),0,s$L1_TRVTIME) + ifelse(is.na(s$L2_TRVTIME),0,s$L2_TRVTIME) + ifelse(is.na(s$L3_TRVTIME),0,s$L3_TRVTIME))/60

