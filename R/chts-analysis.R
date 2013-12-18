
load.libraries(c('stringr','ggplot2','reshape','data.table'))

activity <- read.csv(pp(pevi.shared,'data/CHTS/activity.csv'))
place <- read.csv(pp(pevi.shared,'data/CHTS/place.csv'))
ld <- read.csv(pp(pevi.shared,'data/CHTS/long_distance_trips.csv'))
households <- read.csv(pp(pevi.shared,'data/CHTS/households.csv'))
nssr <- read.csv(pp(pevi.shared,'data/CHTS/north_state_super_region.csv'))
car.modes <- read.csv(pp(pevi.shared,'data/CHTS/car_mode_codes.csv'))

# Apply county labels to household data
households$county <- nssr$COUNTY[match(households$ctfip,nssr$CTFIP)]
nssr.households <- subset(households,!is.na(county))

# Generate the necessary place data from household data
place$activity.purpose <- activity$apurp[match(place$tripno,activity$tripno)]
nssr.place <- subset(place,sampn%in%nssr.households$sampno)
nssr.place$samp.per.place <- pp(nssr.place$sampn,"_",nssr.place$perno,"_",nssr.place$plano)
nssr.activity <- subset(activity,sampno%in%nssr.households$sampno)
nssr.activity$samp.per.place <- pp(nssr.activity$sampno,"_",nssr.activity$perno,"_",nssr.activity$plano)
nssr.activity$stime.hr <- unlist(lapply(str_split(nssr.activity$stime,":"),function(l){ sum(as.numeric(l)/c(1,60,3600)) }))
nssr.activity$etime.hr <- unlist(lapply(str_split(nssr.activity$etime,":"),function(l){ sum(as.numeric(l)/c(1,60,3600)) }))
nssr.activity$duration <- nssr.activity$etime.hr - nssr.activity$stime.hr
nssr.activity$duration[nssr.activity$duration<0] <- nssr.activity$duration[nssr.activity$duration<0] + 24 
# let's ignore activity that lasts all day (1439 minutes) 
nssr.activity <- subset(nssr.activity,duration < 23.9)
nssr.place.acts <- ddply(merge(nssr.place,nssr.activity[,c('samp.per.place','duration','apurp')],by="samp.per.place"),.(samp.per.place),function(df){
  durs <- aggregate(duration~apurp,df,sum)
  data.frame(df[1,1:ncol(nssr.place)],purp=durs$apurp[which.max(durs$duration)])
})

nssr.place.activity.merged <- as.data.table(merge(nssr.place,nssr.activity[,c('samp.per.place','duration','apurp')],by="samp.per.place"))


#purp.mapping <- data.frame(id=c(1,2,21,22,27,30,31,34,37),purp=c('
#place$purpose <- 

# Link county name to sample number in the place data
nssr.place$county <- nssr.households$county[match(nssr.place$sampn,nssr.households$sampno)]

# Remove non-car modes if desired, and link the names to the codes
nssr.place <- subset(nssr.place,mode%in%car.modes$MODE)
nssr.place$mode.name <- car.modes$TRANSPORT[match(nssr.place$mode,car.modes$MODE)]

save(nssr.place,nssr.households,file=pp(pevi.shared,'data/CHTS/nssr-subset.Rdata'))

#Travel attributes by county
#ggplot(subset(nssr.place,tripdistance<200), aes(x=tripdistance)) + geom_histogram(aes(y = ..density..),binwidth=5) + ggtitle(pp("Trip distance distribution by county"))+facet_wrap(~county)+scale_y_log10()
#ggplot(subset(nssr.place,tripdistance<500), aes(x=log10(tripdistance))) + geom_histogram(binwidth=0.1) + ggtitle(pp("Trip distance distribution by county"))

d_ply(nssr.place,.(county),function(df){
	if(length(na.omit(df$tripdistance))>0) {
		p <- ggplot(df, aes(x=tripdistance)) + geom_bar(stat="bin",binwidth=1) + ggtitle(pp("Trip distance distribution for ",df$county[1]," county"))
		ggsave(p,filename=pp(path.to.CHTS,'plots_by_county/Upstate_Trip_Distance_frequency_county_',df$county[1],'.pdf'),width=17,height=11)
	}
	if(length(na.omit(df$tripdur))>0) {
		pt <- ggplot(df, aes(x=tripdur)) + geom_bar(stat="bin") + ggtitle(pp("Trip duration distribution for ",df$county[1]," county"))
		ggsave(pt,filename=pp(path.to.CHTS,'plots_by_county/Upstate_Trip_Duration_frequency_county_',df$county[1],'.pdf'),width=17,height=11)
	}
	if(length(na.omit(df$actdur))>0) {
		pa <- ggplot(df, aes(x=actdur)) + geom_bar(stat="bin") + ggtitle(pp("Activity duration distribution for ",df$county[1]," county"))
		ggsave(pa,filename=pp(path.to.CHTS,'plots_by_county/Upstate_Activity_Duration_frequency_county_',df$county[1],'.pdf'),width=17,height=11)
	}
	if(length(na.omit(df$mode))>0) {
		pm <- ggplot(df, aes(x=mode.name)) + geom_bar(stat="bin",binwidth=1) + ggtitle(pp("Trip mode distribution for ",df$county[1]," county"))	
		ggsave(pm,filename=pp(path.to.CHTS,'plots_by_county/Upstate_Trip_Mode_frequency_county_',df$county[1],'.pdf'),width=17,height=11)
	}
		if(length(na.omit(df$dep_hr))>0) {
		ps <- ggplot(df, aes(x=dep_hr)) + geom_bar(stat="bin",binwidth=1) + ggtitle(pp("Trip departure time distribution for ",df$county[1]," county"))	
		ggsave(ps,filename=pp(path.to.CHTS,'plots_by_county/Upstate_Trip_Depart_Time_frequency_county_',df$county[1],'.pdf'),width=17,height=11)
	}
})


#Generate counts for each purpose trip, long distance
for(i in 1:length(LDPurp$Purpose)){
	LDPurp$Count[i] <- nrow(subset(upstate.LD,ldtpurp==LDPurp$Purpose[i]))
}

#Travel attribute distributions by hour

d_ply(upstate.place,.(dep_hr),function(df){
	if(length(na.omit(df$tripdistance))>0) {
		p <- ggplot(df, aes(x=tripdistance)) + geom_bar(stat="bin",binwidth=1) + ggtitle(pp("Trip distance distribution for hour ",df$dep_hr[1]))
		ggsave(p,filename=pp(path.to.CHTS,'plots_by_hour/Upstate_Trip_Distance_frequency_hour_',df$dep_hr[1],'.pdf'),width=17,height=11)
	}
	if(length(na.omit(df$tripdur))>0) {
		pt <- ggplot(df, aes(x=tripdur)) + geom_bar(stat="bin") + ggtitle(pp("Trip duration distribution for hour ",df$dep_hr[1]))
		ggsave(pt,filename=pp(path.to.CHTS,'plots_by_hour/Upstate_Trip_Duration_frequency_hour_',df$dep_hr[1],'.pdf'),width=17,height=11)
	}
	if(length(na.omit(df$actdur))>0) {
		pa <- ggplot(df, aes(x=actdur)) + geom_bar(stat="bin") + ggtitle(pp("Activity duration distribution for hour ",df$dep_hr[1]))
		ggsave(pa,filename=pp(path.to.CHTS,'plots_by_hour/Upstate_Activity_Duration_frequency_hour_',df$dep_hr[1],'.pdf'),width=17,height=11)
	}
	if(length(na.omit(df$mode))>0) {
		pm <- ggplot(df, aes(x=mode)) + geom_bar(stat="bin",binwidth=1) + ggtitle(pp("Trip mode distribution for hour ",df$dep_hr[1]))	
		ggsave(pm,filename=pp(path.to.CHTS,'plots_by_hour/Upstate_Trip_Mode_frequency_hour_',df$dep_hr[1],'.pdf'),width=17,height=11)
	}
	if(length(na.omit(df$ctfip))>0) {
		pc <- ggplot(df, aes(x=ctfip)) + geom_bar(stat="bin",binwidth=1) + ggtitle(pp("Trip county distribution for hour ",df$dep_hr[1]))
		ggsave(pc,filename=pp(path.to.CHTS,'plots_by_hour/Upstate_Trip_County_frequency_hour_',df$dep_hr[1],'.pdf'),width=17,height=11)
	}
})
