# Apply county labels to household data
combined.region$county <- combined.region.id$COUNTY[match(combined.region$ctfip,combined.region.id$CTFIP)]

# Generate the necessary place data from household data
combined.place.data <- subset(place,sampn%in%combined.region$sampno)

# Link county name to sample number in the place data
combined.place.data$county <- combined.region$county[match(combined.place.data$sampn,combined.region$sampno)]

# Remove non-car modes if desired, and link the names to the codes
combined.place.data <- subset(combined.place.data,mode%in%car.modes$MODE)
combined.place.data$mode.name <- car.modes$TRANSPORT[match(combined.place.data$mode,car.modes$MODE)]

#Travel attribute by county
d_ply(combined.place.data,.(county),function(df){
	if(length(na.omit(df$tripdistance))>0) {
		p <- ggplot(df, aes(x=tripdistance)) + geom_bar(stat="bin",binwidth=1) + ggtitle(pp("Trip distance distribution for ",df$county[1]," county"))
		ggsave(p,filename=pp(path.to.CHTS,'plots_by_county/Upstate_Trip_Distance_frequency_county_',df$county[1],'.pdf'),width=17,height=11)
	}
	if(length(na.omit(df$tripdur))>0) {
		pt <- ggplot(df, aes(x=tripdur)) + geom_bar(stat="bin") + ggtitle(pp("Trip duration distribution for ",df$county[1]," county"))
		ggsave(pt,filename=pp('~/Dropbox/serc/pev-shared/data/CHTS/plots_by_county/Upstate_Trip_Duration_frequency_county_',df$county[1],'.pdf'),width=17,height=11)
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