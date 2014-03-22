## Gov. Brown Executive Order Has an adoption goal of 10% of new light duty vehicles by 2015 and 25% by 2020.

pops <- data.frame(year=rep(seq(2002,2012,by=2),3),county=c(rep('sha',6),rep('teh',6),rep('sis',6)),pop=c(171.1,176.5,178.5,180.5,177.3,178.6,
                                                                                                          57,58.8,60.4,61.1,63.7,63.4,
                                                                                                          43.9,44.2,44.2,44.5,45.0,44.2))
proj <- data.frame(year=rep(2014:2025,3),county=rep(c('sha','teh','sis'),each=12))
proj$pop <- predict(lm('pop ~ county + year:county -1',pops),newdata=proj) * 1000
proj$num.veh <- proj$pop * 0.84 # 0.84 is average # vehicle per capita in California

# share of state hybrid adoption: http://www.environment.ucla.edu/reportcard/article2304.html
teh <- 0.002
sha <- 0.005
sis <- 0.003

# CA hybrid trajectory http://en.wikipedia.org/wiki/Hybrid_electric_vehicles_in_the_United_States: 
# California has been the state leading hybrid sales in the U.S. with 55,553 vehicles sold in 2009,[11] 74,932 in 2008,[12] and 91,417 in 2007
# CA 1.54 hybrids / 1000 peeps in 2009

ca.hyb <- data.frame(year=2007:2009,sales=c(91417,74932,55553),num=c(NA,NA,1.54*36.9612e3),nation=c(352272,312386,290271))
ca.hyb$frac.of.nation <- ca.hyb$sale/ca.hyb$nation
cum.sold.thru.09 <- sum(c(9350,20282,36035,47600,84199,209711,252636))
ca.hyb$num[3] <- cum.sold.thru.09 * mean(ca.hyb$frac.of.nation)
ca.hyb$num[2] <- ca.hyb$num[3] - ca.hyb$sales[3]
ca.hyb$num[1] <- ca.hyb$num[2] - ca.hyb$sales[2]

hybs <- data.frame(year=rep(2007:2009,3),county=rep(c('sha','teh','sis'),each=3))
for(cnt in c('teh','sis','sha')){
  streval(pp('hybs$num[hybs$county=="',cnt,'"] <- ',cnt,' * ca.hyb$num'))
}

hybs$year <- hybs$year + 8
hybs <- data.table(hybs,key=c('year','county'))
proj <- data.table(proj,key=c('year','county'))
proj <- hybs[proj]
proj[,':='(num.hyb=num,num=NULL)]

reg.proj <- proj[,list(num.hyb=sum(num.hyb),num.veh=sum(num.veh),pop=sum(pop),pen= sum(num.hyb)/sum(num.veh)),by='year']
reg.proj$num.hyb[5:12] <- predict(lm('num.hyb ~ year',reg.proj),newdata=data.frame(year=2018:2025))
reg.proj$num.hyb[1] <- 0
reg.proj[,pen:=num.hyb/num.veh]
reg.proj[,num.hyb.10p:=num.hyb*1.1]
reg.proj[,num.hyb.25p:=num.hyb*1.25]

par(mar=c(5,4,6,5)+.1)
plot(reg.proj$year,reg.proj$num.hyb,xlim=c(2013,2026),ylim=c(0,7000),xlab="Year",ylab="Number of PEVs",main="Projection of PEV Adoption in the Upstate Region",xaxt='n')
title(main=paste("(assuming linear growth in total reg. vehicles and PEV adoption"),line=1.7,font.main=1)
title(main=paste("follows same trend as hybrid-electric adoption)"),line=0.5,font.main=1)
axis(4,at=reg.proj$num.veh[reg.proj$year==2020]*c(0.5,1,2)/100,labels=c(0.5,1,2))
axis(1,at=2013:2026)
mtext("% of 2020 Vehicle Stock",side=4,line=3)
grid()
abline(h=0.005*reg.proj$num.veh[reg.proj$year==2020],lty=2)
abline(h=0.01*reg.proj$num.veh[reg.proj$year==2020],lty=2)
abline(h=0.02*reg.proj$num.veh[reg.proj$year==2020],lty=2)
abline(lm('num.hyb~year',subset(reg.proj,year>2014)))
abline(lm('num.hyb.10p ~ year',subset(reg.proj,year>2014)))
abline(lm('num.hyb.25p~ year',subset(reg.proj,year>2014)))


# other illustrative plots for presentations
ggplot(subset(schedule.reps,model=="Upstate"),aes(x=miles,y=..density..)) + geom_histogram() + labs(x="Trip Distance (miles)",y="Density",title="Distribution of Trip Lengths")
ggplot(subset(schedule.reps,model=="Upstate"),aes(x=miles)) + geom_step(stat='ecdf') + labs(x="Trip Distance (miles)",y="Density",title="Cumulative Fraction of Trips Below XX Miles")


