library(colinmisc)
load.libraries(c('plyr','ggplot2','gtools','gdata','googleVis'))

path.to.humveh <- '~/Dropbox/serc/pev-colin/data/Vehicle-Registration/'

years <- c(2003,2005,2007,2009,2010,2011)

first.year <- T

for(yr in years){
  if(first.year){
    veh <- read.csv(paste(path.to.humveh,'vehicles-',yr,'.csv',sep=''))
    veh$year <- yr
    first.year <- F
  }else{
    veh <- rbind(veh,data.frame(read.csv(paste(path.to.humveh,'vehicles-',yr,'.csv',sep='')),year=yr))
  }
}

# Trim the levels then convert to strings
for(col.name in c('FUEL.TYPE','MAKE','MODEL','VEHICLE.CATEGORY')){
  levels(veh[[col.name]]) <- trim(levels(veh[[col.name]]))
  veh[[col.name]] <- as.character(veh[[col.name]])
}

zips <- read.csv(paste(path.to.humveh,'zipcode.csv',sep=''))
veh$city <- zips$city[match(veh$ZIP.CODE,zips$zip)]
veh$zip.city <- paste(veh$city,veh$ZIP.CODE,sep=' - ')

# Hybrid and EV registrations by year and zip

ggplot(subset(veh,FUEL.TYPE%in%c("GAS/ELEC      ","ELECTRIC      ")),aes(x=year,y=COUNT))+stat_summary(fun.y=sum,geom="bar",aes(fill=FUEL.TYPE))+facet_wrap(~zip.city)

# Same but now show regisrations as a fraction of total zip registrations

tot.by.year <- ddply(veh,.(year),function(df){ data.frame(count=sum(df$COUNT,na.rm=T)) })
frac.by.year <- ddply(veh,.(FUEL.TYPE,year),function(df){ data.frame(frac=sum(df$COUNT,na.rm=T)/subset(tot.by.year,year==df$year[1],count)$count,count=sum(df$COUNT,na.rm=T)) })
tot.by.zip.year <- ddply(veh,.(zip.city,year),function(df){ data.frame(count=sum(df$COUNT,na.rm=T)) })
frac.by.zip.year <- ddply(veh,.(FUEL.TYPE,zip.city,year),function(df){ data.frame(zip=df$ZIP.CODE[1],frac=sum(df$COUNT,na.rm=T)/subset(tot.by.zip.year,zip.city==df$zip.city[1] & year==df$year[1],count)$count) })

# Just gas and electric
ggplot(subset(frac.by.zip.year,FUEL.TYPE%in%c("GAS/ELEC","ELECTRIC")),aes(x=year,y=frac*100))+geom_bar(stat="identity",aes(fill=FUEL.TYPE))+facet_wrap(~zip.city)+scale_y_continuous(name="% of Vehicles Registered in ZIP")

ggplot(subset(frac.by.year,FUEL.TYPE%in%c("GAS/ELEC","ELECTRIC")),aes(x=year,y=frac*100))+geom_bar(stat="identity",aes(fill=FUEL.TYPE))+scale_y_continuous(name="% of Vehicles Registered in Humboldt County")

# All fuel types
ggplot(frac.by.zip.year,aes(x=year,y=frac*100))+geom_bar(stat="identity",aes(fill=FUEL.TYPE))+facet_wrap(~zip.city)+scale_y_continuous(name="% of Vehicles Registered in ZIP")

# Look at specific MODELS (too many for the bar chart to be useful)
#frac.by.zip.year.model <- ddply(veh,.(MODEL,zip.city,year),function(df){ data.frame(FUEL.TYPE=df$FUEL.TYPE[1],frac=sum(df$COUNT,na.rm=T)/subset(tot.by.zip.year,zip.city==df$zip.city[1] & year==df$year[1],count)$count) })
#hybrid.models<-unlist(unique(subset(veh,FUEL.TYPE=="GAS/ELEC",MODEL)))
#ggplot(subset(frac.by.zip.year.model,FUEL.TYPE=="GAS/ELEC" & MODEL %in% hybrid.models),aes(x=year,y=frac*100))+geom_bar(stat="identity",aes(fill=MODEL))+facet_wrap(~zip.city)+scale_y_continuous(name="% of Vehicles Registered in ZIP")

# Look at specific MAKES of hybrids 
frac.by.zip.year.make <- ddply(subset(veh,FUEL.TYPE=="GAS/ELEC"),.(MAKE,zip.city,year),function(df){ data.frame(FUEL.TYPE=df$FUEL.TYPE[1],frac=sum(df$COUNT,na.rm=T)/subset(tot.by.zip.year,zip.city==df$zip.city[1] & year==df$year[1],count)$count,stringsAsFactors=F) })
hybrid.makes <- unlist(unique(subset(veh,FUEL.TYPE=="GAS/ELEC",MAKE)))
ggplot(subset(frac.by.zip.year.make, MAKE %in% hybrid.makes),aes(x=year,y=frac*100))+geom_bar(stat="identity",aes(fill=MAKE))+facet_wrap(~zip.city)+scale_y_continuous(name="% of Vehicles Registered in ZIP")

frac.by.year.make <- ddply(subset(veh,FUEL.TYPE=="GAS/ELEC"),.(MAKE,year),function(df){ data.frame(FUEL.TYPE=df$FUEL.TYPE[1],frac=sum(df$COUNT,na.rm=T)/subset(tot.by.year,year==df$year[1],count)$count,stringsAsFactors=F) })
ggplot(subset(frac.by.year.make, MAKE %in% hybrid.makes),aes(x=year,y=frac*100))+geom_bar(stat="identity",aes(fill=MAKE))+scale_y_continuous(name="% of Vehicles Registered in Humboldt")

# Look at specific MODELS of EVS 
frac.by.zip.year.model.ev <- ddply(subset(veh,FUEL.TYPE=="ELECTRIC"),.(MODEL,zip.city,year),function(df){ data.frame(FUEL.TYPE=df$FUEL.TYPE[1],frac=sum(df$COUNT,na.rm=T)/subset(tot.by.zip.year,zip.city==df$zip.city[1] & year==df$year[1],count)$count,count=sum(df$COUNT,na.rm=T),stringsAsFactors=F) })
ev.models <- unlist(unique(subset(veh,FUEL.TYPE=="ELECTRIC",MODEL)))
ggplot(subset(frac.by.zip.year.model.ev, MODEL %in% ev.models),aes(x=year,y=frac*100))+geom_bar(stat="identity",aes(fill=MODEL))+facet_wrap(~zip.city)+scale_y_continuous(name="% of Vehicles Registered in ZIP")
ggplot(subset(frac.by.zip.year.model.ev, MODEL %in% ev.models),aes(x=year,y=count))+geom_bar(stat="identity",aes(fill=MODEL))+facet_wrap(~zip.city)+scale_y_continuous(name="Number of Vehicles Registered in ZIP")

frac.by.year.model.ev <- ddply(subset(veh,FUEL.TYPE=="ELECTRIC"),.(MODEL,year),function(df){ 
  data.frame( FUEL.TYPE=df$FUEL.TYPE[1],
              frac=sum(df$COUNT,na.rm=T)/subset(tot.by.year,year==df$year[1],count)$count,
              count=sum(df$COUNT,na.rm=T),stringsAsFactors=F) })
ggplot(subset(frac.by.year.model.ev, MODEL %in% ev.models),aes(x=year,y=frac*100))+geom_bar(stat="identity",aes(fill=MODEL))+scale_y_continuous(name="% of Vehicles Registered in Humboldt")
ggplot(subset(frac.by.year.model.ev, MODEL %in% ev.models),aes(x=year,y=count))+geom_bar(stat="identity",aes(fill=MODEL))+scale_y_continuous(name="Number of Vehicles Registered in Humboldt")

save.image(file=paste(path.to.humveh,'session.Rdata'))
frac.ev.hybrid.by.zip.year <- subset(frac.by.zip.year,FUEL.TYPE%in%c("GAS/ELEC","ELECTRIC"))
save(frac.ev.hybrid.by.zip.year,file=paste(path.to.humveh,'frac-ev-hybrid-by-zip-year.Rdata'))

# assume PEV total adoption mimics hybrid adoption, what is the yearly penetration levels and total number of PEVs
fit <- lm('count ~ year',data.frame(year=tot.by.year$year,count=tot.by.year$count/1e3))
proj.years <- 2012:2025
proj.count <- predict(fit,newdata=data.frame(year=proj.years))
max.count <- max(c(tot.by.year$count/1e3,proj.count))
plot(tot.by.year$year,tot.by.year$count/1e3,ylim=c(0,max.count),xlim=range(c(tot.by.year$year,proj.years)),ylab="Thousands of Registered Vehicles",xlab="Year",main="Linear Projection of Vehicle Registrations in Humbodlt County")
title(main=paste("(R^2 of Linear Fit: ",roundC(summary(fit)$r.squared,3),")",sep=''),line=0.5,font.main=1)
abline(fit,col='red')
points(proj.years,proj.count,col='red',pch=2)
abline(h=seq(0,max.count,by=10),lty=2,col='lightgrey')
abline(v=(tot.by.year$year[1]):proj.years[length(proj.years)],lty=2,col='lightgrey')

tot.and.proj <- rbind(tot.by.year,data.frame(year=proj.years,count=proj.count*1e3))

hybrids.by.year <- ddply(subset(frac.by.year,FUEL.TYPE %in% c('GAS/ELEC','ELECTRIC')),.(year),function(df){ data.frame(count=sum(df$count)) })

# the year offset is when we think a comparable year to 2003 is in our projection, for now 2013
year.offset <- 9
# now scale the counts in the future by the appropriate amount to reflect growth in total registereted vehicles
pevs.by.year <- hybrids.by.year
pevs.by.year$year <- pevs.by.year$year + year.offset
pevs.by.year$count <- pevs.by.year$count * (tot.and.proj$count[match(pevs.by.year$year,tot.and.proj$year)]/tot.and.proj$count[match(pevs.by.year$year-year.offset,tot.and.proj$year)])

par(mar=c(5,4,4,5)+.1)
plot(pevs.by.year$year,pevs.by.year$count,xlim=c(2010,2020),ylim=c(0,1.05*max(pevs.by.year$count)),xlab="Year",ylab="Number of PEVs",main="Projection of PEV Adoption in Humbodlt County")
title(main=paste("(assuming PEV adoption follows same trend as hybrid-electric adoption)"),line=0.5,font.main=1)
axis(4,at=axTicks(2),labels=roundC(axTicks(2)/tot.and.proj$count[tot.and.proj$year==2020]*100,2))
mtext("% of 2020 Vehicle Stock",side=4,line=3)
grid()
abline(lm('count~year',pevs.by.year),col='red')



# Now do an independent calc based on observed vehicle replacements

k.val <- read.csv(paste(path.to.humveh,'k-values.csv',sep=''))
k.val<-melt(k.val,id.vars='age')
names(k.val) <- c('age','set','k')

k.val$recession <- ifelse(k.val$set %in% c('k.03.05','k.05.07','k.07.09'),'pre','post')

#k.val <- rbind(k.val,data.frame(set="mean",ddply(k.val,.(age),function(df){ data.frame(k=mean(df$k)) })))
#k.val <- rbind(k.val,data.frame(set="median",ddply(subset(k.val,set!='mean'),.(age),function(df){ data.frame(k=median(df$k)) })))

ggplot(k.val,aes(x=age,y=k))+geom_point(aes(colour=set))+geom_line(aes(colour=set))+scale_y_continuous(limits=c(-0.25,1))+facet_wrap(~recession)
ggplot(k.val,aes(x=set,y=k))+geom_point()+scale_y_continuous(limits=c(-0.25,1))+facet_wrap(~age)

k.to.use <- na.omit(ddply(k.val,.(age),function(df){ data.frame(k=mean(df$k,na.rm=T)) }))
k.to.use <- na.omit(ddply(k.val,.(age),function(df){ data.frame(k=median(df$k,na.rm=T)) }))
for(the.set in c('k.03.05','k.05.07','k.07.09','k.09.10','k.10.11')){
  k.to.use <- na.omit(subset(k.val,set==the.set))
  k.to.use$k[k.to.use$age==1] <- 0.75 # bad data in some sets for the first year, so fix to .75

  first.loop <- T
  for(new.pev.pen in c(0.01,.02,.03,.04,.05,seq(0.1,0.5,by=0.1))){
    pevs.by.year.k <- data.frame(year=rep(proj.years,each=nrow(k.to.use)+1),age=rep(c(0,k.to.use$age),length(proj.years)),count=0)
    for(yr in proj.years[2:length(proj.years)]){
      pevs.by.year.k$count[pevs.by.year.k$age==0 & pevs.by.year.k$year == yr] <- diff(tot.and.proj$count[tot.and.proj$year %in% c(yr-1,yr)]) * new.pev.pen
      for(age in sort(k.to.use$age)){
        pevs.by.year.k$count[pevs.by.year.k$age==age & pevs.by.year.k$year == yr] <- pevs.by.year.k$count[pevs.by.year.k$age==(age-1) & pevs.by.year.k$year == (yr-1)] * (1 + k.to.use$k[k.to.use$age == age])
      }
    }
    pevs.by.year.k$penetration <- pevs.by.year.k$count / tot.and.proj$count[match(pevs.by.year.k$year,tot.and.proj$year)]
    pevs.by.year.k$new.pev.pen <- new.pev.pen
    if(first.loop){
      k.pens <- pevs.by.year.k
      first.loop <- F
    }else{
      k.pens <- rbind(k.pens,pevs.by.year.k)
    }
  }

  ## plot the counts
  #ggplot(pevs.by.year.k,aes(x=year,y=count))+geom_bar(stat='identity',aes(fill=age))
  ## plot the penetrations
  #ggplot(pevs.by.year.k,aes(x=year,y=penetration))+geom_bar(stat='identity',aes(fill=age))

  #ggplot(subset(k.pens,year<=2020),aes(x=year,y=penetration*100))+geom_bar(stat='identity',aes(fill=age))+facet_wrap(~new.pev.pen)
  #ggplot(subset(k.pens,year==2020),aes(x=new.pev.pen*100,y=penetration*100))+stat_summary(fun.y='sum',geom='line')

  # make a polished plot
  #k.pens$new.pev.pen<-k.pens$new.pev.pen*100
  #ggplot(subset(k.pens),aes(x=year,y=penetration*100))+geom_bar(stat='identity',aes(fill=age))+facet_wrap(~new.pev.pen)+scale_y_continuous(name="% Penetration of PEVS")+opts(title="Adoption of PEVs for Various Penetration Rates into New Vehicle Sales")

  # ratio between 2020 PEV penetration and fraction of new vehicles every year answer 0.4, so 2.5% pen of new vehicles needed to get to 1% by 2020
  print(paste('set:',the.set,' magic ratio:',summary(lm('pen~new.pev.pen',ddply(subset(k.pens,year==2020),.(new.pev.pen),function(df){ data.frame(pen=sum(df$penetration)) })))$coefficients[2,1]))
}

p2p.ratio <- data.frame(set=c('mean','median','k.03.05','k.05.07','k.07.09','k.09.10','k.10.11'),r=c(0.433,0.447926019414215,0.479665208350499,0.464166220755651,0.461791166534611,0.383206445571681,0.378188912088241))+scale_y_continuous(name="PEV Penetration into New Vehicles (%)")+opts(title="hi")






