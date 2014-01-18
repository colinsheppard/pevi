######################################################################################################
# OD Processing
# 
# The Upstate model requires some processing of the OD data provided by SRTA in adddition to the 
# creation of travel demand data based on CHTS
######################################################################################################

load.libraries(c('maptools','plotrix','stats','gpclib','plyr','png','RgoogleMaps','lattice','stringr','ggplot2','XML','plotKML','rgeos','doMC','reshape','data.table','DEoptim'))
gpclibPermit()
registerDoMC(num.cpu)

###################################################################################################################################################
# LOAD DATA NEEDED FOR TRIP DISTRIBUTION WORK
###################################################################################################################################################

# Take the pa matrix and distribute it by time of day according to the od distribution
if(!file.exists(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-by-purpose.Rdata'))){
  # Load the OD data
  od <- read.table(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/sh20_adjvehtrips_3per_3occ.txt'),header=T)

  # For now, aggregate occupancy levels into a single column
  od$trips.24 <- od$trips.1occ.24 + od$trips.2occ.24 + od$trips.3occ.24
  od$trips.am <- od$trips.1occ.am + od$trips.2occ.am + od$trips.3occ.am
  od$trips.pm <- od$trips.1occ.pm + od$trips.2occ.pm + od$trips.3occ.pm
  od <- od[,c(1,2,(ncol(od)-2):ncol(od))]

  # For 2010 OD matrix, zero rows are excluded from the data, add a row for from==292 for convenience in processing 
  #od <- rbind(od,data.frame(from=292,to=101,trips.24=0,trips.am=0,trips.pm=0))

  pa <- read.table(pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/sh20_vehtrips_7purp_3occ.txt'),header=T)
  ## create an omni-directional id over which we can sum PA/AP pairs in both directions
  pa$omni <- apply(pa,1,function(x){
    pp(sort(x[1:2]),collapse=".")
  })
  purp.cols <- c('com','ho','hs','hsc','hw','oo','wo')
  pa <- pa[,c('p','a',purp.cols,'tot','omni')]
  # the following data were from a DOT report for the city of Jacksonville Florida, only used b/c quicker than 
  # analyzing CHTS for the same data, but this should be done eventually
  temporal.dists <- data.frame(pa.ap=c(rep('pa',7),rep('ap',7)),
    purp=c('hw','hs','ho','wo','oo','hsc','com','hw','hs','ho','wo','oo','hsc','com'),
    am=c(.67,.1,.46,.12,.12,.67,.20,.03,.03,.09,.13,.13,.03,.2),
    pm=c(.06,.28,.17,.29,.29,.06,.25,.67,.38,.39,.29,.29,.66,.3))
  temporal.dists <- cast(melt(temporal.dists,id.vars=c('pa.ap','purp'),measure.vars=c('am','pm')),'pa.ap ~ purp ~ variable')
  #pa.ap <- ddply(pa,.(omni),function(df){
    #colSums(df[,c('hw','hs','ho','wo','oo','hsc','com','tot')])
  #})
  #od$omni <- apply(od,1,function(x){
    #pp(sort(x[1:2]),collapse=".")
  #})
  #od.do <- ddply(od,.(omni),function(df){
    #colSums(df[,c('trips.24','trips.am','trips.pm')])
  #})
  #od.do$frac.am <- od.do$trips.am / od.do$trips.24
  #od.do$frac.am[od.do$frac.am>1] <- NA
  #od.do$frac.pm <- od.do$trips.pm / od.do$trips.24
  #od.do$frac.pm[od.do$frac.pm>1] <- NA
  #pa.z <- apply(pa.ap[,c('hw','hs','ho','wo','oo','hsc','com')],1,function(x){ sum(x==0) })
  odp <- data.frame(expand.grid(unique(c(od$from,od$to)),unique(c(od$from,od$to))))
  names(odp) <- c('from','to')
  odp$omni <- apply(odp,1,function(x){
    pp(sort(x[1:2]),collapse=".")
  })
  odp <- ddply(odp,.(from,to),function(df){
    pa.row <- subset(pa,p==df$from & a==df$to)[,purp.cols]/2
    if(nrow(pa.row)==0)pa.row[1,] <- 0
    ap.row <- subset(pa,a==df$from & p==df$to)[,purp.cols]/2
    if(nrow(ap.row)==0)ap.row[1,] <- 0
    am.row <-  pa.row * temporal.dists['pa',,'am'] + ap.row * temporal.dists['ap',,'am']
    pm.row <-  pa.row * temporal.dists['pa',,'pm'] + ap.row * temporal.dists['ap',,'pm']
    all.day.row <- colSums(subset(pa,omni==df$omni)[,purp.cols])/2
    data.frame(t(all.day.row),am.row,pm.row)
  },.parallel=T)
  names(odp) <- c('from','to',purp.cols,pp(purp.cols,'.am'),pp(purp.cols,'.pm'))
  save(purp.cols,odp,file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-by-purpose.Rdata'))
}else{
  load(file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-by-purpose.Rdata'))
}

taz <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/Shasta_TAZ'))
taz.centroids <- data.frame(taz=taz$TAZ,longitude=coordinates(taz)[,1],latitude=coordinates(taz)[,2])

taz.centroids.sp <-SpatialPointsDataFrame(coordinates(taz),data=taz.centroids)
redd <- readShapePoly(pp(pevi.shared,'data/UPSTATE/shapefiles/GreaterReddingArea'))
taz.centroids.sp$near.redding <- !is.na(over(taz.centroids.sp,redd)$Name)

# Find # trips in/out of each TAZ per capita
odp.internal <- subset(odp,from>=100 & to>=100)
tot.trips <- data.frame(taz=sort(unique(odp.internal$from)),orig=ddply(odp.internal,.(from),function(df){ sum(df[,purp.cols]) })$V1,dest=ddply(odp.internal,.(to),function(df){ sum(df[,purp.cols]) })$V1)

dem <- read.csv(pp(pevi.shared,'data/UPSTATE/demographics/TAZ-Demography-2004-Base-Scenario.csv'))
dem$long  <-  taz.centroids$long[match(dem$TAZ,taz.centroids$taz)]
dem$lat   <-  taz.centroids$lat[match(dem$TAZ,taz.centroids$taz)]
dem$near.redding <-  taz.centroids.sp$near.redding[match(dem$TAZ,taz.centroids.sp$taz)]
dem$trips.from  <- tot.trips$orig[match(dem$TAZ,tot.trips$taz)]
dem$trips.to    <- tot.trips$dest[match(dem$TAZ,tot.trips$taz)]

# get rid of data from within redding area, very low populations and the outliers in terms of trips/capita (college and industrial TAZs)
to.remove <- c(975,1136)
dem.sub <- subset(dem,!near.redding & Population>20 & !TAZ %in% to.remove)
names(dem.sub) <- str_replace(str_replace(names(dem.sub),"Population","population"),"TOTAL.1","employment")

# Explore / Analyze / Model
#ggplot(dem.sub,aes(x=Population,y=trips.from/Population))+geom_point()+facet_wrap(~Community)
#ggplot(dem.sub,aes(x=Population,y=trips.from/Population))+geom_point()+facet_wrap(~Community)
#ggplot(dem.sub,aes(x=TOTAL.1,y=trips.from/Population))+geom_point()
#ggplot(subset(dem,!near.redding),aes(x=long,y=lat,size=trips.from/Population,colour=TOTAL.1))+geom_point()
#pairs(dem.sub[,c('trips.to','Population','Service.Commercial','Office','School','Restaurant','TOTAL','TOTAL.1','Pop.HU')],pch='.')
#summary(lm('trips.from ~ Population + Service.Commercial + Office + School + Restaurant + TOTAL + TOTAL.1 + Pop.HU',dem.sub))

# load the pointTAZS
if(file.exists(pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs.Rdata'))){
  pt.taz <- readShapePoints(pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs'))
  if(!"trips_from" %in% names(pt.taz@data)){
    #write.csv(pt.taz@data,pp(pevi.shared,'data/UPSTATE/demographics/pt-tazs.csv'))
    # in here I manually added demog data
    pt.taz.data <- read.csv(pp(pevi.shared,'data/UPSTATE/demographics/pt-tazs.csv'))
    fit.from <- lm('trips.from ~ population + employment',dem.sub)
    fit.to <- lm('trips.to ~ population + employment',dem.sub)
    pt.taz$population <- pt.taz.data$population[match(pt.taz$Name,pt.taz.data$Name)]
    pt.taz$employment <- pt.taz.data$employment[match(pt.taz$Name,pt.taz.data$Name)]
    pt.taz$trips_from <- predict(fit.from,newdata=pt.taz@data)
    pt.taz$trips_to <- predict(fit.to,newdata=pt.taz@data)
    pt.taz$Name <- as.character(pt.taz$Name)
    pt.taz$Name[pt.taz$Name=="Platina Center"] <- "Platina"
    pt.taz@data <- pt.taz@data[,c(1,(ncol(pt.taz@data)-3):ncol(pt.taz@data))]
    write.csv(pt.taz@data,pp(pevi.shared,'data/UPSTATE/demographics/pt-tazs.csv'))
    writePointsShape(pt.taz,pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs'))
  }
  save(pt.taz,file=pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs.Rdata'))
}else{
  load(file=pp(pevi.shared,'data/UPSTATE/shapefiles/PointTAZs.Rdata'))
}

# After excluding XI trips from tot.trips I used the following to update the agg.taz data frame:
# pt.taz$Name[pt.taz$Name=="Mt. Shasta"] <- "MtShasta"
# agg.taz$total.demand[agg.taz$point]<-pt.taz$trips_from[match(agg.taz$name[agg.taz$point],pt.taz$Name)]
#save(agg.taz,file=pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))

# load the agg.taz that include the pt TAZs 
# (these are created by TAZ-aggregate.R which requires the point TAZs developed above)
load(file=pp(pevi.shared,'data/UPSTATE/shapefiles/AggregatedTAZsWithPointTAZs.Rdata'))

# Load od.agg, the aggregated OD matrix
load(file=pp(pevi.shared,'data/UPSTATE/Shasta-OD-2020/od-aggregated.Rdata'))


#fric.ids <- subset(agg.taz,!point & !near.redding)$agg.id
#fric.ids <- subset(agg.taz,!point)$agg.id
#fric.dts <- subset(time.distance,origin.id %in% fric.ids & destination.id %in% fric.ids)[,c('origin.id','destination.id','miles')]
#fric.dts$key <- pp(fric.dts$origin.id,"_",fric.dts$destination.id)
#fric.od <- subset(od.agg,from %in% fric.ids & to %in% fric.ids)
#fric.od$key <- pp(fric.od$from,"_",fric.od$to)
#fric.m <- melt(fric.od[,c(ncol(fric.od),3:(ncol(fric.od)-1))],id.vars='key')
#fric.m$dist <- fric.dts$miles[match(fric.m$key,fric.dts$key)]
#fric.m$value <- round(fric.m$value)
#fric.mm <- ddply(fric.m,.(key,variable),function(df){ data.frame(dist=rep(df$dist,df$value)) })
#ggplot(fric.m,aes(x=dist))+geom_histogram()+facet_wrap(~variable)

###################################################################################################################################################
# Load CHTS for NSSR (North State Super Region)
###################################################################################################################################################
load(file=pp(pevi.shared,'data/CHTS/nssr-subset.Rdata'))
setkey(nssr.place,"td.purpose")
time.by.purp <- dlply(subset(nssr.place,tripdistance<400),.(td.purpose),function(df){ df$tripdur / 60 })
fric.functions <- llply(time.by.purp,function(l){ den <- density(l)
  positive.inds <- which(den$x>0)
  # the following is how we'd create and emprical CDF but that's not needed here, duh
  #dx <- diff(head(den$x,2))
  #y <- cumsum(dx*den$y[positive.inds])
  #data.frame(x=den$x[positive.inds],y=y/max(y))
  data.frame(x=den$x[positive.inds],y=den$y[positive.inds])
})

###################################################################################################################################################
# define the tazs and gates for convenience
###################################################################################################################################################
sis.ids <- subset(agg.taz@data,jurisdiction=="Siskiyou")$agg.id
sis.gates <- -c(205,213)
teh.ids <- subset(agg.taz@data,jurisdiction=="Tehama")$agg.id
teh.gates <- -c(207,215)
sha.ids <- subset(agg.taz@data,agg.id>=100 & agg.id<200)$agg.id
#sha.gates <- -112
sha.gates <- c(4,5,6,14,15)
sha.gate.agg.id <- c(111,111,107,120,112)
new.gates <- c(sis.gates,teh.gates)

###################################################################################################################################################
# GATEWAYS
# here we get setup to integrate the final gateways into the PA data and then into the final OD matrix, P/A will be based on
# traffic volumes from the screenline data set
###################################################################################################################################################
# load up the screenline data 
load(file=pp(pevi.shared,"data/UPSTATE/screenline-counts.Rdata"))

# there seems to be little in the way of long term trends in the counts data, so let's just take the overall average at each relevant postmile
# also, average over the Ahead/Back as the distinction is too complicated to deal with for now
cnts <- data.table(na.omit(cnts),key=c('County','Route','Postmile'))
cnts.avg <- cnts[,list(avg=mean(value,na.rm=T)),by=c('County','Route','Postmile')]
#ggplot(cnts.avg,aes(x=Postmile,y=avg))+geom_point()+facet_wrap(County~Route)+geom_line()

# define one-way productions for the gateways
xp <- data.frame(taz=-213,p=mean(subset(cnts.avg,County=="SIS" & Route==5 & Postmile>69)$avg))
xp <- rbind(xp,data.frame(taz=-205,p=mean(subset(cnts.avg,County=="SIS" & Route==3 & Postmile==21.470)$avg)))
xp <- rbind(xp,data.frame(taz=-207,p=mean(subset(cnts.avg,County=="TEH" & Route==99 & Postmile==12.308)$avg)))
xp <- rbind(xp,data.frame(taz=-215,p=mean(subset(cnts.avg,County=="TEH" & Route==5 & Postmile==0.000)$avg)))
xp <- rbind(xp,data.frame(taz=4,p=mean(subset(cnts.avg,County=="SHA" & Route==299 & Postmile > 99)$avg)))
xp <- rbind(xp,data.frame(taz=5,p=mean(subset(cnts.avg,County=="SHA" & Route==299 & Postmile > 99)$avg)/3))
xp <- rbind(xp,data.frame(taz=6,p=mean(subset(cnts.avg,County=="SHA" & Route==44 & Postmile > 71)$avg)))
xp <- rbind(xp,data.frame(taz=14,p=subset(cnts.avg,County=="SHA" & Route==36 & Postmile < 5)$avg))
xp <- rbind(xp,data.frame(taz=15,p=mean(subset(cnts.avg,County=="SHA" & Route==299 & Postmile==0)$avg)))
xp <- data.table(xp,key='taz')

# add the gateways to agg.taz
agg.taz.data <- agg.taz@data
agg.taz.data <- merge(agg.taz.data,data.frame(taz=new.gates,agg.id=new.gates,
                                              name=pp("GATE_",agg.taz.data$name[match(-new.gates, agg.taz.data$agg.id)]),
                                              jurisdiction=agg.taz.data$jurisdiction[match(-new.gates, agg.taz.data$agg.id)],
                                              area=0,shp.id=NA,total.demand=NA,population=NA,employment=NA,point=T,near.redding=F),all=T)
agg.taz.data <- merge(agg.taz.data,data.frame(taz=sha.gates,agg.id=sha.gates,
                                              name=pp("GATE_",agg.taz.data$name[match(sha.gate.agg.id, agg.taz.data$agg.id)]),
                                              jurisdiction=agg.taz.data$jurisdiction[match(sha.gate.agg.id, agg.taz.data$agg.id)],
                                              area=0,shp.id=NA,total.demand=NA,population=NA,employment=NA,point=T,near.redding=F),all=T)
agg.taz.data$name[agg.taz.data$agg.id==4] <- 'GATE_FallRiver_299'
agg.taz.data$name[agg.taz.data$agg.id==5] <- 'GATE_FallRiver_Old'

# specify gateways from SRTA that will become internalized and lose their status as gateways in new matrix associate the 
# shasta TAZs that should be assumed to flow through those gateways en-route to SIS or TEH

gates.to.intern <- list('1'=list('Siskiyou'=as.numeric(grep('111|107|152|117',c(sha.ids,sha.gates),value=T,invert=T)),'Tehama'=c()),
                        '3'=list('Siskiyou'=c(111,107,152,117),'Tehama'=c()),
                        '13'=list('Tehama'=120,'Siskiyou'=c()),
                        '11'=list('Tehama'=as.numeric(grep('120',c(sha.ids,sha.gates),value=T,invert=T)),'Siskiyou'=c()),
                        '12'=list('Tehama'=as.numeric(grep('120',c(sha.ids,sha.gates),value=T,invert=T)),'Siskiyou'=c()))
gates.to.intern.ids <- as.numeric(names(gates.to.intern))
new.tazs <- c(sis.ids,sis.gates,teh.ids,teh.gates,sha.ids,sha.gates)

# Load time.distance and add ids to dist/time matrix
load(pp(pevi.shared,'data/UPSTATE/driving-distances/time.distance.Rdata'))
time.distance$from <- agg.taz$agg.id[match(time.distance$orig,agg.taz$name)] 
time.distance$to <- agg.taz$agg.id[match(time.distance$dest,agg.taz$name)] 
# make the time/distance to the gates be based on their closest TAZs
all.gates <- c(new.gates,sha.gates)
all.gates.agg.id <- c(-new.gates,sha.gate.agg.id)
for(i in 1:length(all.gates)){
  gate <- all.gates[i]
  if(gate %in% time.distance$from)next
  agg.id <- all.gates.agg.id[i]
  temp.time.distance <- subset(time.distance,from==agg.id | to==agg.id)
  temp.time.distance$from[temp.time.distance$from==agg.id] <- gate
  temp.time.distance$to[temp.time.distance$to==agg.id] <- gate

  tmp2 <- subset(time.distance,from==agg.id & to%in%all.gates.agg.id)
  tmp2 <- tmp2[match(all.gates.agg.id,tmp2$to),]
  tmp2$from <- gate
  temp.time.distance <- rbind(temp.time.distance,tmp2)
  tmp2 <- subset(time.distance,to==agg.id & from%in%all.gates.agg.id)
  tmp2 <- tmp2[match(all.gates.agg.id,tmp2$from),]
  tmp2$to <- gate
  temp.time.distance <- rbind(temp.time.distance,tmp2)

  time.distance <- rbind(time.distance,temp.time.distance)
}

###################################################################################################################################################
# produce a unique P/A matrix for each case (SIS vs TEH) from the aggregated OD matrix and the total demand estimates on the point TAZs
###################################################################################################################################################
purps <- c('ho','hs','hsc','hw','oo','wo')
new.pa <- expand.grid(juris=c('Siskiyou','Tehama'),taz=new.tazs,purp=purps)
new.pa$trips <- NA
new.pa <- data.table(new.pa)
setkey(new.pa,'taz')
agg.dt <- data.table(agg.taz.data)
agg.dt[,taz:=agg.id]
setkey(agg.dt,"taz")
new.pa.temp <- new.pa[agg.dt]
new.pa <- subset(new.pa.temp,agg.id<200 | juris==jurisdiction)[,list(taz=taz,juris=juris,purp=purp,trips=trips)]

od.agg <- as.data.table(od.agg)

setkey(od.agg,'from')
od.agg.sums <- od.agg[,list(ho=sum(ho),hs=sum(hs),hsc=sum(hsc),hw=sum(hw),oo=sum(oo),wo=sum(wo),tot=sum(c(ho,hs,hsc,hw,oo,wo))),by="from"]
od.agg.sums <- od.agg.sums[,':='(ho=ho/tot,hs=hs/tot,hsc=hsc/tot,hw=hw/tot,oo=oo/tot,wo=wo/tot)]
od.agg.sums.m <- data.table(melt(od.agg.sums,id.vars='from',measure.vars=c('ho','hs','hsc','hw','oo','wo')),key=c('from'))

###################################################################################################################################################
# look into potential demographic relationships
###################################################################################################################################################
#dem <- data.table(dem)
#dem[,from:=TAZ]
#dem[,':='(from=TAZ,population=Population,Population=NULL,employment=TOTAL.1)]
#setkey(dem,"from")
#od.agg.sums.m <- dem[od.agg.sums.m]
#ggplot(od.agg.sums.m,aes(x=population,y=value,shape=near.redding))+geom_point()+facet_wrap(~variable)

# just find the mean ratio by purp
setkey(od.agg.sums.m,"variable")
mean.frac.purp <- od.agg.sums.m[,list(frac=mean(value,na.rm=T)),by=variable][,':='(purp=variable,variable=NULL)]

###################################################################################################################################################
# for SHA the p's and a's are taken as the trips to/from the gates which need to be internalized
###################################################################################################################################################
setkey(new.pa,'juris','taz','purp')
od.agg.m <- data.table(melt(od.agg,id.vars=c('from','to'),measure.vars=c('ho','hs','hsc','hw','oo','wo')),key=c('from','to'))[,':='(purp=variable,variable=NULL,trips=value,value=NULL)]
for(gate in gates.to.intern.ids){
  for(juris in c('Siskiyou','Tehama')){
    if(length(gates.to.intern[[as.character(gate)]][[juris]])>0){
      prods <- od.agg.m[J(gates.to.intern[[as.character(gate)]][[juris]],gate),list(trips=sum(trips)),by=c("from","purp")]
      prods[,':='(taz=from,from=NULL,juris=juris)]
      setkey(prods,'juris','taz','purp')
      new.pa <- prods[new.pa]
      new.pa[,':='(trips=ifelse(is.na(trips),trips.1,trips),trips.1=NULL)]
    }
  }
}

###################################################################################################################################################
# for SHA GATES that are not internalized, make p/a the difference between screenlines and trips internal to SHA
###################################################################################################################################################
xp[,from:=taz]
setkey(xp,'from')
xp <- data.table(od.agg.sums[from %in% sha.gates,list(from,tot)],key='from')[xp]
xp[,p:=ifelse(is.na(tot),p,p-tot)]
xp[,taz:=from]
xp[,tot:=NULL]
new.pa[,trips:=ifelse(taz>0 & taz<100,NA,trips)]

###################################################################################################################################################
# for the gateways, make p/a be the one-way trips
# based on analysis of CHTS trip lengths, X trips nearby are ~5 times more frequent than X trips through to the other county
###################################################################################################################################################

setkey(new.pa,'taz')
setkey(xp,'taz')
setkey(mean.frac.purp,"purp")
xp$sha.frac <- c(1/6,5/6,1/6,5/6,0.75,0.75,0.25,1,0.8)
xp$teh.frac <- 1-xp$sha.frac
new.pa <- mean.frac.purp[data.table(xp[new.pa],key="purp")]
new.pa[,trips:=ifelse(is.na(trips),ifelse(juris=='Siskiyou',p*sha.frac*frac,p*teh.frac*frac),trips)]
new.pa[,':='(p=NULL,sha.frac=NULL,teh.frac=NULL)]

###################################################################################################################################################
# for SIS and TEH p's and a's come from the total demand modeled above are are 
# disaggregated according to the average non-redding trip purpose distribution
###################################################################################################################################################
setkey(new.pa,"taz")
setkey(agg.dt,"taz")
setkey(mean.frac.purp,"purp")
new.pa.with.tot <- data.table(agg.dt[new.pa],key="purp")[,':='(name=NULL,agg.id=NULL,jurisdiction=NULL,area=NULL,shp.id=NULL,population=NULL,employment=NULL,point=NULL,near.redding=NULL)]
new.pa <- mean.frac.purp[new.pa.with.tot]
new.pa[,':='(trips=ifelse(is.na(trips),total.demand*frac,trips),total.demand=NULL,frac=NULL)]
#ggplot(new.pa,aes(x=taz,y=trips,fill=purp))+geom_bar(stat='identity')+facet_wrap(~juris)

###################################################################################################################################################
# setup 'friction' which is really the inverse of friction, or the ease of reaching destinations
###################################################################################################################################################
find.friction <- function(x,f,purp){
  f[[purp]]$y[findInterval(x,f[[purp]]$x,all.inside=T)]
}
time.dist.by.purp <- time.distance[rep(seq_len(nrow(time.distance)),length(purps)),list(from,to,hours)]
time.dist.by.purp[,':='(purp=rep(purps,each=nrow(time.distance)))] 
setkey(time.dist.by.purp,"purp")
time.dist.by.purp[,fric:=find.friction(hours,fric.functions,purp[1]),by="purp"]

setkey(time.dist.by.purp,'purp','from','to')
time.dist.by.purp <- unique(time.dist.by.purp)
distribed <- data.table(expand.grid(from=new.tazs,to=new.tazs,purp=purps),key=c("purp","from","to"))
distribed$trips <- -999
distribed <- time.dist.by.purp[distribed]
distribed$pft <- pp(distribed$purp,'-',distribed$from,'-',distribed$to)
shasta.rows <- which((distribed$from>=100 & distribed$from<199) | (distribed$to>=100 & distribed$to<199))

# we need to associate gateways that were internalized from original OD data need to be reassigned to the appropriate point TAZ
# for use in the constraints inside the gravity objective function
rows.for.gate.map <- list()
for(juris in c('Siskiyou','Tehama')){
  rows.for.gate.map[[juris]] <- list()
  rows.for.gate.map[[juris]][['from']] <- c()
  rows.for.gate.map[[juris]][['to']] <- c()
  if(juris=='Siskiyou'){
    pt.tazs.in.juris <- c(sis.ids,sis.gates)
  }else{
    pt.tazs.in.juris <- c(teh.ids,teh.gates)
  }
  rows.for.gate.map[[juris]][['pt.to.pt']] <- which(distribed$from %in% pt.tazs.in.juris & distribed$to %in% pt.tazs.in.juris)
  for(id in gates.to.intern.ids){
    shatsa.tazs.of.interest <- gates.to.intern[[as.character(id)]][[juris]]
    rows.for.gate.map[[juris]][['from']] <- c(rows.for.gate.map[[juris]][['from']],which(distribed$from %in% shatsa.tazs.of.interest & distribed$to %in% pt.tazs.in.juris))
    rows.for.gate.map[[juris]][['to']] <- c(rows.for.gate.map[[juris]][['to']],which(distribed$to %in% shatsa.tazs.of.interest & distribed$from %in% pt.tazs.in.juris))
  }
}
non.zero.k.rows.base <- which(!(distribed$from %in% c(sha.ids,sha.gates) & distribed$to %in% c(sha.ids,sha.gates)))

# this function assumes key is set on distribed to 'purp','from','to'
gravity <- function(k){
  distribed$k[non.zero.k.rows] <- k
  distribed[,':='(trips=ifelse(a==0 | k==0,0,p*a*fric*k/sum(a*fric*k))),by=c('purp','from')]
  # implement the constraints in the form of an objective that penalizes deviation from the constraints
  # we want the Shasta trips to/from the appropriate gateways to be consistent with the SRTA OD data, 
  # so we penalize for all deviations from productions and attractions from/to TAZs from 100-199.
  #sum(distribed[rows.for.gate.map[[juris]][['from']],list(from.sr=(sum(trips)-p[1])^2),by=c('purp','from')]$from.sr) + 
          sum(distribed[rows.for.gate.map[[juris]][['to']],list(to.sr=(sum(trips)-a[1])^2),by=c('purp','to')]$to.sr)
}

od.agg.all <- subset(od.agg,(from > 100 | from %in% all.gates) & (to > 100 | to %in% all.gates))
for(juris in c('Siskiyou','Tehama')){
  pa <- streval(pp('subset(new.pa,juris=="',juris,'")'))
  setkey(pa,"purp",'taz')

  if(juris=='Tehama'){
    pt.tazs.in.other.juris <- c(sis.ids,sis.gates)
  }else{
    pt.tazs.in.other.juris <- c(teh.ids,teh.gates)
  }
  non.zero.k.rows <- non.zero.k.rows.base[-which(distribed$from[non.zero.k.rows.base] %in% pt.tazs.in.other.juris | distribed$to[non.zero.k.rows.base] %in% pt.tazs.in.other.juris)]
  # for testing:
  #non.zero.k.rows <- head(non.zero.k.rows,20)

  distribed$pf <- pp(distribed$purp,'-',distribed$from)
  distribed$pt <- pp(distribed$purp,'-',distribed$to)
  pa$pt <- pp(pa$purp,'-',pa$taz)
  distribed$p <- pa$trips[match(distribed$pf,pa$pt)]
  distribed[,p:=ifelse(is.na(p),0,p)]
  distribed$a <- pa$trips[match(distribed$pt,pa$pt)]
  distribed[,a:=ifelse(is.na(a),0,a)]
  distribed$k <- 0
  setkey(distribed,'purp','from','to')
  #distribed.bak <- distribed
  k <- rep(1,length(non.zero.k.rows))
  distribed$k[non.zero.k.rows] <- k

  for(i in 1:50){
    setkey(distribed,'purp','from','to')
    distribed[,':='(trips=ifelse(a==0 | k==0,0,p*a*fric*k/sum(a*fric*k))),by=c('purp','from')]
    setkey(distribed,'purp','to')
    if('k.ratio' %in% names(distribed))distribed[,k.ratio:=NULL]
    setkey(distribed,'purp','from','to')
    distribed.to.change <- data.table(distribed[rows.for.gate.map[[juris]][['to']],list(k.ratio=ifelse(sum(trips)==0,1,a[1]/sum(trips))),by=c('purp','to')],key=c('purp','to'))[data.table(distribed[rows.for.gate.map[[juris]][['to']]],key=c('purp','to'))]
    #distribed[,k:=k*k.ratio]
    distribed.to.change[,k:=k*k.ratio]
    setkey(distribed.to.change,'pft')
    setkey(distribed,'pft')
    new.ks <- distribed.to.change$k[match(distribed$pft,distribed.to.change$pft)]
    distribed$k <- ifelse(is.na(new.ks),distribed$k,new.ks)

    setkey(distribed,'purp','from','to')
    obj <- gravity(distribed$k[non.zero.k.rows])
    print(pp('iter ',i,': ',obj))
    if(obj<1e-4)break
  }
  od.agg.all <- merge(od.agg.all,cast(melt(distribed[c(rows.for.gate.map[[juris]][['pt.to.pt']],rows.for.gate.map[[juris]][['to']],rows.for.gate.map[[juris]][['from']]),list(purp,from,to,trips)],id.vars=c('from','to','purp'),measure.vars=c('trips')),from + to ~ purp),by=c('from','to'),all=T)
}

# merge results to od.agg
for(purp in purps){
  streval(pp('od.agg.all[,',purp,':=ifelse(is.na(',purp,'),ifelse(is.na(',purp,'.x),',purp,'.y,',purp,'.x),',purp,')]'))
  streval(pp('od.agg.all[,',purp,'.x:=NULL]'))
  streval(pp('od.agg.all[,',purp,'.y:=NULL]'))
}
od.agg.all[,tot:=ho+hs+hsc+hw+oo+wo]

# add in missing rows and set their values to 0
from.to <- data.table(expand.grid(from=unique(od.agg.all$from),to=unique(od.agg.all$from)),key=c('from','to'))
od.agg.all <- merge(od.agg.all,from.to,all=T)
for(purp in c(purps,'com')){
  streval(pp('od.agg.all[,',purp,':=ifelse(is.na(',purp,'),0,',purp,')]'))
  streval(pp('od.agg.all[,',purp,'.am:=ifelse(is.na(',purp,'.am),0,',purp,'.am)]'))
  streval(pp('od.agg.all[,',purp,'.pm:=ifelse(is.na(',purp,'.pm),0,',purp,'.pm)]'))
}
od.agg.all[,tot:=ho+hs+hsc+hw+oo+wo]


# Check out the diff between the system trips and internal screenlines 
juris <- 'Siskiyou'
if(juris=='Siskiyou'){
  pt.tazs.in.juris <- c(sis.ids,sis.gates)
}else{
  pt.tazs.in.juris <- c(teh.ids,teh.gates)
}
vols.intern <- head(unlist(lapply(gates.to.intern,function(l){
  sum(od.agg.all[(from%in%pt.tazs.in.juris & to%in%l[[juris]]) | (to%in%pt.tazs.in.juris & from%in%l[[juris]]),tot])
})),2)
juris <- 'Tehama'
if(juris=='Siskiyou'){
  pt.tazs.in.juris <- c(sis.ids,sis.gates)
}else{
  pt.tazs.in.juris <- c(teh.ids,teh.gates)
}
vols.intern <- c(vols.intern,tail(unlist(lapply(gates.to.intern,function(l){
  sum(od.agg.all[(from%in%pt.tazs.in.juris & to%in%l[[juris]]) | (to%in%pt.tazs.in.juris & from%in%l[[juris]]),tot])
})),1))
names(vols.intern) <- c('5-SIS','89','5-TEH')

# looking at 5, don't forget to multiply counts by 2 to make the estimate bi-directional
delta.SIS <- subset(cnts.avg,County=="SIS" & Route==5 & Postmile==0)$avg*2 - vols.intern['5-SIS']
delta.TEH <- subset(cnts.avg,County=="TEH" & Route==5 & Postmile>42)$avg*2 - vols.intern['5-TEH']

# We're now going to distribute these missing trips into XI, XX, and SHA-TEH travel, we need to define the gateways 
# for the whole region to do this.  Gateways will be geographically associated with the nearest TAZ and the IDs will be 
# the negative of that TAZ
agg.gates <- -c(112,205,213,215,207)

load(pp(pevi.shared,'data/UPSTATE/driving-distances/time.distance.Rdata'))
time.distance$from <- agg.taz$agg.id[match(time.distance$orig,agg.taz$name)] 
time.distance$to <- agg.taz$agg.id[match(time.distance$dest,agg.taz$name)] 
for(gate in agg.gates){
  td.sub <- subset(time.distance,from==-gate | to==-gate)
  td.sub$from[td.sub$from==-gate] <- gate
  td.sub$to[td.sub$to==-gate] <- gate
  time.distance <- rbind(time.distance,td.sub)
}


# Find the minimum travel distance we're dealing with and use that to subset the CHTS data to only consider trips longer
time.dist.rows <- which(time.distance$from %in% sis.gates & time.distance$to %in% c(sha.ids,teh.ids) )
min.dist <- min(time.distance[time.dist.rows,]$miles)
x.den <- density(subset(nssr.place,tripdistance>min.dist)$tripdur/60)


# Iteratively distribute the delta.SIS and delta.TEH until the gateway productions are met.
for(i in 1:100){
  # Distribute the delta.SIS
  if(i==1){
    setkey(time.distance,'from','to')
    x.trips <- time.distance[data.table(expand.grid(from=sis.gates,to=c(sha.ids,teh.ids,sha.gates,teh.gates),trips=NA),key=c('from','to'))]
    x.trips <- xp[x.trips]
    x.trips$fric <- x.den$y[findInterval(x.trips$hours,x.den$x,all.inside=T)]
    x.trips$fric[which(x.trips$to<0)] <- 1
  }
  x.trips[,trips:=delta.SIS/2*fric*p/sum(fric*p)]

  # Distribute the delta.TEH
  if(i==1){
    setkey(time.distance,'from','to')
    x.trips.teh <- time.distance[data.table(expand.grid(from=teh.gates,to=c(sha.ids,sis.ids,sha.gates,sis.gates),trips=NA),key=c('from','to'))]
    x.trips.teh <- xp[x.trips.teh]
    x.trips.teh$fric <- x.den$y[findInterval(x.trips.teh$hours,x.den$x,all.inside=T)]
    x.trips.teh$fric[which(x.trips.teh$to<0)] <- 1
  }
  x.trips.teh[,trips:=delta.TEH/2*fric/sum(fric)]

  for(gate in c(-205,-207,-213,-215)){
    resid.ratio <- xp[J(gate)]$p/(x.trips.teh[from==gate | to==gate,sum(trips)] + x.trips[from==gate | to==gate,sum(trips)])
    x.trips$fric[x.trips$to==gate] <- x.trips$fric[x.trips$to==gate] * resid.ratio
    x.trips.teh$fric[x.trips.teh$to==gate] <- x.trips.teh$fric[x.trips.teh$to==gate] * resid.ratio
    print(pp(gate,': ',resid.ratio))
  }
}





