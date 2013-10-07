#!/usr/bin/Rscript --no-save --no-restore
##############################################################################################################################################
# Script to pre-process count data from Caltrans
##############################################################################################################################################

##############################################################################################################################################
# LOAD LIBRARIES NEED BY THIS SCRIPT
load.libraries(c('optparse','stringr'),quietly=T)

##############################################################################################################################################
# COMMAND LINE OPTIONS 
option_list <- list(
  make_option(c("-v", "--verbose"), action="store_true", default=F, help="Print extra output"),
  make_option(c("-o", "--outfile"), type="character", default='nolog.out', help="Output file to write the commented code [\"%default\"]",metavar="out-file")
)
if(interactive()){
  setwd(pp(pevi.home,'netlogo'))
  args<-c('-o','PEVI-nolog.nlogo','PEVI.nlogo')
  args <- parse_args(OptionParser(option_list = option_list,usage = "nolog.R [options] INFILE"),positional_arguments=T,args=args)
}else{
  args <- parse_args(OptionParser(option_list = option_list,usage = "nolog.R [options] INFILE"),positional_arguments=T)
}
opts <- data.frame(args$options,stringsAsFactors=F)
infile <- args$args

exp.name <- "scale-experiment"
path.to.inputs <- pp(pevi.shared,'data/inputs/sensitivity/',exp.name,'/',sep='')

taz <- read.table(pp(path.to.inputs,"/taz-52.txt"),header=T,stringsAsFactors=F)
names(taz) <- c('from','to','miles','time','enroute','perf')
enroutes <- sapply(taz$enroute,function(s){ na.omit(as.numeric(str_split(s,",")[[1]]))})

chargers <- read.table(pp(path.to.inputs,"/chargers-52.txt"),header=T,stringsAsFactors=F)

for(n in c(104,208,416)){
  new.taz <- data.frame(permutations(n,2,repeats.allowed=T))
  names(new.taz) <- c('from','to')
  new.taz$miles <- taz$miles 
  new.taz$time  <- taz$time
  new.taz$enroute <- taz$enroute
  scale.enroute <- n/52
  for(i in 53:(n*n)){
    taz.i <- i%%52+1
    new.taz$enroute[i] <- pp(sample(max(1,(new.taz$from[i] - scale.enroute * length(enroutes[[taz.i]]))):min(n,(new.taz$from[i] + scale.enroute * length(enroutes[[taz.i]]))), scale.enroute * length(enroutes[[taz.i]])),collapse=",")
  }
  # make sure we don't have enroute for intra-TAZ travel
  for(i in 1:n){
    new.taz$enroute[(i-1)*n+i] <- ""
  }
  new.taz$perf  <- taz$perf
  write.table(new.taz,file=pp(path.to.inputs,"/taz-",n,".txt"),row.names=F,sep="\t")
}

for(n in c(104,208,416)){
  new.chargers <- data.frame(taz=1:n)
  new.chargers$L0 <- 1
  new.chargers$L1 <- chargers$L1
  new.chargers$L2 <- chargers$L2
  new.chargers$L3 <- chargers$L3
  names(new.chargers) <- c(";TAZ","L0","L1","L2","L3")

  write.table(new.chargers,file=pp(path.to.inputs,"/chargers-",n,".txt"),row.names=F,sep="\t",quote=F)
}

# analysis of scaling results
prof <- read.table(pp(pevi.shared,"/data/outputs/profiling/scale.txt"),header=T,stringsAsFactors=F)
prof$time <- prof$time/60
prof$drivers <- prof$driver/1000
prof.m <- melt(prof,id.vars=c("tazs","drivers","run"),measure.vars=c("mem","time"))

# plot all at once
ggplot(subset(prof.m,variable=="time"),aes(x=drivers,y=value,colour=run))+geom_point()+facet_wrap(~tazs)
ggplot(subset(prof.m,variable=="time"),aes(x=tazs,y=value,colour=run))+geom_point()+facet_wrap(~drivers)

# Reults
first.run <- 0.1925   # minutes per 1k drivers
subs.runs <- 0.022059 # minutes per 1k drivers

mem.drivers <- 2.6738 # mb per 1k drivers
mem.tazs    <- 1.8853 # mb per taz

evse.rate <- 9 # num EVSE per 1k drivers in Humboldt results
evse.per.unit <- 5 # the number of EVSE's considered each iteration of the optimization

reps.agg <- 10 # at the highest pen (most aggregated) how many replicates, for 50% pen, double reps assumed

# According to Delhi travel demand report, 28.7mm daily trips occur, 27% of which are POV at a trip rate of 1.25 trips/capita

tot.veh <- 28.7e6 * 0.27 / 1.25
pens <- c(0.01,0.02,0.04)
n.tazs <- c(100,150,200,223) # current model has 223 including external zones

sims <- expand.grid(n.tazs,pens)
names(sims) <- c('tazs','pens')
sims$drivers <- sims$pens * tot.veh / 1000
sims$tot.evse <- sims$drivers * evse.rate
sims$tot.evse.units <- sims$tot.evse / evse.per.unit
sims$evse.units.2 <- sims$tot.evse.units * 0.25
sims$evse.units.1 <- sims$tot.evse.units * 0.5

sims$time1 <- sims$drivers/4 * (first.run + subs.runs * (sims$tazs-1) * reps.agg * 4 * sims$evse.units.1 ) / 60 
sims$time2 <- sims$drivers/2 * (first.run + subs.runs * (sims$tazs-1) * reps.agg * 2 * sims$evse.units.2) / 60
sims$time3 <- sims$drivers * (first.run + subs.runs * (sims$tazs-1) * reps.agg * sims$evse.units.2) / 60
sims$tot.time <- sims$time1 + sims$time2 + sims$time3

sims$max.mem <- (sims$drivers * mem.drivers + sims$tazs * mem.tazs)/1000




summary(lm('value ~ drivers',subset(prof.m,variable=="time" & run=="BatchRun")))

#Call:
#lm(formula = "value ~ drivers", data = subset(prof.m, variable == 
    #"time" & run == "BatchRun"))

#Residuals:
     #Min       1Q   Median       3Q      Max 
#-2.20622 -0.11797  0.05541  0.49784  1.33604 

#Coefficients:
             #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -0.594773   0.327648  -1.815   0.0995 .  
#drivers      0.022059   0.002416   9.131 3.63e-06 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.9226 on 10 degrees of freedom
#Multiple R-squared:  0.8929,	Adjusted R-squared:  0.8822 
#F-statistic: 83.37 on 1 and 10 DF,  p-value: 3.633e-06

summary(lm('value ~ drivers',subset(prof.m,variable=="time" & run=="Setup&Run")))

#Call:
#lm(formula = "value ~ drivers", data = subset(prof.m, variable == 
    #"time" & run == "Setup&Run"))

#Residuals:
     #Min       1Q   Median       3Q      Max 
#-12.6710  -2.8222  -0.1466   4.5974   9.0623 

#Coefficients:
            #Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  -5.6399     2.3055  -2.446   0.0345 *  
#drivers       0.1925     0.0170  11.327 5.02e-07 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 6.492 on 10 degrees of freedom
#Multiple R-squared:  0.9277,	Adjusted R-squared:  0.9205 
#F-statistic: 128.3 on 1 and 10 DF,  p-value: 5.017e-07

summary(lm('value ~ drivers + tazs',subset(prof.m,variable=="mem" & run=="BatchRun")))

#Call:
#lm(formula = "value ~ drivers + tazs", data = subset(prof.m, 
    #variable == "mem" & run == "BatchRun"))

#Residuals:
    #Min      1Q  Median      3Q     Max 
#-188.81 -103.69   15.19   70.82  206.59 

#Coefficients:
            #Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 150.6242    72.7979   2.069 0.068467 .  
#drivers       2.6738     0.3706   7.214    5e-05 ***
#tazs          1.8853     0.3088   6.105 0.000178 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 133.4 on 9 degrees of freedom
#Multiple R-squared:  0.8823,	Adjusted R-squared:  0.8561 
#F-statistic: 33.72 on 2 and 9 DF,  p-value: 6.592e-05

# find the 20 most expensive procedures in the highest scale-out case
top.20 <- as.character(subset(prof.melted,Drivers==384000 & TAZs==52)$variable[order(subset(prof.melted,Drivers==384000 & TAZs==52)$value)][1:20])

# plot just top 20 
ggplot(subset(prof.melted,variable %in% top.20),aes(x=Drivers,y=value/1000,colour=variable))+geom_line()+facet_wrap(~TAZs)
