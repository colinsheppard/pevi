
c <- read.table(pp(pevi.shared,'data/inputs/charger-input-file/delhi/chargers-lots.txt'),header=T)
names(c) <- c(';TAZ',tail(names(c),-1))
c[,6] <- 0 # exclude battery swapping
c[c[,1]<0,3:6] <- 0 # externals should be 0

for(n in c(5,10,20,50,100,1000)){
  c[c[,1]>0,3:5] <- n
  write.table(c,file=pp(pevi.shared,'data/inputs/charger-input-file/delhi/chargers-lots-',n,'.txt'),quote=F,row.names=F,sep='\t')
}

#load(pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/logs.Rdata'))

logs[['results']]$infrastructure.scenario <- as.numeric(unlist(lapply(str_split(logs[['results']]$charger.input.file,'.txt'),function(x){ tail(str_split(x,"-")[[1]],1)})))

ggplot(logs[['results']],aes(x=factor(penetration),y=total.delay.cost/1e6))+geom_point()+facet_wrap(~infrastructure.scenario)


baseline.delay <- ddply(subset(logs[['results']],infrastructure.scenario==1000),.(penetration),function(df){
  data.frame(delay.cost=mean(df$total.delay.cost),min.delay.cost=min(df$total.delay.cost))
})

save(baseline.delay,file=pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/mean-delay.Rdata'))


# for veh scens

logs[['results']]$vehicle.scenario <- unlist(lapply(str_split(logs[['results']]$vehicle.type.input.file,'.txt'),function(x){ tail(str_split(x,"-")[[1]],1)}))
logs[['results']]$infrastructure.scenario <- as.numeric(unlist(lapply(str_split(logs[['results']]$charger.input.file,'.txt'),function(x){ tail(str_split(x,"-")[[1]],1)})))
baseline.delay.veh <- ddply(subset(logs[['results']],infrastructure.scenario==1000),.(penetration,vehicle.scenario),function(df){
  data.frame(delay.cost=mean(df$total.delay.cost),min.delay.cost=min(df$total.delay.cost))
})
save(baseline.delay.veh,file=pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/mean-delay-veh-scens.Rdata'))

# for opp cost high

baseline.delay.opp.cost <- ddply(logs[['results']],.(penetration),function(df){
  data.frame(delay.cost=mean(df$total.delay.cost),min.delay.cost=min(df$total.delay.cost))
})

save(baseline.delay.opp.cost,file=pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/mean-delay-opp-cost.Rdata'))
