
c <- read.table(pp(pevi.shared,'data/inputs/charger-input-file/delhi/chargers-lots.txt'),header=T)
names(c) <- c(';TAZ',tail(names(c),-1))
c[,6] <- 0 # exclude battery swapping
c[c[,1]<0,3:6] <- 0 # externals should be 0

for(n in c(5,10,20,50,100,1000)){
  c[c[,1]>0,3:5] <- n
  write.table(c,file=pp(pevi.shared,'data/inputs/charger-input-file/delhi/chargers-lots-',n,'.txt'),quote=F,row.names=F,sep='\t')
}

all.res <- data.frame()
load(pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/logs-no-homeless.Rdata'))
logs[['results']]$scen <- 'no-homeless'
all.res <- rbind(all.res,logs[['results']])
load(pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/logs-homeless.Rdata'))
logs[['results']]$scen <- 'homeless'
all.res <- rbind(all.res,logs[['results']])
load(pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/logs-opp-cost-high.Rdata'))
logs[['results']]$scen <- 'opp-cost'
all.res <- rbind(all.res,logs[['results']])
all.res$vehicle.type.input.file <- NA
all.res$vehicle.scenario <- NA
all.res$vehicle.scenario.named <- NA
all.res$vehicle.scenario.order <- NA
load(pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/logs-veh-scens.Rdata'))
logs[['results']]$scen <- logs[['results']]$vehicle.scenario.named
logs[['results']]$scen[logs[['results']]$scen=='Low/Med/High'] <- 'base'
all.res <- rbind(all.res,logs[['results']])

baseline.delay <- ddply(all.res,.(penetration,scen),function(df){
  data.frame(delay.cost=mean(df$total.delay.cost),min.delay.cost=min(df$total.delay.cost),total.delay=mean(df$total.delay),num.stranded=mean(df$num.stranded))
})
save(baseline.delay,file=pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/mean-delay-and-strand.Rdata'))

ggplot(baseline.delay,aes(x=scen,y=delay.cost)) + geom_bar(stat='identity') + facet_wrap(~penetration) + labs(x="",y="",title="")

# make plots of pain for base scenario
load(pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/logs-delay-final-infrastructure.Rdata'))
load(pp(pevi.shared,'data/inputs/compare/delhi-baseline-pain/mean-delay.Rdata'))
logs[['results']]$infrastructure.scenario <-unlist(lapply(str_split(logs[['results']]$charger.input.file,'.txt'),function(x){ tail(str_split(x,"-")[[1]],1)}))
res <- subset(logs[['results']],as.character(penetration)==infrastructure.scenario | infrastructure.scenario=="external")
res$total.delay <- res$total.delay - baseline.delay$total.delay[match(res$penetration,baseline.delay$penetration)]
res$num.stranded <- res$num.stranded - baseline.delay$num.stranded[match(res$penetration,baseline.delay$penetration)]
res <- ddply(res,.(penetration,infrastructure.scenario),function(df){
  data.frame(delay=mean(df$total.delay/df$num.drivers),strands=mean(df$num.stranded/df$num.drivers))
})
res$strands[res$strands<0] <- 0
res$delay[res$delay<0] <- 0
res$scen <- revalue(factor(res$infrastructure.scenario),c('0.5'='With EVSE','1'='With EVSE','2'='With EVSE','external'='Without EVSE'))
res$penetration <- revalue(factor(res$penetration),c('0.5'='0.5%','1'='1%','2'='2%'))

my.purp <- '#984ea3'
my.oran <- '#ff7f00'
my.red <- '#e41a1c'
my.blue <- '#377eb8'
my.green <- '#4daf4a'
charger.cols <- c(my.green,my.blue,my.purp,my.oran,my.red)
p <- ggplot(res,aes(x=factor(penetration),y=delay,fill=scen)) + geom_bar(stat='identity',position='dodge') + labs(x="Fleet Penetration",y="Average Daily Hours of Delay per Driver",title="Delay",fill="")+scale_fill_manual(values=charger.cols) # +theme(axis.text.x = element_text(colour='black',size=14),axis.text.y = element_text(colour='black',size=14),plot.title = element_text(size=16))
ggsave(file=pp(pevi.home,'../plots/delhi-analysis/base/base-mean-delay.pdf'),p,width=6,height=6)
p <- ggplot(res,aes(x=factor(penetration),y=strands,fill=scen)) + geom_bar(stat='identity',position='dodge') + labs(x="Fleet Penetration",y="Average Daily Incidence of Strandings per Driver",title="Strandings",fill="")+scale_fill_manual(values=charger.cols)
ggsave(file=pp(pevi.home,'../plots/delhi-analysis/base/base-mean-standings.pdf'),p,width=6,height=6)
p <- ggplot(res,aes(x=scen,fill=factor(penetration),y=delay)) + geom_bar(stat='identity',position='dodge') + labs(x="Fleet Penetration",y="Average Daily Hours of Delay per Driver",title="Delay",fill="")+scale_fill_manual(values=charger.cols) # +theme(axis.text.x = element_text(colour='black',size=14),axis.text.y = element_text(colour='black',size=14),plot.title = element_text(size=16))
ggsave(file=pp(pevi.home,'../plots/delhi-analysis/base/base-mean-delay.pdf'),p,width=6,height=6)
p <- ggplot(res,aes(x=scen,fill=factor(penetration),y=strands)) + geom_bar(stat='identity',position='dodge') + labs(x="Fleet Penetration",y="Average Daily Incidence of Strandings per Driver",title="Strandings",fill="")+scale_fill_manual(values=charger.cols)
ggsave(file=pp(pevi.home,'../plots/delhi-analysis/base/base-mean-standings.pdf'),p,width=6,height=6)

