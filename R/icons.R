library(colinmisc)
library('png')

path.to.google <- '~/Dropbox/serc/pev-colin/data/google-earth/'
make.dir(paste(path.to.google,'icons',sep=''))

img <- readPNG(paste(path.to.google,'icons/charging-icon.png',sep=''))

for(l2 in 0:30){
  for(l3 in 0:20){
    png(paste(path.to.google,"icons/charger-icon-",l2,"-",l3,".png",sep=''), bg="transparent", width=323, height=386)
    par(mar=c(0,0,0,0))
    plot(1:2,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    lim <- par()
    rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    text(1.5,1.7,paste(l2," / ",l3,sep=''),pos=3,cex=8,font=2,family="Impact",col='white')
    dev.off()
  }
}

img <- readPNG(paste(path.to.google,'icons/EV-car-icon.png',sep=''))

ev.count <- 1

  while(ev.count<=200){
    png(paste(path.to.google,"icons/ev-icon-",ev.count,".png",sep=''), bg="transparent", width=440, height=350)
    par(mar=c(0,0,0,0))
    plot(1:2,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    lim <- par()
    rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    text(1.5,1.8,paste(ev.count,sep=''),pos=3,cex=7,font=2,family="Impact",col='white')
    dev.off()
    ev.count = ev.count + 1
  }
