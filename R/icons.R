library('png')

path.to.google <- '~/Dropbox/serc/pev-colin/pev-shared/data/google-earth/'
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

img <- readPNG(paste(path.to.google,'icons/charging-icon.png',sep=''))
img.existing <- readPNG(paste(path.to.google,'icons/charging-icon-existing.png',sep=''))
img.l3 <- readPNG(paste(path.to.google,'icons/charging-icon-l3.png',sep=''))
img.l3.existing <- readPNG(paste(path.to.google,'icons/charging-icon-existing-l3.png',sep=''))
outline.thickness <- 0.04
x.vary <- sin(seq(0,2*pi,by=pi/8))
y.vary <- cos(seq(0,2*pi,by=pi/8))
for(l2 in 0:100){
  png(paste(path.to.google,"icons/charger-icon-single/charger-icon-",l2,".png",sep=''), bg="transparent", width=323, height=386)
  par(mar=c(0,0,0,0))
  plot(1:2,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
  lim <- par()
  rasterImage(img, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  text(1.5+outline.thickness*x.vary,1.7+outline.thickness*y.vary,l2,pos=3,cex=8,font=2,family="Impact",col='#00529c')
  text(1.5,1.7,l2,pos=3,cex=8,font=2,family="Impact",col='white')
  dev.off()
  png(paste(path.to.google,"icons/charger-icon-single/charger-icon-",l2,"-existing.png",sep=''), bg="transparent", width=323, height=386)
  par(mar=c(0,0,0,0))
  plot(1:2,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
  lim <- par()
  rasterImage(img.existing, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  text(1.5+outline.thickness*x.vary,1.7+outline.thickness*y.vary,l2,pos=3,cex=8,font=2,family="Impact",col='#1ea74b')
  text(1.5,1.7,l2,pos=3,cex=8,font=2,family="Impact",col='white')
  dev.off()
}
for(l3 in 0:10){
  png(paste(path.to.google,"icons/charger-icon-single/charger-icon-l3-",l3,".png",sep=''), bg="transparent", width=323, height=386)
  par(mar=c(0,0,0,0))
  plot(1:2,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
  lim <- par()
  rasterImage(img.l3, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  text(1.5+outline.thickness*x.vary,1.7+outline.thickness*y.vary,l3,pos=3,cex=8,font=2,family="Impact",col='#00529c')
  text(1.5,1.7,l3,pos=3,cex=8,font=2,family="Impact",col='white')
  dev.off()
  png(paste(path.to.google,"icons/charger-icon-single/charger-icon-l3-",l3,"-existing.png",sep=''), bg="transparent", width=323, height=386)
  par(mar=c(0,0,0,0))
  plot(1:2,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
  lim <- par()
  rasterImage(img.l3.existing, lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
  text(1.5+outline.thickness*x.vary,1.7+outline.thickness*y.vary,l3,pos=3,cex=8,font=2,family="Impact",col='#1ea74b')
  text(1.5,1.7,l3,pos=3,cex=8,font=2,family="Impact",col='white')
  dev.off()
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
