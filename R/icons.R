library('png')

path.to.google <- pp(pevi.shared,'data/google-earth/')
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


### DELHI icons are now arranged with some heirarchy since we need so many
path.to.icons <- pp(pevi.shared,'data/google-earth/icons/delhi/')
img <- list()
img[[1]] <- readPNG(paste(path.to.icons,'charging-icon-l1.png',sep=''))
img[[2]] <- readPNG(paste(path.to.icons,'charging-icon-l2.png',sep=''))
img[[3]] <- readPNG(paste(path.to.icons,'charging-icon-l3.png',sep=''))
img[[4]] <- readPNG(paste(path.to.icons,'battery-swap-icon.png',sep=''))

outline.thickness <- 0.04
x.vary <- sin(seq(0,2*pi,by=pi/8))
y.vary <- cos(seq(0,2*pi,by=pi/8))
make.dir(pp(path.to.icons,'l1'))
for(l1 in (0:3)*1e3){
  make.dir(pp(path.to.icons,'l1/',l1)) 
  for(l1.sub in (0:9)*1e2){
    make.dir(pp(path.to.icons,'l1/',l1,'/',l1+l1.sub)) 
  }
}
make.dir(pp(path.to.icons,'l2'))
for(l2 in 0*1e3){
  make.dir(pp(path.to.icons,'l2/',l2)) 
  for(l2.sub in (0:4)*1e2){
    make.dir(pp(path.to.icons,'l2/',l2,'/',l2+l2.sub)) 
  }
}
make.dir(pp(path.to.icons,'l3'))
for(l3 in 0*1e3){
  make.dir(pp(path.to.icons,'l3/',l3)) 
  for(l3.sub in 0*1e2){
    make.dir(pp(path.to.icons,'l3/',l3,'/',l3+l3.sub)) 
  }
}
make.dir(pp(path.to.icons,'l4'))
for(l4 in 0*1e3){
  make.dir(pp(path.to.icons,'l4/',l4)) 
  for(l4.sub in 0*1e2){
    make.dir(pp(path.to.icons,'l4/',l4,'/',l4+l4.sub)) 
  }
}

for(lev in 1:4){
  for(num in 0:3000){
    if((lev == 1 & num < 1000) | (lev == 2 & num > 499) | (lev == 3 & num > 99) | (lev==4 & num > 49))next
    thou.dir <- floor(num/1e3)*1e3
    hund.dir <- thou.dir + floor((num - thou.dir)/1e2)*1e2
    ico.file <- pp(path.to.icons,'l',lev,'/',thou.dir,'/',hund.dir,'/',num,'.png')
    png(ico.file,bg="transparent", width=323, height=386)
    par(mar=c(0,0,0,0))
    plot(1:2,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='')
    lim <- par()
    rasterImage(img[[lev]], lim$usr[1], lim$usr[3], lim$usr[2], lim$usr[4])
    text(1.5+outline.thickness*x.vary,1.7+outline.thickness*y.vary,num,pos=3,cex=8,font=2,family="Impact",col='#00529c')
    text(1.5,1.7,num,pos=3,cex=8,font=2,family="Impact",col='white')
    dev.off()
  }
}
