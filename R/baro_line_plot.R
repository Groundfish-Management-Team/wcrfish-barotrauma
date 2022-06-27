rate.table <- read.csv('c:/GMT/Barotrauma/all_rates_for_plot.csv')

for(spec1 in c("Canary","Yelloweye","Cowcod")){
  spec <- tolower(spec1)
  spec2 <- ifelse(spec1=="Cowcod", spec1, paste(spec1,"Rockfish"))

  png(paste('c:/GMT/Barotrauma/Barotrauma_plot_',spec,'.png',sep=""),
      res=300,units='in',width=8,height=6)
  tab  <- rate.table[rate.table$Type=="N5" & rate.table$Species==spec,]
  tab2 <- rate.table[rate.table$Type!="N5" & rate.table$Species==spec,]

  par(mar=c(5,.6,1,.6),oma=c(0,4,3,4),mfrow=c(1,2),cex.main=1)
  plot(0,type='n',xlim=c(0,77),ylim=c(0,1.05),xaxs='i',yaxs='i',axes=FALSE,
       xlab="Depth (fathoms)",ylab="",main="N/5 method")
  abline(h=seq(0,1,.2),lty=3,col='grey')
  axis(1,at=c(0,10,20,30,50,100))
  ## axis(1,at=c(0,10,20,30,50,100),lab=rep("",6))
  ## axis(1,at=c(5,15,25,40,60),lab=c("0-10","10-20","20-30","30-50","50+"),col.ticks="white")
  axis(2,las=1,at=seq(0,1,.2),lab=paste(seq(0,1,.2)*100,"%",sep=""))
  #axis(4,las=1,at=seq(0,1,.2),lab=rep("",6))
  xvec <- c(5,15,25,40,60)
  xvec2 <- c(5,20,40,60)
  colvec <- c(1,rgb(seq(1,.6,length=5),0,0,alpha=seq(.8,.4,length=5)))
  colvec2 <- c(1,rgb(0,0,seq(1,.6,length=5),alpha=seq(.8,.4,length=5)))
  lwdvec <- c(2,seq(3.5,2,length=5))
  quantlabs <- c("Surface","Point\nestimate","60%","75%","90%","95%")
  for(iquant in 1:6){
    col <- iquant+3
    lines(xvec[c(4,5)],tab[c(4,6),col],lwd=lwdvec[iquant],lty=2,
          col=colvec[iquant],type='l',pch=16,cex=.8)
    lines(xvec,tab[1:5,col],lwd=lwdvec[iquant],lty=1,
          col=colvec[iquant],type='o',pch=16,cex=.8)
    text(x=xvec[5], y=tab[5,col],labels=quantlabs[iquant],
         pos=4,col=colvec[iquant],cex=.8)
  }

  plot(0,type='n',xlim=c(0,77),ylim=c(0,1.05),xaxs='i',yaxs='i',axes=FALSE,
       xlab="Depth (fathoms)",ylab="",main="Hierarchical method")
  abline(h=seq(0,1,.2),lty=3,col='grey')
  #axis(1,at=c(0,10,20,30,50,100),lab=rep("",6))
  axis(1,at=c(0,10,20,30,50,100))
  #axis(1,at=c(5,15,25,40,60),lab=c("0-10","10-20","20-30","30-50","50+"),col.ticks="white")
  #axis(2,las=1,at=seq(0,1,.2),lab=rep("",6))
  axis(4,las=1,at=seq(0,1,.2),lab=paste(seq(0,1,.2)*100,"%",sep=""))
  for(iquant in 1:6){
    col <- iquant+3
    lines(xvec[c(4,5)],tab2[c(4,6),col],lwd=lwdvec[iquant],lty=2,
          col=colvec2[iquant],type='l',pch=16,cex=.8)
    lines(xvec,tab2[1:5,col],lwd=lwdvec[iquant],lty=1,
          col=colvec2[iquant],type='o',pch=16,cex=.8)
    adj <- ifelse(iquant==2,-.03,0)
    text(x=xvec[5], y=adj+tab2[5,col],labels=quantlabs[iquant],
         pos=4,col=colvec2[iquant],cex=.8)
  }
  
  mtext(side=3,line=1,outer=TRUE,paste("Total discard mortality estimates for",spec2),font=2,cex=1.2)
  mtext(side=2,line=2.7,outer=TRUE,"Total discard mortality")
  dev.off()

}
