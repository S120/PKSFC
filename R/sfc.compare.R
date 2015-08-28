sfc.compare<-function(data,name,greyScale=F,ltyScale=F,legendPos="topleft",legends,ylab="",xlab="",lwd=2,...){
  #First build the matrix
  nbScen<-length(data)
  baseline<-data$baseline[,name]
  time<-rownames(data$baseline)
  res<-as.data.frame(matrix(NA,ncol=(nbScen-1), nrow=length(time)))
  for(i in 2:nbScen){
    scen<-data[[i]]
    res[,(i-1)]<-scen[,name]/baseline
  }
  if(greyScale){
    cols<-paste("grey",seq(0,100,round(100/(nbScen-1),digit=0)))
    ltys<-1
  }else if(ltyScale){
    cols<-1
    ltys<-1:(nbScen-1)
  }else{
    cols<-1:(nbScen-1)
    ltys<-1
  }
  matplot(time,res,type="l",col=cols,lty=ltys,lwd=lwd,ylab=ylab,xlab=xlab,...)
  legend(legendPos,bty='n',col=cols,lty=ltys,legend=legends,lwd=lwd)  
}