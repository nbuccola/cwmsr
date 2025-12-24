########################################################################
# #make daily range and mean data.frame
########################################################################
make.daily.mean.range<-function(x.hourly){
    x.dm<-x.hourly[1:length(unique(round(x.hourly$JDAY))),]
    x.dm$JDAY<-unique(round(x.hourly$JDAY));x.dm[,-1]<-NA
    x.r<-x.dm
    for(i in 1:nrow(x.dm)){
    x.dm[i,-1]<-apply(x.hourly[round(x.hourly$JDAY)
                            %in%x.dm$JDAY[i],-1],2,mean)
    x.r[i,-1]<-apply(x.hourly[round(x.hourly$JDAY)
                            %in%x.dm$JDAY[i],-1],2,range)
    }
    return(list(x.dm=x.dm,x.r=x.r))
}
########################################################################
# a simple function to plot the model results
########################################################################
plot.mod<-function(mod=NA,input.data,plot.name=NA,fitstats=TRUE,...){
  #library(Hmisc)
  directory<-getwd()
  x.col<-match(rownames(attr(mod$terms,'factors'))[1],colnames(input.data))
  #if(!exists('input.data$JDAY')){
  #    input.data$JDAY<-julian(index(input.data),origin='2005-12-31')
  #    }
  meas.mod<-data.frame(JDAY=input.data$JDAY,
    meas=as.numeric(input.data[,x.col]),
    mod=as.numeric(predict(mod,input.data)))
  colnames(meas.mod)[-1]<-c('meas','mod')
  windows(20,10);
  par(oma=c(0.2,0.5,0.2,1.5))#,mfrow=c(1,2))
  #plot(meas ~ mod,meas.mod);
  #plot(mod ~ JDAY,meas.mod,type='l',col='red') #,ylab='temp,C')
  #lines(meas ~ JDAY,meas.mod)
  plotTSbyJDAY(x.df=meas.mod[,c('JDAY','meas','mod')],
               #ylab="NFMFW Water Temperature, Degrees C",
               xlab='Month',
               type='l',ylim=c(5,25),col=c('black','red'),
               bot=TRUE)#,...)
  er<-signif(errs(meas.mod[,-1]),2)
  xy.r<-diff(par('usr'))[c(1,3)]
  #es<-emptyspace(meas.mod[,1],meas.mod[,2])
  #es<-largest.empty(meas.mod[,1],meas.mod[,2],xy.r[1]/5,xy.r[2]/30)
  if(fitstats){
      for(i in 1:5){#
          text(seq(par('usr')[1]+10,par('usr')[2]-10,
                   length.out=5)[i],y=par('usr')[3]+2,er[1,i])
          text(seq(par('usr')[1]+10,par('usr')[2]-10,
                   length.out=5)[i],y=par('usr')[3]+1,colnames(er)[i])

    #text(seq(es$x+xy.r[1]/10,es$x-xy.r[1]/10,length.out=5)[i],
    #  y=es$y+xy.r[2]/60,er[1,i],cex=0.6)
    #text(seq(es$x+xy.r[1]/10,es$x-xy.r[1]/10,length.out=5)[i],
    #  y=es$y-xy.r[2]/60,colnames(er)[i],cex=0.6)
      }
  }
  legend('topleft',c('meas','mod'),col=c('black','red'),lty=c(1,1))
  print(er)
  if(!is.na(plot.name)){
  #setwd("E:/Users/nbuccola/MFWillamette/boundary_conditions/temp_est")
  savePlot(paste(plot.name,'.png',sep=''),type='png')
  savePlot(paste(plot.name,'.pdf',sep=''),type='pdf')
  }
  setwd(directory)
  }
########################################################################
# a simple function to plot the predicted model results
########################################################################
plot.pred<-function(mod=NA,input.data,plot.name=NA,return.data=FALSE){
  directory<-getwd()
  x.nms<-attr(attr(mod$terms,'factors'),'dimnames')[[2]]
  y.nm<-rownames(attr(mod$terms,'factors'))[1]
  x.cols<-na.omit(match(x.nms,colnames(input.data)))
  meas.mod<-data.frame(JDAY=input.data$JDAY,
    meas=input.data[,x.cols],
    mod=predict(mod,input.data))
  colnames(meas.mod)<-gsub(y.nm,'mod',colnames(meas.mod))
  meas.mod<-meas.mod[!apply(apply(meas.mod,1,is.na),2,any),]
  str(meas.mod)
  windows(20,15)#;par(mfrow=c(1))
  plot(mod ~ JDAY,meas.mod,type='l');  # type='n',  ylab='temp,C',
  #for(i in 1:length(x.nms)){
  #    lines(x=meas.mod$JDAY,y=meas.mod[,1+i],
  #          type='l',col=c('red','violet','blue')[i])
  #    }
  #lines(mod ~ jday,meas.mod,col='gray')
  colnames(meas.mod)<-gsub('mod',paste('mod.',y.nm,sep=''),colnames(meas.mod))
  #legend('topleft',colnames(meas.mod)[sapply(c(y.nm,x.nms),grep,colnames(meas.mod))],
  #       col=c('gray',c('red','violet','blue')[1:length(x.nms)]),
  #       lty=rep(1,ncol(meas.mod)-1))
  if(!is.na(plot.name)){
  #setwd("E:/Users/nbuccola/MFWillamette/boundary_conditions/temp_est")
  savePlot(paste(plot.name,'.png',sep=''),type='png')
  }
  if(return.data)      return(meas.mod)
  setwd(directory)
  }
########################################################################
#reconstruct data using equation from S.Rounds
# where y is the dependent variable and x is the explanatory variable
########################################################################
est.t<-function(dm.mod,#daily mean model
  r.mod,#daily range model
  x.hourly,#hourly data.frame
  x.dm,#daily mean data.frame
  x.r #daily range data.frame
  ){
  y.dm<-predict(dm.mod,x.dm);y.r<-predict(r.mod,x.r)
  x.dm.col<-match(rownames(attr(dm.mod$terms,'factors'))[2],colnames(x.hourly))
  x.r.col<-match(rownames(attr(r.mod$terms,'factors'))[2],colnames(x.hourly))
  i=1;y<-data.frame(JDAY=x.hourly$JDAY);y$mod<-NA
  for(i in 1:nrow(x.hourly)){
    daily.i<-x.dm$JDAY%in%round(x.hourly$JDAY)[i]
    # calculate: y.mean+(x-x.mean)*y.range/x.range
    y[y$JDAY%in%x.hourly$JDAY[i],2]<-
      y.dm[daily.i]+(x.hourly[i,x.dm.col]-x.dm[daily.i,x.dm.col])*
      y.r[daily.i]/x.r[daily.i,x.r.col]
  }
  return(y)
  }
########################################################################
# a simple function to plot the calculated results
########################################################################
plot.meas.mod<-function(input.data,plot.name=NA){ #a data.frame with JDAY, meas, mod
  directory<-getwd()
  colnames(input.data)<-c('JDAY','meas','mod')
  windows(20,10);par(mfrow=c(1,2))
  plot(meas ~ mod,input.data);
  plot(meas ~ JDAY,input.data,type='l',ylab='temp,C')
  lines(mod ~ JDAY,input.data,col='red')
  legend('topright',c('meas','mod'),col=c('black','red'),lty=c(1,1))
  er<-errs(input.data[,-1]);print(er)
  for(i in 1:5){#
    text(seq(par('usr')[1]+10,par('usr')[2]-10,
             length.out=5)[i],y=par('usr')[3]+2,er[1,i])
    text(seq(par('usr')[1]+10,par('usr')[2]-10,
             length.out=5)[i],y=par('usr')[3]+1,
       colnames(er)[i])
  }
  if(!is.na(plot.name)){
  setwd("E:/Users/nbuccola/MFWillamette/boundary_conditions")
  savePlot(paste(plot.name,'.png',sep=''),type='png')
  setwd(directory)
  }
}

##############################################################################
##Compute the Error statistics
##############################################################################
errs<-function(out){ narows<-apply(apply(out,2,is.na),1,any)
#which(!is.na(out[,1]))
NSE = function(sim,obs){
  1 - ( sum( (obs - sim)^2 ) / sum( (obs - mean(obs, na.rm = TRUE))^2 ))
}
rmse = function(sim,obs){
  sqrt( mean( (sim - obs)^2, na.rm = TRUE) )
}
#measured should be first column, modeled in second!
meas<-out[!narows,1];mod<-out[!narows,2];
nonzero<-which(meas>=1e-2|meas<=-1e-2)# Do not include "zero" values from measured data in MAPE calculation
RMSE<-round(rmse(as.numeric(meas),as.numeric(mod)),digits=3)
NS<-round(NSE(as.vector(mod),as.vector(meas)),digits=3)
#NashSutliffe Error:  1-mean((mod-meas)^2)/var(meas)
ME<-round(mean(as.numeric(mod),na.rm=TRUE)-mean(as.numeric(meas),na.rm=TRUE),digits=3)
MAE<-round(mean(abs(as.numeric(meas[nonzero])-
                      as.numeric(mod[nonzero])),na.rm=TRUE),digits=3)
COR<-round(cor(as.vector(mod),as.vector(meas)),digits=3)
SMAPE<-round(mean(abs((as.numeric(meas[nonzero])-as.numeric(mod[nonzero]))/
                        ((as.numeric(meas[nonzero])+as.numeric(mod[nonzero]))/2)),na.rm=TRUE),digits=3)
#ABDEV<-round(mean(abs(as.numeric(mod)-mean(as.numeric(meas),na.rm=TRUE))),digits=3)
return(data.frame(N=length(meas),RMSE=RMSE,NS=NS,ME=ME,MAE=MAE,COR=COR,SMAPE=SMAPE))
}
##############################################################################
##Write w2 input file
##############################################################################
write.w2<-function(x,note,new.npt.filename){ #x is a dataframe with columns jday, temp
    x<-x[!apply(apply(x,1,is.na),2,any),]
    if(x$JDAY[1]>1){
        x<-rbind(x[1,],x)
        x$JDAY[1]<-1
    }
    if(x$JDAY[nrow(x)]<366){
        x<-rbind(x,x[nrow(x),])
        x$JDAY[nrow(x)]<-366
    }
  write(paste('#', note),new.npt.filename)
  write('# all times in PST',new.npt.filename,append=TRUE)
#  write(c(paste('#',deparse(colnames(x)))),new.npt.filename,append=TRUE)
  write.fwf(as.data.frame(t(matrix(substr(colnames(x),0,8),byrow=FALSE,
                                   dimnames=list(colnames(x))))),
            file=new.npt.filename,
            sep="", na="",
            colnames=FALSE, justify="right",formatInfo=FALSE,
            width=8,append=TRUE)
  write.fwf(x, file=new.npt.filename, sep="", na="",
    colnames=FALSE, justify="right",formatInfo=TRUE,  width=8,append=TRUE)
}

##############################################################################
##Write w2 input file in csv format
##############################################################################
write.w2csv<-function(x,note,new.npt.filename){ #x is a dataframe with columns jday, temp
  #x<-x[!apply(apply(x,1,is.na),2,any),]
  if(x$JDAY[1]>1){
    x<-rbind(x[1,],x)
    x$JDAY[1]<-1
  }
  if(x$JDAY[nrow(x)]<366){
    x<-rbind(x,x[nrow(x),])
    x$JDAY[nrow(x)]<-366
  }
  zeroPad <- 1E-8
  
  for(j in 2:ncol(x)){
    if(any(x[,j]>=zeroPad & x[,j] < 1)){
      x[,j] <- sprintf(x[,j],fmt = "%.2e")    
    }
  }
  write(paste('$', note),new.npt.filename)
  write('# all times in PST',new.npt.filename,append=TRUE)
  #  write(c(paste('#',deparse(colnames(x)))),new.npt.filename,append=TRUE)
  write.table(as.data.frame(t(matrix(substr(colnames(x),0,8),byrow=FALSE,
                                   dimnames=list(colnames(x))))),
              file=new.npt.filename,row.names=F,
            sep=",", na="",col.names=FALSE, append=TRUE)
  
  write.table(x, file=new.npt.filename, sep=",", na="",row.names=F,quote=F,
            col.names=FALSE, append=TRUE)
}

