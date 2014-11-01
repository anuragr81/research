processWindow <- function(){
  
}

risk_neutral_probability <-function(callprice_vec,
                                    strike_vec,
                                    S_0,
                                    divyield,
                                    r,
                                    n){
  
  if(length(callprice_vec)<=1){
    print("ignoring vector");
    return(NULL);
  }
  p=array();
  rn<-(1+r)**n;
  divmul<-1/(1+divyield)**n;
  p[1]=2*(1-rn*(S_0*divmul-callprice_vec[1])/strike_vec[1]);
  sum_until_i=p[1];
  for (i in seq(2,length(callprice_vec))){
    cdiff=rn*(callprice_vec[i-1]-callprice_vec[i])*
      (1/(strike_vec[i]-strike_vec[i-1]));
    p[i]=2*(1-sum_until_i-cdiff);
    sum_until_i=sum_until_i+p[i];
  }
  p[i+1]=1-sum_until_i;
  K_next=strike_vec[i]+2*rn*callprice_vec[i]/p[i+1];
  return(data.frame(rnp=p,strike_vec=c(strike_vec,K_next)));
}

test <- function(snpopt) {
  
  require(zoo);
  require(plot3D);
  par(mfrow=c(1,1));
  vix=read.csv("vix.csv");
  vix=data.frame(vix=vix$vix,date=strptime(vix$Date,"%d%b%Y"));
  #plot(vix$date,vix$vix,type='l',xlab='Time',ylab='VIX');
  spx=read.csv("spx.csv");
  spx=data.frame(spx=spx$spindx,date=strptime(spx$caldt,"%Y%m%d"));
  
  sz=20;
  
  #plot(spx$date[sz:length(spx$date)],rollapply(spx$spx,20,sd),type='l',
  #     xlab='Time',ylab='SPX Historical Vol');
  opt=split(snpopt,snpopt$cp_flag);
  copt=opt[["C"]];
  copt_atdate=split(copt,copt$date);
  last_time=strptime("19700101","%Y%m%d");
  for (i in seq(length(copt_atdate))){
    copt_atdate[[i]]$date<-strptime(copt_atdate[[i]]$date,"%Y%m%d");  
    curDate=copt_atdate[[i]]$date[1];
    S0=spx[spx$date==curDate,]$spx;
    #if (as.double(curDate-last_time,units='days')>40){
    #  scatter3D(main=curDate,xlab="K",
    #            ylab="T",
    #            zlab="ImplVol",
    #            copt_atdate[[i]]$strike_price/1e+3, as.integer(copt_atdate[[i]]$exdate), 
    #            copt_atdate[[i]]$impl_volatility, phi =20,
    #            theta=58, bty = "f",type='h',
    #            col = gg.col(100), xlim=c(1,3000),zlim=c(0,1.5),
    #            pch = 18, cex = 2, 
    #            ticktype = "detailed")
    #      Sys.sleep(.2);
    #  last_time=curDate;
    #}
    
    if (length(S0)>0){
      c_atexdate=split(copt_atdate[[i]],copt_atdate[[i]]$exdate);
      arr=c(0,0,0)
      p=matrix(arr,1,3);
      
      for (j in seq(length(c_atexdate))) {  
        c_atexdate[[j]]$exdate=strptime(c_atexdate[[j]]$exdate,"%Y%m%d");
        c_atexdate[[j]]=c_atexdate[[j]][with(
          c_atexdate[[j]],
          order(c_atexdate[[j]]$strike_price)),]; #ordering by strike_price
        
        ndays=as.numeric(c_atexdate[[j]]$exdate[1]-curDate,units="days");
        cpvec=(c_atexdate[[j]]$best_offer+c_atexdate[[j]]$best_bid)/2;
        strike_vec=1e-3*c_atexdate[[j]]$strike_price
        rnp=risk_neutral_probability(callprice_vec=cpvec,
                                     strike_vec=strike_vec,
                                     divyield=0,
                                     r=.02,
                                     n=ndays/250,
                                     S_0=S0);
        dx=rep(ndays,length(rnp$strike_vec));
        for (i in seq(length(length(rnp$strike_vec)))){
          p=rbind(p,c(dx[i],rnp$strike_vec[i],rnp$rnp[i]));
        }
        #print(rep(ndays,length(rnp$strike_vec)));
        breaks=seq(-1.5,1.5,.1);        
        
        #        print(hist(rnp,breaks=breaks)$counts);
        #print(paste("size(rnp)=",length(rnp)));
        
      }
      scatter3D(main=curDate,xlab="T",
                ylab="K",
                zlab="p",
                p[,1],p[,2] , 
                p[,3], phi =20,
                theta=58, bty = "f",type='p',
                col = gg.col(100), xlim=c(1,3000),zlim=c(-2,2),
                pch = 18, cex = 2, 
                ticktype = "detailed");
      
#      ch=readline();
#      if (ch=="1"){
#        return(1);
#      }
    } else {
      print(paste("Ignoring Date:",curDate));
    }
  }
  
}
