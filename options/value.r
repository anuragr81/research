processWindow <- function(){
  
}

risk_neutral_probability <-function(callprice_vec,
                                    strike_vec,
                                    S_0,
                                    divyield,
                                    r,
                                    n){
  
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
  return(p);
}

test <- function(snpopt) {
  
  require(zoo);
  par(mfrow=c(2,1));
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
  
  for (i in seq(length(copt_atdate))){
    copt_atdate[[i]]$date<-strptime(copt_atdate[[i]]$date,"%Y%m%d");  
    curDate=copt_atdate[[i]]$date[1];
    S0=spx[spx$date==curDate,]$spx;
    if (length(S0)>0){
      print(paste("curDate:",curDate));
      c_atexdate=split(copt_atdate[[i]],copt_atdate[[i]]$exdate);
      for (j in seq(length(c_atexdate))) {
        c_atexdate[[j]]$exdate=strptime(c_atexdate[[j]]$exdate,"%Y%m%d");
        c_atexdate[[j]]=c_atexdate[[j]][with(
          c_atexdate[[j]],
          order(c_atexdate[[j]]$strike_price)),]; #ordering by strike_price
        
        print(paste("Price",S0));
        ndays=as.numeric(c_atexdate[[j]]$exdate[1]-curDate,units="days");
        print(paste("ndays:",ndays));
        cpvec=(c_atexdate[[j]]$best_offer+c_atexdate[[j]]$best_bid)/2;
        plot(c_atexdate[[j]]$strike_price,cpvec,type='l');
        rnp=risk_neutral_probability(callprice_vec=cpvec,
                                     strike_vec=1e-3*c_atexdate[[j]]$strike_price,
                                     divyield=0,
                                     r=.02,
                                     n=ndays/250,
                                     S_0=S0);
        hist(rnp);
        print(paste("size(rnp)=",length(rnp)));
        strike=1e-3*c_atexdate[[j]]$strike_price;
        
        ch=readline();
        if (ch=="1"){
          return(1);
        }
      }
      #return(1)
    } else {
      print(paste("Ignoring Date:",curDate));
    }
  }
  #return(copt_atdate);
  
  #print(copt);
  #copt=data.frame(date=strptime(copt$date,"%Y%m%d"),
  #               price=mean(c(copt$best_bid,copt$best_offer)),
  #               implvol=copt$impl_volatility);
  #plot(opt$date,opt$implvol,type='l',
  #     xlab='Time',ylab='SPX Historical Vol');
  
}
