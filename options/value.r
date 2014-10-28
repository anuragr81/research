processWindow <- function(){
  
}


test <- function(snpopt) {
  
  require(zoo);
  par(mfrow=c(2,1));
  vix=read.csv("vix.csv");
  vix=data.frame(vix=vix$vix,date=strptime(vix$Date,"%d%b%Y"));
  plot(vix$date,vix$vix,type='l',xlab='Time',ylab='VIX');
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
      c_atexdate$exdate=strptime(c_atexdate$exdate,"%Y%m%d");
      print(paste("Price",S0));
    } else {
      print(paste("Ignoring Date:",curDate));
    }
  }
  return(copt_atdate);
  
  #print(copt);
  #copt=data.frame(date=strptime(copt$date,"%Y%m%d"),
  #               price=mean(c(copt$best_bid,copt$best_offer)),
  #               implvol=copt$impl_volatility);
  #plot(opt$date,opt$implvol,type='l',
  #     xlab='Time',ylab='SPX Historical Vol');
  
}
