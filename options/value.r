

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
  opt=split(snpopt,snpopt$cp_flag)
  copt=opt[["C"]]
  #print(copt);
  copt=data.frame(date=strptime(copt$date,"%Y%m%d"),
                 price=mean(c(copt$best_bid,copt$best_offer)),
                 implvol=copt$impl_volatility);
  #plot(opt$date,opt$implvol,type='l',
  #     xlab='Time',ylab='SPX Historical Vol');
  
}
