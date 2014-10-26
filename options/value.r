

test <- function() {
  
  require(zoo);
  
  par(mfrow=c(2,1));
  opt=read.csv("options.csv");
  vix=data.frame(vix=opt$vix,date=strptime(opt$Date,"%d%b%Y"));
  plot(vix$date,vix$vix,type='l',xlab='Time',ylab='VIX');
  spx=read.csv("spx.csv");
  spx=data.frame(spx=spx$spindx,date=strptime(spx$caldt,"%Y%m%d"));
  
  sz=20;
  
  plot(spx$date[sz:length(spx$date)],rollapply(spx$spx,20,sd),type='l',
       xlab='Time',ylab='Historical Vol');
  
}
