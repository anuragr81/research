setwd('c:/local_files/research/consumption/datamapper/')
source('translation/frameutils.R');source('lsms/lsms_normalizer.r');source('lsms/lsms_loader.r');ll=lsms_loader(fu=fu,ln=lsms_normalizer)

#m2010<-ll@load_market_prices(year = 2010, dirprefix = "../",fu = fu, ln = lsms_normalizer,use_pieces = FALSE)
#regs<-as.character(unique(m2010$region))
#items<-as.character(unique(m2010$shortname))
#iname="beef";par(mfrow=c(5,5));sapply(regs,function(x) { print(x); g=plot_commodity(sname = iname,data=subset(m2010,shortname==iname & region==as.character(x)),year=paste(2010,"- region:",x)) } );
#for (x in regs) {g=plot_commodity(sname = iname,data=subset(m2010,shortname==iname & region==as.character(x)),year=paste(2010,"- region:",x)) }
plot_commodity <- function(sname,data,year)
{
  #print(paste("sname=",sname,"year=",year))
  if (missing(year)){
    plotname <- sname
  }
  else {
    plotname <- paste(sname,"(",year,")")
  }
  
  
  dat <- subset(data,shortname==sname)
  if (dim(dat)[1]>1 && any(abs(diff(dat$price))>0) ){
  lmres <- lm(data=dat,price~lwp)
  plot(dat$lwp,dat$price,xlab = "quantity", ylab="price",main=plotname)
  if (any(abs(diff(dat$lwp))!=0)) {
    abline(lmres)
  } else {
    print("Could not plot line")
  }
  
  return(lmres$coefficients[[1]]+lmres$coefficients[[2]])
  #return(lmres)
  }
  
  else if (dim(dat)[1]==0 || !any(abs(diff(dat$price))>0)){
    print(paste("No prices - sname=",sname,"year=",year))
    return(NULL)
  } else {
    print("Single price entry")
    return(dat$price[1])
  }
}