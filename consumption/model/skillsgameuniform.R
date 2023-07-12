
require(latex2exp)


plot_winning_probabilities <- function(){
  x = seq(0,.499,.01);
  par (mfrow=c(1,2))
  
  
  plot(0,0,type='l',xlim=c(0,.5),ylim=c(0,1),xlab=TeX("$\\mu$"),ylab=TeX("$P_1$(win)")); 
  
  lines(x,(x)/(8*(1-x)),type='l',lty=1) # only rich
  lines(x,(-16*x**3 + 65*x**2 - 90*x + 40 )/( 120 * (1-x)**2),lty=2) # only poor
  lines(x,rep(1/3,length(x)),lty=3) # neither or both
  
  legend(0.3, .7,legend=TeX(paste("Case",c("I","II","III,IV"))),lty=c(1,2,3),cex=.7) 
  
  plot(0,0,type='l',xlim=c(0,.5),ylim=c(0,1),xlab=TeX("$\\mu$"),ylab=TeX("$P_2$(win)")); 
  
  lines(x,1-(x)/(4*(1-x)),type='l',lty=1) # only rich
  lines(x,1-(-16*x**3 + 65*x**2 - 90*x + 40 )/( 60 * (1-x)**2),lty=2) # only poor
  lines(x,rep(1/3,length(x)),lty=3) # neither or both
  
  legend(0.3, .7,legend=TeX(paste("Case",c("I","II","III,IV"))),lty=c(1,2,3),cex=.7) 
  
}


simulate_wins <- function(){
   
  N = 1e+9 
  mu = .2
  
  r11=runif(N)
  s11=runif(N)
  
  r12=runif(N)
  s12=runif(N)
  
  r2=runif(N)
  s2=runif(N)
  
  df=data.frame(r11=r11,s11=s11,r12=r12,s12=s12,r2=r2,s2=s2)
  
  result = nrow(subset(df,mu*r2 + (1-mu)*s2 < mu*r11  & mu*r12 < mu*r11 ))/nrow(df)
  print(result-(mu/(8*(1-mu))))
  return(result)
}