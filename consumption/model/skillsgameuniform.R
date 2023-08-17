
require(latex2exp)


plot_winning_probabilities <- function(){
  x = seq(0,.499,.01);
  par (mfrow=c(2,1))
  cexval = .5
  xpos=.3
  ypos = .74
  plot(0,0,type='l',xlim=c(0,.5),ylim=c(0,1),xlab=TeX("$\\mu$"),ylab=TeX("$P_1$(win)")); 
  
  lines(x,(x)/(6*(1-x)),type='l',lty=1) # only rich
  lines(x,(6-7*x)/(6*(1-x)) ,lty=2) # only poor
  lines(x,rep(1/2,length(x)),lty=3) # neither or both
  
  legend(xpos, ypos,legend=TeX(paste("Case",c("I","II","III,IV"))),lty=c(1,2,3),cex=cexval) 
  
  plot(0,0,type='l',xlim=c(0,.5),ylim=c(0,1),xlab=TeX("$\\mu$"),ylab=TeX("$P_2$(win)")); 
  
  lines(x,1-(x)/(6*(1-x)),type='l',lty=1) # only rich
  lines(x,1-(6-7*x)/(6*(1-x)),lty=2) # only poor
  lines(x,rep(1/2,length(x)),lty=3) # neither or both
  
  legend(xpos, ypos,legend=TeX(paste("Case",c("I","II","III,IV"))),lty=c(1,2,3),cex=cexval) 
  
}


simulate_wins <- function(){
   
  N = 1e+8
  mu = .2
  
  r11=runif(N)
  s11=runif(N)
  
  r12=runif(N)
  s12=runif(N)
  
  r2=runif(N)
  s2=runif(N)
  
  df=data.frame(r11=r11,s11=s11,r12=r12,s12=s12,r2=r2,s2=s2)
  
  result_richonly = nrow(subset(df,mu*r2 + (1-mu)*s2 < mu*r11  & mu*r12 < mu*r11 ))/nrow(df)
  print(paste("Result when rich-only invest",result_richonly," diff=",result_richonly-(mu/(8*(1-mu)))))
  
  
  result_pooronly = nrow(subset(df,mu*r2 + (1-mu)*s2 < mu*r11  & mu*r12 < mu*r11 ))/nrow(df)
  print(paste("Result when poor-only invest",result_pooronly," diff=",result_pooronly-(mu/(8*(1-mu)))))
  
}

plot_winning_probabilities()