
require(latex2exp)
par (mfrow=c(1,2))

x = seq(0,.499,.01); 
plot(0,0,type='l',xlim=c(0,.5),ylim=c(0,1),xlab=TeX("$\\mu$"),ylab=TeX("$P_1$(win)")); 

lines(x,1-(x*(x^2-2*x-3)+4)/(6*(1-x)),type='l',lty=1) # only rich
lines(x,1-(-16*x**3 + 65*x**2 - 90*x + 40 )/( 60 * (1-x)**2),lty=2) # only poor
lines(x,rep(1/3,length(x)),lty=3) # neither or both

legend(0.3, .7,legend=TeX(paste("Case",c("I","II","III,IV"))),lty=c(1,2,3),cex=.7) 