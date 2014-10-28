
set.seed(12345)
 
n <- 3000
phi <- 0.99 # AR(1) persistence parameter
sigma <- 0.08 # standard deviation of AR(1) noise
mu <- 0.01 # scalar factor in observation process 
nu <- 8 # degrees of freedom of the t-distributed error variable in the observation process
g <- obs <- rep(NA,n)
 
g[1] <- rnorm(1,0,sigma/sqrt(1-phi^2)) # initial log-volatility drawn from stationary distribution of AR(1) process
for (k in 2:n){
  g[k] <- phi*g[k-1]+rnorm(1,0,sigma)
}
for (k in 1:n){
  obs[k] <- mu*exp(g[k]/2)*rt(1,nu)
}

plot(obs,xlab="time",ylab="simulated returns",type="l",ylim=c(-max(abs(obs)),max(abs(obs))))
