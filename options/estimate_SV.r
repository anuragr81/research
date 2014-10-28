
## function that converts natural (constrained) parameters to working (unconstrained) parameters
pn2pw <- function(mu,phi,sigma,nu){
  lmu <- log(mu)
  lphi <- log((1+phi)/(1-phi))
  lsigma <- log(sigma)
  lnu <- log(nu)
  parvect <- c(lmu,lphi,lsigma,lnu)
  return(parvect)
}

## function that performs the inverse transformation
pw2pn <- function(parvect){
  mu <- exp(parvect[1])
  phi <- (exp(parvect[2])-1)/(exp(parvect[2])+1)
  sigma <- exp(parvect[3])
  nu <- exp(parvect[4])
  return(list(mu=mu,phi=phi,sigma=sigma,nu=nu))
}

## function that computes minus the (approximate) log-likelihood
mllk <- function(parvect,x,m){
  nx <- length(x)
  p <- pw2pn(parvect)
  if (model=="SV0") p$nu <- 10^10
  K <- m+1
  gb <- seq(-gbmax,gbmax,length=K)
  g <- (gb[-1]+gb[-K])*0.5             
  beg <- p$mu*exp(g/2)   
  gamma <- matrix(0,m,m)
  E <- p$phi*g
  intlen <- gb[2]-gb[1]
  for (i in 1:m){
    goo <- dnorm(g,E[i],p$sigma)*intlen
    gamma[i,] <- goo/sum(goo)
  }
  delta <- dnorm(g,0,p$sigma/sqrt(1-p$phi^2))*intlen
  foo <- delta*1/beg*dt(x[1]/beg,p$nu)
  lscale <- 0
  for (t in 2:nx){
    foo <- foo%*%gamma*1/beg*dt(x[t]/beg,p$nu)
    sumfoo <- sum(foo); lscale <- lscale+log(sumfoo); foo <- foo/sumfoo # scaling
  }
  return(-lscale)
}

## function that, for given initial values, runs the numerical maximization of the log-likelihood and returns the results
mle <- function(x,m,mu0,phi0,sigma0,nu0){
  nx <- length(x)
  parvect <- pn2pw(mu0,phi0,sigma0,nu0)
  mod <- nlm(mllk,parvect,x=x,m=m,stepmax=3,print.level=2)
  p <- pw2pn(mod$estimate)
  if (model=="SVt") return(list(mu=p$mu,phi=p$phi,sigma=p$sigma,nu=p$nu,mllk=mod$minimum))
  if (model=="SV0") return(list(mu=p$mu,phi=p$phi,sigma=p$sigma,mllk=mod$minimum))
}

## choice of initial parameter values
phi0 <- 0.98
sigma0 <- 0.1
mu0 <- 0.01
nu0 <- 10

## choice of tuning parameters controlling the accuracy of the likelihood approximation
gbmax <- 2 
m <- 150 

## choice of the model to be fitted (either with conditionally normally distribution or with conditionally t-distributed returns)
model <- "SVt" # change to "SV0" if an SV model with conditionally normally distributed, rather than t-distributed, returns is to be fitted
s <- Sys.time()
mod <- mle(obs,m,mu0,phi0,sigma0,nu0)
mod
Sys.time()-s
