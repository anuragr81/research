
setwd('C:/local_files/development/bayesian.learning/')

library(rethinking)
data("Howell1")
d <- Howell1
d2 <- d [ d$age >= 18,]

x <- 6
N <- 1000
n <- 6
koopch2_data <- function()
{
  n=50;
  beta=2;
  sigma=1;
  error= sigma*rnorm(n);
  x=runif(n);
  y=beta*x+error;
  
  return(matrix(c(y,x),n,2))
}

koopch2_main <- function(){
  ch2data <- koopch2_data()
  y=(ch2data[,1]);
  x=(ch2data[,2]);
  
  #Hyperparameters for natural conjugate prior
  v0=10; # prior
  b0=1.5;
  s02=1;
  capv0=.25; #(capital V)
  capv0inv=1/(capv0);
  
  #things used to plot marginal student t prior 
  incr=.01;
  alower=1;
  aupper=3;
  #plotpri = tdensity(b0,s02*capv0,v0,alower,aupper,incr);
  
  #Call script which carries actually does posterior analysis
  #ch2post;
  n=length(y);
  bols = solve(t(x)%*%x)%*%(t(x)%*%y);
  s2 = t(y-x%*%bols)%*%(y-x%*%bols)/(n-1);
  bolscov = s2*solve(t(x)%*%x);
  bolssd=sqrt(bolscov);
  v=n-1;
  
  xsquare=t(x)%*%x;
  v1=v0+n;
  capv1inv = capv0inv+ xsquare;#2.9
  capv1=1/(capv1inv);
  b1 = capv1*(capv0inv*b0 + bols%*%xsquare);#2.10
  if (capv0inv>0)
  {
    v1s12 = v0*s02 + v*s2 + ((bols-b0)**2)/(capv0+(1/xsquare)); #2.12
  }else {
    v1s12 = v0*s02 + v*s2 ; 
  }
  s12 = v1s12/v1;
  
  bcov = capv1*v1s12/(v1-2);#2.15
  bsd=sqrt(bcov);
  
  
  hmean = 1/s12;
  hvar=2/(v1s12);
  hsd=sqrt(hvar); #2.18
  
  #predictive inference
  xstar=.5;
  ystarm = xstar*b1; #2.40
  ystarcapv = (1+capv1*xstar^2)*s12;
  ystarv = ystarcapv*v1/(v1-2);
  ystarsd=sqrt(ystarv);
  
  
  #log of marginal likelihood for the model if prior is informative
  if (v0>0) {
    
    #intcon=lgamma(.5*v1) + .5*v0*log(v0*s02)- lgamma(.5*v0) -.5*n*log(pi);
    #lmarglik=intcon + .5*log(capv1/capv0) - .5*v1*log(v1s12);
    
  }
}

koopch3_main <- function(){
  hprice <- read.csv('houseprices.csv')
  
  n=dim(hprice)[1];
  hprice$ones <- rep(1,n)
  y=matrix(hprice[,1])
  
  x <- as.matrix(hprice[,c("ones","lotsz" ,"nbed","nbath","nstor")])
  
  k=5;
  v0=5;
  b0=as.matrix(0*rep(1,k));
  b0[2,1]=10;
  b0[3,1]=5000;
  b0[4,1]=10000;
  b0[5,1]=10000;
  s02=1/4.0e-8;
  capv0=2.4*diag(k);
  capv0[2,2]=6e-7;
  capv0[3,3]=.15;
  capv0[4,4]=.6;
  capv0[5,5]=.6;
  capv0inv=solve(capv0);
  #Ordinary least squares quantities
  bols = solve(t(x)%*%x)%*%(t(x)%*%y);
  s2 = (t(y-x%*%bols)%*%(y-x%*%bols))/(n-k);
  v=n-k;
  
  
  #Posterior hyperparameters for Normal-Gamma
  xsquare=t(x)%*%x
  v1=v0+n
  capv1inv = capv0inv+ xsquare
  capv1=solve(capv1inv)
  b1 = capv1%*%(capv0inv%*%b0 + xsquare%*%bols)
  if (det(capv0inv)>0){
  v1s12 = v0%*%s02 + v%*%s2 + t(bols-b0)%*% (solve(capv0 + solve(xsquare)))%*%(bols-b0)
} else {
    v1s12 = v0%*%s02 + v%*%s2;
}
  
  s12 = v1s12/v1;
  
  bcov = capv1*as.numeric(v1s12)
  bsd=as.matrix(rep(0,k));
  for (i in seq(k)) {
    bsd[i,1]=sqrt(bcov[i,i]);  
  }
  
  return(hprice)
}
get_posterior <- function(n,N,x){
  p_grid <- seq(from=0, to=1,length.out = N)
  prior <- rep(1,N)
  likelihood <- dbinom(x=x,size=n,prob=p_grid)
  posterior <- likelihood * prior
  posterior <- (posterior)/sum((posterior))
  return(posterior)
}


demo_quad_approx <- function() {
  globe.qa <- map(alist ( w ~ dbinom(9,p), p ~ dunif(0,1) ) , data = list(w=6) )
}


plot_sample_posterior <-function(){ 
  data("Howell1")
  d <- Howell1
  d2 <- d[d$age>=18,]
  mu.list <- seq(from=140,to=160,length.out=200)
  sigma.list <- seq(from=4,to=9,length.out=200)
  post <- expand.grid(mu=mu.list , sigma=sigma.list)
  
  post$LL <- sapply(1:nrow(post), function(i) sum (dnorm(
    d2$height,
    mean=post$mu[i],
    sd=post$sigma[i],
    log=TRUE)))
  
  post$prod <- post$LL + dnorm(post$mu,178,20,TRUE) + dunif(post$sigma,0,50,TRUE) 
  post$prob <- exp(post$prod - max(post$prod))
  sample.rows   <- sample(1:nrow(post),size=1e+4,replace=TRUE,prob=post$prob)
  sample.mu     <- post$mu [ sample.rows  ]
  sample.sigma  <- post$sigma[ sample.rows ]
  plot(sample.mu,sample.sigma,cex=0.5,pch=16,col=col.alpha(rangi2,0.1))
}

plot_sample_posterior_2 <-function(){ 
  data("Howell1")
  d <- Howell1
  d2 <- d[d$age>=18,]
  d3 <- sample(d2$height,size=20)
  
  mu.list <- seq(from=150,to=170,length.out=200)
  sigma.list <- seq(from=4,to=20,length.out=200)
  post2 <- expand.grid(mu=mu.list , sigma=sigma.list)
  
  post2$LL <- sapply(1:nrow(post2), function(i) sum (dnorm(
    d3,
    mean=post$mu[i],
    sd=post$sigma[i],
    log=TRUE)))
  
  post2$prod <- post2$LL + dnorm(post2$mu,178,20,TRUE) + dunif(post2$sigma,0,50,TRUE) 
  post2$prob <- exp(post2$prod - max(post2$prod))
  sample2.rows   <- sample(1:nrow(post2),size=1e+4,replace=TRUE,prob=post2$prob)
  sample2.mu     <- post2$mu [ sample.rows  ]
  sample2.sigma  <- post2$sigma[ sample.rows ]
  #plot(sample2.mu,sample2.sigma,cex=0.5,pch=16,col=col.alpha(rangi2,0.1))
  dens(sample2.sigma,norm.comp = TRUE)
}


fit_basic_model <- function(){
  flist <- alist(height ~dnorm(mu,sigma) , mu ~dnorm(178,20), sigma ~ dunif(0,50))
  m4.1 <- rethinking::map(flist, data=d2)
  flist2 <- alist(height ~dnorm(mu,sigma) , mu ~dnorm(178,.1), sigma ~ dunif(0,50))
  m4.2 <- rethinking::map(flist2, data=d2)
}

fit_linear_model <- function(){
  data("Howell1")
  d <- Howell1
  d2 <- d [ d$age >= 18,]
  
  flist <- alist(height ~dnorm(mu,sigma) , mu ~ a + b*weight, a ~ dnorm(178,100), b ~ dnorm(0,10), sigma ~ dunif(0,50))
  model <- rethinking::map(flist, data=d2)
  N     <- 10
  dN    <- d2[1:N,]
  post  <- extract.samples(model , n = 20)
  if (FALSE){
    plot ( dN$weight, dN$height, xlim = range(d2$weight), ylim = range(d2$height),col = rangi2 , xlab="weight", ylab="height")
    mtext (concat("N=",N))
  }
  
  if (FALSE){
    mu_at_50 <- post$a + post$b * 50
    dens(mu_at_50)
    print(HPDI(mu_at_50,prob=0.9))
  }
  if (FALSE){
    mu <- link (model,data=data.frame(weight=seq(25,70,1)))
    plot(height ~ weight, d2, type='n')
    for ( i in 1:100) { points (seq(25,70,1),mu[i,],pch=16,col=col.alpha(rangi2,0.1))}
    print(apply(mu, 2 , mean))
    
  }
  return(model)
}

chapter5_1 <- function(){
  data("WaffleDivorce")
  d <- WaffleDivorce
  d$MedianAgeMarriage.s <- with (d , (MedianAgeMarriage - mean(MedianAgeMarriage))/(sd(MedianAgeMarriage) ))
  d$Marriage.s <- with (d , (Marriage - mean(Marriage))/(sd(Marriage) ))
  m5.1 <- map(alist(
    Divorce ~ dnorm(mu,sigma),
    mu ~ a + bA * MedianAgeMarriage.s,
    a ~ dnorm(10,10),
    bA ~ dnorm(0,1),
    sigma ~ dunif(0,10)
  ),data = d)
  #mu <- link (m5.1, data=data.frame(MedianAgeMarriage.s=seq(-3,3.5,length.out=30)))
  #mu.PI <- apply(mu,2,PI)
  
  
  m5.3 <- map(alist (
    Divorce ~ dnorm(mu,sigma),
    mu <- a + bR*Marriage.s + bA*MedianAgeMarriage.s , 
    a ~ dnorm(10,10),
    bR ~ dnorm(0,1),
    bA ~ dnorm(0,1),
    sigma  ~ dunif(0,10)
  ) , data = d)
  
  
  A.avg <- mean(d$MedianAgeMarriage.s)
  R.seq <- seq(-3,3,length.out = 30)
  pred.data <- data.frame( Marriage.s = R.seq, MedianAgeMarriage.s = A.avg)
  
  if (FALSE){ # counterfactual plots
    mu <- link(m5.3,data= pred.data)
    mu.mean <- apply(mu,2,mean)
    mu.PI   <- apply(mu, 2, PI)
    R.sim <- sim(m5.3,data=pred.data, n=1e+4)
    R.PI  <- apply(R.sim, 2, PI)
    #plot(data = d , Divorce ~ Marriage.s , type='n')
    #lines(R.seq, mu.mean)
    
    #shade(mu.PI, R.seq)
    
  }
  
  
  if (TRUE){
    mu <- link(m5.3)
    mu.mean <- apply(mu,2,mean)
    mu.PI   <- apply(mu,2,PI)
    
    divorce.sim <- sim(m5.3, n=1e+4)
    divorce.PI  <- apply(divorce.sim,2,PI)
    plot(mu.mean ~ d$Divorce, col= rangi2, ylim = range(mu.PI))
    abline(a=0,b=1,lty=2)
    for ( i in 1:nrow(d)) { lines(rep(d$Divorce[i],2) , c(mu.PI[1,i],mu.PI[2,i]), col=rangi2)}
  }
  res = list()
  res[["d"]] = d
  res[["model"]] = m5.3
  return(res)
  
}

chapter5_2<- function(){
  data(milk)
  d <- milk
  dcc <- d[complete.cases(d), ]
  m5.5 <- map(alist (
    kcal.per.g ~ dnorm (mu, sigma),
    mu <- a+ bn*neocortex.perc, 
    a ~ dnorm(0,100),
    bn ~ dnorm(0,1),
    sigma ~ dunif(0,1)
  ) , data = dcc)
  
  
  res = list()
  res[["d"]] = dcc
  res[["model"]] = m5.5
  return(res)
  
}

chapter6 <- function(){
  sppnames <- c("afarensis","africanus","habilis","boisei","rudolfensis","ergaster","sapiens")
  brainvolcc <- c(438,452,612,521,752,871,1350)
  masskg     <- c(37,35.5,34.5,41.5,55.5,61,53.5)
  d <- data.frame(species = sppnames, brain = brainvolcc, mass = masskg)
  m6.1 <- lm(brain~mass, data=d)
  res = list()
  res[["d"]] = d
  res[["model"]] = m6.1
  
  return(res)
}