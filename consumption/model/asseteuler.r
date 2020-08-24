require(latex2exp)
tol = 1e-7

# constraint is applied only at reset times
# one possible reset is every year - when the total consumption
# may be forced to be consume all that left in the savings
# the other type of reset is a death of the individual when the 
# assets would need to be transferred and there would be no consumption
# this is the only constraint that is required
# otherwise, all non-consumption would be saved
constraint <- function(savings, consumption, assets){
  return (abs(sum(assets) + sum(consumption) - sum(savings))<tol)
}


next_available_asset<-function(cost){
  costs <- seq(10,100,10)
  return(min(costs[costs > cost]))
}

# we can assume that assets are purchased at the beginning i.e. the decisions are 
# made at the beginning of the period
can_buy <- function(savings, assets){
  next_delta <- next_available_asset(sum(savings)-sum(assets))
}

next_A <-function (A,i,Gamma,n){
  return (A+i - n*A*Gamma)
}

next_i <- function(beta,i,sigma,A){
  return(beta*(i + sigma*A*rnorm(1)))
}

next_n <- function(n){
  return(n)
}
allu <- function(arrA,arrn,arrGamma){
  u = array()
  for ( j in seq(length(arrA))) {
    cc <- arrA[j]*arrn[j]*arrGamma[j]
    u [j]  = (log(arrA[j]**(0.2)) + log(cc**(0.5)) + log(arrn[j]**(0.3)))
  } 
  return (u)
}

runsim <- function(){
  A = array()
  n = array()
  G = array()
  i = array()
  N = 10
  A[1] = 1
  n[1] = 1
  i[1] = 10
  G[1] = .2
  
  for (k in seq(1,N-1)){
    G[k+1] <- G[k]   
    A[k+1] <- next_A(A = A[k],i = i[k], Gamma = G[k],n = n[k])
    i[k+1] <- next_i(beta = .2, i = i[k],sigma = .1, A = A[k])
    n[k+1] <- next_n(n[k])
  }
  u <- allu(arrA = A, arrn = n, arrGamma = G)
  retlist = list()
  retlist[["A"]] = A
  retlist[["u"]] = u
  return(retlist)
}

ns_eta_next <- function(t,T){
  return(1)
}

ns_c <- function(A,nu,eta,a,m,income) {
  return(A**(a)*m*(nu)*eta*income)
}
ns_next_A <- function(A,income,a,m,nu,eta,next_income){
  ct  <- ns_c(A=A,eta=eta,a=a,m=m,nu=nu,income=income)
  if (next_income < ct ){
    stop("consumption cannot be higher than income")
  }
  return (A + next_income - ct)
}

ns_income_next <- function(income,k){
  return (k*income)
}

ns_u <- function(arrA,arrc,arreta){
  u = array()
  for ( j in seq(length(arrA))) {
    u [j]  = (log(arrA[j]**(0.2)) + log(arrc[j]**(0.5)) + log(arreta[j]**(0.3)))
  } 
  return (u)
}


#r <- ns_runsim(nu=.1,N=50,alpha=.3) ;print (r$c) ; print(r$A) ; par(mfrow=c(2,1)); plot(r$t,r$A);plot(r$t,r$c)

#arr = seq(1,16,.2) ; res = sapply(arr, function(x) { ns_runsim(nu = x,N = 10,a = .2)$totu }); plot (arr,res,type='l')
#(A_1+i_1*k)/(a*i_1*(A_1**(alpha))*eta_1*(alpha/beta+1))
ns_runsim <- function(nu,N,a){
  A = array()
  ct = array()
  eta = array()
  i = array()
  gk = 1.02
  
  m = .05
  
  A[1] = 1
  eta[1] = 1
  i[1] = 10
  ct[1] = 2
  
  for (k in seq(1,N-1)){
    i[k+1] <- ns_income_next(income=i[k], k=gk)
    A[k+1] <- ns_next_A(A = A[k], income = i[k], a = a, m =m, nu = nu, eta = eta[k] ,next_income = i[k+1])
    
    eta[k+1] <- ns_eta_next(t=k, T=N)
    ct[k+1] <-    ns_c(A = A[k],nu = nu, eta = eta[k],a = a, m = m, income = i[k])
  }
  #u <- allu(arrA = A, arrn = n, arrGamma = G)
  retlist = list()
  retlist[["A"]] = A
  retlist[["c"]] = ct
  retlist[["i"]] = i
  retlist[["u"]] = nsf_u(arrA = A, arrc = ct,arreta = eta)
  retlist[["t"]] = seq(N)
  retlist[["totu"]] = sum(retlist[["u"]])
  
  
  
  #retlist[["u"]] = u
  return(retlist)
}

nsf_c <- function(A,nu, eta, Psi, a, m,Q){
  return(eta*Q + Psi*nu + m*A**a)
}

nsf_next_A <- function(A,income,a,m,nu,eta,Q,Psi, next_income){
  ct  <- nsf_c(A = A,nu = nu, eta = eta,a = a, m = m, Q=Q, Psi=Psi)
  
  if (next_income < ct ){
    stop(paste("consumption (",ct, ") cannot be higher than income (",next_income, ")"))
  }
  return (A + next_income - ct)
}

nsf_u <- function(arrA,arrnu,arrbeta){
  # each u should be based on a certain beta - that beta is also set by the consumer (it varies by year the consumer is this should be an array) 
  # 
  u = array()
  for ( j in seq(length(arrA))) {
    u [j]  = arrbeta[j] * (log(arrA[j]**(0.2)) + log(arrnu[j]**(0.8)) )
  } 
  return (u)
}

nsf_income_next <- function(income,gk){
  return (gk*income)
}

peerf <- function()
{
  N= 100
  nu <- sapply(rnorm(N,1)+10, function(x) { max(0,x)} ) 
  rho <- floor(runif(N)*3)+1
  
  df <- data.frame(nu = nu , rho = rho)
  
  df <- merge(df, ddply(df,.(rho),summarise, mnu = mean(nu)), by = c("rho")) %>% mutate ( nudiff = nu - mnu)
  return(df)
  
}


ns_runsimf <- function(nu,N,a,i0,gk,eta0,A0, m){
  #x <- seq(-10,50,.1) ; plot(x,5*x*exp(-x*x*.001),type='l')
  A = array()
  ct = array()
  eta = array()
  i = array()
  
  A[1] = A0
  eta[1] = eta0
  i[1] = i0*gk
  Q = 10
  Psi = 5
  ct[1] = nsf_c(A = A[1],nu = nu, eta = eta[1],a = a, m = m, Q = Q , Psi)
  
  for (k in seq(1,N-1)){
    i[k+1] <- nsf_income_next(income=i[k], gk=gk)
    A[k+1] <- nsf_next_A(A = A[k], income = i[k], a = a, m =m, nu = nu, eta = eta[k] ,next_income = i[k+1],
                         Q = Q , Psi = Psi)
    
    eta[k+1] <- ns_eta_next(t=k, T=N)
    ct[k+1] <-  nsf_c(A = A[k],nu = nu, eta = eta[k],a = a, m = m,Q = Q , Psi= Psi)
    
  }
  #u <- allu(arrA = A, arrn = n, arrGamma = G)
  retlist = list()
  retlist[["A"]] = A
  retlist[["c"]] = ct
  retlist[["i"]] = i
  
  retlist[["u"]] = nsf_u(arrA = A, arrnu = rep(nu,length(ct)))
  retlist[["t"]] = seq(N)
  retlist[["totu"]] = sum(retlist[["u"]])
  
  
  
  #retlist[["u"]] = u
  return(retlist)
}

needs <- function(x){
  return(x/3*exp(-x*x*.0002))
}

next_asset_iteration <- function(A,i_next,tau,delta, m , alpha, r) {
  #\delta_{\tau+1}A_{\tau+1}&=A_{\tau}(1+r)+ki_{\tau}-\eta_{\tau+1}-mA_{\tau}^{\alpha}
  return( ( 1/ delta) * ( A*(1+r) + i_next - needs(tau) - m*(A**alpha) ) )
}

recover_delta <- function( df, m , alpha, r , k) {
  i0 = df$i0 
  
  A = df$A0
  tau = df$age
  needs <- df$hsize
  
  t2 <- (tau-median(tau))**2
  df$t2 <- t2
  gA <- (A*(1+r) - m*A**alpha - needs + k**tau * i0 + 1e-19)
  df$gA <- gA
  df<- subset(df, gA>0)
  print("Ignored non-positive gA")
  lgA <- log(df$gA)
  tA_next <- log(df$A1 + 1e-19)
  res <- lm (data=df, tA_next ~  lgA + t2)
  print(summary(res))
}

get_sample_df <- function(){
  N <- 1000
  percapitaneedscost <- 20
  
  inc <- sapply((200-100*abs(rnorm(N))), function(x) { max(x, 10)})
  # assets are clearly more skewed than incomes
  x <- rnorm(N); 
  A0 <- sapply( N-exp(x*x) , function(x) { max (1, 10*x)} )
  A1 <- sapply(A0+ inc*rnorm(N)*.01,function(x) { max (1, 10*x)} )
  age <- sapply(30+rnorm(1000)*5,function(x){ max(x,1)}); 
  
  hsize <- sapply(age, function(x) { percapitaneedscost*floor(needs(x))})
  df = data.frame(i0 = inc, age = age, A0 = A0, A1=A1 , hsize = hsize)
  return(df)
}

evolve_assets <- function(i0,k,A0,m,alpha , r){
  if (missing(m)){
    m <- 2
  }
  if (missing(alpha)){
    alpha <- .5
  }
  
  tauvec <- seq(20,60)
  #delta <- tauvec/10 * exp(-tauvec*tauvec/2000)
  delta <- rep(1,length(tauvec))
  #ppl_ages <- sapply((40+rnorm(100)*10), function(x) { max(x,20)} ) ;  
  i = i0
  A <- A0
  assets <- array()
  for (i in seq(length(tauvec))){
    
    inc_next =  i0*k
    A <- next_asset_iteration(A = A, i_next = inc_next, tau = tauvec[i], delta = delta[i], m = m , alpha = alpha, r=r)
    assets[i] <- A
  }
  return(assets)
  
}


evolve_relative_wealth <-function(nsim,delta,sigma1,sigma2, risksz){
  
  T = 1
  dt = 1e-1
  A1_init = 100
  A2_init = 100
  
  
  A1df <- data.frame()
  A2df <- data.frame()
  incdf <- data.frame()
  
  for ( j in seq(nsim)){
    A1 = A1_init
    A2 = A2_init
    A1_arr = array()
    A2_arr = array()
    inc_arr = array()
    
    count <- 1
    A1_arr[count] = A1_init
    A2_arr[count] = A2_init
    inc_arr[count] = 0
    
    timepoints <- seq(0,T,dt)
    #TODO: replace dW -> sqrt(dt)*rnorm(1)
    for (i in timepoints)
    {  
      dW1 = rnorm(1)
      dW2 = rnorm(1)
      inc  = - delta * (A1-A2)  
      dA1 = inc* dt -sigma1 +  sigma1 * risksz * dW1
      dA2 = -inc* dt - sigma2 + sigma2 * risksz* dW2
      A1 = A1 + dA1
      A2 = A2 + dA2
      #if (A1<0){
      #  stop("Cannot be less than 0")
      #}
      #if (A2<0){
      #  stop("Cannot be less than 0")
      #}
      
      
      count <- count+1
      A1_arr[count] <- A1
      A2_arr[count] <- A2
      inc_arr[count] <- inc
      
    }
    A1add <- t(data.frame(x=A1_arr))
    colnames(A1add) <- paste0("t_",c(timepoints, T+dt))
    A1df <- rbind(A1df,A1add)
    
    A2add <- t(data.frame(x=A2_arr))
    colnames(A2add) <- paste0("t_",c(timepoints, T+dt))
    A2df <- rbind(A2df,A2add)
    
    iadd <- t(data.frame(x=inc_arr))
    colnames(iadd) <- paste0("t_",c(timepoints, T+dt))
    incdf <- rbind(incdf,iadd)
    
  }
  retlist = list()
  retlist[["A1"]] <- A1df
  retlist[["A2"]] <- A2df
  retlist[["inc"]] <- incdf
  return(retlist)
}


evolve_relative_wealth_discrete <-function(nsim,delta,sigma1,sigma2, risksz, p, T, dt, A1_init, A2_init){
  
  if (missing(T)){
    T <- 1
  } 
  if (missing(dt)){
    dt = 1e-1  
  }
  if (missing(A1_init)){
    A1_init = 100  
  }
  if (missing(A2_init)){
    A2_init = 100
  }
  
  A1df <- data.frame()
  A2df <- data.frame()
  incdf <- data.frame()
  
  for ( j in seq(nsim)){
    A1 = A1_init
    A2 = A2_init
    A1_arr = array()
    A2_arr = array()
    inc_arr = array()
    
    count <- 1
    A1_arr[count] = A1_init
    A2_arr[count] = A2_init
    inc_arr[count] = 0
    
    timepoints <- seq(0,T,dt)
    
    for (i in timepoints)
    {  
      dW1 = rbinom(1,1,p)
      dW2 = rbinom(1,1,p)
      inc  = - delta * (A1-A2)  
      dA1 = inc* dt -sigma1 +  sigma1 * risksz * dW1
      dA2 = -inc* dt - sigma2 + sigma2 * risksz* dW2
      A1 = A1 + dA1
      A2 = A2 + dA2
      #if (A1<0){
      #  stop("Cannot be less than 0")
      #}
      #if (A2<0){
      #  stop("Cannot be less than 0")
      #}
      
      
      count <- count+1
      A1_arr[count] <- A1
      A2_arr[count] <- A2
      inc_arr[count] <- inc
      
    }
    A1add <- t(data.frame(x=A1_arr))
    colnames(A1add) <- paste0("t_",c(timepoints, T+dt))
    A1df <- rbind(A1df,A1add)
    
    A2add <- t(data.frame(x=A2_arr))
    colnames(A2add) <- paste0("t_",c(timepoints, T+dt))
    A2df <- rbind(A2df,A2add)
    
    iadd <- t(data.frame(x=inc_arr))
    colnames(iadd) <- paste0("t_",c(timepoints, T+dt))
    incdf <- rbind(incdf,iadd)
    
  }
  retlist = list()
  retlist[["A1"]] <- A1df
  retlist[["A2"]] <- A2df
  retlist[["inc"]] <- incdf
  return(retlist)
}

draw_crra<- function(gamma,x){
  lines(x,(x**(1-gamma)-1)/(1-gamma),type='l'); 
}

draw_value_function <- function(){
  x<- seq(-100,100,.01)
  plot(0,0,type='l',xlim=c(-100,100), ylim=c(-20,20))
  df = data.frame( gamma = c(.1,.2,.5,1,2) , lty = c(1,2,3,4,5))
  for (i in seq(nrow(df))){
    row <- df[i,]
    gamma <- row$gamma
    lty <- row$lty
    lines(x,sapply(x,function(x) { if(x>0) return((x**gamma)) else return(-(-x)**gamma) }), lty=lty)
  }
  
}
compare_bernoulli_sum<-function(p,nsims){
  par(mfrow=(c(2,1)))
  hist((rbinom(nsims,10,p))) 
  mean(rbinom(nsims,10,p))
  x <- data.frame(); for ( i in seq(nsims) ) { x <- rbind(x,(rbinom(10,1,p))) } ; hist(rowSums(x))
}

analyse_relative_wealth_results <- function(res){
  # assume that there is infinite credit available thre is an optimum to be achieved
  print(colMeans(res[["A1"]]))
  print(colMeans(res[["A2"]]))
  x<- seq(-1,100,.01)
  plot(0,0,type='l',xlim=c(-10,100), ylim=c(-1,20))
  draw_crra(0,x=x)
  draw_crra(0.5,x=x)
  draw_crra(.99,x=x)
}

adjust_geom<-function(A1,A2){
  m = sqrt(A1*A2)
  retlist = list()
  retlist[["A1"]] <- A1/m
  retlist[["A2"]] <- A2/m
  return(retlist[["A1"]]/retlist[["A2"]])
}

cost_function_linear <-function(A,a,b){
  return(a*A+b)
}

cost_function_polynom <-function(A,a,b){
  #loglinear_test(.3,1,a=.10,b=1.5,incpi=1)
  return(a*A**b)
}

cost_function_exp <-function(A,a,b){
  return(a*exp(b*A))
}


cost_function_jump <-function(A,a,b){
  m <- 40
  #try with: loglinear_test(.3,1,a=5,b=50,incpi=1)
  return (b * ((1-exp(-a*(A-m) ))/(1+exp(-a*(A-m)))) )
}

loglinear_test<-function(alpha,A0,a,b,incpi){
  psivec = seq(0.1,100,.01)
  #cf = cost_function_exp(A0 + incpi*psivec,a=a,b=b)
  cf = cost_function_polynom(A0 + incpi*psivec,a=a,b=b)
  #cf = cost_function_jump(A0 + incpi*psivec,a=a,b=b)
  #cf = cost_function_linear(A0 + incpi*psivec,a=a,b=b)
  A = A0 - cf + incpi * psivec
  dat = data.frame(A=A,psi=psivec)
  dat$u = with(dat,alpha/2*log(A) + (1-alpha)*log(psi))
  par(mfrow=c(2,2))
  plot(psivec,cf,type='l',main = "cost-function")
  plot(psivec,A,type='l', main="assets")
  plot(dat$psi,dat$u,type='l', main="utility")
  return(dat)
}

explicit_parabolic_cost_solution <- function(a,alpha,A,incpi){
  expression1 = (alpha * A / 2 -   (alpha-2)/(4*a))
  expression2 = (1/2)* sqrt( alpha**2 * A**2 + (alpha-2)**2/(4*a**2) - alpha*A*(alpha-4)/a)
  
  psi1=((expression1+expression2)-A)/incpi
  psi2=((expression1-expression2)-A)/incpi
  print(psi1)
  print(psi2)
}

loglinear_incredist<-function(alpha,A10,A20,a,b,incpi){
  psivec1 = seq(0.1,100,.01)
  psivec2 = seq(0.1,100,.01)
  cf1 = cost_function_polynom(A10 + incpi*psivec1,a=a,b=b)
  cf2 = cost_function_polynom(A20 + incpi*psivec2,a=a,b=b)
  #cf1 = cost_function_jump(A10 + incpi*psivec,a=a,b=b)
  #cf2 = cost_function_jump(A20 + incpi*psivec,a=a,b=b)
  
  A1 = A10 - cf1 + incpi * psivec1
  A2 = A20 - cf2 + incpi * psivec2
  dat1 = data.frame(A=A1,psi=psivec1)
  dat2 = data.frame(A=A2,psi=psivec2)
  dat1$u = with(dat1,alpha/2*log(A) + (1-alpha)*log(psi))
  dat2$u = with(dat2,alpha/2*log(A) + (1-alpha)*log(psi))
  par(mfrow=c(2,3))
  plot(psivec1,cf1,type='l',main = "Consumer 1 - cost" , xlab= TeX("$\\psi_1$") , ylab = "cost of assets")
  plot(psivec1,A1,type='l', main="Consumer 1 - wealth", xlab= TeX("$\\psi_1$") , ylab = "A")
  plot(dat1$psi,dat1$u,type='l', main="Consumer 1 - utility", xlab= TeX("$\\psi_1$") , ylab="u")
  #plot(A10 + incpi*psivec1,cf1,type='l', main="Consumer 1 - A vs costs", xlab= "A" , ylab="cost")
  
  plot(psivec2,cf2,type='l',main = "Consumer 2 - cost", xlab= TeX("$\\psi_2$") , ylab = "cost of assets")
  plot(psivec2,A2,type='l', main="Consumer 2 - wealth", xlab= TeX("$\\psi_2$") , ylab = "A")
  plot(dat2$psi,dat2$u,type='l', main="Consumer 2 - utility", xlab= TeX("$\\psi_2$"), ylab="u")
  #plot(A20 + incpi*psivec2,cf2,type='l', main="Consumer 2 - A vs costs", xlab= "A" , ylab="cost")
}

karmakar <- function(p,alpha){
  return (p**alpha/(p**alpha + (1-p)**alpha))
  #return(p)
}

pt_value <- function(x,gamma,lambda){
  if (x>=0){
    return (x**gamma) 
  } else {
    return(-lambda* (-x)**gamma)
  }
}
riskf <- function(x,a){
  #return (sqrt(x))
  return (x**a)
}

pt_utility <-function(p,alpha,psi,pinc,A0,Aref, lambda, gamma,a){
  val1 =  karmakar(p,alpha)  * pt_value( x = A0 - pinc*psi  + riskf(psi,a) - Aref, gamma = gamma , lambda =  lambda )
  val2 =  karmakar(1-p,alpha) * pt_value( x = A0 - pinc*psi - Aref, gamma = gamma, lambda = lambda )
  return(val1 + val2)
}

plot_pt_utilities <-function(p,alpha,pinc,A0, gamma, lambda, Aref,a=a){
  
  #see plot_pt_utilities(p = .1,alpha = .2, pinc =10, A0 = 100, gamma = .7, lambda = 10) for a dip
  #plot_pt_utilities(p = .1,alpha = .2, pinc =.10, A0 = 20, gamma = .7, lambda = 10, Aref = 0, a=.5) for a peak (low incpi)
  # gamma >1 would be more natural
  
  arr = array()
  count = 1
  psivec = seq(0,80,.1) # psi cannot be negative
  for (psi in psivec){
    arr[count] = pt_utility(p = p, alpha = alpha, psi = psi, pinc = pinc, A0 = A0,Aref = Aref, gamma= gamma, lambda = lambda,a=a)
    count = count + 1
  }
  plot(psivec,arr,type='l', xlab=TeX("$\\psi$"), ylab="u",
       main = TeX(paste("$\\gamma$=",gamma, "$\\lambda$=",lambda, "$\\delta$=",pinc,"$\\alpha$=",alpha,"p=",p)))
  
}

plot_value_functions<-function(){
  xvec <- seq(-1,1,.01)
  lambda = 10
  plot(0,0,type='l',xlim = c(-1,1.5), ylim=c(-10,2) , main=TeX(paste0("Value Function for $\\lambda$=",lambda)))
  df = data.frame( gamma = c(.1,.2,.5,1,1.5,3) , lty = c(1,2,3,4,5,6))
  for (i in seq(nrow(df))){
    row <- df[i,]
    lines(xvec,sapply(xvec,function(y) { pt_value(x = y,gamma = row$gamma,lambda = lambda) } ), type='l' , lty=row$lty)
  }
  
  legend(0.2, -5, legend=paste("gamma=",df$gamma), lty=df$lty, cex=0.7)
}


plot_pwf<-function(){
  pvec <- seq(0,1,.01)
  
  plot(0,0,type='l',xlim = c(0,1.5), ylim=c(0,1) , main="Probability weighting function")
  df = data.frame( alpha = c(.1,.2,.5,1,1.5,3) , lty = c(1,2,3,4,5,6))
  for (i in seq(nrow(df))){
    row <- df[i,]
    lines(pvec,sapply(pvec,function(y) { karmakar(p = y, alpha = row$alpha) } ), type='l' , lty=row$lty)
  }
  
  legend(.8, .4, legend=TeX(paste("$\\alpha$=",df$alpha)), lty=df$lty, cex=0.8)
}