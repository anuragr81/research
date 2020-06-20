
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


evolve_relative_wealth_discrete <-function(nsim,delta,sigma1,sigma2, risksz, p){
  
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

analyse_relative_wealth_results <- function(res){
  # assume that there is infinite credit available thre is an optimum to be achieved
  print(colMeans(res[["A1"]]))
  print(colMeans(res[["A2"]]))
  
}