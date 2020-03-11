
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

