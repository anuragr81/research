
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

ns_c <- function(A,nu,eta,alpha,a) {
  return(A**(alpha)*a*(1+nu)*eta)
}
ns_next_A <- function(A,income,alpha,a,nu,eta){
  ct  <- ns_c(A=A,eta=eta,alpha=alpha,a=a,nu=nu)
  return (A + income - ct)
}

ns_income_next <- function(income,k){
  return (k*income)
}

ns_runsim <- function(nu,N){
  A = array()
  ct = array()
  eta = array()
  i = array()
  gk = 1.02
  alpha = .1
  a = .05

  A[1] = 1
  eta[1] = 1
  i[1] = 10

  for (k in seq(1,N-1)){

    A[k+1] <- ns_next_A(A = A[k], income = i[k], alpha = alpha, a =a, nu = nu, eta = eta[k])
    i[k+1] <- ns_income_next(income=i[k], k=gk)
    eta[k+1] <- ns_eta_next(t=k, T=N)
    ct[k] <-    ns_c(A = A[k],nu = nu, eta = eta[k],alpha = alpha, a = a)
  }
  #u <- allu(arrA = A, arrn = n, arrGamma = G)
  retlist = list()
  retlist[["A"]] = A
  retlist[["c"]] = ct
  retlist[["i"]] = i
  retlist[["t"]] = seq(N)
  #retlist[["u"]] = u
  return(retlist)
}