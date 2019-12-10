
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

#arr = seq(1,16,.2) ; res = sapply(arr, function(x) { ns_runsim(nu = x,N = 10,alpha = .2)$totu }); plot (arr,res)
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
  retlist[["u"]] = ns_u(arrA = A,arrc = ct,arreta = eta)
  retlist[["t"]] = seq(N)
  retlist[["totu"]] = sum(retlist[["u"]])
  
  
  
  #retlist[["u"]] = u
  return(retlist)
}
