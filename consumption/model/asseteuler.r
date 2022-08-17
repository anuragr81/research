require(latex2exp)
#require(dplyr)
require(plyr)
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

J_nu <- function(t,a,b,bound) {
  return(bound * (exp(a*(t-b))/(1+exp(a*(t-b)))))
}

expected_asset_value <- function(B0,B1,y0,y1,nu,ja,jb,pb){
  p = J_nu(t=nu,a=ja,b=jb,bound=pb)
  eval = p *( B1 + (y1- nu) ) + (1-p)*(B0 + (y0- nu) )
  return(eval)
}

plot_discrete_band_scenarios <- function(B0,B1,y0,y1,ja,pb){
  par(mfrow=c(1,2))
  x <- seq(0,y1,.1); 
  plot(x,J_nu(t=x,a=ja,b=(y1+y0)/2,bound = pb),ylab="J_nu",type='l')
  plot(x,expected_asset_value(B0=B0,B1=B1,nu=x,y0=y0,y1=y1,ja=ja,jb=(y1+y0)/2,pb=pb),ylab="expected AV",type='l')
  
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

get_sigma_array<-function(xarr, decay = 0.99){
  # param xarr - the time array over which the decay happens
  sigma_array=array()
  sigma<- 1
  for (i in seq(length(xarr))){
    sigma <- decay*sigma
    sigma_array[i] <- sigma
  }
  return(sigma_array)
}

#TEST with:
# At any point, there is a certain amount of effort that has already been done
#arr=array(); total_x = 0; dx = .1; for ( i in seq(10000)) { arr[i]=sigma_func_decline(start_p=.5,decay_factor=1e-2, past_psi=total_x,0) ; total_x = total_x + dx  }; par(mfrow=c(1,1)); plot(arr,type='l')
sigma_func_decline <- function(start_p,decay_factor, past_psi,psi){
  start_p * exp(-decay_factor * (past_psi + psi ))
}
#TEST with:
#arr=array(); total_x = 0; dx = .1; for ( i in seq(10000)) { arr[i]=sigma_func_rise(start_p=.5,decay_factor=1e-2, end_p=.9,past_psi=total_x,psi=0) ; total_x = total_x + dx  }; par(mfrow=c(1,1)); plot(arr,type='l')
sigma_func_rise <- function(start_p,end_p, decay_factor, past_psi,psi){
  start_p  + (end_p-start_p)*(1-exp(-decay_factor * (past_psi + psi)))
}

#big size risksz would enable psi1 >0 and/or psi2>0
# NEED MORE SIMULATIONS - It seems we're hitting the boundary a lot.
#y <-evolve_relative_wealth_discrete_natural(nsim = 3000, delta = 0 ,alpha1 = 3, alpha2 = .1,risksz = 10,T = 1,dt = .01,A1_init = 10, A2_init = 100,decay = .9,start_p = .5, gamma=.7, lambda = 10, A_costs_factor = 0); 
# scenarios: 
# rich-guy low-alpha (regular overvalue-low-p but undervalue-high-p guy), no-asset costs, no taxes, starts with high-risk size 10 and start_p = .5- rich becomes super-rich for a bit gets back to zero  - poorer risk-averse always remains low.
# rich-guy high-alpha (super-risk-seeking overvalue-high-p and ignore-low-p guy),  no-asset costs, no taxes, starts with high-risk size 10 and start_p = .5-  

evolve_relative_wealth_discrete_natural <-function(nsim,delta,alpha1,alpha2,gamma, lambda, risksz, T, dt, A1_init, A2_init, decay, start_p, A_costs){
  
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
  if (missing(decay)){
    decay <- .9
  }
  if (missing(start_p)){
    start_p <- .7
  }
  
  if (missing(A_costs)){
    A_costs <- 0
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
    
    sigma <- start_p
    sigma_arr <- array()
    
    for (i in timepoints)
    {  
      
      inc  = - delta * (A1-A2)
      #psi should be chosen so that immediate gain u under risk is optimised
      
      
      # if K = inc* dt -A_costs*A - ((A1+A2)/2)
      # the choice would be argmin { w(sigma,alpha)*pt_value( K - psi + psi*risksz,gamma,lambda)+  w(1-sigma,alpha)* pt_value (K -psi,gamma,lambda) }
      D1 = A1 + inc* dt -A_costs*A1
      D2 = A2 - inc* dt -A_costs*A2
      K1 = D1 - ((A1+A2)/2)
      K2 = D2 - ((A1+A2)/2)
      
      optim_func <- function(psi,K,risksz,sigma0,gamma,lambda,alpha){
        # we cannot assume that the payoff would be psi*risksz when dW=1
        # karmakar(sigma,alpha) is fine because the  bet-size doesn't influence the weighting the -probability itself is of course dependent of which both effort psi and the state itself
        return (karmakar(sigma_func,alpha)*pt_value(K - psi + psi*risksz,gamma,lambda)+  karmakar(1-sigma,alpha)* pt_value (K -psi,gamma,lambda) )
      }
      
      #psivec <- seq(-100,100,.1)  ; plot(psivec, sapply(psivec, function(x) { optim_func(psi=x,K=K1,risksz=risksz,gamma=gamma,sigma=sigma,lambda=lambda,alpha=alpha1) }),type='l' )
      # consumer always makes the decision before the draw
      psi1 <- optimise(function(x) { -optim_func(psi=x,K=K1,risksz=risksz,sigma0=sigma,gamma=gamma,lambda=lambda,alpha=alpha1) },c(0,D1))$minimum
      psi2 <- optimise(function(x) { -optim_func(psi=x,K=K2,risksz=risksz,sigma0=sigma,gamma=gamma,lambda=lambda,alpha=alpha2) },c(0,D2))$minimum
      
      if ( (D1-psi1) <.1 ){
        message = "All D1 spent"
      }
      if ( (D2-psi2) <.1 ){
        message = "All D2 spent"
      }
      
      dW1 = rbinom(1,1,sigma) 
      dW2 = rbinom(1,1,sigma)
      
      # natural decay
      sigma_arr[count] <- sigma
      if (dW1 + dW2 >0){
        if (dW1+dW2==1){
          sigma <- decay*sigma
        } else if (dW1+dW2 == 2 ) {
          sigma <- decay*decay*sigma
        } else {
          stop("Cannot have more than two decays")
        }
      }
      
      A1 = D1 -psi1 + psi1 * risksz* dW1
      A2 = D2 -psi2 + psi2 * risksz* dW2
      
      #if (A1<0){
      #  A1 <- 0
      #}
      #if (A2<0){
      #  A2 <- 0 
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
  max_y <- max( max(colMeans(retlist$A1)) , colMeans(retlist$A2) )
  min_y <- min( min(colMeans(retlist$A1)) , colMeans(retlist$A2) )
  
  par(mfrow=c(2,2)); 
  ptimepoints <- c(timepoints,timepoints[length(timepoints)]+dt)
  plot(ptimepoints,colMeans(retlist$A2)-colMeans(retlist$A1),type='l', main="A2-A1", xlab="T", ylab="A2-A1"); 
  plot(ptimepoints,colMeans(retlist$A1),type='l',ylim=c(min_y,max_y),main="A1", xlab="T", ylab="A1"); 
  plot(ptimepoints,colMeans(retlist$A2),type='l',ylim=c(min_y,max_y),main="A2", xlab="T", ylab="A2")
  plot(timepoints,sigma_arr,type='l',main="natural probability", xlab="T")
  return(retlist)
}

sample_run <- function(){
  df <- data.frame(x=c(10,50,100,150,200),y=c(490,450,400,350,300))
  rise_func <- function(start_p,decay_factor,past_psi,psi){ sigma_func_rise (start_p=start_p,decay_factor=decay_factor,past_psi=past_psi,psi=psi,end_p=start_p+.3)}
  for (i in seq(nrow(df)))
  { 
    frow=df[i,];
    
    x <- evolve_relative_wealth_discrete_contnatural(nsim = 2000, delta = 0 ,alpha1 =.3, alpha2 = 2, risksz = 10,
                                                     T = 1,dt = .001,A1_init=frow$x, A2_init=frow$y,decay = .01,
                                                     start_p = .2, gamma=.7, lambda = 10, A_costs_factor = 0., plot_range = F, sigma_func=rise_func)
    
    #x <- evolve_relative_wealth_discrete_contnatural(nsim = 3000, delta = 0 ,alpha1 =2, alpha2 = .1, risksz = 10,
    #                                                 T = 1,dt = .005,A1_init=frow$x, A2_init=frow$y,decay = .01,
    #                                                 start_p = .2, gamma=.7, lambda = 10, A_costs_factor = 500, plot_range = F, sigma_func=sigma_func_decline)
    print(paste("(",frow$x,",",frow$y,") A1:",tail(colMeans(x$A1),1),"A2:",tail(colMeans(x$A2),1)))
    
    if(abs(tail(diff(colMeans(x$A1)),1) /tail(colMeans(x$A1),1)) >5e-2){
      #stop("A1")
    } 
    if(abs(tail(diff(colMeans(x$A2)),1) /tail(colMeans(x$A2),1)) >5e-2){
      #stop("A2")
    }
  }
}


taxes_due_for_1 <- function(delta, A1,A2){
  return (delta * (A1-A2))
}

taxes_due_for_2 <- function(delta, A1,A2){
  return (-delta * (A1-A2))
}


reference_point <- function(A1,A2){
  return ((A1+A2)/2)
}

disposable_income <- function(A, A_costs_factor, tax_func){
  if (A_costs_factor==0){
    return(A - tax_func(A) )
  }
  else{
    return(A - tax_func(A) -(A**2.25)/(A_costs_factor**1.5))
  }
  
}

optim_func <- function(psi,A,risksz,start_p,total_psi,gamma,lambda,alpha,tax_func, A_costs_factor,ref_pt,decay){
  # we cannot assume that the payoff would be psi*risksz when dW=1
  # we can assume that the consumer considers p-state only considering her own effort - given that she can't be sure about what psi would be chosen by the other (the other
  # option is for us to assume that the consumers assumes an expected value of psi - or the last value of psi in the previous draw)
  # karmakar(sigma,alpha) is fine because the  bet-size doesn't influence the weighting the -probability itself is of course dependent of which both effort psi and the state itself
  
  upside_relative = disposable_income(A = A - psi + psi*risksz, A_costs_factor = A_costs_factor, tax_func = tax_func) -ref_pt
  downside_relative = disposable_income(A = A - psi, A_costs_factor = A_costs_factor, tax_func = tax_func) -ref_pt
  
  p = sigma_func(start_p=start_p,decay_factor=decay, past_psi = total_psi,psi= psi)
  wp = karmakar(p,alpha) 
  w1minusp = karmakar(1-p,alpha)
  return (wp*pt_value(upside_relative,gamma,lambda)+  w1minusp* pt_value (downside_relative,gamma,lambda) )
}

#evolve_relative_wealth_discrete_contnatural(nsim = 1000, delta = 0 ,alpha1 = 3, alpha2 = .1,risksz = 10,T = 1,dt = .01,A1_init = 10, A2_init = 100,decay = .9,start_p = .5, gamma=.7, lambda = 10, A_costs_factor = 0, plot_range=F, sigma_func=sigma_func_decline)
evolve_relative_wealth_discrete_contnatural <-function(nsim,delta,alpha1,alpha2,gamma, lambda, risksz, T, dt, A1_init, A2_init, decay, start_p, A_costs_factor, plot_range,sigma_func){
  
  print(paste("delta=",delta,"alpha1=",alpha1,"alpha2=",alpha2,"gamma=",gamma, "lambda=",lambda, "risksz=",risksz, "T=",T, "dt=",dt, "A1_init=",A1_init, "A2_init=",A2_init, "decay=",decay, "start_p=",start_p, "A_costs_factor=",A_costs_factor))
  
  if (missing(plot_range)){
    plot_range <- F
  }
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
  if (missing(decay)){
    decay <- .9
  }
  if (missing(start_p)){
    start_p <- .7
  }
  
  if (missing(A_costs_factor)){
    A_costs_factor <- 0
  }
  
  A1df <- data.frame()
  A2df <- data.frame()
  incdf <- data.frame()
  sigmadf <- data.frame()
  psi1df <- data.frame()
  psi2df <- data.frame()
  
  for ( j in seq(nsim)){
    A1 = A1_init
    A2 = A2_init
    A1_arr = array()
    A2_arr = array()
    inc_arr = array()
    psi1_arr <- array()
    psi2_arr <- array()
    
    count <- 1
    A1_arr[count] = A1_init
    A2_arr[count] = A2_init
    inc_arr[count] = 0
    psi1_arr[count] = 0
    psi2_arr[count] = 0
    
    timepoints <- seq(0,T,dt)
    total_psi <- 0
    
    sigma_arr <- array()
    
    for (i in timepoints)
    {  
      
      #psi should be chosen so that immediate gain u under risk is optimised
      
      D1 = disposable_income(A = A1,A_costs_factor = A_costs_factor, tax_func = function(x) {taxes_due_for_1(delta=delta, A1=x,  A2=A2)})
      D2 = disposable_income(A = A2,A_costs_factor = A_costs_factor, tax_func = function(x) {taxes_due_for_2(delta=delta, A1=A1, A2=x) })
      
      ref_pt = reference_point(A1=A1,A2=A2)
      
      # consumer always makes the decision before the draw
      psi1 <- optimise(function(x) { -optim_func(decay=decay,psi=x,A=A1,risksz=risksz,start_p=start_p,total_psi=total_psi,gamma=gamma,lambda=lambda,alpha=alpha1,tax_func=function(x) {taxes_due_for_1(delta=delta, A1=x,A2=A2)}, ref_pt= ref_pt, A_costs_factor=A_costs_factor) },c(0,D1))$minimum
      psi2 <- optimise(function(x) { -optim_func(decay=decay,psi=x,A=A2,risksz=risksz,start_p=start_p,total_psi=total_psi,gamma=gamma,lambda=lambda,alpha=alpha2,tax_func=function(x) {taxes_due_for_2(delta=delta, A1=A1,A2=x)}, ref_pt= ref_pt, A_costs_factor=A_costs_factor) },c(0,D2))$minimum
      
      if (plot_range){
        par(mfrow=c(2,1))
        psivec1 <- seq(0-10,D1+200,.1)  ; plot(psivec1, sapply(psivec1, function(x) { optim_func(decay=decay,psi=x,A=A1,risksz=risksz,start_p=start_p,total_psi=total_psi,gamma=gamma,lambda=lambda,alpha=alpha1,ref_pt=ref_pt, A_costs_factor=A_costs_factor, tax_func=function(x) {taxes_due_for_1(delta=delta, A1=x,A2=A2)}) }),type='l' , main=paste0("argmax=",round(psi1,2)))
        psivec2 <- seq(0-10,D2+200,.1)  ; plot(psivec2, sapply(psivec2, function(x) { optim_func(decay=decay,psi=x,A=A2,risksz=risksz,start_p=start_p,total_psi=total_psi,gamma=gamma,lambda=lambda,alpha=alpha2,ref_pt=ref_pt, A_costs_factor=A_costs_factor, tax_func=function(x) {taxes_due_for_2(delta=delta, A1=A1,A2=x)}) }),type='l', main=paste0("argmax=",round(psi2,2)))
      }
      if ( (D1-psi1) <.1 ){
        message = "All D1 spent"
      }
      if ( (D2-psi2) <.1 ){
        message = "All D2 spent"
      }
      
      cur_sigma = sigma_func(start_p=start_p,decay_factor=decay, past_psi = total_psi,psi= 0)
      dW1 = rbinom(1,1,cur_sigma) 
      dW2 = rbinom(1,1,cur_sigma)
      
      
      # natural decay
      sigma_arr[count] <- cur_sigma
      # sigma change based on total psi 
      combined_effort = dW1*psi1 + dW2*psi2
      total_psi = total_psi  + combined_effort
      
      A1 = D1 -psi1 + psi1 * risksz* dW1
      A2 = D2 -psi2 + psi2 * risksz* dW2
      
      count <- count+1
      A1_arr[count] <- A1
      A2_arr[count] <- A2
      inc_arr[count] <- taxes_due_for_1(delta=delta, A1=A1,  A2=A2)
      psi1_arr[count] <- psi1
      psi2_arr[count] <- psi2
      
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
    
    psi1add <- t(data.frame(x=psi1_arr))
    colnames(psi1add) <- paste0("t_",c(timepoints, T+dt))
    psi1df <- rbind(psi1df,psi1add)
    
    psi2add <- t(data.frame(x=psi2_arr))
    colnames(psi2add) <- paste0("t_",c(timepoints, T+dt))
    psi2df <- rbind(psi2df,psi2add)
    
    sigmaadd <- t(data.frame(x=sigma_arr))
    colnames(sigmaadd) <- paste0("t_",c(timepoints))
    sigmadf <- rbind(sigmadf,sigmaadd)
    
  }
  retlist = list()
  retlist[["A1"]] <- A1df
  retlist[["A2"]] <- A2df
  retlist[["inc"]] <- incdf
  retlist[["psi1"]] <- psi1df
  retlist[["psi2"]] <- psi2df
  
  max_y <- max( max(colMeans(retlist$A1)) , colMeans(retlist$A2) )
  min_y <- min( min(colMeans(retlist$A1)) , colMeans(retlist$A2) )
  
  par(mfrow=c(2,3)); 
  ptimepoints <- c(timepoints,timepoints[length(timepoints)]+dt)
  plot(ptimepoints,colMeans(retlist$A2)-colMeans(retlist$A1),type='l', main=latex2exp::TeX("$A_2-A_1$"), xlab="T", ylab=latex2exp::TeX("$A_2-A_1$")); 
  if (alpha1>alpha2){
    tag_A1  <- "RiskAverse"  
  } else{
    tag_A1  <- "RiskSeeking"  
  }
  if (alpha2>alpha1){
    tag_A2  <- "RiskAverse"  
  } else{
    tag_A2  <- "RiskSeeking"  
  }
  
  tag_A1=""
  tag_A2=""
  plot(ptimepoints,colMeans(retlist$A1),type='l',ylim=c(min_y,max_y),main=latex2exp::TeX(paste0("$A_1(\\alpha_1=",alpha1,")$",tag_A1)), xlab="T", ylab=latex2exp::TeX("$A_1$")); 
  plot(ptimepoints,colMeans(retlist$A2),type='l',ylim=c(min_y,max_y),main=latex2exp::TeX(paste0("$A_2(\\alpha_2=",alpha2,")$",tag_A2)), xlab="T", ylab=latex2exp::TeX("$A_2$"))
  plot(timepoints,colMeans(sigmadf),type='l',main=latex2exp::TeX("$p$"), xlab="T", ylab="s")
  plot(ptimepoints,colMeans(psi1df),type='l',main=latex2exp::TeX("$\\psi_1$"), xlab="T", ylab="s")
  plot(ptimepoints,colMeans(psi2df),type='l',main=latex2exp::TeX("$\\psi_2$"), xlab="T", ylab="s")
  return(retlist)
}

crra <- function(A,gamma){
  if (gamma <0 || gamma >1 ){
    stop("Invalid CRRA parameter")
  }
  return ( (A**gamma-1)/(1-gamma))
}

evolve_plain_asset_crra <-function(nsim, p, T, dt, A_init,gamma, nu){
  # use the function sigma_func to evolve the asset and compare with theoretical mean
  
  A<- A_init
  
  sigma_func <- function(x){
    return(sqrt(x))
  }
  
  Adf <- data.frame()
  for ( j in seq(nsim)){
    A = A_init
    A_arr = array()
    
    count <- 1
    A_arr[count] = A_init
    
    #timepoints <- seq(0,T,dt)
    timepoints <- c(1)
    
    for (i in timepoints)
    {  
      dW = rbinom(1,1,p)
      
      dA = -nu +  sigma_func(nu) * dW
      if (A + dA <0 ){
        A <- 0 
      } else {
        A = A + dA
      }
      
      count <- count+1
      A_arr[count] <- A
      
    }
    Aadd <- t(data.frame(x=A_arr))
    colnames(Aadd) <- paste0("t_",c(timepoints, T+dt))
    
    Adf <- rbind(Adf,Aadd)
    
    
  }
  print(paste("Theortical Mean:",A_init-nu+sigma_func(nu)*p))
  print(paste("Final Value:",colMeans(Adf)[-1]))
  print(paste("Final Util:",crra(colMeans(Adf)[-1],gamma )))
  return (Adf)
  
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

get_vara_df <- function(){
  df <- data.frame()
  df <- rbind(df,data.frame(a  = .6, b = 2, lty=1))
  df <- rbind(df,data.frame(a  = .8, b = 2 , lty=2))
  df <- rbind(df,data.frame(a  = 1.0, b = 2 , lty=3))
  df <- rbind(df,data.frame(a  = 1.2, b = 2 , lty=4))
  df <- rbind(df,data.frame(a  = 1.4, b = 2 , lty=5))
  return (df)
}

get_varb_df <- function(){
  
  df <- data.frame()
  df <- rbind(df,data.frame(a  = 1, b = 2.2, lty=1))
  df <- rbind(df,data.frame(a  = 1, b = 2.4 , lty=2))
  df <- rbind(df,data.frame(a  = 1, b = 2.6 , lty=3))
  return (df)
}

plot_piglog_vara <- function(maxX,maxY){
  if (missing(maxX)){
    maxX = 100
  }
  if (missing(maxY)){
    maxY = 10
  }
  plot_piglog(maxX=maxX,maxY=maxY,df=get_vara_df())
}

plot_piglog_varb <- function(maxX,maxY){
  if (missing(maxX)){
    maxX = 100
  }
  if (missing(maxY)){
    maxY = 10
  }
  plot_piglog(maxX=maxX,maxY=maxY,df=get_varb_df())
}

plot_piglog <- function(maxX,maxY,df){
  
  x<-seq(.01,maxX,.01); 
  plot(0,0,xlim = c(0,maxX), ylim=c(0,maxY), type='l',main=latex2exp::TeX("$u=\\frac{log(x)-a}{b-a}$"), xlab="x", ylab="u")
  
  for (i in seq(nrow(df))){
    lines(x,(log(x)-df$a[i])/(df$b[i]-df$a[i]),type='l',lty=df$lty[i]);
  }
  
  legend(10, 2, legend=paste("a=",df$a, "b=",df$b), lty=df$lty, cex=0.7)
  
}

optim_pt_func <- function(p,alpha,ref,nu,x,chi,phi,g,l){
  return (karmakar(p,alpha)*pt_value(chi + P_nu(nu=nu) - ref,gamma=g,lambda=l)+  karmakar(1-p,alpha)* pt_value (S_xi(xi=x-nu)-phi-ref,gamma=g,lambda=l) )
} 
optim_eu_func <- function(p,nu,x,chi,phi){
  return (p*(chi + P_nu(nu=nu))+  (1-p)* (S_xi(xi=x-nu)-phi) )
}
optim_crra_func <- function(p,nu,x,chi,phi,g){
  return (p*crra(chi + P_nu(nu=nu),gamma = g)+  (1-p)* crra(S_xi(xi=x-nu)-phi,gamma = g) )
}
P_nu <- function(nu) { 100*(nu**.3) ; }
S_xi <- function(xi) { 100*(xi**.2); }

iota <- function(x,alpha,r,g_pt,l_pt,p_draw,chi,phi){
  nu_pt <- optimise(function(y) { -optim_pt_func(g=g_pt,l=l_pt,x=x,nu=y,alpha=alpha, ref= r, p = p_draw,chi=chi,phi=phi) },c(0,x))$minimum;
  return(nu_pt)
}
J_cons <- function(x,alpha,r,g_pt,l_pt,p_draw,chi,phi) {
  
  nu_pt <- iota(x=x,alpha=alpha,r=r,g_pt=g_pt,l_pt=l_pt,p_draw=p_draw,chi=chi,phi=phi)
  
  expected_result = optim_eu_func(p=p_draw,chi=chi,phi=phi,x=x,nu=nu_pt)
  return(expected_result)
}

optim_pt_u <- function(x,alpha,r,g_pt,l_pt,p_draw,chi,phi) {
  nu_pt = iota(x=x,alpha=alpha,r=r,g_pt=g_pt,l_pt=l_pt,p_draw=p_draw,chi=chi,phi=phi)
  max_u = optim_pt_func(g=g_pt,l=l_pt,x=x,nu=nu_pt,alpha=alpha, ref= r, p = p_draw,chi=chi,phi=phi)
  return(max_u)
}
plot_u <- function()
{
  # Lottery definitions
  
  CHI=100
  PHI=150
  p_draw = .2
  
  par(mfrow=c(1,1))
  # plot settings 
  g_pt = .88
  l_pt = 2.25
  refs <- c(-200,0,200)
  base_ref <- 100
  alphas <- c(.2,1,2,5)
  x_arr <- seq(.1,20,.01)
  
  us <- sapply(x_arr, function (y) {optim_pt_u(x = y,alpha = alphas[1],r = base_ref,g_pt = g_pt,l_pt = l_pt,p_draw = p_draw,chi = CHI, phi=PHI)}) 
  us_diff <- us - p_draw*CHI + (1-p_draw)*(-PHI)
  plot(x_arr,us_diff,type='l')
}
plot_Js <-function(){
  # Lottery definitions
  
  CHI=100
  PHI=150
  p_draw = .2
  
  # plot settings 
  g_pt = .88 # .2 for kinks
  l_pt = 2.25
  
  
  base_alpha_1= .2
  base_alpha_2= 1
  base_alpha_3= 2
  x_arr <- seq(.1,200,.1)
  
  base_ref <- 200
  cex_zoom_setting <- .65
  #plot(0,0,type='l')
  refs <- c(-200,0,200)
  alphas <- c(.2,1,2,5)
  ltys <- c(1,2,3,4,5,6)
  par(mfrow=c(2,2))
  legend_y= 50
  
  plot(x_arr,sapply(x_arr, function(y) { J_cons(x = y,alpha = base_alpha_1,r = refs[1],p_draw = p_draw,chi = CHI,phi = PHI,g_pt = g_pt,l_pt = l_pt) } ),type='l',lty=ltys[1], main=TeX(paste("$\\alpha$=",base_alpha_1)), xlab="x",ylab="J(x)")
  lines(x_arr,x_arr)
  for ( i in seq(2,length(refs))){
    
    lines(x_arr,sapply(x_arr, function(y) { J_cons(x = y,alpha = base_alpha_1,r = refs[i],p_draw = p_draw,chi = CHI,phi = PHI,g_pt = g_pt,l_pt = l_pt) } ),type='l',lty=ltys[i], main=TeX(paste("$\\alpha$=",base_alpha_1)), xlab="x",ylab="J(x)")
  }
  legend(100, legend_y, legend=paste("r=",refs), lty=ltys, cex=cex_zoom_setting)
  
  
  plot(x_arr,sapply(x_arr, function(y) { J_cons(x = y,alpha = base_alpha_2,r = refs[1],p_draw = p_draw,chi = CHI,phi = PHI,g_pt = g_pt,l_pt = l_pt) } ),type='l',lty=ltys[1], main=TeX(paste("$\\alpha$=",base_alpha_2)), xlab="x",ylab="J(x)")
  lines(x_arr,x_arr)
  for ( i in seq(2,length(refs))){
    
    lines(x_arr,sapply(x_arr, function(y) { J_cons(x = y,alpha = base_alpha_2,r = refs[i],p_draw = p_draw,chi = CHI,phi = PHI,g_pt = g_pt,l_pt = l_pt) } ),type='l',lty=ltys[i], main=TeX(paste("$\\alpha$=",base_alpha_2)), xlab="x",ylab="J(x)")
  }
  legend(100, legend_y, legend=paste("r=",refs), lty=ltys, cex=cex_zoom_setting)
  
  
  plot(x_arr,sapply(x_arr, function(y) { J_cons(x = y,alpha = base_alpha_3,r = refs[1],p_draw = p_draw,chi = CHI,phi = PHI,g_pt = g_pt,l_pt = l_pt) } ),type='l',lty=ltys[1], main=TeX(paste("$\\alpha$=",base_alpha_3)), xlab="x",ylab="J(x)")
  lines(x_arr,x_arr)
  for ( i in seq(2,length(refs))){
    
    lines(x_arr,sapply(x_arr, function(y) { J_cons(x = y,alpha = base_alpha_3,r = refs[i],p_draw = p_draw,chi = CHI,phi = PHI,g_pt = g_pt,l_pt = l_pt) } ),type='l',lty=ltys[i], main=TeX(paste("$\\alpha$=",base_alpha_3)), xlab="x",ylab="J(x)")
  }
  legend(100, legend_y, legend=paste("r=",refs), lty=ltys, cex=cex_zoom_setting)
  
  
  plot(x_arr,sapply(x_arr, function(y) { J_cons(x = y,alpha = alphas[1],r = base_ref,p_draw = p_draw,chi = CHI,phi = PHI,g_pt = g_pt,l_pt = l_pt) } ),type='l',lty=ltys[1], main=TeX(paste("r=",base_ref)), xlab="x",ylab="J(x)")
  lines(x_arr,x_arr)
  for ( i in seq(2,length(alphas))){
    
    lines(x_arr,sapply(x_arr, function(y) { J_cons(x = y,alpha = alphas[i],r = base_ref,p_draw = p_draw,chi = CHI,phi = PHI,g_pt = g_pt,l_pt = l_pt) } ),type='l',lty=ltys[i], main=TeX(paste("r=",base_ref)), xlab="x",ylab="J(x)")
  }
  
  
  legend(100, legend_y, legend=latex2exp::TeX(paste("$\\alpha$=",alphas)), lty=ltys, cex=cex_zoom_setting)
  
  
}

evolve_lottery_maker_account<-function(nsims, ref_start,pt_g,pt_lambda,x1_start,alpha1,alpha2,M, r,R){
  #evolve_lottery_maker_account(ref_start = 0,pt_g = .88,x1_start = 80,alpha1 = .2,pt_lambda = 2.25 ) gets 738 as expected return but 
  #evolve_lottery_maker_account(ref_start = 0,pt_g = .88,x1_start = 120,alpha1 = 5,pt_lambda = 2.25 ) would get just 699.63 despite higher x
  
  #with simulation of lottery makers's account : evolve_lottery_maker_account(ref_start = 0,pt_g = .88,x1_start = 280,alpha1 = .2,pt_lambda = 2.25,nsims = 1 ,alpha2 = .2,M=10) 
  
  # g = .88 and lambda=2.25 are the PT (KT) recommended values
  #par(mfrow=c(2,1)); xvec <- seq(0,100,.1); plot(xvec,sapply(xvec,function(x){P_nu(x)),type='l'); plot(xvec,sapply(xvec,function(x){S_xi(x)}),type='l')
  p_draw <- .3
  
  
  l = 2
  crra_g <- .6
  CHI = 1e+6
  PHI = 1e+2
  
  #
  x1 <- x1_start
  x2 <- x1_start
  ref <- ref_start
  
  
  nu1_pt <- optimise(function(y) { -optim_pt_func(g=pt_g,l=pt_lambda,x=x1,nu=y,alpha=alpha1, ref= ref, p = p_draw,chi=CHI,phi=PHI) },c(0,x1))$minimum
  nu1_eu <- optimise(function(y) { -optim_eu_func(x=x1,nu=y,p = p_draw,chi=CHI,phi=PHI) },c(0,x1))$minimum
  nu1_crra <- optimise(function(y) { -optim_crra_func(g=crra_g,x=x1,nu=y,p = p_draw,chi=CHI,phi=PHI) },c(0,x1))$minimum
  
  if (FALSE){
    par(mfrow=c(3,1))
    nu_vec <- seq(0,x1,.01)  ; plot(nu_vec, sapply(nu_vec, function(y) { optim_pt_func(g=pt_g,l=l,x=x1,nu=y,alpha=alpha1, ref= ref, p = p_draw,chi=CHI,phi=PHI) }),type='l' ,main=sprintf("%.2f",nu1_pt))
    nu_vec <- seq(0,x1,.01)  ; plot(nu_vec, sapply(nu_vec, function(y) { optim_eu_func(x=x1,nu=y,p = p_draw,chi=CHI,phi=PHI) }),type='l' ,main=sprintf("%.2f",nu1_eu))
    nu_vec <- seq(0,x1,.01)  ; plot(nu_vec, sapply(nu_vec, function(y) { optim_crra_func(g=crra_g,x=x1,nu=y,p = p_draw,chi=CHI,phi=PHI) }),type='l' ,main=sprintf("%.2f",nu1_crra))
  }
  expected_payoff <- optim_eu_func(p = p_draw,nu = nu1_pt, x = x1,chi = CHI,phi = PHI)
  #return(data.frame(nu1_pt=nu1_pt,expected_payoff=expected_payoff))
  
  # The lottery maker must advertise the lottery with the security offered
  # To emphasise the liquidity conditions, the lottery maker must start with no money and just pool resources to bet. We have two options, Gamma and Delta as lottery makers own lottery which is then distributed into P(nu),S(x-nu)
  # from every consumer. The other option is to have a multiplier lottery that literally bets the money taken from the consumers. The former case separate the lottery makers own lottery from the cosumer lottery while 
  # the latter removes the need of the lottery makers own lottery.
  # Only the second way makes sense, because the lottery maker is meant to do something with the nu and xi she gets. The collected money must be invested in a lottery and the second approach provides 
  # the easiest way to do that.
  # So in this latter case, the lottery maker starts with a promised amount M (she does not have this amount already but it is given out after playing the lottery on the amount x she does received )
  # The lottery maker does not use the funds of her own - in fact she takes the money and takes some risk in an investment that would give a return r on the investment in good times and R in bad times
  xres <- NULL
  lmres <- NULL
  
  for (i in seq(nsims)){
    
    #nobody would want to play a risk-neutral lottery - the ratio should be based on r, R rather than p_draw")
    nu1_pt <- optimise(function(y) { -optim_pt_func(g=pt_g,l=pt_lambda,x=x1,nu=y,alpha=alpha1, ref= ref, p = p_draw,chi=(1+r)*M,phi=(1+R)*M) },c(0,x1))$minimum
    nu2_pt <- optimise(function(y) { -optim_pt_func(g=pt_g,l=pt_lambda,x=x2,nu=y,alpha=alpha2, ref= ref, p = p_draw,chi=(1+r)*M,phi=(1+R)*M) },c(0,x2))$minimum
    
    expected_payoff1 <- optim_eu_func(p = p_draw,nu = nu1_pt, x = x1,chi = (1+r)*M,phi = (1+R)*M)
    expected_payoff2 <- optim_eu_func(p = p_draw,nu = nu2_pt, x = x2,chi = (1+r)*M,phi = (1+R)*M)
    
    if (expected_payoff1<x1){
      print(xres)
      print(lmres)
      stop(paste("Player 1 dropped (",expected_payoff1,"<",x1,")"))
    }
    
    if (expected_payoff2<x2){
      print(xres)
      print(lmres)
      stop(paste("Player 2 dropped (",expected_payoff2,"<",x2,")"))
    }
    costs <- costs_per_draw()
    xres <- rbind(xres,(data.frame(nsim=i, x1=x1,nu1_pt=nu1_pt,expected_payoff_1=expected_payoff1,M=M,x2=x2,nu2_pt=nu2_pt,expected_payoff_2=expected_payoff2)))
    
    lmres <- rbind(lmres,(data.frame(nsim=i,P_to_give = p_draw*(P_nu(nu1_pt)+P_nu(nu2_pt)), S_to_give = (1-p_draw)*(S_xi(x1-nu1_pt)+S_xi(x2-nu2_pt)),
                                     received_total = x1 + x2,
                                     average_x = (x1 + x2)/2,
                                     promised_M = M*(1+r)*p_draw - M*(1+R)*(1-p_draw),
                                     accounts_payable = -expected_payoff1 - expected_payoff2,
                                     total_lottery_proceeds = (p_draw)*(x1+x2)*(1+r) - (1-p_draw)*(x1+x2)*(1+R)) %>% mutate(account_value=total_lottery_proceeds + accounts_payable)))
    if (lmres$account_value[length(lmres$account_value)]<0){
      print(xres)
      print(lmres)
      stop("Lottery maker ran out of cash")
    }
    
    x1 <- expected_payoff1 - costs
    x2 <- expected_payoff2 - costs
  }
  print(xres)
  print(lmres)
}


costs_per_draw <- function(x1){
  return (100)
}

my_band <-function(A,A_bands){
  if (any(diff(A_bands)<=0)){
    stop("my_band: expects A_bands with increasing boundaries ")
  }
  
  if (is.atomic(A)){
    mb_ge = A_bands[A_bands >= A]
    ub <- mb_ge[1]
    mb_lt = A_bands[A_bands < A]
    lb <- mb_lt[length(mb_lt)]
    if (length(mb_lt)==0){
      return(c("Lower than lowest A_band"))
    }
    if (length(mb_ge)==0){
      return(c("Higher than highest A_band"))
    } else {
      return(c(lb,ub))
    }
  }else {
    stop("A must be atomic")
  }
}

band_nu_reference <- function(A,A_bands) {
  # returns the reference relevant for A in the list specifying A_bands
  band_lb_ub = my_band(A = A,A_bands = A_bands)
  band_lb = band_lb_ub[1]
  band_ub = band_lb_ub[2]
  
  #arbitrary function for reference
  refval=(band_lb + band_ub)/2
  return(refval)
}

pt_cont_value_func <- function(x){
  # PT like continuous value-function to be used if the consumers obtained utility from nu directly
  exponent_value = 3
  return(x**exponent_value*(2+exp(x))/(1+exp(x)))
} 

probabilistic_nu_jump <- function(x,p){
  #W = rbinom(1,1,p)
  # The function that provides a discrete jump with increasing probability (higher p) for higher consumption
  # The value x normalises the variation in asset-bands by letting the probability function
  #    be the same for all reference levels
  return(rnorm(1)*p)
}

util_nu_A <-function (nu,A,A_bands, ref_func, J_func)
{
  # this is the function to be optimised by the consumer, nu affects consumer choice only through
  # the consumer evaluates current asset based on the probability implied from nu and J
  numYears = 10
  JT= J_func(nu-ref_func(A=A,A_bands=A_bands))
  total_value = 0
  JTProd = 1
  tol = .2
  
  for (i in seq(numYears)) {
    JTProd = JTProd*JT
    next_total_value = total_value+ A * JTProd
    if ((next_total_value/total_value-1)> tol){
      stop("More number of years required for convergence")
    }
    total_value = next_total_value
  }
  
  return(total_value)
}
segmented_asset_band_nu <- function(A_bands, A, ref_func){
  
  #income is directly proportional to asset-bands
  #utility is availed from A and nu-ref. nu is not merely utility but some risk too.
  #having a certain nu-ref influences J which directly rewards with increments in A
  #evaluation of future A is therefore based on nu
  
}

sigmoid_function <-function(x,a,u) {
  return(exp(a*(x-u) )/(1+exp(a*(x-u))))
}

evolve_segmented_asset_bands_uniform_selection_asset_zeroing<-function(T,debug){
  sc <- T
  #band_incomes y
  y <- c(10,100,1000,5000)
  n <- c(10,8,5,2)
  if(missing(debug)){
    debug <- F
  }
  A <- list()
  for (i in seq(y)){
    A [[i]] <- array()
    for (j in seq(n[i])){
      A[[i]][j] <- 0
    }
  }
  
  for (t in seq(T)) {
    if (debug){
      print("=====")
      print(A)
    }
    # increment with nu
    for (i in seq(length(y))){
      candidates <- seq(n[i])
      for (ci in candidates){
        nu <- 0
        A [[i]][ci] <-A[[i]][ci]  + y[i] -nu
      }
    }
    
    if (sc){
      for (i in seq(length(y))){
        candidates <- seq(n[i])
        winner_i <- sample(candidates,1)
        loser_i <- sample(setdiff(candidates,winner_i),1)
        
        if(debug) { 
          print (paste("winner[",i,"]=",winner_i,"losser[",i,"]=",loser_i)) 
        }
        if (i<length(y)){
          #Winner's spot would be taken by the loser
          A[[i]][winner_i] <- 0 
        }
        
        if (i>1){
          #losers spot would be taken by the winner from the lower band
          A[[i]][loser_i] <- 0
        }
        
      }
    }
    
    
    
    
  }
  print("<<<<<<<<>>>>>>>>>>>>")
  print(A)
}


evolve_segmented_asset_bands_uniform_selection<-function(T,nus,sc,debug){
  #alpha <- 10 ; plot(x,plogis(x)**alpha/(plogis(x)**alpha+(1-plogis(x))**alpha))
  
  #A<-0 ; y1 <- 10; y2 <- 20; x <- seq(0,10,.1);plot(x,plogis(x)*log(A+y1-x+y2) + (1-plogis(x)) * (log(A+y1-x+y1)) ,type='l')
  #band_incomes y
  y <- c(10,100,1000,5000)
  n <- c(10,8,5,2)
  if (missing(nus)){
    nus <- c(5,20,200,1000)
  }
  if(missing(debug)){
    debug <- F
  }
  A <- list()
  for (i in seq(y)){
    A [[i]] <- array()
    for (j in seq(n[i])){
      A[[i]][j] <- 0
    }
  }
  
  for (t in seq(T)) {
    if (debug){
      print("=====")
      print(A)
    }
    # increment with nu
    for (i in seq(length(y))){
      candidates <- seq(n[i])
      for (ci in candidates){
        nu <- nus[i]
        A [[i]][ci] <-A[[i]][ci]  + y[i] -nu
      }
    }
    
    if (sc){
      winners <- array()
      losers <- array()
      for (i in seq(length(y))){
        candidates <- seq(n[i])
        winner_i <- sample(candidates,1)
        loser_i <- sample(setdiff(candidates,winner_i),1)
        winners[i] <- winner_i
        losers[i] <- loser_i
      }
      for (i in seq(length(y))){
        if (i<length(y)){
          #Winner's spot would be taken by the loser from the higher band
          # so the winner's assets are now in the next band
          temp = A[[i]][winners[i]]
          
          
          A[[i]][winners[i]] = A[[i+1]][losers[i+1]]
          A[[i+1]][losers[i+1]] = temp
          if(debug){
            print(paste("winners[i]=",winners[i],",losers[i+1]=",losers[i+1],"i=",i))
            print(A)
          }
        }
        
        
      }
    }
    
    
    
    
  }
  print("<<<<<<<<>>>>>>>>>>>>")
  print(A)
}

is_winner <- function(nu_bars, i, selected_nu_fracs, y, sigma) {
  # every band is supposed to have a fixed bar{nu} which controls the probability of selection (we can choose 
  #  to plot the historical plots of nu-bar{nu} to see how the probability function looks).
  #  Notice that we can only talk about about a certain distribution of nu in the reference/band and
  # the consumer's position in it would influence the consumer's fortune.
  
  # When looking what the consumer maximises - i.e. the best outcome - we are not looking for a pareto outcome
  # for all the consumers but onlywhat interests the consumer for a given certain queuing mechanism. This
  # Nash result depends on a certain known/fixed behaviour of other consumers. To fix the consumer behaviour for 
  # all other consumer's we need to be sure about what nu they would choose i.e. the  bar{nu} as well as 
  # the distribution of nu around bar{nu} a consumer face.s
  
  # By simulating we can talk about the final equilibrium that would give us an optimal configuration
  # that maximises outcome for a certain consumer in a certain band. 
  nu <- selected_nu_fracs[i] *y [i]
  bar_nu <- nu_bars[i]
  # nu-nu_bar should generate a certain probabilty and higher nu -nu_bar should give higher winning probability
  # notice that nu/y varies from 0 to 1.
  # lets assume consumers are normally distributed around nu_bar. so a consumer with a certain nu
  # would be selected pnorm number of times if we allow a winner when r > nu_bar (which is the mean)
  # over time a consumer wins n number of times which is the cdf
  # the assumption is that nu/bar{nu} is normally distributed with stdev 1 and mean 1.
  # check that mean(.2 > rnorm(10000000,mean = 0,sd = 1)) is same as pnorm(.2,mean = 0,sd = 1)
  
  # in every draw we are picking a random winner so that the exceptional consumers are found and rewarded
  # as such we would pick anybody but people who have high nu-frac have a higher chance
  
  r <- nu/bar_nu -1 - sigma  # increasing nu increases the chance of winning - decreasing it decreases it
  if (r > rnorm(n=1,mean=0,sd=1)){
    return(T)
  } else{
    return(F)
  }
  
}


is_loser <- function(nu_bars, i, selected_nu_fracs, y , sigma) {
  nu <- selected_nu_fracs[i] *y [i]
  bar_nu <- nu_bars[i]
  
  #increasing nu  decreases r and decreases the chance of losing #
  
  r <- 1-nu/bar_nu- sigma # nu < bar_nu => bar_nu - nu > 0 => 1 - nu/bar_nu > 0 (random greater than this number is less likely )
  # nu > bar_nu => bar_nu - nu < 0 => 1 - nu/bar_nu < 0 (random greater than this number is more likely)
  if (r > rnorm(n=1,mean=0,sd=1)){
    return(T)
  } else{
    return(F)
  }
  
}


evolve_segmented_asset_bands_indiv<-function(T,sigma,debug,selected_nu_fracs,consumer_pos,nu_bars,y){
  # No need for the simulation of A for the entire population - since we assume that rest of the consumers
  # have chose nu that spreads around bar{nu} with a known (normal) distribution
  
  #band_incomes y
  if (missing(y)){
    y <- c(10,100,1000,5000)
  }
  #consumer's current band
  if (missing(consumer_pos)){
    consumer_pos = 1
  }
  #band population
  # n <- c(10,8,5,2)
  
  #band averages nus
  if(missing(nu_bars)){
    nu_bars <- c(5,20,300,4000)
  }
  if(missing(sigma)){
    sigma <- 1
  }
  #consumer strategy in equilibrium
  if(missing(selected_nu_fracs)){
    selected_nu_fracs <- c(.2,.2,.3,.4)
  }
  
  if(missing(debug)){
    debug <- F
  }
  A <- 0
  cur_pos <- consumer_pos
  As <- array()
  is <- array()
  for (t in seq(T)) {
    # if the consumer is a winner
    # then she just exercises her choice being in the next band
    # if she is a loser then she exercises the choice of being in the loser band 
    # in equilibrium pushing her either way would eventually push her back in her final situation
    
    # for the normal distribution, a consumer with high nu would be rewarded more often and 
    # the consumer with low nu would be punished more often
    # we allow sigma on both sides to be safe - expanding sigma suppreses status competitions
    # since a winner would be less likely to and loser less likely to win - since we decrease sigma from both
    # provide this window of saftey
    
    # The winner rises up and losers falls down
    if (is_winner(nu_bars = nu_bars, i = cur_pos, selected_nu_fracs = selected_nu_fracs, y = y , sigma = sigma) ){
      if (cur_pos <length(y)){
        A <- 0 
        cur_pos <- cur_pos + 1 
      }
    } else if(is_loser(nu_bars = nu_bars, i = cur_pos, selected_nu_fracs = selected_nu_fracs, y = y , sigma = sigma) ) {
      if(cur_pos > 1){
        A <- 0
        cur_pos <- 1
      }
    } else {
      A <- A + y[cur_pos] * ( 1- selected_nu_fracs[cur_pos]) 
    }
    As[t] = A
    is[t] = cur_pos
  }
  return(data.frame(t=seq(T),A=As,i=is))
}


test <- function(){
  
  if (F){
    x <- seq(0,100,1); alpha <- .5; G1=1; G2=.9;
    A = 100; y1 = 100; y2=200;
    
    M1=A + y1 + y1
    M2=A + y1 + y2
    par(mfrow=c(3,1)); 
    plot(x,(M1**alpha-(M1-x)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha),type='l'); 
    plot(x,(M1**alpha-(M1-x)**alpha),type='l'); 
    plot(x,(G2*(M2-x)**alpha-G1*(M1-x)**alpha),type='l');
    print((M1**alpha-(M1-x)**alpha))
    print((G2*(M2-x)**alpha-G1*(M1-x)**alpha))
    print((M1**alpha-(M1-x)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha))
  }
  if (F){
    x <- seq(0,100,1); alpha <- .5; G1=1; G2=.9;
    A = 100; y1 = 100; y2=200;
    W0 =.2;
    
    M1=A + y1 + y1
    M2=A + y1 + y2
    
    # Checking W1
    x<- seq(0,y1,.1);
    delta = 0
    A_delta = 0
    W0_adjustment = .5
    
    
    plot(0,0,xlim = c(0,y1),ylim=c(-5,5));
    lines(x,W0*(G2*(M2)**alpha-G1*(M1)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha) + (M1**alpha-(M1-x)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha),type='l',lty=1);
    y1 <- y1 + delta
    
    
    G1_adjustment = 1
    G2_adjustment = 1 #1.01
    
    #y1 <- y1 + delta
    y2 <- y2 + delta
    #A <- A+ A_delta
    M1=A + y1 + y1
    M2=A + y1 + y2
    G1 = G1_adjustment*G1
    G2 = G2_adjustment*G2
    W0 = W0*W0_adjustment
    
    lines(x,W0*(G2*(M2)**alpha-G1*(M1)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha) + (M1**alpha-(M1-x)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha),type='l',lty=2);
    legend(y1/2, -3, legend=c("1","2"), lty=c(1,2), cex=0.7)
  }
  
  if (T){
    x <- seq(0,100,1); alpha <- .5; G1=1; G2=.9;
    A = 100; y1 = 100; y2=200;
    L0 = .2;
    
    M1=A + y1 + y1
    M2=A + y1 + y2
    # Checking L2
    x<- seq(0,y1,.1);
    delta = 0
    A_delta = 0
    G1_adjustment = 1
    G2_adjustment = 1
    L0_adjustment = 2
    
    plot(0,0,xlim = c(0,y1),ylim=c(-5,5));
    lines(x,L0*(G2*(M2)**alpha-G1*(M1)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha) +  (M2**alpha-(M2-x)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha),type='l',lty=1);
    y1 <- y1 + delta
    #y2 <- y2 + delta
    A <- A+ A_delta
    G1 = G1_adjustment*G1
    G2 = G2_adjustment*G2
    L0 = L0*L0_adjustment
    M1=A + y1 + y1
    M2=A + y1 + y2
    lines(x,L0*(G2*(M2)**alpha-G1*(M1)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha) +  (M2**alpha-(M2-x)**alpha)/(G2*(M2-x)**alpha-G1*(M1-x)**alpha),type='l',lty=2);
    legend(y1/2, -3, legend=c("1","2"), lty=c(1,2), cex=0.7)
  }
  
}


W_expr <- function(alpha,G1,G2,y1,y2,A,x,x0,W0){
  M1=A + y1 + y1
  M2=A + y1 + y2
  D = G2*((M2-x)**alpha)- G1*((M1-x)**alpha)
  N1 = W0*(G2*(M2**alpha)-G1*(M1**alpha))
  N2 = G1*((M1-x0)**alpha-(M1-x)**alpha)
  return((N1+N2)/D)
}

W_deriv_expr <- function(alpha,G1,G2,y1,y2,A,x,x0,W0){
  M1=A + y1 + y1
  M2=A + y1 + y2
  
  B = W0 *(G2*(M2**alpha) - G1*(M1**alpha))
  D = G2*((M2-x)**alpha)- G1*((M1-x)**alpha)
  N1 = alpha * G1 * G2 *(M2-M1)*((M1-x)**(alpha-1))*((M2-x)**(alpha-1))
  N2 = alpha * B* (  G2 *( (M2-x)**(alpha-1))  - G1 *( (M1-x)**(alpha-1))  )
  N3 = alpha * G1* G2 * ((M1-x0)**(alpha)) * ((M2-x)**(alpha-1))
  N4 = -alpha * G1*G1 * ((M1-x0)**(alpha)) * ((M1-x)**(alpha-1))
  
  return((N1+N2+N3+N4)/(D**2))
  
}


W_p <- function(omega,x){
  return(1-exp(-omega*x))
}

L_p <- function(lambda,x){
  return(exp(-lambda*x))
}

dW_p <- function(omega,x){
  return(omega*exp(-omega*x))
}

nolog_util <- function (G,A,alpha){
  return(G*A**alpha)
}

expected_A_1<-function(y,y1,y2,alpha,omega,A_0){
  upside <- W_p(omega=omega,x=y) * (A_0+y1-y+y2) 
  downside <- (1-W_p(omega=omega,x=y))*(A_0+y1-y+y1)
  return (upside+downside)
}

expected_A_2<-function(y,y1,y2,alpha,lambda,A_0){
  upside <-  (1-L_p(lambda=lambda,x=y))* (A_0+y1-y+y2) 
  downside <-  L_p(lambda=lambda,x=y) *(A_0+y1-y+y1)
  return (upside+downside)
}

expected_nolog_util_1<-function(y,y1,y2,G1,G2,alpha,omega,A_0){
  upside <- W_p(omega=omega,x=y) * nolog_util(A=A_0+y1-y+y2,G=G2,alpha=alpha) 
  downside <- (1-W_p(omega=omega,x=y))*nolog_util(A=A_0+y1-y+y1,G=G1,alpha=alpha)
  return (upside+downside)
}

expected_nolog_util_2<-function(y,y1,y2,G1,G2,alpha,lambda,A_0){
  downside <- L_p(lambda=lambda,x=y)*nolog_util(A=A_0+y1-y+y1,G=G1,alpha=alpha)
  upside <- (1-L_p(lambda=lambda,x=y)) * nolog_util(A=A_0+y1-y+y2,G=G2,alpha=alpha)
  
  return (upside+downside)
}



d_expected_nolog_util_1 <-function(y,y1,y2,G1,G2,alpha,omega,A_0){
  M1=A_0 + y1 + y1
  M2=A_0 + y1 + y2
  
  first_term <- dW_p(omega=omega,x=y)  * (G2*(M2-y)**alpha - G1*(M1-y)**alpha)
  second_term <-  W_p(omega=omega,x=y) *  alpha * (G1*(M1-y)**(alpha-1) - G2*(M2-y)**(alpha-1))
  third_term <- -alpha * G1*(M1-y)**(alpha-1)
  return (first_term+second_term+third_term)
}

calculate_df_1 <-function(alpha,G1,G2,y1,y2,x,A,x0,W0){
  dWx<- W_deriv_expr(alpha = alpha,G1 = G1,G2 = G2,y1 = y1,y2 = y2,A = A,x = x,x0 = x0,W0 = W0)
  Wx <- W_expr(alpha = alpha,G1 = G1,G2 = G2,y1 = y1,y2 = y2,A = A,x = x,x0 = x0,W0 = W0)
  
  D = G2*((M2-x)**alpha)- G1*((M1-x)**alpha)
  df = D*dWx - alpha * G1 * ((M1-x)**(alpha-1)) + alpha * Wx* (G1*((M1-x)**(alpha-1)) - G2 * ((M2-x)**(alpha-1)))
  
  wpexpr <- (Wx * alpha * (G2*((M2-x)**(alpha-1))-G1*((M1-x)**(alpha-1)))
             + (alpha*G1*((M1-x)**(alpha-1))))/D
  #plot(y,sapply(y,function(x){W_expr(alpha = alpha,G1 = G1,G2 = G2,y1 = y1,y2 = y2,A = A,x = x,x0 = x0,W0 = W0)})/100,type='l')
  return(df)
}
evolve_long_term_w <- function(omega,A,G2_1,n){
  alpha <- .5; G1=1; G2=G2_1*G1;
  # increasing A has a parallel shift
  
  y1 = 300; y2=420;
  
  if (F){
    x <- seq(0,100,1); 
    plot(0,0,xlim = c(0,y1),ylim=c(-5,5));
    lines(x,sapply(x,function(y){W_expr(W0 = W_p(omega=omega,x=0), alpha=alpha,G1 = G1,G2=G2,y1 = y1, y2=y2,A=A,x=y)}),type='l',lty=1)
  }
  r <- seq(0,y1,y1/1000.0)
  
  #plot(r, sapply(r,function(y){expected_nolog_util_1(A_0=A,omega=omega,y=y,y1=y1,y2=y2,G1=G1,G2=G2,alpha=alpha)}),type='l')
  #plot(r, sapply(r,function(y){d_expected_nolog_util_1(A_0=A,omega=omega,y=y,y1=y1,y2=y2,G1=G1,G2=G2,alpha=alpha)}),type='l')
  
  #print(W_expr(W0 = W_p(omega=omega,x=0), alpha=alpha,G1 = G1,G2=G2,y1 = y1, y2=y2,A=A,x=result$minimum) - W_p(omega=omega,x=result$minimum))
  #plot(r,sapply(r,function(y){W_expr(W0 = W_p(omega=omega,x=0), alpha=alpha,G1 = G1,G2=G2,y1 = y1, y2=y2,A=A,x=y)}),type='l')
  #lines(r,sapply(r,function(y){W_expr(W0 = W_p(omega=omega,x=0), alpha=alpha,G1 = G1,G2=G2,y1 = y1, y2=y2,A=A,x=y)}),type='l')
  #lines(r,sapply(r,function(y){W_p(omega=omega,x=y)}),type='l')
  As <- array()
  ys <- array()
  for (i in seq(n)){
    result <- optimise(function(y){-expected_nolog_util_1(A_0=A,omega=omega,y=y,y1=y1,y2=y2,G1=G1,G2=G2,alpha=alpha)},c(0,y1))
    #print(expected_nolog_util_1(A_0=A,omega=omega,y=result$minimum,y1=y1,y2=y2,G1=G1,G2=G2,alpha=alpha))
    A <- expected_A_1(y = result$minimum,y1 = y1,y2 = y2,alpha = alpha,omega = omega  ,A_0 = A)
    As[i]<- A
    ys[i] <- result$minimum
  }
  
  res=list()
  res[["A"]] <- As
  res[["nu"]] <- ys
  par(mfrow=c(1,2))
  plot(ys,type='l')
  plot(As,type='l')
  #return(optimise(function(y){W_p(omega=omega,x=y)-W_expr(W0 = W_p(omega=omega,x=0), alpha=alpha,G1 = G1,G2=G2,y1 = y1, y2=y2,A=A,x=y)},c(0,y1)))
}

evolve_long_term_l <- function(lambda,A,G2_1,n){
  alpha <- .5; G1=1; G2=G2_1*G1;
  y1 = 300; y2=420;
  
  r <- seq(0,y1,y1/1000.0)
  
  As <- array()
  ys <- array()
  for (i in seq(n)){
    
    if(F){
      par(mfrow=c(1,1));    plot(r, sapply(r,function(y){expected_nolog_util_2(A_0=A,lambda=lambda,y=y,y1=y1,y2=y2,G1=G1,G2=G2,alpha=alpha)}),type='l')
    }
    result <- optimise(function(y){-expected_nolog_util_2(A_0=A,lambda=lambda,y=y,y1=y1,y2=y2,G1=G1,G2=G2,alpha=alpha)},c(0,y1))
    
    A <- expected_A_2(y = result$minimum,y1 = y1,y2 = y2,alpha = alpha,lambda = lambda  ,A_0 = A)
    As[i]<- A
    ys[i] <- result$minimum
  }
  
  res=list()
  res[["A"]] <- As
  res[["nu"]] <- ys
  par(mfrow=c(1,2))
  plot(ys,type='l')
  plot(As,type='l')
  #return(optimise(function(y){W_p(omega=omega,x=y)-W_expr(W0 = W_p(omega=omega,x=0), alpha=alpha,G1 = G1,G2=G2,y1 = y1, y2=y2,A=A,x=y)},c(0,y1)))
  return(res)
}

W_p_logis <- function(omega,omega_bar,nu){
  #  return(plogis(q = nu,location = omega_bar, scale = omega))
  #  print(paste("W_p_logis=",nu))
  return (1/(1+exp(-omega * (nu-omega_bar))))
}

L_p_logis <- function(lambda,lambda_bar,nu){
  #return(plogis(q = nu,location = lambda_bar, scale = lambda))
  #  print(paste("L_p_logis=",nu))
  return (1-1/(1+exp(-lambda* (nu-lambda_bar))))
}

k <- function(A,beta,psi){
  return(A-psi*(A**beta))
}

evolve_plogis <- function(A1,A2,omega_bar,omega,lambda_bar,lambda,psi,beta,y1,y2){
  
  
  r <- seq(0,y1,.1)
  w <-  function(nu) { W_p_logis(omega=omega,omega_bar=omega_bar, nu=nu) }
  l <-  function(nu) { L_p_logis(lambda=lambda,lambda_bar=lambda_bar, nu=nu) }
  
  ea_w <- function(nu,A_1) { w(nu) * k (A=A_1 + y2 - nu,psi=psi,beta=beta) + (1-w(nu))*k(A=A_1+y1-nu,psi=psi,beta=beta) }
  ea_l <- function(nu,A_2) { (1-l(nu)) * k (A=A_2 + y2 - nu,psi=psi,beta=beta) + l(nu)*k(A=A_2+y1-nu,psi=psi,beta=beta) }
  
  As_1 <- array()
  ys_1 <- array()
  
  As_2 <- array()
  ys_2 <- array()
  
  
  for (i in seq(100)){
    nu_1 <- optimise(function(x) { -ea_w(x,A_1=A1)},c(0,y1))
    if (F){
      par(mfrow=c(1,1))
      plot(seq(0,y1),sapply(seq(0,y1),function(k){ea_w(nu=k,A_1=A1)}),type='l')
    }
    nu_2 <- optimise(function(x) { -ea_l(x,A_2=A2)},c(0,y2))
    
    new_income_1 <- ea_w(nu_1$minimum,A_1=A1)
    new_income_2 <- ea_l(nu_2$minimum,A_2=A2)
    if (new_income_1<0){
      stop("Cannot have game with negative income")
    }
    A1 <- A1 + new_income_1
    
    ys_1[i] <- nu_1$minimum
    As_1[i]<- A1
    
    ys_2[i] <- nu_2$minimum
    As_2[i]<- A2
    
  }
  
  
  #plot(r,sapply(r,ea),type='l')
  res=list()
  res[["A_1"]] <- As_1
  res[["nu_1"]] <- ys_1
  par(mfrow=c(1,2))
  plot(res[["nu_1"]],type='l')
  plot(res[["A_1"]],type='l')
  
  return(res)
}


polyn_util <- function(A,G,alpha) { 
  #print(paste("polyn_util:,","G=",toString(G),"A=",toString(A),"u=",G* A**alpha)) ; 
  return (G* A**alpha)
}
inv_util <- function(u,G,alpha) { return (u/G)**(1/alpha) }


two_stage_sol <- function(omega_bar,omega,lambda_bar,lambda,G1,G2,y1,y2,alpha,plot){
  
  w <-  function(nu) { W_p_logis(omega=omega,omega_bar=omega_bar, nu=nu) }
  l <-  function(nu) { L_p_logis(lambda=lambda,lambda_bar=lambda_bar, nu=nu) }
  
  E1 <- function(x,y1,y2) { w(x)*y2  + (1-w(x))*y1}
  E2 <- function(x,y1,y2) { l(x)*y1  + (1-l(x))*y2}
  
  # optimise(function(x) { -stage1_ew(x,y1,y2) },c(0,y1))$minimum
  
  #plot(seq(0,y1,.1),sapply(seq(0,y1,.1),function(x){stage1_ew(x = x,y1=y1,y2=y2)}))
  #plot(seq(0,y2,.1),sapply(seq(0,y2,.1),function(x){stage1_el(x = x,y1=y1,y2=y2)}))
  
  Aw_band1 <-function(x) {y1-x+y2-x+E2(x,y1,y2) }
  Al_band1 <-function(x) {y1-x+y1-x+E1(x,y1,y2) }
  
  Aw_band2 <- function(x) {y2-x+y2-x+E2(x,y1,y2) }
  Al_band2 <- function(x) {y2-x+y1-x+E1(x,y1,y2) }
  
  ea_uw = function(x) {  return(w(x)*polyn_util(G=G2,A=Aw_band1(x),alpha=alpha)  + (1-w(x))*polyn_util(G=G1,A=Al_band1(x),alpha=alpha))}
  ea_ul = function(x) {  return(l(x)*polyn_util(G=G1,A=Al_band2(x),alpha=alpha)  + (1-l(x))*polyn_util(G=G2,A=Aw_band2(x),alpha=alpha))}
  
  nu1 <- seq(0,y1,.1)
  nu2 <- seq(0,y2,.1)
  
  par(mfrow=c(1,2))
  nu_1_chosen = optimise(function(x) { -ea_uw(x) },c(0,y1))$minimum
  nu_2_chosen = optimise(function(x) { -ea_ul(x) },c(0,y2))$minimum
  
  
  
  ea_w = function(x) { w(x)*Aw_band1(x)  + (1-w(x))*Al_band1(x)}
  ea_l = function(x) { l(x)*Al_band2(x)  + (1-l(x))*Aw_band2(x)}
  
  kappa = omega/lambda
  if (plot){
    plot(nu1,sapply(nu1,ea_uw),type='l',main=paste('A(nu_1=',round(nu_1_chosen,3),")=",round(ea_w(nu_1_chosen),3)))
    plot(nu2,sapply(nu2,ea_ul),type='l',main=paste('A(nu_2=',round(nu_2_chosen,3),")=",round(ea_l(nu_2_chosen),3)))
  }
  
  return(abs(nu_1_chosen  + kappa* (nu_2_chosen - lambda_bar) - omega_bar))
  
}



plot_missing_vars<-function(){
  xvals <- c(1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.40,1.45)
  yvals <- array()
  for (i in seq(length(xvals))){
    x <- xvals[i]
    sol<- get_missing_var("omega_bar",x)
    if (sol[["error"]]>1e-2){
      stop("No solution")
    }
    yvals[i] <- sol[["lambda_bar_optimised"]]
  }
  plot(xvals,yvals,type='o')
}

get_missing_var<-function(fixed_varname,fixed_varval,plot)
{
  fixed_y1 = 1 
  fixed_y2 = 5
  fixed_alpha = .3
  
  if (fixed_varname=="omega_bar"){
    fixed_lambda=1
    fixed_kappa = 3
    
    fixed_G2 = 1
    fixed_G1 = fixed_varval*fixed_G2
    
    omega_bar_fixed = .5
    #lambda_bar
    find_lambda_bar <- function(lbar){
      two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lbar,lambda=fixed_lambda,G1=fixed_G1,G2=fixed_G2,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
    }
    
    lambda_bar_optimised <- (optimise(function(x){abs(find_lambda_bar(x))},c(0,10)))$minimum
    
    error <- two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_optimised,lambda=fixed_lambda,G1=fixed_G1,G2=fixed_G2,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["lambda_bar_optimised"]] = lambda_bar_optimised
    return(res)
  }
  
  if (fixed_varname=="omega"){
    lambda_bar_fixed = 2
    omega_bar_fixed  = 0
    
    fixed_G2 = 1
    fixed_G1 = fixed_varval*fixed_G2
    fixed_lambda=1
    
    find_kappa <- function(kappa){
      two_stage_sol(omega_bar=omega_bar_fixed,omega=kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,G1=fixed_G1,G2=fixed_G2,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
    }
    
    kappa_optimised <- (optimise(function(x){abs(find_kappa(x))},c(1e-3,1e+3)))$minimum
    
    error <- two_stage_sol(omega_bar=omega_bar_fixed,omega=kappa_optimised*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,G1=fixed_G1,G2=fixed_G2,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["kappa_optimised"]] = kappa_optimised
    return(res)
  }
  
  if (fixed_varname=="contentment"){
    fixed_lambda=1
    fixed_kappa = 3
    
    fixed_G2 = 1
    omega_bar_fixed = fixed_varval
    lambda_bar_fixed = 0
    
    find_G1 <- function(g1){
      two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,G1=g1*fixed_G2,G2=fixed_G2,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
    }
    
    G1_optimised <- (optimise(function(x){abs(find_G1(x))},c(1e-4,1e+2)))$minimum
    error <- two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,G1=G1_optimised*fixed_G2,G2=fixed_G2,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
    res=list()
    res[["error"]] = error
    res[["G1_optimised"]] = G1_optimised
    return(res)
    
  }
  stop("Could not identify fixed_varname")
  
}

plot_w_params<-function(){
  par(mfrow=c(1,1))
  if(T){
    lambda = .1 ; bar_lambda = 50; x<-seq(0,100,.1); 
    plot(x,1/(1+exp(-lambda * (x-bar_lambda))),type='l',xlab=latex2exp::TeX("$\\nu$"),ylab=latex2exp::TeX("$W(\\nu)$"),lty=1); 
    lines(x,1/(1+exp(-lambda*2 * (x-bar_lambda))),type='l',lty=2); 
    lines(x,1/(1+exp(-lambda*.5 * (x-bar_lambda))),type='l',lty=3); legend(60, .4, legend=c(latex2exp::TeX("$ \\omega = 0.1$"),latex2exp::TeX("$ \\omega = 0.2$"),latex2exp::TeX("$ \\omega = 0.05$")) , lty=c(1,2,3), cex=0.7) 
  } else {
    lambda = .1 ; bar_lambda = 50; x<-seq(0,100,.1); plot(x,1/(1+exp(-lambda * (x-bar_lambda))),type='l',xlab=latex2exp::TeX("$\\nu$"),ylab=latex2exp::TeX("$W(\\nu)$"),lty=1); 
    lines(x,1/(1+exp(-lambda*(x-bar_lambda-20))),type='l',lty=2)
    lines(x,1/(1+exp(-lambda*(x-bar_lambda+20))),type='l',lty=3)
    legend(60, .4, legend=latex2exp::TeX(paste("$ \\bar{\\omega}=",c(50,70,30),"$")) , lty=c(1,2,3), cex=0.7)
  }
}

plot_l_params<-function(speed){
  par(mfrow=c(1,1))
  if(speed){
    lambda = .1 ; bar_lambda = 50; x<-seq(0,100,.1); 
    plot(x,1-1/(1+exp(-lambda * (x-bar_lambda))),type='l',xlab=latex2exp::TeX("$\\nu$"),ylab=latex2exp::TeX("$L(\\nu)$"),lty=1); 
    lines(x,1-1/(1+exp(-lambda*2 * (x-bar_lambda))),type='l',lty=2); 
    lines(x,1-1/(1+exp(-lambda*.5 * (x-bar_lambda))),type='l',lty=3); 
    legend(80, .4, legend=c(latex2exp::TeX("$ \\lambda = 0.1$"),latex2exp::TeX("$ \\lambda = 0.2$"),latex2exp::TeX("$ \\lambda = 0.05$")) , lty=c(1,2,3), cex=0.9) 
  } else {
    lambda = .1 ; bar_lambda = 50; x<-seq(0,100,.1); plot(x,1-1/(1+exp(-lambda * (x-bar_lambda))),type='l',xlab=latex2exp::TeX("$\\nu$"),ylab=latex2exp::TeX("$L(\\nu)$"),lty=1); 
    lines(x,1-1/(1+exp(-lambda*(x-bar_lambda-20))),type='l',lty=2)
    lines(x,1-1/(1+exp(-lambda*(x-bar_lambda+20))),type='l',lty=3)
    legend(85, .4, legend=latex2exp::TeX(paste("$ \\bar{\\lambda}=",c(50,70,30),"$")) , lty=c(1,2,3), cex=1.1)
  }
}


