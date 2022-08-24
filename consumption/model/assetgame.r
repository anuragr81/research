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


plain_util <- function(A){
  #return(log(A))
  if (A<0){
    return(0)
  } else {
    return (A**.3)
  }
}

combined_util <- function (beta, u1,u2){
 U <- beta*u1**.5 +(1-beta)*u2**.5  
 return (U)  
}

onedimensional_bisection <- function(f,a,b,N = 1000){
  
  if (a >=b ){
    stop("Invalid limits")
  }
  x <- seq(a,b+(b-a)/N,(b-a)/N)
  y <- sapply(x,f)
  return(x[which.max(y)])
  
}

plot_selections <- function(){
  a <- .3
  b <- .8
  #k <- 3
  k <- 1.5
  
  y_start = 1
  y_max = 10
  dy = .02
  y_end <- 4
  omega_bar_selections <-array()
  nu1_chosen <-array()
  nu2_chosen <-array()
  A1_array <-array()
  A2_array <-array()
  y1_array <- array()
  y2_array <- array()
  i <- 1
  while (y_end <y_max){
    omega_bar_found = plot_societal_optimal(beta = b, y1=y_start,y2=y_end,alpha = a,lambda=1,kappa=k)
    omega_bar_selections[i] <- omega_bar_found
    res <- print_results_for_chosen_omega_bar(beta = b, y1 = y_start, y2 = y_end, alpha = a,lambda = 1,kappa = k,omega_bar = omega_bar_found,print_all = F,plot = F)
    nu1_chosen [i] <- res[["nu_1_chosen"]]
    nu2_chosen [i] <- res[["nu_2_chosen"]]
    A1_array [i] <- res[["LTA1"]]
    A2_array [i] <- res[["LTA2"]]
    y1_array[i] <- y_start
    y2_array[i] <- y_end
    y_end <- y_end + dy
    print(paste("(y_start,y_end)=(",y_start,",",y_end,") omega_bar=",omega_bar_found))
    
    i <- i + 1
  }
  return (data.frame(omega_bar=omega_bar_selections,nu1=nu1_chosen,nu2=nu2_chosen,y1=y1_array,y2=y2_array,A1=A1_array,A2=A2_array))
  
  
}

test_plot <- function(rr){
  g <- subset(rr, (delta <3.5 & delta > 3) | (delta > 4.2 & delta < 5) | ( delta > 6 & delta < 6.5) | (delta > 7 & delta < 7.5)|  (delta > 8 & delta < 8.09 ))  ; 
  par(mfrow=c(2,3));
  plot(g$y2-g$y1,g$omega_bar,type='l',xlab=latex2exp::TeX(paste("$\\Delta")),ylab=latex2exp::TeX(paste("$\\underline{\\omega}$")))
  
  plot(g$y2-g$y1,g$nu1,type='l',xlab = latex2exp::TeX(paste("$\\Delta$")),ylab=latex2exp::TeX(paste("$\\bar{\\nu}_{1}$")));
  plot(g$y2-g$y1,g$nu2,type='l',xlab=latex2exp::TeX(paste("$\\Delta")),ylab=latex2exp::TeX(paste("$\\bar{\\nu}_{2}$"))); 
  
  plot(g$y2-g$y1,g$A1,type='l',xlab=latex2exp::TeX(paste("$\\Delta")),ylab=latex2exp::TeX(paste("$A_{1}$")))
  plot(g$y2-g$y1,g$A2,type='l',xlab=latex2exp::TeX(paste("$\\Delta")),ylab=latex2exp::TeX(paste("$A_{2}$")))
  
  
}
print_results_for_chosen_omega_bar <- function(omega_bar,beta,y1,y2,alpha, lambda,kappa,print_all,plot=F){
  lbar = get_pareto_lambda_bar(omega_bar_fixed=omega_bar, fixed_alpha=alpha, omega=kappa*lambda,fixed_lambda=lambda,
                               fixed_y1=y1,fixed_y2=y2)
  if (lbar==-1){
    return(-1)
  }
  result <- no_greed_two_stage_sol(omega_bar=omega_bar, omega=kappa*lambda,lambda_bar=lbar,
                                   lambda=lambda,y1=y1,y2=y2,alpha=alpha,plot=plot,
                                   return_choices = T)
  #print(result)
  res = list()
  res <- result
  res[['U']] <- combined_util(beta=beta, u1 = res[["u1"]], u2 = res[["u2"]])
  
  if (missing(print_all) || print_all == F){
    
    
    return(res)
  } else if (print_all == T){
    print(paste("result[[nu_1_chosen]]=",result[["nu_1_chosen"]],"result[[nu_2_chosen]]=",result[["nu_2_chosen"]],
                "Total U=",res[['U']]))
    
    return(res)
    
  } else {
    stop("Invalid Condition")
  }
}
get_pareto_lambda_bar <-function(omega_bar_fixed,omega,fixed_lambda,fixed_alpha,fixed_y1=fixed_y1,fixed_y2=fixed_y2)
{
  #lambda_bar
  find_lambda_bar <- function(lbar){
    no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=omega, lambda_bar=lbar,lambda=fixed_lambda,
                           y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
  }
  
  lambda_bar_optimised <- (optimise(function(x){abs(find_lambda_bar(x))},c(0,max(fixed_y1,fixed_y2))))$minimum
  
  error <- no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=omega,lambda_bar=lambda_bar_optimised,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
  
  if (error >1e-1){
    return(-1)
  } else {
    
    return(lambda_bar_optimised)
  }
}


plot_societal_optimal <- function(beta,y1,y2, alpha,lambda, kappa,use_xlim){
  #    fixed_y1 = 1 
  #    fixed_y2 = 10
  #  fixed_alpha = .3
  #  fixed_lambda = 1
  #  fixed_kappa =3
  
  societal_optimal_for_omega_bar <- function(omega_bar){
    lbar = get_pareto_lambda_bar(omega_bar_fixed=omega_bar, 
                                 fixed_alpha=alpha, 
                                 omega=kappa*lambda,
                                 fixed_lambda=lambda,
                                 fixed_y1=y1,fixed_y2=y2)
    if (lbar==-1) { # Error: could not find lbar
      return(-1)
    }
    #print(omega_bar)
    
    result <- no_greed_two_stage_sol(omega_bar=omega_bar, omega=kappa*lambda,lambda_bar=lbar,
                                     lambda=lambda,y1=y1,y2=y2,alpha=alpha,plot=F,
                                     return_choices = T)
    
    return(combined_util(beta = beta,u1 = result[["u1"]] , u2 = result[["u2"]]))
    
  }
  
  #  omega_bar_optimised <- (optimise(function(x){societal_optimal_for_omega_bar(x)},c(0,max(fixed_y1,fixed_y2)),maximum = T))$maximum
  rr <- seq(0,max(y1,y2),.1)
  ss <- sapply(rr, societal_optimal_for_omega_bar)
  par(mfrow=c(1,1))
  if (missing(use_xlim)){
    use_xlim<- max(rr)
  }
  plot (rr,ss,type='l',xlab="Omega-bar",xlim=c(0,use_xlim),panel.first=grid())
  # Plot nu_1 nu_2 
  
  #xx <- seq(0,use_xlim,.05)
  #yy <- sapply(xx,function(x){ print_results_for_chosen_omega_bar(beta=beta,
  #                                                                y1=y1,y2=y2, alpha=alpha,lambda=lambda, kappa=kappa,
  #                                                                omega_bar = x)})
  
  #zz=data.frame(x=xx,y=yy)
  #return(ddply(zz,.(y),summarise, xx=toString(range(x))))
  return(onedimensional_bisection(f=societal_optimal_for_omega_bar,a=0,b=max(y1,y2)))
  
  
}


no_greed_two_stage_sol <- function(omega_bar,omega,lambda_bar,lambda,y1,y2,alpha,plot,return_choices,return_utils){
  
  w <-  function(nu) { W_p_logis(omega=omega,omega_bar=omega_bar, nu=nu) }
  l <-  function(nu) { L_p_logis(lambda=lambda,lambda_bar=lambda_bar, nu=nu) }
  
  E1 <- function(x,y1,y2) { w(x)*y2  + (1-w(x))*y1}
  E2 <- function(x,y1,y2) { l(x)*y1  + (1-l(x))*y2}
  
  #E1 <- function(x,y1,y2) { w(x)*(y2-x)  + (1-w(x))*(y1-x)}
  #E2 <- function(x,y1,y2) { l(x)*(y1-x)  + (1-l(x))*(y2-x)}
  
  Aw_band1 <-function(x) {y1-x+y2-x+E2(x,y1,y2) }
  Al_band1 <-function(x) {y1-x+y1-x+E1(x,y1,y2) }
  
  Aw_band2 <- function(x) {y2-x+y2-x+E2(x,y1,y2) }
  Al_band2 <- function(x) {y2-x+y1-x+E1(x,y1,y2) }
  
  ea_suw = function(x) {  return(w(x)*plain_util(y1-x+y2)  + (1-w(x))*plain_util(y1-x+y1))}
  ea_sul = function(x) {  return(l(x)*plain_util(y2-x+y1)  + (1-l(x))*plain_util(y2-x+y2))}
  
  ea_uw = function(x) {  return(w(x)*plain_util(Aw_band1(x))  + (1-w(x))*plain_util(Al_band1(x)))}
  ea_ul = function(x) {  return(l(x)*plain_util(Al_band2(x))  + (1-l(x))*plain_util(Aw_band2(x)))}
  
  
  nu1 <- seq(0,y1,y1/100)
  nu2 <- seq(0,y2,y2/100)
  
  par(mfrow=c(1,2))
  nu_1_chosen = optimise(function(x) { -ea_uw(x) },c(0,y1))$minimum
  nu_2_chosen = optimise(function(x) { -ea_ul(x) },c(0,y2))$minimum
  
  nu_1_st_chosen = optimise(function(x) { -ea_suw(x) },c(0,y1))$minimum
  nu_2_st_chosen = optimise(function(x) { -ea_sul(x) },c(0,y2))$minimum
  
  
  ea_w = function(x) { w(x)*Aw_band1(x)  + (1-w(x))*Al_band1(x)}
  ea_l = function(x) { l(x)*Al_band2(x)  + (1-l(x))*Aw_band2(x)}
  
  kappa = omega/lambda
  if (plot){
    plot(nu1,sapply(nu1,ea_uw),type='l',main=latex2exp::TeX(paste("$A_1(\\bar{\\nu}_1$=",round(nu_1_chosen,2),")=",round(ea_w(nu_1_chosen),2))),xlab=latex2exp::TeX("$\\bar{\\nu}_1$"),ylab=latex2exp::TeX("$u_1$"))
    plot(nu2,sapply(nu2,ea_ul),type='l',main=latex2exp::TeX(paste("$A_2(\\bar{\\nu}_2$=",round(nu_2_chosen,2),")=",round(ea_l(nu_2_chosen),2))),xlab=latex2exp::TeX("$\\bar{\\nu}_2$"),ylab=latex2exp::TeX("$u_2$"))
    plot_title=paste("$y_1$=",round(y1,2),
                     "$y_2$=",round(y2,2),
                     "$\\omega$=",round(omega,2),
                     "$\\underline{\\omega}$=",round(omega_bar,2),
                     "$\\lambda$=",round(lambda,2),
                     "$\\underline{\\lambda}$=",round(lambda_bar,2))
    mtext(latex2exp::TeX(plot_title),side=1,line=-1,outer=T)
    
    
    #plot(nu1,sapply(nu1,ea_suw),type='l',main=paste('ST A(nu_1=',round(nu_1_st_chosen,3),")=",round(ea_w(nu_1_st_chosen),3)))
    #plot(nu2,sapply(nu2,ea_sul),type='l',main=paste('ST A(nu_2=',round(nu_2_st_chosen,3),")=",round(ea_l(nu_2_st_chosen),3)))
  }
  if (missing(return_choices) || return_choices==F){
    return(abs(nu_1_chosen  + kappa* (nu_2_chosen - lambda_bar) - omega_bar))
  } else if(return_choices==T) {
    res = list()
    res[["nu_1_chosen"]]=nu_1_chosen
    res[["nu_2_chosen"]]=nu_2_chosen
    res[["LTA1"]]=ea_w(nu_1_chosen)
    res[["LTA2"]]=ea_l(nu_2_chosen)
    res[["u1"]]=ea_uw(nu_1_chosen)
    res[["u2"]]=ea_ul(nu_2_chosen)
    return (res)
  }
  else {
    stop("Invalid return_choices value")
  }
  
}





get_no_greed_missing_var<-function(fixed_varname,fixed_varval,plot)
{
  
  if (fixed_varname=="omega_bar"){
    fixed_y1 = 1 
    fixed_y2 = 5
    fixed_alpha = .3
    
    fixed_lambda=1
    fixed_kappa =3
    
    omega_bar_fixed = fixed_varval
    #lambda_bar
    find_lambda_bar <- function(lbar){
      no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lbar,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
    }
    
    lambda_bar_optimised <- (optimise(function(x){abs(find_lambda_bar(x))},c(0,max(fixed_y1,fixed_y2))))$minimum
    
    error <- no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_optimised,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["lambda_bar_optimised"]] = lambda_bar_optimised
    return(res)
  }
  
  
  if (fixed_varname=="omega_bar_y1"){
    fixed_y1 = fixed_varval 
    fixed_y2 = 12
    fixed_alpha =1
    
    fixed_lambda=1
    fixed_kappa =10
    
    lambda_bar_fixed = 3
    
    find_omega_bar <- function(obar){
      no_greed_two_stage_sol(omega_bar=obar,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
    }
    
    omega_bar_optimised <- (optimise(function(x){abs(find_omega_bar(x))},c(0,max(fixed_y1,fixed_y2))))$minimum
    
    error <- no_greed_two_stage_sol(omega_bar=omega_bar_optimised,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["omega_bar_optimised"]] = omega_bar_optimised
    return(res)
  }
  
  if (fixed_varname=="omega_bar_y2"){
    fixed_y1 = 2
    fixed_y2 = fixed_varval
    fixed_alpha =1
    
    fixed_lambda=1
    fixed_kappa =2
    
    lambda_bar_fixed =.2
    
    find_omega_bar <- function(obar){
      no_greed_two_stage_sol(omega_bar=obar,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
    }
    
    omega_bar_optimised <- (optimise(function(x){abs(find_omega_bar(x))},c(0,max(fixed_y1,fixed_y2))))$minimum
    
    error <- no_greed_two_stage_sol(omega_bar=omega_bar_optimised,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["omega_bar_optimised"]] = omega_bar_optimised
    return(res)
  }
  
  
  
  if (fixed_varname=="lambda_bar_y1"){
    fixed_y1 = fixed_varval 
    fixed_y2 = 7 #5
    fixed_alpha = 1
    
    fixed_lambda=1
    fixed_kappa =3
    
    omega_bar_fixed = .3
    #lambda_bar
    find_lambda_bar <- function(lbar){
      no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lbar,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
    }
    
    lambda_bar_optimised <- (optimise(function(x){abs(find_lambda_bar(x))},c(0,max(fixed_y1,fixed_y2))))$minimum
    
    error <- no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_optimised,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["lambda_bar_optimised"]] = lambda_bar_optimised
    return(res)
  }
  
  if (fixed_varname=="lambda_bar_y2"){
    fixed_y1 = 1 
    fixed_y2 = fixed_varval
    fixed_alpha = 1
    
    fixed_lambda=1
    fixed_kappa =3
    
    omega_bar_fixed = .2
    #lambda_bar
    find_lambda_bar <- function(lbar){
      no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lbar,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=F)
    }
    
    lambda_bar_optimised <- (optimise(function(x){abs(find_lambda_bar(x))},c(0,max(fixed_y1,fixed_y2))))$minimum
    
    error <- no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_optimised,lambda=fixed_lambda,y1=fixed_y1,y2=fixed_y2,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["lambda_bar_optimised"]] = lambda_bar_optimised
    return(res)
  }
  
  
  if (fixed_varname=="y2_lambda_bar"){
    fixed_y1 = 1 
    fixed_alpha = 1
    fixed_lambda=1
    fixed_kappa = 2
    
    omega_bar_fixed = .3
    lambda_bar_fixed = fixed_varval
    #lambda_bar
    find_y2 <- function(y2var){
      no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=fixed_y1,y2=y2var,alpha=fixed_alpha,plot=F)
    }
    
    y2_optimised <- (optimise(function(x){abs(find_y2(x))},c(1e-2,lambda_bar_fixed*5) ) ) $minimum
    
    error <- no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=fixed_y1,y2=y2_optimised,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["y2_optimised"]] = y2_optimised
    return(res)
  }
  
  
  if (fixed_varname=="y2_omega_bar"){
    fixed_y1 = 1 
    fixed_alpha = 1
    fixed_lambda=1
    fixed_kappa = 2
    
    omega_bar_fixed = fixed_varval
    lambda_bar_fixed = 1.2
    #lambda_bar
    find_y2 <- function(y2var){
      no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=fixed_y1,y2=y2var,alpha=fixed_alpha,plot=F)
    }
    
    y2_optimised <- (optimise(function(x){abs(find_y2(x))},c(1e-2,lambda_bar_fixed*5) ) ) $minimum
    
    error <- no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=fixed_y1,y2=y2_optimised,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["y2_optimised"]] = y2_optimised
    return(res)
  }
  
  if (fixed_varname=="y1_lambda_bar"){
    y2_fixed = 5
    fixed_alpha = 1
    fixed_lambda=1
    fixed_kappa = 4
    
    omega_bar_fixed = .2
    lambda_bar_fixed = fixed_varval
    #lambda_bar
    find_y1 <- function(y1var){
      no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=y1var,y2=y2_fixed,alpha=fixed_alpha,plot=F)
    }
    
    y1_optimised <- (optimise(function(x){abs(find_y1(x))},c(1e-2,omega_bar_fixed*5) ) ) $minimum
    
    error <- no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=y1_optimised,y2=y2_fixed,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["y1_optimised"]] = y1_optimised
    return(res)
  }
  
  if (fixed_varname=="y1_omega_bar"){
    y2_fixed = 5
    fixed_alpha = 1
    fixed_lambda=1
    fixed_kappa = 2
    
    omega_bar_fixed = fixed_varval
    lambda_bar_fixed = 1.2
    #lambda_bar
    find_y1 <- function(y1var){
      no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=y1var,y2=y2_fixed,alpha=fixed_alpha,plot=F)
    }
    
    y1_optimised <- (optimise(function(x){abs(find_y1(x))},c(1e-2,omega_bar_fixed*5) ) ) $minimum
    
    error <- no_greed_two_stage_sol(omega_bar=omega_bar_fixed,omega=fixed_kappa*fixed_lambda,lambda_bar=lambda_bar_fixed,lambda=fixed_lambda,y1=y1_optimised,y2=y2_fixed,alpha=fixed_alpha,plot=plot)
    
    res=list()
    res[["error"]] = error
    res[["y1_optimised"]] = y1_optimised
    return(res)
  }
  
  stop("Could not identify fixed_varname")
  
}
