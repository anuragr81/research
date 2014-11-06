stringsort<-function(s){
  return(paste(sort(unlist(strsplit(s, ""))), collapse = "",sep=""));
}

generate_paths<-function(np){
  paths=list();
  paths[[1]]=c("u","d");
  if (np>=2){
    for (n in seq(2,np)){
      newpath=NULL;
      for (path in paths[[n-1]]){
        for(state in path){
          newpath=c(newpath,
                    stringsort(paste("u",state,sep="",collapse="")));         
          newpath=c(newpath,
                    stringsort(paste("d",state,sep="",collapse="")));
        }
      }
      paths[[n]]=sort(unique(newpath));
    } # end period-loop
  }
  return(paths);
}

value_state<-function(state,u,d){
  i=1;
  prod=1;
  ucount=0;
  dcount=0;
  state=unlist(strsplit(state, ""));
  for (letter in state){
    if (letter == "u"){
      prod=prod*u;
      ucount=ucount+1;
    }else if (letter=="d"){
      prod=prod*d;
      dcount=dcount+1
    }else{
      stop(paste("invalid letter",letter));
    }
  }
  return(data.frame(prod=prod,ucount=ucount,dcount=dcount));
}

payoff<-function(S,payoffparams){
  if(payoffparams$K>0){
    K<-payoffparams$K;
    pf<-max(S-K,0);
    #    print(paste("payoff",pf));
    return(pf);
  }else{
    stop("Invalid Params to payoff");
  }
}

evaluate_pair<-function(pf_u,pf_d,p,r_f,Dt,payoffparams){
  val=exp(-r_f*Dt)*(p*pf_u+(1-p)*pf_d);
  return(val);
}

mergevec<-function(valvec,resvec){
  if (is.null(resvec)){
    return (valvec);
  }
  
  i=1;
  retvec=array();
  for (val in valvec){
    if(val>resvec[i]){
      retvec[i]=val;
    }else{
      retvec[i]=resvec[i];
    }
    i=i+1;
  }
  return (retvec);
}

evaluate_paths <- function(S_0,r_f,Dt,u,d,p,payoffparams,paths){
  exercise_flag=FALSE;
  np=length(paths);
  resvec=NULL;
  for (n in seq(np)){
    
    #    print(paste("Before resvec:",toString(resvec)));
    last_n=np-n+1;
    curvec=paths[[last_n]];
    #    print(paste("curvec:",toString(curvec)));
    if (exercise_flag==TRUE || exercise_flag==FALSE && n==1){
      # valuation on stock-path would be done every time if it's an American (or path dependent)
      # otherwise only once at the nodes
      exercise_payoffvec=array();
      
      for (i in seq(1,length(curvec))){
        asset_val<-S_0*(value_state(state=curvec[i],u=u,d=d)$prod);
        exercise_payoffvec[i]=payoff(S=asset_val,payoffparams=payoffparams);        
      }
      
      #      print(paste("exercise_payoffvec:",toString(exercise_payoffvec)));
      payoffvec=mergevec(valvec=exercise_payoffvec,resvec=resvec)
    } else {
      payoffvec=resvec;
    }
    #    print(paste("payoffvec:",toString(payoffvec)));
    
    #prepare resvec for next level (size reduces by 1 at every level)
    next_resvec=array();
    for (i in seq(2,length(curvec))){
      next_resvec[i-1]=evaluate_pair(pf_d=payoffvec[i-1],
                                     pf_u=payoffvec[i],
                                     p=p,
                                     r_f=r_f,
                                     Dt=Dt);
    }
    resvec=next_resvec;
    #    print(paste("After resvec:",toString(resvec)));
  } # loop over levels
  return(resvec);
}

binval<-function(S_0,r_f,sg,np,T,K){
  #estimate P without caring about ud=1 - just p
  require(zoo);
  Dt=T/np;
  u=exp(sg*sqrt(Dt));
  d=1/u;
  p=(exp(r_f*Dt)-d)/(u-d);
  #  print(paste("Dt=",Dt,"p=",p,"d=",d,"u=",u))
  paths=generate_paths(np=np);
  payoffparams=data.frame("K"=K);
  resvec=evaluate_paths(S_0=S_0,r_f=r_f,Dt=Dt,u=u,d=d,p=p,
                        payoffparams=payoffparams,paths=paths);
  return(resvec);
}

crrbinval<-function(S_0,r_f,sg,np,T,K){
  require(zoo);
  Dt=T/np;
  u=exp(sg*sqrt(Dt));
  d=1/u;
  p=(exp(r_f*Dt)-d)/(u-d);
  #  print(paste("Dt=",Dt,"p=",p,"d=",d,"u=",u))
  paths=generate_paths(np=np);
  payoffparams=data.frame("K"=K);
  resvec=evaluate_paths(S_0=S_0,r_f=r_f,Dt=Dt,u=u,d=d,p=p,
                        payoffparams=payoffparams,paths=paths);
  return(resvec);
}

asset_distr<-function(np){
  S_0=30;
  T=1;
  sg=.4;
  r_f=.1;
  K=35;
  Dt=T/np;
  u=exp(sg*sqrt(Dt));
  d=1/u;
  p=(exp(r_f*Dt)-d)/(u-d);
  
  payoffparams=data.frame("K"=K);
  paths=generate_paths(np);
  curvec=paths[[np]];
  asset_val=array();
  prob=array();
  expsum=0;
  for (i in seq(1,length(curvec))){
    valres<-value_state(state=curvec[i],u=u,d=d)
    asset_val[i]=S_0*(valres$prod);
    ucount=valres$ucount;
    dcount=valres$dcount;
    n=ucount+dcount;
    ncr=factorial(n)/(factorial(ucount)*factorial(dcount));
    prob[i]=ncr*value_state(state=curvec[i],u=p,d=(1-p))$prod;
    expsum=expsum+prob[i]*payoff(S=asset_val[i],payoffparams=payoffparams);
  }
  plot(asset_val,prob,type='l');
  print(expsum);
  return(prob);
}

simul<-function(){
  tvec=seq(50);
  p=array();
  i=1;
  for (iperiod in tvec){
    p[i]=binval(S_0=30,r_f=.1,sg=.4,np=iperiod,T=1,K=35);
    i=i+1;
  }
  plot(tvec,p,xlab="Number of Periods",ylab="Price of European Option",type='l')
  print(p)
}
risk_neutral_probability <-function(callprice_vec,
                                    strike_vec,
                                    S_0,
                                    divyield,
                                    r,
                                    n){
  
  if(length(callprice_vec)<=1){
    print("ignoring vector");
    return(NULL);
  }
  p=array();
  rn<-(1+r)**n;
  divmul<-1/(1+divyield)**n;
  p[1]=2*(1-rn*(S_0*divmul-callprice_vec[1])/strike_vec[1]);
  sum_until_i=p[1];
  for (i in seq(2,length(callprice_vec))){
    cdiff=rn*(callprice_vec[i-1]-callprice_vec[i])*
      (1/(strike_vec[i]-strike_vec[i-1]));
    p[i]=2*(1-sum_until_i-cdiff);
    sum_until_i=sum_until_i+p[i];
  }
  p[i+1]=1-sum_until_i;
  K_next=strike_vec[i]+2*rn*callprice_vec[i]/p[i+1];
  return(data.frame(rnp=p,strike_vec=c(strike_vec,K_next)));
}

test <- function(snpopt) {
  
  require(zoo);
  require(plot3D);
  par(mfrow=c(1,1));
  vix=read.csv("vix.csv");
  vix=data.frame(vix=vix$vix,date=strptime(vix$Date,"%d%b%Y"));
  #plot(vix$date,vix$vix,type='l',xlab='Time',ylab='VIX');
  spx=read.csv("spx.csv");
  spx=data.frame(spx=spx$spindx,date=strptime(spx$caldt,"%Y%m%d"));
  
  sz=20;
  
  #plot(spx$date[sz:length(spx$date)],rollapply(spx$spx,20,sd),type='l',
  #     xlab='Time',ylab='SPX Historical Vol');
  opt=split(snpopt,snpopt$cp_flag);
  copt=opt[["C"]];
  copt_atdate=split(copt,copt$date);
  last_time=strptime("19700101","%Y%m%d");
  for (i in seq(length(copt_atdate))){
    copt_atdate[[i]]$date<-strptime(copt_atdate[[i]]$date,"%Y%m%d");  
    curDate=copt_atdate[[i]]$date[1];
    S0=spx[spx$date==curDate,]$spx;
    #if (as.double(curDate-last_time,units='days')>40){
    #  scatter3D(main=curDate,xlab="K",
    #            ylab="T",
    #            zlab="ImplVol",
    #            copt_atdate[[i]]$strike_price/1e+3, as.integer(copt_atdate[[i]]$exdate), 
    #            copt_atdate[[i]]$impl_volatility, phi =20,
    #            theta=58, bty = "f",type='h',
    #            col = gg.col(100), xlim=c(1,3000),zlim=c(0,1.5),
    #            pch = 18, cex = 2, 
    #            ticktype = "detailed")
    #      Sys.sleep(.2);
    #  last_time=curDate;
    #}
    
    if (length(S0)>0){
      c_atexdate=split(copt_atdate[[i]],copt_atdate[[i]]$exdate);
      arr=c(0,0,0)
      p=matrix(arr,1,3);
      
      for (j in seq(length(c_atexdate))) {  
        c_atexdate[[j]]$exdate=strptime(c_atexdate[[j]]$exdate,"%Y%m%d");
        c_atexdate[[j]]=c_atexdate[[j]][with(
          c_atexdate[[j]],
          order(c_atexdate[[j]]$strike_price)),]; #ordering by strike_price
        
        ndays=as.numeric(c_atexdate[[j]]$exdate[1]-curDate,units="days");
        cpvec=(c_atexdate[[j]]$best_offer+c_atexdate[[j]]$best_bid)/2;
        strike_vec=1e-3*c_atexdate[[j]]$strike_price
        rnp=risk_neutral_probability(callprice_vec=cpvec,
                                     strike_vec=strike_vec,
                                     divyield=0,
                                     r=.02,
                                     n=ndays/250,
                                     S_0=S0);
        dx=rep(ndays,length(rnp$strike_vec));
        for (i in seq(length(length(rnp$strike_vec)))){
          p=rbind(p,c(dx[i],rnp$strike_vec[i],rnp$rnp[i]));
        }
        #print(rep(ndays,length(rnp$strike_vec)));
        breaks=seq(-1.5,1.5,.1);
        #        print(hist(rnp,breaks=breaks)$counts);
        #print(paste("size(rnp)=",length(rnp)));
        
      }
      #      ch=readline();
      #      if (ch=="1"){
      #        return(1);
      #      }
    } else {
      print(paste("Ignoring Date:",curDate));
    }
  }
  
}
