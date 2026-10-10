%% lambda_RN under the PANEL SCRIPT'S OWN conventions, correctly oriented.
%% Panel: variable = log(FSKRC_PT) = log(E/RWA), demeaned by an EXPANDING
%% MEAN (-> the stationary mean for a long series), split by sign, and the
%% ratio taken as DEFICIT/SURPLUS per Theorem 1 (the script itself returns
%% the reciprocal -- see the orientation trace).
%% Two sub-cases, because E/RWA maps to x differently depending on whether
%% RWA tracks liabilities or assets:
%%   (a) RWA ~ L : log(E/RWA) = log(x-1) + const      Jacobian 1/(x-1)
%%   (b) RWA ~ A : log(E/RWA) = log((x-1)/x) + const  Jacobian 1/(x(x-1))
P.sigma=0.08; P.sigma_L=0.03; P.a1=0.045; P.a2=0.05; P.a3=0.30;
P.r=0.025; P.mu_s=0.04; P.mu_L=0.03; P.gamma=0.02; P.c=0.20;
P.kappa=0.01; P.R=1.15;
function [b,s2]=ds(x,pi,P)
  mu_pi=(1-pi)*P.r+pi*P.mu_s; b=x*(mu_pi-P.mu_L)+P.gamma;
  s2=pi^2*P.sigma^2*x^2+2*pi*P.c*P.sigma*P.sigma_L*x*(1-x)+P.sigma_L^2*(1-x)^2; s2=max(0,s2);
end
S=load('resultM_lambda_1.0000.mat'); sol=S.sol;
y=sol.y; v=sol.v; vp=sol.v_prime; dy=y(2)-y(1);
act=abs(v-sol.Mv)<1e-9; xL=max(y(act)); ystar=sol.y_star;
tgt=1+P.kappa;
for i=find(abs(y-xL)<dy/2,1):length(y)-1
  if vp(i)>tgt && vp(i+1)<=tgt; xT=interp1(vp(i:i+1),y(i:i+1),tgt); break; end
end
% pass 1: stationary means of each variable
rand('seed',999); randn('seed',999);
dt=1e-4; nst=600000; nburn=60000;
x=0.5*(xL+ystar); sa=0; sb=0; n=0; XS=zeros(nst-nburn,1); S2=XS; PI=XS;
for k=1:nst
  xi=min(max(round((x-y(1))/dy)+1,1),length(y)); pist=sol.pi_star(xi);
  [b,s2]=ds(x,pist,P);
  if k>nburn
    n++; XS(n)=x; S2(n)=s2;
    sa += log(x-1); sb += log((x-1)/x);
  end
  x=x+b*dt+sqrt(s2*dt)*randn();
  if x<=xL; x=xT; end
  if x>=ystar; x=ystar-1e-12; end
end
XS=XS(1:n); S2=S2(1:n);
ma=sa/n; mb=sb/n;
printf("stationary mean of log(x-1)      = %.6f  -> split at x = %.4f\n", ma, 1+exp(ma));
printf("stationary mean of log((x-1)/x)  = %.6f\n\n", mb);
printf("%-42s %10s %10s %9s\n","convention (deficit/surplus, Thm 1 order)","ratio","lambda_RN","split x");
% (a) RWA ~ L
va = S2 ./ (XS-1).^2; da = log(XS-1) < ma;
ra = mean(va(da))/mean(va(~da));
printf("%-42s %10.4f %10.4f %9.4f\n","(a) RWA ~ L   w=log(x-1), split=stat mean",ra,ra^0.25,1+exp(ma));
% (b) RWA ~ A
vb = S2 ./ (XS.*(XS-1)).^2; db = log((XS-1)./XS) < mb;
rb = mean(vb(db))/mean(vb(~db));
printf("%-42s %10.4f %10.4f %9.4f\n","(b) RWA ~ A   w=log((x-1)/x), split=stat mean",rb,rb^0.25,NaN);
% (a) but split at R, for comparison with the earlier run
dR = XS < P.R;
rR = mean(va(dR))/mean(va(~dR));
printf("%-42s %10.4f %10.4f %9.4f\n","(a) but split at R=1.15 (earlier run)",rR,rR^0.25,P.R);
printf("\npanel median as REPORTED by the script: 0.962\n");
printf("panel median CORRECTLY ORIENTED (1/0.962) = %.4f\n", 1/0.962);
