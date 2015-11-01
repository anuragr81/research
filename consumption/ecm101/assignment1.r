

# works for scalar area
apt_cost <- function(area,subsidy){
  cost_per_ft = 0.5*(1-subsidy);
  council_cost = 500;
  council_area = 1500;
  private_cost = cost_per_ft*area ; 
  if (area>council_area) {
    return (private_cost);
  } else {
  if (private_cost > council_cost){
    return (council_cost);
  }  else {
     return (private_cost);
  }
  }

}

question1d<-function() {
other_cons=array();
other_cons2=array();
area_vec=seq(0,2000,1) ;
count = 1;
apt_array=array();
apt2_array=array();
par(mfrow=c(1,1));
plot(0,0);
for (i in area_vec) {
  #ac = apt_cost(i,0);
  #ac2 = apt_cost(i,0.1);
  ac = i*.5
  ac2 = i*0.5*(.9)
  other_cons[count]=1500-ac;
  other_cons2[count]=1500-ac2;
  apt_array[count]=ac;
  apt2_array[count]=ac2;
  count = count + 1;
}
#plot(area_vec,other_cons,type='l');
cl<-rainbow(2);
plot(0,0,xlim=c(0,2000),ylim=c(600,2000))
lines(area_vec,other_cons,col=cl[1],lty=1)
lines(area_vec,other_cons2,col=cl[2],lty=2)
legend(0,1000,c("Original Consumption","Subsidised Consumption"),col=cl, lty=c(1,2));
}

question1a<-function() {
other_cons=array();
area_vec=seq(0,2000,1) ;
count = 1;
apt_array=array();
for (i in area_vec) {
  ac = apt_cost(i,0);
  other_cons[count]=1500-ac;
  apt_array[count]=ac;
  count = count + 1;
}
plot(area_vec,other_cons,type='l');
}
