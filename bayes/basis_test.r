
eats_redmeat <- function(cases){
  pop=runif(length(cases))
  p_redmeat=1/3;
  return(pop<p_redmeat)
}

has_meateater_liverissues<-function (cases) {
  pop=runif(length(cases))
  redmeat_liverissues=1/4;
  return(pop<redmeat_liverissues)
}

has_nonmeateater_liverissues <- function(cases){
  pop=runif(length(cases))
  noredmeat_liverissues=1/7;
  return(pop<noredmeat_liverissues)
}

simul <- function() {
    cases=seq(1000);
    redmeat_eaters = eats_redmeat(cases) 
    redmeat_eater_patients= redmeat_eaters & has_meateater_liverissues(cases) 
    nonredmeat_eater_patients=!redmeat_eaters & has_nonmeateater_liverissues(cases)
    patients=redmeat_eater_patients | nonredmeat_eater_patients;
    print(length(patients[patients==TRUE])/length(patients))
    print((1/4)*(1/3)+(1/7)*(2/3))
}

run <- function(){
    res=array();
    for (i in seq(10000)){
       x=simul()
       res[i]=length(x[x==TRUE])/length(x)
    }
    hist(res)
}
