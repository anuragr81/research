require(tuneR)
threshold_trim <- function (x,threshold){
  # the concern is that of cumulative values
  
}

intellitrim <-function (wave){
  threshold<-.75;
  x<- attributes(wave)$left;
  reset_min= FALSE
  # make it positive for cumsum to result in a non-decreasing sequence
  if (min(x)<0){ minx = min(x); x = x-minx; reset_min= TRUE}; 
  cx <- cumsum(x);
  last_indices = which(cx > max(cx)*threshold);
  if (length(last_indices) == 0 ){
    stop('invalid wave');
  } else {
    x[last_indices[1]:length(x)]<-0;
    if(reset_min){
      x=x+minx;
    }
  }
  return(Wave(x,samp.rate=attributes(wave)$samp.rate,bit=attributes(wave)$bit));
}

play_sounds <- function (characters,sdir){
  waves = c();
  sound_files = data.frame(file=list.files(sdir,pattern="*.wav"));
  print(sound_files);
  for (c in characters){
    cfile = paste(c,".wav",sep="");
    sfile = toString(sound_files[sound_files$file==cfile,])
    if (sfile!=""){
      waves =c(waves, readWave(sfile));
    }else{
      print(paste("Not Found",cfile)); # error etc.
    }
  }
  res = NULL
  count = 1;
  for (wave in waves) {
    if (is.null(res)) {
      print(paste("Adding",count));
      #res = prepComb(normalize(wave,unit=c('16')),zero=0);
      res = prepComb(intellitrim(normalize(wave,unit=c('16'))));
      count = count + 1;
    } else {
      print(paste("Adding",count));
      res = bind(res,prepComb(intellitrim(normalize(wave,unit=c('16')))));
      #res = bind(res,prepComb(normalize(wave,unit=c('16')),zero=0));
      count = count + 1;
    }
  }
  return(res);
}

# source('sounds_analysis.r');waves=play_sounds(characters=c('b','a'),sdir="."); play(waves,'vlc')

create_sound <-function (freq) {
  sr=10000; 
  t = seq(0,2,1/sr);  
  #y = (2^15-1)*( (sin(2*pi*300*t)+sin(2*pi*200*t)+sin(2*pi*100*t)))/3
  #y = (2^15-1)*(sin(2*pi*100*t)+sin(2*pi*1000*t))/2
  # freq = 300 ; 
  A = 2^15
  # t= exp(-t); // added fade
  y = (
    (A-1)*sin(2*pi*freq*t)
    +(A/4-1)*sin(2*pi*freq*.5*t)
    +(A/16-1)*sin(2*pi*freq*.3*t)
    +(A/32-1)*sin(2*pi*freq*.1*t)
  )/3
  print(paste("max(y)=",max(y)));
  w = Wave(y, samp.rate = sr, bit = 16) ; 
  plot(w);
  return (w);
}

# telephone: y = (2^15-1)*( (sin(2*pi*300*t)+sin(2*pi*320*t)))/2

