
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
         res = prepComb(normalize(wave,unit=c('16')),zero=127);
         count = count + 1;
      } else {
         print(paste("Adding",count));
         res = bind(res,prepComb(normalize(wave,unit=c('16')),zero=127));
         count = count + 1;
      }
  }
  return(res);
}

# source('sounds_analysis.r');waves=play_sounds(characters=c('b','a'),sdir="."); play(waves,'vlc')
