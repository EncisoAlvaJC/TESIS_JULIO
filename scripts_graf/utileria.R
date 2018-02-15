t2hms = function(tt){
  hh = floor(tt/(60**2))
  mm = floor((tt-hh*60*60)/60)
  ss = tt -hh*60*60 -mm*60
  return(c(hh,mm,ss))
}

hms2t = function(hms){
  return(sum(hms*c(60**2,60,1)))
}

corregir.hms = function(hms.pre){
  hms = hms.pre
  if(hms[3]>=60){
    tmp = floor(hms[3]/60)
    hms[2] = hms[2] + tmp
    hms[3] = hms[3] - tmp*60
  }
  if(hms[2]>=60){
    tmp = floor(hms[2]/60)
    hms[1] = hms[1] + tmp
    hms[2] = hms[2] - tmp*60
  }
  return(hms)
}

hms2txt = function(hms.y){
  agregar = c('','','')
  for(i in 1:3){
    if(hms.y[i]<10){
      if(hms.y[i]>0){
        agregar[i] = '0'
      }
    }
  }
  return(paste0(agregar[1],hms.y[1],':',
                agregar[2],hms.y[2],':',
                agregar[2],hms.y[3]))
}