###############################################################################
# parametros del script

ver_avance  = T
usar_loess  = F
filtrar     = F

#################################################
# parametros para el zoom
zoom           = F
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(0,0,0)

#################################################
# parametros opcionales
canales = kanales$Nombre_archivo
if(length(canales)<1){
  stop('ERROR: Lista de canales tiene longitud cero')
}

#################################################
# parametros dependientes de los datos
ventana   = fr_muestreo*dur_epoca
n_canales = length(canales)
usar_stl  = T
if(dur_epoca<=2){
  usar_stl = F
}
if(usar_loess){
  usar_stl = F
}
if(fr_muestreo==512){
  dur_epoca_reg = 30
}
if(fr_muestreo==200){
  dur_epoca_reg = 10
}
ventana_reg = fr_muestreo*dur_epoca_reg

# procesamiento parametros opcionales (ajuste)
if(unidad_par_t =='tiempo'){
  ini_t   = hms2t(ajuste_ini_hms)
  ini_epo = ini_t/dur_epoca
  ini_pt  = floor(ini_t*fr_muestreo)
}
if(unidad_par_t =='puntos'){
  ini_epo = ajuste_ini_epo
  ini_t   = ini_epo*dur_epoca_reg
  ini_pt  = ini_epo*ventana_reg
}
str_t   = 0
str_epo = 1
str_pt  = 1

# ini : cuando inicia el archivo, util si es un fragmento
# str : cuando inicia el zoom, evita la confusion provocada por ini

min_e = 1

# procesamiento parametros opcionales (zoom)
if(zoom){
  confirma_zoom = FALSE
  if(unidad_par_t == 'tiempo'){
    min_t  = hms2t(min_hms) - ini_t -.5
    max_t  = hms2t(max_hms) - ini_t +.5
    min_e  = floor((min_t+ini_t)/dur_epoca -ini_epo)
    max_e  = ceiling((max_t+ini_t)/dur_epoca -ini_epo)
    
    confirma_zoom = TRUE
  }
  if(unidad_par_t == 'puntos'){
    min_t = min_epo*dur_epoca_reg -ini_t
    max_t = max_epo*dur_epoca_reg -ini_t
    
    min_e = floor((min_epo+ini_t) -ini_epo)
    max_e = ceiling((max_epo+ini_t) -ini_epo)
    
    confirma_zoom = TRUE
  }
  if(!confirma_zoom){
    stop('WARNING: Indique unidad de tiempo para zoom (epocas o segundos)')
  }
  min_pt = floor(min_e*ventana)
  max_pt = ceiling(max_e*ventana)
  
  str_t   = max(min_t, 0)
  str_epo = max(min_e, 1)
  str_pt  = max(min_pt,1)
}

#################################################
# optimizacion: lee un canal, obtiene los indices a analizar
setwd(dir_datos)
ch          = 1
ch_actual   = canales[ch]
nom_archivo = paste0(nombre,'_',ch_actual,'.txt')
DATOS       = scan(nom_archivo)
#DATOS       = as.numeric(unlist(DATOS))
max_epoca   = floor(length(DATOS)/ventana)
n_epocas    = max_epoca

if(zoom){
  end_t    = min(max_t, n_epocas*dur_epoca)
  end_epo  = min(max_e, n_epocas)
  end_pt   = min(max_pt,n_epocas*ventana)
  
  ini_t    = ini_t   + str_t
  ini_epo  = ini_epo + str_epo
  ini_pt   = ini_pt  + str_pt
  n_epocas = length(str_epo:end_epo)
} 

#################################################
# inicio del ciclo que recorre los canales
for(ch in rev(1:n_canales)){
#for(ch in (6:n_canales)){
  
  # construye el nombre del archivo
  ch_actual   = canales[ch]
  nom_archivo = paste0(nombre,'_',ch_actual,'.txt')
  
  # cargar los datos
  setwd(dir_datos)
  if(!file.exists(nom_archivo)){
    warning('ERROR: En canal ',ch_actual,
            ', no se encontro el archivo ',nom_archivo)
    next()
  }
  DATOS = scan(nom_archivo)
  #DATOS = as.numeric(unlist(DATOS))
  
  if(zoom){
    DATOS = DATOS[ini_pt:end_pt]
  }
  
  # cuantas epocas pueden formarse
  max_epoca = floor(length(DATOS)/ventana)
  if(max_epoca==0){
    warning(paste0('ERROR: En canal ',ch_actual,
                   ', no se pudieron leer datos'))
    next()
  }

  #################################################
  # conenedor de datos
  #banda.. = rep(0,max_epoca)
  #banda.d = rep(0,max_epoca)
  #banda.t = rep(0,max_epoca)
  #banda.a = rep(0,max_epoca)
  #banda.b = rep(0,max_epoca)
  #banda.g = rep(0,max_epoca)
  #banda._ = rep(0,max_epoca)
  #banda.S = rep(0,max_epoca)
  #dfa_r   = rep(0,max_epoca)
  #exp.exp = rep(0,max_epoca)
  #exp.co  = rep(0,max_epoca)
  
  #informacion sobre el progreso, si fue requerida
  if(ver_avance){
    print( paste0('  Sujeto : ',etiqueta) )
    print( paste0('   Canal : ',ch_actual,
                  ' (',toString(ch),'/',toString(n_canales),')') )
  }
  
  #################################################
  # inicializacion del cluster
  n_nucleos = detectCores()-1
  closter   = makeCluster(n_nucleos)
  registerDoParallel(closter)
  
  #################################################
  # inicio del ciclo que recorre las epocas
  #for ( i in 0:(max_epoca-1) ){
  #
  #
  # la variable 'pesca' reune los datos
  
  pesca = foreach(i = 0:(max_epoca-2),
                  .combine=rbind,
                  .export =c('DATOS','ventana','fr_muestreo',
                             'filtrar','usar_stl','ch_actual'),
                  .packages=c('fractal','psd')) %dopar%{
    tmp   = DATOS[ (i*ventana+1) : ((i+2)*ventana) ]
    tmp.t = ts(tmp,frequency=fr_muestreo,start=c(0,0))
    
    if(filtrar){
      # filtro STL, robusto y forzado a periodico estandar
      if(usar_stl){
        tmp.s = stl(tmp.t,robust=T,s.window='periodic')
        tmp.r = tmp.s$time.series[,'remainder']
        tmp   = as.numeric(unclass(tmp.r))
      }else{
        tmp.l = loess(tmp~time(tmp.t))
        tmp.s = predict(tmp.l,time(tmp.t))
        tmp   = tmp - tmp.s$fit
      }
      tmp.t = ts(tmp,frequency=fr_muestreo,start=c(0,0))
    }
    
    # espectro de potencias usando el metodo este
    sss = pspectrum(tmp.t,plot=F,verbose=F)
    fff = sss$freq
    spp = abs(sss$spec)
    d.f = fff[2]-fff[1]
    
    i_banda.. = sum(spp[fff<=  0.5])*d.f
    {
      mini = min(fff[fff>=0.5])
      maxi = max(fff[fff<=3.5])
      i_banda.d = sum(spp[mini:maxi])*d.f
    }
    {
      mini = min(fff[fff>=3.5])
      maxi = max(fff[fff<=7  ])
      i_banda.t = sum(spp[mini:maxi])*d.f
    }
    {
      mini = min(fff[fff>= 7])
      maxi = max(fff[fff<=12])
      i_banda.a = sum(spp[mini:maxi])*d.f
    }
    {
      mini = min(fff[fff>=12])
      maxi = max(fff[fff<=30])
      i_banda.b = sum(spp[mini:maxi])*d.f
    }
    {
      mini = min(fff[fff>=30])
      maxi = max(fff[fff<=100])
      i_banda.g = sum(spp[mini:maxi])*d.f
    }
    {
      mini = min(fff[fff>=100])
      i_banda._ = sum(spp[mini:length(spp)])*d.f
    }
    
    i_banda.S = sum(spp)*d.f
    
    VARR = var(tmp)
    #if(VARR>0.005){
    #  i_dfa_r  = as.numeric(DFA(tmp.t))
    #}else{
    #  i_dfa_r  = 0
    #}
    i_dfa_r  = 0

    return(c(i,i_banda..,i_banda.d,i_banda.t,i_banda.a,i_banda.b,
      i_banda.g,i_banda._,i_banda.S,
      i_dfa_r,VARR))
  }
  # fin del ciclo que recorre las epocas
  #################################################
  beep()
  
  #################################################
  # procesamiento a posteriori de los datos
  indices = as.numeric(pesca[,1])
  
  banda.. = pesca[order(indices), 2]
  banda.d = pesca[order(indices), 3]
  banda.t = pesca[order(indices), 4]
  banda.a = pesca[order(indices), 5]
  banda.b = pesca[order(indices), 6]
  banda.g = pesca[order(indices), 7]
  banda._ = pesca[order(indices), 8]
  banda.S = pesca[order(indices), 9]
  dfa_r   = pesca[order(indices),10]
  vec.var = pesca[order(indices),11]
  
  # los resultados se guardan en un archivo .csv
  setwd(dir_res)
  write.table(banda..,paste0('SP_INT_',nombre,'_',ch_actual,'_SUB.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(banda.d,paste0('SP_INT_',nombre,'_',ch_actual,'_DELTA.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(banda.t,paste0('SP_INT_',nombre,'_',ch_actual,'_THETA.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(banda.a,paste0('SP_INT_',nombre,'_',ch_actual,'_ALFA.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(banda.b,paste0('SP_INT_',nombre,'_',ch_actual,'_BETA.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(banda.g,paste0('SP_INT_',nombre,'_',ch_actual,'_GAMMA.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(banda._,paste0('SP_INT_',nombre,'_',ch_actual,'_SUPER.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(banda.S,paste0('SP_INT_',nombre,'_',ch_actual,'_TOTAL.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(dfa_r,paste0('SP_DFA_',nombre,'_',ch_actual,'.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(vec.var,paste0('VAR_',nombre,'_',ch_actual,'.txt'  ),
              row.names=FALSE,col.names=FALSE)
  
  #################################################
  # liberacion del cluster
  stopCluster(closter)
}
# fin del ciclo que recorre canales
#################################################

# fin del script
###############################################################################