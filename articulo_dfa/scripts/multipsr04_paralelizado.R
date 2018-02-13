###############################################################################
# parametros

ver_avance  = T
no_repetir  = F
filtrar     = F
usar_loess  = T

canales = kanales$Nombre_archivo

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

#################################################
# inicio del ciclo que recorre los canales
for(ch in 1:n_canales){
  
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
  
  DATOS = read.csv(nom_archivo)
  DATOS = as.numeric(unlist(DATOS))
  #DATOS = scan(nom_archivo)
  
  # cuantas epocas pueden formarse
  max_epoca = floor(length(DATOS)/ventana)
  if(max_epoca==0){
    warning(paste0('ERROR: En canal ',ch_actual,
                   ', no se pudieron leer datos'))
    next()
  }
  
  # contenedores de los resltados
  #pv.t   = rep(0,max_epoca)
  #pv.ir  = rep(0,max_epoca)
  #pv.tir = rep(0,max_epoca)
  
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
  
  pesca = foreach(i = 0:(max_epoca-1),
                  .combine=rbind,
                  .export =c('DATOS','ventana','fr_muestreo',
                             'filtrar','usar_stl'),
                  .packages=c('fractal')) %dopar%{ 
   
    # filtro STL, robusto y forzado a periodico estandar
    tmp   = DATOS[ (i*ventana+1) : ((i+1)*ventana) ]
    if(filtrar){
      tmp.t = ts(tmp,frequency=fr_muestreo,start=c(0,0))
      if(usar_stl){
        tmp.s = stl(tmp.t,robust=T,s.window='periodic')
        tmp.r = tmp.s$time.series[,'remainder']
        tmp   = as.numeric(unclass(tmp.r))
      }else{
        tmp.l = loess(tmp~time(tmp.t))
        tmp.s = predict(tmp.l,time(tmp.t))
        tmp   = tmp - tmp.s
      }
    }
    
    # test de PSR, los archivos se recolectan
    z        = stationarity(tmp)
    t.pv.t   = as.numeric( attr(z,'pvals')[1])
    t.pv.ir  = as.numeric( attr(z,'pvals')[2])
    t.pv.tir = as.numeric( attr(z,'pvals')[3])
    
    return(c(i , t.pv.t , t.pv.ir , t.pv.tir))
  }
  # fin del ciclo que recorre las epocas
  #################################################
  
  #################################################
  # procesamiento a posteriori de los datos
  indices = as.numeric(pesca[,1])
  
  pv.t   = pesca[order(indices), 2]
  pv.ir  = pesca[order(indices), 3]
  pv.tir = pesca[order(indices), 4]
  
  # los resultados se guardan en un archivo .txt
  setwd(dir_res)
  tag = toString(dur_epoca)
  write.table(pv.t  , paste0('EST_',nombre,'_',ch_actual,'_T_',tag,'.txt'  ),
              row.names=FALSE,col.names=FALSE)
  write.table(pv.ir , paste0('EST_',nombre,'_',ch_actual,'_IR_',tag,'.txt' ),
              row.names=FALSE,col.names=FALSE)
  write.table(pv.tir, paste0('EST_',nombre,'_',ch_actual,'_TIR_',tag,'.txt'),
              row.names=FALSE,col.names=FALSE)
  
  #################################################
  # liberacion del cluster
  stopCluster(closter)
}
# fin del ciclo que recorre canales
#################################################

# fin del script
###############################################################################