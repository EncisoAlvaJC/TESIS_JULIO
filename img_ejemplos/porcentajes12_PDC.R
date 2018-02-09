  # libreria especifica para el grafico tipo matriz
  library(plotrix)
  
  #####

  frecuenciasss = c(200,
                    512,512,
                    200,200,
                    512,512,
                    200,
                    512,512,
                    512,512)
  
  frecuencia = frecuenciasss[sujeto]
  
  # nombres de las carpetas
  #       data  directorio donde estan los datos
  #    central  directorio donde guardar los graficos, debe
  #             contener el subdirectorio con las epocas
  data_dir    = '/home/julio/Tesis/trabajo/scripts170620/estacionariedad_'
  central_dir = '/home/julio/Tesis/trabajo/scripts170620'
  
  #####
  #####
  #####
  
  # control manual, paso intermedio a la automatizacion
  porcent = T     # cantidad total o proporcional
  escala  = F     # el porcentaje se grafica entre 0 y 1
  
  #####
  #####
  #####
  
  duracionnn = c(30,10,60,2.5)
  durrr_nomb = c('30','10','60','2_5')
  
  duracion_tal=1
  
  duracion = duracionnn[duracion_tal]
  dur_nomb = durrr_nomb[duracion_tal]
  
  #####
  #####
  #####
  
  # nombres de los archivos con epocas de diferentes longitudes
  #nom_len = c('60s','30s','15s','10s','05s','02_5s')
  
  # VARIABLES PARA RETIRAR
  # control manual de epocas graficadas
  binario = T   # contraste acepta/rechaza estacionariedad
  nuevo   = T   # en ves de RES(ultado) usa EST(acionario)
  
  # automatizacion de directorio datos segun sujeto
  nombre_abreviado = nomb_facil[sujeto]
  nombre           = nomb_arch[sujeto]
  nom_dir          = nomb_dir[sujeto]
  
  # directorios de trabajo, nombres abreviados
  #d_dir   = paste0(data_dir,nom_dir)      # d(atos)
  e_dir   = paste0(central_dir,'/epocas3') # e(pocas)
  r_dir   = central_dir                   # r(esultados)
  #g_dir   = paste0(central_dir,
  #                 '/grafiquitos')# g(raficos)
  g_dir   = save_dir
  
  #####
  
  # nombre del archivo que contiene las epocas MOR
  setwd(e_dir)
  ar_indice = paste0('epocas_mor_',nombre,'.txt')
  indice    = scan(ar_indice)
 
  if(frecuencia==512){
    if(duracion==60){
      # 60 s  : 2 epocas por bloque
      indixe = ceiling(indice/2)
      
      indixe = unique(indixe)
      indixe = sort(indixe)
      
      indice = indixe
      
      epo_s_min = 1
    }
    if(duracion==30){
      # 30 s  : 1 epoca por bloque
      # no se hace nada
      
      epo_s_min = 2
    }
    if(duracion==10){
      # 10 s  : 1 epoca 3 bloques
      indixe = c( 3*indice  ,
                  3*indice-1,
                  3*indice-2)
      
      indixe = unique(indixe)
      indixe = sort(indixe)
      
      indice = indixe
      
      epo_s_min = 6
    }
    if(duracion==2.5){
      # 2.5 s : 1 epoca 12 bloques
      indixe = c( 12*indice  ,
                  12*indice- 1,
                  12*indice- 2,
                  12*indice- 3,
                  12*indice- 4,
                  12*indice- 5,
                  12*indice- 6,
                  12*indice- 7,
                  12*indice- 8,
                  12*indice- 9,
                  12*indice-10,
                  12*indice-11)
      
      indixe = unique(indixe)
      indixe = sort(indixe)
      
      indice = indixe
      
      epo_s_min = 24
    }
  }
  if(frecuencia==200){
    if(duracion==60){
      # 60 s  : 6 epocas en 1 bloque
      indixe = ceiling(indice/6)
      
      indixe = unique(indixe)
      indixe = sort(indixe)
      
      indice = indixe
      
      epo_s_min = 1
    }
    if(duracion==30){
      # 30 s  : 3 epocas en 1 bloque
      indixe = ceiling(indice/3)
      
      indixe = unique(indixe)
      indixe = sort(indixe)
      
      indice = indixe
      
      epo_s_min = 2
    }
    if(duracion==10){
      # 10 s  : 1 epoca por bloque
      # no se hace nada
      
      epo_s_min = 6
    }
    if(duracion==2.5){
      # 2.5 s : 1 epoca 4 bloques
      indixe = c( 4*indice  ,
                  4*indice-1,
                  4*indice-2,
                  4*indice-3)
      
      indixe = unique(indixe)
      indixe = sort(indixe)
      
      indice = indixe
      
      epo_s_min = 24
    }
  }
  
  
  #####

  # contenedores de los datos
  RES_T   = c()
  max_epo = c()
 
  #####
  
  tag  = 'EST'
  lain = '_' 
    
  d_dir = paste0(data_dir,dur_nomb,'s/',nom_dir) #d_# d(atos)
    
  setwd(d_dir)
  
  # ciclo que recorre los 22 canales
  for(ch in 1:22){
    # forma el nombre del archivo con daos
    canal  = channel[ch]
    ar_t   = paste0(tag,'_',nombre,
                    lain,canal,'_T.csv'  )
    # carga los datos
    pv_t_pre = read.csv( ar_t,row.names=1 )
    pv_t     = as.numeric(unlist(pv_t_pre))
   
    # pone los datos en una matriz
    RES_T   = do.call(rbind,list(RES_T  ,pv_t ))
    max_epo = append(max_epo,length(pv_t))
  }
  
  for(ii in 1:22){
    for(jj in 1:length(RES_T[1,])){
      if(is.na(RES_T[ii,jj])){
        RES_T[ii,jj] = 0
      }
    }
  }
  
  IND_T = 1:min(max_epo)
    
  # variable auxiliar, numero de epocas totales
  n.epo = length(IND_T)
  
  # numero total de epocas y numero de epocas MOR
  print(paste0('Total : ', toString(n.epo)))
  print(paste0('  MOR : ', toString(length(indice) )))
  
  ####
  
  ####
  
  # nombre de archivo acorde a argumentos opcionales
  if(porcent){
    tag = 'porcentaje'
  }
  if(!porcent){
    tag = 'total'
  }
  
  # guardado automatico del grafico resultante
   if(grabar){
     setwd(g_dir)
     pdf(paste0(nomb_facil[sujeto],'_',
     #png(paste0(nomb_facil[sujeto],'_',
                toString(length(indice)),
                #'_',toString(n.epo),
                toString(100*p.val),
                '_difporcentaje',
                #'_',tag,
                '.pdf'),width=10.3/1.5,height=8/1.5)
                #'.png'),units='in',res=150,width=12,height=6)
   }
  
  ####
  ####
  ####
  
  # nombres un poco mas sencillos
  mor   = indice
  n.mor = setdiff(1:n.epo,mor)
  
  # se cuentan las epocas estacionarias en 3 categorias
  #      tot : todas las epocas del registro
  #      mor : las epocas mor
  #     nmor : 
  
  # contenedores de resultado
  res_tot  = rep(0,22)
  res_mor  = rep(0,22)
  res_nmor = rep(0,22)
  
  # conteo de epocas estacionarias
  for(ch in 1:22){
    res_tot[ch]  = sum((RES_T[ch,]>p.val)*1)
    res_nmor[ch] = sum((RES_T[ch,n.mor]>p.val)*1)
    res_mor[ch]  = sum((RES_T[ch,mor]>p.val)*1)
  }
  
  ################################
  significados = rep(0,22)
  
  for(ch in 1:22){
    tu = prop.test(x=c(res_tot[ch],res_mor[ch]),
                   n=c(length(RES_T[ch,]),length(mor)),
                   correct=T)
    significados[ch] = as.numeric(tu['p.value'])
  }
  ################################
  
  # participacion relativa
  if(porcent){
    res_tot  =  res_tot/length(RES_T[1,])
    res_nmor = res_nmor/length(n.mor)
    res_mor  =  res_mor/length(mor)
  }
  
  # matriz con todos los datos
  ress = rbind(res_tot,res_nmor,res_mor)
  #ress = rbind(res_tot,res_mor)
  #ress = t(ress)
  
  # fijar el maximo del grafico
  max_r = (max(ress))
  if(escala){
    max_r = 1
  }
  
  # el mensaje cambia si es total o proporcion
  if(porcent){
    yl = 'Proporcion'
  }
  if(!porcent){
    yl = 'Total'
  }
  
  MINI = floor(20*min(c(ress[1,],ress[3,])))/20
  MAXI = ceiling(20*max(c(ress[1,],ress[3,])))/20
  
  plot(100*ress[2,],xaxt='n',
       ylim=100*c(MINI,MAXI),
       xlab='',ylab='% stationary epochs',
       main=paste0('Subject : ',nombre_abreviado),
       type='l',col='black',lwd=2,las=2)
  
  lines(100*ress[2,],xaxt='n',
        ylim=c(MINI,MAXI),
        type='p',col='black',pch=19)
  
  lines(100*ress[3,],xaxt='n',
        ylim=c(MINI,MAXI),
        type='l',col='green4',lwd=2)
  lines(100*ress[3,],xaxt='n',
        ylim=c(MINI,MAXI),
        type='p',col='green4',pch=19)
  
  axis(1,at=1:22,labels=(channel),las=2,
       tick=T)
  
  legend('bottomleft',
         legend=c('NREM','REM'),
         col=c('black','green4'),
         lty=1,lwd=2,cex=1.3)
  
  ####
  ####
  
  strst=(MAXI-MINI)/40
  
  for(i in 1:22){
    if(is.nan(significados[i])){
      significados[i] = 1
    }
  }
  
  suma = rep(0,22)
  
  ch_lin = 1:22
  equis = (significados<.05)
  suma = suma + equis*1
  lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  equis = (significados<.01)
  suma = suma + equis*1
  lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  equis = (significados<.005)
  suma = suma + equis*1
  lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  
  ####
  ####
  
  for(w in 1:22){
    dif_significativas[w,sujeto] = ast[suma[w]+1]
  }
  
  # guardado automatizado de los resultados
  if(grabar){
    setwd(g_dir)
    dev.off()
  }
  
  
  
  for(ch in 1:22){
    matriz_mor[sujeto,ch] = ress[3,ch]
  }
  
  for(ch in 1:22){
    matriz_nmor[sujeto,ch] = ress[2,ch]
  }
  
  for(ch in 1:22){
    matriz_tot[sujeto,ch] = ress[1,ch]
  }
  
  #row.names(matriz_mor)=channel
  #colnames(matriz_mor) = nomb_facil
  
  ####
  