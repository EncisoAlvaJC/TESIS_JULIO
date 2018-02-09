#################################################
# parametros dependientes de los datos
n_canales = length(canales)
ventana   = dur_epoca*fr_muestreo

#################################################
# procesamiento parametros opcionales (ajuste)
if(unidad_par_t =='tiempo'){
  ini_t   = ajuste_ini_hms[1]*60*60+
            ajuste_ini_hms[2]*60+
            ajuste_ini_hms[3]
  ini_epo = ini_t/dur_epoca
  ini_pt  = floor(ini_t*fr_muestreo)
}
if(unidad_par_t =='puntos'){
  ini_epo = ajuste_ini_epo
  ini_t   = ini_epo*dur_epoca
  ini_pt  = ini_epo*ventana
}
str_t   = 0
str_epo = 1
str_pt  = 1

min_e = 1

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_mid)
ch            = 1
ch_actual     = canales[ch]
nom_arch      = paste0('EST_',nombre,'_',ch_actual,
                       '_T_',toString(dur_epoca),'.txt')
pv_t          = scan(nom_arch)
pv_t          = as.numeric(t(pv_t))

factor_escala = dur_epoca/(30/32)
n_epocas      = length(pv_t)
max_e         = n_epocas

#################################################
# contenedores de los datos
RES_T   = matrix(0,nrow=n_canales,ncol=n.epo)
RES_TIR = matrix(0,nrow=n_canales,ncol=n.epo)

#################################################
# inicio ciclo que recorre todos los canales
for(ch in 1:n_canales){
  # cargar los datos
  ch_actual = canales[ch]
  nom_arch  = paste0('EST_',nombre,'_',ch_actual,
                     '_T_'  ,toString(dur_epoca),'.txt')
  pv_t      = scan(nom_arch)
  pv_t      = as.numeric(t(pv_t))
  
  nom_arch  = paste0('EST_',nombre,'_',ch_actual,
                     '_TIR_',toString(dur_epoca),'.txt')
  pv_tir    = scan(nom_arch)
  pv_tir    = as.numeric(t(pv_tir))
  
  mmm1 = min(n_epocas,length(pv_t))
  mmm2 = min(n_epocas,length(pv_tir))
  
  if(zoom){
    distr.t  = rep(0,mm1*factor_escala)
    indice_aux = 1:mm1
    for(k in 0:(factor_escala-1)){
      distr.t[indice_aux*factor_escala-k] = pv_t
    }
    distr.tir  = rep(0,mm1*factor_escala)
    indice_aux = 1:mm2
    for(k in 0:(factor_escala-1)){
      distr.tir[indice_aux*factor_escala-k] = pv_tir
    }
    
    pv_t   =   distr.t[e.ini:e.fin]
    pv_tir = distr.tir[e.ini:e.fin]
  }
  
  # organizacion de los datos en una matriz
  RES_T[ch,]   = pv_t
  RES_TIR[ch,] = pv_tir
}
# fin ciclo que recorre canales
#################################################

# pedazo final de la prueba de PSR
if(binario){
  if(FALSE){
    n_pvals = length(p.vales)
    M_RES   = matrix(0,nrow=n_canales,ncol=n_epocas)
    for(i in 1:n_pvals){
      M_RES = M_RES + 1*( RES_T>p.vales[i])
    }
    RES_T   = (M_RES/length(p.vales))**3
  }else{
    M_RES1 = 1*( RES_TIR<.05 )
    M_RES2 = 1*( RES_T  <.05 )
    M_RES = pmin(M_RES1,M_RES2)
    RES_T = (-M_RES+1)*.95 + .025
  }
  
}

#################################################
# inicia grafico
colorgram(z=t(RES_T[rev(1:n_canales),]),outlier='black',bty='n',axes=F,
          #xlab='Tiempo (hh:mm)',ylab='',
          #xlab='Num. de epoca',
          xlab='',
          #ylab='',
          ylab=paste0(toString(dur_epoca),'s'),las=2,cex.lab=2,font.lab=2,
          colFn=grayscale,
          #zlab=paste0('Sujeto : ',etiqueta,
          #            '  | Estacionariedad'),
          zlab='',
          #breaks=seq(0,1,by=.05),
          #key=vkey,key.args=c(skip=10,stretch=.4*k)
          key=0
)

#################################################
# graficacion de epocas
setwd(dir_epocas)
arch_indice_e = paste0('epocas_mor_',nombre,'.txt')
indice_e      = scan(arch_indice_e)
factor.extra = 1
if(fr_muestreo==200){
  #parche_indice = ceiling(parche_indice/3)
  #parche_indice = sort(unique(parche_indice))
  factor.extra = 3
}
for(i in indice_e){
  rect(i/(factor.extra*factor_escala),0.5,
       (i+1)/(factor.extra*factor_escala),22.5,
       col=rgb(128,255,128,
               alpha=128,
               maxColorValue=255),
       border=NA)
}


#################################################
# graficacion de ejes
canales_par = rep('',n_canales)
canales_non = rep('',n_canales)
for(i in 1:n_canales){
  if(i-2*floor(i/2)==0){
    canales_par[i] = canales[i]
  }else{
    canales_non[i] = canales[i]
  }
}

# ejes
#axis(2,at=1:n_canales,labels=rev(canales),las=2,tick=F)
axis(2,at=1:n_canales,labels=rev(canales_par),las=2,tick=F,cex.axis=.8)
axis(2,at=0:n_canales+.5,    labels=F,           las=2,tick=T)
axis(4,at=1:n_canales,labels=rev(canales_non),las=2,tick=F,cex.axis=.8)
axis(4,at=0:n_canales+.5,    labels=F,           las=2,tick=T)

axis(3,labels=F,tick=T,at=c(0,n_epocas)+.5)
axis(4,at=c(0,n_canales)+.5,    labels=F,           las=2,tick=T)

axis(1,labels=F,tick=T,at=c(0,n_epocas)+.5)
skip = seq(1,n_epocas+1,by=paso/factor_escala)+.5
#axis(1,at=skip-1,labels=txt_t[skip],las=2,tick=T)
#axis(3,at=skip-1,labels=txt_t[skip],las=2,tick=T)
axis(1,at=skip-1,labels=F,las=2,tick=T)
axis(3,at=skip-1,labels=F,las=2,tick=T)

# fin grafico
#################################################