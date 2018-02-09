#################################################
# parametros dependientes de los datos
n_canales = length(canales)
ventana   = dur_epoca*fr_muestreo

#################################################
# procesamiento parametros opcionales (ajuste)
if(unidad_par_t =='tiempo'){
  ini_t   = ajuste_ini_hms[1]*60*60
  +ajuste_ini_hms[2]*60
  +ajuste_ini_hms[3]
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

# ini : cuando inicia el archivo, util si es un fragmento
# str : cuando inicia el zoom, evita la confusion provocada por ini

min_e = 1

# procesamiento parametros opcionales (zoom)
if(zoom){
  confirma_zoom = FALSE
  if(unidad_par_t == 'tiempo'){
    min_t  = min_hms[1]*60*60 + min_hms[2]*60 + min_hms[3] -ini_t
    max_t  = max_hms[1]*60*60 + max_hms[2]*60 + max_hms[3] -ini_t
    
    min_e  = floor((min_t+ini_t)/dur_epoca -ini_epo)
    max_e  = ceiling((max_t+ini_t)/dur_epoca -ini_epo)
    
    confirma_zoom = TRUE
  }
  if(unidad_par_t == 'puntos'){
    min_t = min_epo*dur_epoca -ini_t
    max_t = max_epo*dur_epoca -ini_t
    
    min_e = floor((min_epo+ini_t) -ini_epo)
    max_e = ceiling((max_epo+ini_t) -ini_epo)
    
    confirma_zoom = TRUE
  }
  if(!confirma_zoom){
    warning('WARNING: Indique unidad de tiempo para zoom (epocas o segundos)')
  }
  min_pt = floor(min_e*ventana)
  max_pt = ceiling(max_e*ventana)
  
  str_t   = max(min_t, 0)
  str_epo = max(min_e, 1)
  str_pt  = max(min_pt,1)
}

#################################################
# modo seguro: revisa si estan los archivos
correctos = rep(FALSE,n_canales)
n_datos   = Inf

setwd(dir_res_mid)
for(ch in 1:n_canales){
  ch_actual = canales[ch]
  if(file.exists(paste0('SP_INT_',nombre,'_',ch_actual,'_SUB.txt'))){
    correctos[ch] =TRUE
  }else{
    warning('ERROR: En canal ',ch_actual,', no se encontro el archivo ',
            paste0('SP_INT_',nombre,'_',ch_actual,'_(...).txt'))
  }
}
canales   = canales[correctos]
n_canales = length(canales)

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_mid)
ch        = 1
ch_actual = canales[ch]
nom_arch  = paste0('SP_INT_',nombre,'_',ch_actual,'_SUB.txt')
pv_t      = scan(nom_arch)
n_epocas  = length(pv_t)
max_e     = n_epocas

lab_epo_ini = ajuste_ini_epo
lab_epo_fin = ajuste_ini_epo + length(pv_t)*dur_epoca/dur_epo_reg

# ajustes en el tiempo

if(zoom){
  end_t    = min(max_t, n_epocas*dur_epoca)
  end_epo  = min(max_e, n_epocas)
  end_pt   = min(max_pt,n_epocas)
  
  ini_t    = ini_t   + str_t
  ini_epo  = ini_epo + str_epo
  ini_pt   = ini_pt  + str_pt
  n_epocas = length(str_epo:end_epo)
} 
# end : cuando termina el zoom, evita la confusion provocada por ini

#################################################

# contenedores de los datos
RES = matrix(0,nrow=n_canales,ncol=n_epocas)

#################################################
# inicio ciclo que recorre todos los canales
for(ch in 1:n_canales){
  # cargar los datos
  ch_actual = canales[ch]
  nom_arch  = nom_arch  = paste0('VAR_',nombre,'_',
                                 ch_actual,'.txt')
  pv_t      = scan(nom_arch)
  #nom_arch  = nom_arch  = paste0('SP_INT_',nombre,'_',ch_actual,'_',
  #                               banda[nbandas-2],'.txt')
  #norm      = scan(nom_arch)
  #pv_t      = pv_t/norm
  
  if(zoom){
    pv_t    = pv_t[min_e:max_e]
  }
  
  # organizacion de los datos en una matriz
  RES[ch,] = pv_t[1:n_epocas]
}
# fin ciclo que recorre canales
#################################################

#################################################
# creacion etiquetas de tiempo
ind_t  = (0:n_epocas)*(dur_epoca) + ini_t
ind_hh = floor(ind_t/(60*60))
ind_mm = floor( (ind_t - ind_hh*60*60)/60 )
ind_ss = floor(  ind_t - ind_hh*60*60 - ind_mm*60 )
txt_t  = character(n_epocas+1)
for(i in 1:(n_epocas+1)){
  txt_mm = toString(ind_mm[i])
  if(ind_mm[i]<10){
    txt_mm = paste0('0',ind_mm[i])
}
  txt_ss = toString(ind_ss[i])
  if(ind_ss[i]<10){
    txt_ss = paste0('0',ind_ss[i])
  }
  txt_t[i] = paste0(toString(ind_hh[i]),':',txt_mm,':',txt_ss)
  #txt_t[i] = paste0(toString(ind_hh[i]),':',txt_mm)
}

etiqueta_epocas = character(lab_epo_fin-lab_epo_ini)
s = seq(lab_epo_ini,lab_epo_fin)
for(i in 1:length(s)){
  etiqueta_epocas[i] = toString(s[i])
}

pass  = paso/dur_epoca

#################################################
# inicio guardado automatico del grafico
k = 1.6
if(grabar){
  setwd(dir_graf)
  #pdf(paste0(nombre,
  png(paste0(nombre,'_2_',
             '_VAR',
  #           '.pdf'),width=12,height=6)
             '.png'),units='in',res=300,width=8*k,height=1.5*k)
}

kk = quantile(as.numeric(RES),.9)
RES = pmin(RES,kk)

kk = quantile(as.numeric(RES),.9)
RES = pmin(RES,kk)

kk = quantile(as.numeric(RES),.9)
RES = pmin(RES,kk)

# grafico principal
colorgram(z=t(RES[rev(1:n_canales),]),outlier='white',bty='n',axes=F,
          #xlab='Tiempo (hh:mm)',ylab='',
          xlab='Num. de epoca',
          #ylab='',
          ylab='Varianza',las=2,cex.lab=2,font.lab=2,
          #colFn=grayscale,
          #zlab=paste0('Sujeto : ',etiqueta,
          #            #'  (',toString(dur_epoca),' s)', 
          #            ' | Varianza'),
          #            #' | DFA'),
          zlab='',
          #breaks=seq(mmin,mmax,by=(mmax-mmin)/100),
          #key=vkey,key.args=c(skip=10,stretch=.08*k)
          key=vkey,key.args=c(stretch=.6*k)
          )
#title(paste0('Sujeto : ',etiqueta,
#             #'  (',toString(dur_epoca),' s)'
#             ' | ',
#             #'DFA'
#             #'B, con h(w) = exp(A+Bw)'
#             #'A, con h(w) = exp(A+Bw)'
#             ))

#################################################
# graficacion de epocas
# setwd(dir_epocas)
# arch_indice_e = paste0('epocas_mor_',nombre,'.txt')
# indice_e      = scan(arch_indice_e)
# factor.extra = 1
# if(fr_muestreo==200){
#   factor.extra = 3
# }
# for(i in indice_e){
#   rect(i/(factor.extra/2),0.5,
#        (i+1)/(factor.extra/2),22.5,
#        col=rgb(255,255,255,
#                alpha=128,
#                maxColorValue=255),
#        border=NA)
# }

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
axis(4,at=c(0,n_canales)+.5,    labels=F,las=2,tick=T)

axis(1,labels=F,tick=T,at=c(0,n_epocas)+.5)
skip = seq(1,n_epocas+1,by=paso)+.5
axis(1,at=skip-1,labels=F,las=2,tick=T)
axis(3,at=skip-1,labels=F,las=2,tick=T)

if(grabar){
  setwd(dir_graf)
  dev.off()
}
# fin guardado automatico del grafico
#################################################

setwd(dir_actual)