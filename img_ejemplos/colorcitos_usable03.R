###############################################################################
# PARCHE : volver a la carpeta central
dir_actual  = getwd()

frecuenciasss = c(200,
                  512,512,
                  200,
                  200,#segun valeria GUR=200 Hz
                  #512, #segun la libreta GURM=512
                  512,512,
                  200,#solo tiene 3 horas
                  512,
                  512,512,
                  200)

###############################################################################
# FACILITADOR : nombres y directorios de los sujetos que analizo
v.nombres    = c('VCNNS1',
                 'MJNNVIGILOS',
                 'JANASUE',
                 'GH24031950SUEÑO',
                 'GURM251148SUE',
                 'CLMN10SUE',
                 'RLMN10SUE',
                 'RRMNS',
                 'JGMN6SUE',
                 'FGHSUE',
                 'MGNA5SUE',
                 'EMNNS')
v.etiqueta   = c('VCR',
                 'MJH',
                 'JAE',
                 'GHA',
                 'MFGR',
                 'CLO',
                 'RLO',
                 'RRU',
                 'JGZ',
                 'FGH',
                 'MGG',
                 'EMT')
v.directorio = c('VCNNS',
                 'MJNNVIGILOScCanal',
                 'JANASUE_revisado',
                 'GH',
                 'GURM_revisado',
                 'CLMN10SUE',
                 'RLMN',
                 'RRMNS_2',
                 'JGMN6SUE',
                 'FGH_EEGdescompuesto',
                 'MGNA',
                 'EMNN')

# epoca_ini = c(712 -10,
#               183 -10,
#               108 -10,
#               1184-10,
#               824 -10,
#               166 -10,
#               242 -10,
#               697 -10,
#               368 -10,
#               276 -10,
#               0,
#               202 -10)
# epoca_fin = c(721 +1,
#               192 +1,
#               140 +1,
#               1195+1,
#               833 +1,
#               176 +1,
#               251 +1,
#               706 +1,
#               378 +1,
#               289 +1,
#               0,
#               220 +1)

epoca_ini = c(712 -30,
              183 -10,
              108 -10,
              1184-30,
              824 -30,
              166 -10,
              242 -10,
              697 -30,
              368 -10,
              276 -10,
              0,
              202 -30)
epoca_fin = c(741 +1,
              192 +1,
              140 +1,
              1213+1,
              853 +1,
              176 +1,
              251 +1,
              726 +1,
              378 +1,
              289 +1,
              0,
              231 +1)

###############################################################################
# parametros del script, ver documantacion para mas informacion
#sujeto = 6
nombre      = v.nombres[sujeto]
etiqueta    = v.etiqueta[sujeto]
#dir_res_mid  = paste0(getwd(),'/estacionariedad_10s/',v.directorio[sujeto])
dir_res_mid  = paste0('C:/Users/EQUIPO 1/Desktop/julio/estacionariedad_sinfiltro/',
                      v.directorio[sujeto])
#dir_graf     = 'C:/Users/EQUIPO 1/Desktop/julio/espectro_int_g_art2'
dir_graf     = 'C:/Users/EQUIPO 1/Desktop/julio/reportar_170927/'
#fr_muestreo  = 512
fr_muestreo  = frecuenciasss[sujeto]
dur_epoca    = 30

if(fr_muestreo==512){
  dur_epo_reg = 30
}
if(fr_muestreo==200){
  dur_epo_reg = 10
}

stam         = T
orden_stam  = c(9,8,7,6,5,4,17,16,2,1,19,18,14,13,12,11,10,3,15,20,21,22)

grabar      = T
anotaciones = ''

reemplazar  = TRUE
canales      = 'PSG'
#canales     = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
#                'P3','P4','PZ','T3','T4','T5','T6')

binario = T
p.vales = c(.05,.01,.005)
escala  = F

zoom           = T
#unidad_par_t   = 'tiempo'
#ajuste_ini_hms = c(0,0,0)
#min_hms        = c(3,17,20)
#max_hms        = c(3,19,10)
unidad_par_t   = 'puntos'
ajuste_ini_epo = 0
#min_epo        = epoca_ini[sujeto]
#max_epo        = epoca_fin[sujeto]-1
min_epo        = 0
if(fr_muestreo==512){
  max_epo = 20
}
if(fr_muestreo==200){
  max_epo = 60
}

# parametros de dibujo
paso    = 10*60*2

#################################################
# libreria especifica para el grafico tipo matriz
#require('plotrix')
require('squash')

#################################################
# procesamiento parametros opcionales
if(reemplazar){
  if(canales=='10-20'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','T3','T4','T5','T6')
  }
  if(canales=='PSG'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','T3','T4','T5','T6','LOG','ROG','EMG')
    if(stam){
      canales = canales[orden_stam]
    }
  }
}
if(length(canales)<1){
  stop('ERROR: Lista de canales tiene longitud cero')
}

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
  if(file.exists(paste0('EST_',nombre,'_',ch_actual,'_T.txt'))){
    correctos[ch] =TRUE
  }else{
    warning('ERROR: En canal ',ch_actual,', no se encontro el archivo ',
            paste0(nombre,'_',ch_actual,'.txt'))
  }
}
canales   = canales[correctos]
n_canales = length(canales)

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_mid)
ch        = 1
ch_actual = canales[ch]
nom_arch  = paste0('EST_',nombre,'_',ch_actual,'_T.txt')
#pv_t      = scan(nom_arch)
pv_t      = read.csv(nom_arch)
#pv_t      = as.numeric(t(pv_t[2]))
pv_t      = as.numeric(t(pv_t))

# PARCHE
if(fr_muestreo==200){
  pv_t2     = rep(0,3*length(pv_t))
  for(i in 1:length(pv_t)){
    pv_t2[3*i-2] = pv_t[i]
    pv_t2[3*i-1] = pv_t[i]
    pv_t2[3*i  ] = pv_t[i]
  }
  pv_t = pv_t2
}

n_epocas  = length(pv_t)
max_e     = n_epocas

# ajustes en el tiempo

if(zoom){
  end_t    = min(max_t  , n_epocas*dur_epoca)
  end_epo  = min(max_epo, n_epocas)
  end_pt   = min(max_pt ,n_epocas)
  
  ini_t    = ini_t   + str_t
  ini_epo  = ini_epo + str_epo
  ini_pt   = ini_pt  + str_pt
  n_epocas = length(str_epo:end_epo)
} 
# end : cuando termina el zoom, evita la confusion provocada por ini

#################################################

# contenedores de los datos
RES_T   = matrix(nrow=n_canales,ncol=n_epocas)

#################################################
# inicio ciclo que recorre todos los canales
for(ch in 1:n_canales){
  # cargar los datos
  ch_actual = canales[ch]
  nom_arch  = paste0('EST_',nombre,'_',ch_actual,'_T.txt')
  #pv_t      = scan(nom_arch)
  pv_t      = read.csv(nom_arch)
  #pv_t      = as.numeric(t(pv_t[2]))
  pv_t      = as.numeric(t(pv_t))
  
  # PARCHE
  if(fr_muestreo==200){
    pv_t2     = rep(0,3*length(pv_t))
    for(i in 1:length(pv_t)){
      pv_t2[3*i-2] = pv_t[i]
      pv_t2[3*i-1] = pv_t[i]
      pv_t2[3*i  ] = pv_t[i]
    }
    pv_t = pv_t2
  }
  
  if(zoom){
    pv_t    = pv_t[min_epo:max_epo]
  }
  
  # si en algun canal se analizaron mas o menos epocas
  if(length(pv_t)!=n_epocas){
    warning('ERROR: En canal ',ch_actual,', no coincide el numero de epocas')
    if(length(pv_t)>n_epocas){
      pv_t  = pv_t[1:n_epocas]
    }else{
      pv_t2 = pv_t
      pv_t  = rep(1,n_epocas)
      pv_t[1:length(pv_t2)] = pv_t2
    }
  }
  
  # organizacion de los datos en una matriz
  RES_T[ch,] = pv_t
}
# fin ciclo que recorre canales
#################################################

#################################################
# creacion etiquetas de tiempo
#ind_t  = (0:n_epocas)*(dur_epoca) + ini_t
#ind_hh = floor(ind_t/(60*60))
#ind_mm = floor( (ind_t - ind_hh*60*60)/60 )
#ind_ss = floor(  ind_t - ind_hh*60*60 - ind_mm*60 )
#txt_t  = character(n_epocas+1)
#for(i in 1:(n_epocas+1)){
#  txt_mm = toString(ind_mm[i])
#  if(ind_mm[i]<10){
#    txt_mm = paste0('0',ind_mm[i])
#  }
#  txt_ss = toString(ind_ss[i])
#  if(ind_ss[i]<10){
#    txt_ss = paste0('0',ind_ss[i])
#  }
#  txt_t[i] = paste0(toString(ind_hh[i]),':',txt_mm,':',txt_ss)
#  #txt_t[i] = paste0(toString(ind_hh[i]),':',txt_mm)
#}

etiqueta_epocas = character(max_epo-min_epo)
s = seq(min_epo,max_epo)
for(i in 1:length(s)){
  etiqueta_epocas[i] = toString(s[i])
}

pass  = paso/dur_epoca
#IND_T = ind_t-1

#################################################
# tratamiento para contraste de color
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
    RES_T = (-M_RES+1)*.9 + .05
  }
  
}

#################################################
# inicio guardado automatico del grafico
if(grabar){
  k = 1.6
  setwd(dir_graf)
  #pdf(paste0(nombre,
  png(paste0(nombre,'_2_',
             '_est',
  #           '.pdf'),width=12,height=6)
             '.png'),units='in',res=300,width=8*k,height=1.5*k)
}

# grafico principal
par(oma=c(0,0,0,0),
    mar=c(.5,2.6,0,3))
#par(mar=c(4,3,3.5,1))
#color2D.matplot(RES_T,
#                #colores claro->oscuro ~ menor->mayor
#                cs1=c(1,0),cs2=c(1,0),cs3=c(1,0),
#                border=NA,
#                axes=F,na.color=NA,
#                xlab='Num de epoca',ylab='',
#                main=paste0('Sujeto : ',etiqueta
#                            #'  (',toString(dur_epoca),' s)'
#                            )
#                )
colorgram(z=t(RES_T[rev(1:n_canales),]),outlier='black',bty='n',axes=F,
          #xlab='Tiempo (hh:mm)',ylab='',
          xlab='Num. de epoca',ylab='',
          colFn=grayscale,
          #zlab=paste0('Sujeto : ',etiqueta,
          #            '  | Estacionariedad'),
          zlab='',
          breaks=seq(0,1,by=.05),
          key=vkey,key.args=c(skip=10,stretch=.4*k)
)
#title(paste0('Sujeto : ',etiqueta,
#             '  | Estacionariedad (',toString(dur_epo_reg),' s)'))

#if(fr_muestreo==512){
#  abline(v=10+.5,col='green',lwd=10)
#}
#if(fr_muestreo==200){
#  abline(v=30+.5,col='green',lwd=10)
#}

# PARCHE
que.epocas = min_e:max_e
letras     = rep('',length(que.epocas))
for(i in 1:length(que.epocas)){
  letras[i] = toString(que.epocas[i]) 
}

# los ejes
axis(2,at=1:n_canales,labels=rev(canales),las=2,tick=F)
axis(2,at=0:n_canales+.5,    labels=F,           las=2,tick=T)
axis(3,labels=F,tick=T,at=c(0,n_epocas)+.5)
#skip = seq(1,n_epocas+1,by=paso)
#axis(1,at=skip-1,labels=txt_t[skip],las=2,tick=T)
axis(4,at=c(0,n_canales)+.5,    labels=F,           las=2,tick=T)

# PARCHE
tik = seq(0,length(etiqueta_epocas))
axis(1,at=tik+.5,labels=F,las=2,tick=T)
tik = (seq(1,length(etiqueta_epocas))-0.5)
axis(1,at=tik+.5,labels=etiqueta_epocas,las=1,tick=F)

#axis(1,at=1:n_epocas-.5,labels=letras,las=2,tick=F)
#axis(1,at=0:n_epocas,labels=F,las=2,tick=T)

if(grabar){
  setwd(dir_graf)
  dev.off()
}
# fin guardado automatico del grafico
#################################################

setwd(dir_actual)

#abline(v = 10,col='green',lwd=3)
# 650 x 400

