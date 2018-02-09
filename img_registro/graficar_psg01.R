#################################################
# volver a la carpeta central
dir_actual  = '~/TESIS/TESIS/img_registro'
dir_graf    = '~/TESIS/TESIS/img_registro'
dir_datos   = '~/TESIS/graf_datos/registros'
dir_epocas  = '~/TESIS/graf_datos/epocas3/'
dir_res     = '~/TESIS/TESIS/img_registro'
setwd(dir_actual)

#################################################
# parametros
#sujeto = 1
grabar = T
hora   = 1

zoom           = TRUE
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(hora,0,0)
min_hms        = c(1,57, 0)
max_hms        = c(1,57,30)
#unidad_par_t   = 'puntos'
#ajuste_ini_epo = 0
#min_epo        = 0
#max_epo        = 0

#################################################
# parametros de dibujo
escala_mv  = 60  #7.5
salto      = 8
sep_lineas = 1
n_tiempos  = 6

anotaciones = ''

###############################################################################
# datos generales
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
grupo_de = c(0,0,0,0,0,1,1,1,1,-1,-1,-1)

banda.n = c('Delta','Theta','Alfa','Beta','Gamma','Potencia total',
            'Ondas lentas fuera de rango','Ondas rapidas fuera de rango')
banda   = c('DELTA','THETA','ALFA','BETA','GAMMA','TOTAL','SUB','SUPER')
nbandas = length(banda.n)

###############################################################################
# parametros del script
nombre      = v.nombres[sujeto]
etiqueta    = v.etiqueta[sujeto]
fr_muestreo = frecuenciasss[sujeto]

dur_epoca   = 30

if(fr_muestreo==512){
  dur_epo_reg = 30
}
if(fr_muestreo==200){
  dur_epo_reg = 10
}

stam         = T
orden_stam  = c(9,8,7,6,5,4,17,16,2,1,19,18,14,13,12,11,10,3,15,20,21,22)

reemplazar  = TRUE
canales      = 'PSG'
#canales     = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
#                'P3','P4','PZ','T3','T4','T5','T6')

binario = F
escala  = F

hora_t = toString(hora)

#################################################
# procesamiento parametros opcionales
if(reemplazar){
  if(canales=='10-20'){
    canales = c( 'C3', 'C4', 'CZ', 'F3', 'F4',
                 'F7', 'F8','FP1','FP2', 'FZ',
                 'O1', 'O2', 'P3', 'P4', 'PZ',
                 'T3', 'T4', 'T5', 'T6')
  }
  if(canales=='PSG'){
    canales = c( 'C3', 'C4', 'CZ', 'F3', 'F4',
                 'F7', 'F8','FP1','FP2', 'FZ',
                 'O1', 'O2', 'P3', 'P4', 'PZ',
                 'T3', 'T4', 'T5', 'T6', 'LOG',
                 'ROG','EMG')
    if(stam){
      canales = canales[orden_stam]
    }
  }
}
if(length(canales)<1){
  stop('ERROR: Lista de canales tiene longitud cero')
}

#################################################
# parametros que dependen de los datos
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
if(unidad_par_t =='epoca'){
  ini_epo = ajuste_ini_epo
  ini_t   = ini_epo*dur_epoca
  ini_pt  = ini_epo*ventana
}
str_t   = 0
str_epo = 0
str_pt  = 1

# ini : cuando inicia el archivo, util si es un fragmento
# str : cuando inicia el zoom, evita la confusion provocada por ini

# procesamiento parametros opcionales (zoom)
if(zoom){
  confirma_zoom = FALSE
  if(unidad_par_t == 'tiempo'){
    min_t  = min_hms[1]*60*60 + min_hms[2]*60 + min_hms[3] -ini_t
    max_t  = max_hms[1]*60*60 + max_hms[2]*60 + max_hms[3] -ini_t
    
    min_e  = (min_t+ini_t)/dur_epoca -ini_epo
    max_e  = (max_t+ini_t)/dur_epoca -ini_epo
    
    confirma_zoom = TRUE
  }
  if(unidad_par_t == 'puntos'){
    min_t = min_epo*dur_epoca -ini_t
    max_t = max_epo*dur_epoca -ini_t
    
    min_e = (min_epo+ini_t) -ini_epo
    max_e = (max_epo+ini_t) -ini_epo
    
    confirma_zoom = TRUE
  }
  if(!confirma_zoom){
    warning('WARNING: Indique unidad de tiempo para zoom (epocas o segundos)')
  }
  min_pt = floor(min_t*fr_muestreo)
  max_pt = ceiling(max_t*fr_muestreo)
  
  str_t   = max(min_t, 0)
  str_epo = max(min_e, 0)
  str_pt  = max(min_pt,1)
}

#################################################
# modo seguro: revisa si estan los archivos
correctos = rep(FALSE,n_canales)
n_datos   = Inf

setwd(dir_datos)
for(ch in 1:n_canales){
  ch_actual = canales[ch]
  if(file.exists(paste0(nombre,'_',ch_actual,'_',
                        hora_t,'.txt'))){
    correctos[ch] =TRUE
  }else{
    warning('ERROR: En canal ',ch_actual,'no se encontro el archivo ',
            paste0(nombre,'_',ch_actual,'.txt'))
  }
}
canales   = canales[correctos]
n_canales = length(canales)

#################################################
# graficacion

# carga los datos para inicializar la ventana
setwd(dir_datos)
ch        = 1
ch_actual = canales[ch]
nom_arch  = paste0(nombre,'_',ch_actual,'_',hora_t,'.txt')
DATA      = scan(nom_arch)
n_datos   = length(DATA)

# ajustes en el tiempo
if(zoom){
  end_t   = min(max_t, n_datos/fr_muestreo)
  end_epo = min(max_e, n_datos/ventana)
  end_pt  = min(max_pt,n_datos)
  
  DATA    = DATA[str_pt:end_pt]
  
  ini_t   = ini_t   + str_t
  ini_epo = ini_epo + str_epo
  ini_pt  = ini_pt  + str_pt
  n_datos   = length(DATA)
} 
# end : cuando termina el zoom, evita la confusion provocada por ini

# miscelanea de indices para detalles tecnicos
n_puntos  = floor(n_datos/salto)
ind_t     = floor(seq(0,n_puntos,by=n_puntos/n_tiempos))+1
ind_pt    = seq(0,n_datos,by=salto)
ind_pt[1] = 1
ind_linea = seq(0,n_datos,by=sep_lineas*fr_muestreo)
ind_linea[1] = 1

# creacion etiquetas de tiempo
num_t  = (0:n_puntos)*(salto/fr_muestreo) + ini_t
num_t  = num_t[ind_t]
num_hh = floor(num_t/(60*60))
num_mm = floor( (num_t - num_hh*60*60)/60 )
num_ss = floor(  num_t - num_hh*60*60 - num_mm*60 )
num_ms = num_t - num_hh*60*60 - num_mm*60 - num_ss
txt_t  = character(length(num_t))
for(i in 1:(length(num_t))){
  txt_mm = toString(num_mm[i])
  if(num_mm[i]<10){
    txt_mm = paste0('0',num_mm[i])
  }
  txt_ss = toString(num_ss[i])
  if(num_ss[i]<10){
    txt_ss = paste0('0',num_ss[i])
  }
  txt_t[i] = paste0(toString(num_hh[i]),':',txt_mm,':',txt_ss)
}

# etiquetas para guardar el archivo
i = 1
txt_mm = toString(num_mm[i])
if(num_mm[i]<10){
  txt_mm = paste0('0',num_mm[i])
}
txt_ss = toString(num_ss[i])
if(num_ss[i]<10){
  txt_ss = paste0('0',num_ss[i])
}
tag_tiempo = paste0(toString(num_hh[i]),txt_mm,txt_ss)
i = length(num_t)
txt_mm = toString(num_mm[i])
if(num_mm[i]<10){
  txt_mm = paste0('0',num_mm[i])
}
txt_ss = toString(num_ss[i])
if(num_ss[i]<10){
  txt_ss = paste0('0',num_ss[i])
}
tag_tiempo = paste0(tag_tiempo,
                    toString(num_hh[i]),txt_mm,txt_ss)

#################################################
# colores del grafico
vec_colores = c(rgb(132,120, 32,maxColorValue=255),  # C3
                rgb(132,120, 32,maxColorValue=255),  # C4
                rgb(132,120, 32,maxColorValue=255),  # CZ
                rgb(166, 58, 40,maxColorValue=255),  # F3
                rgb(166, 58, 40,maxColorValue=255),  # F4
                rgb(166, 58, 40,maxColorValue=255),  # F7
                rgb(166, 58, 40,maxColorValue=255),  # F8
                rgb(128,  0,128,maxColorValue=255),  # FP1
                rgb(128,  0,128,maxColorValue=255),  # FP2
                rgb(166, 58, 40,maxColorValue=255),  # FZ
                rgb( 72, 64, 65,maxColorValue=255),  # O1
                rgb( 72, 64, 65,maxColorValue=255),  # O1
                rgb( 29,118, 68,maxColorValue=255),  # P3
                rgb( 29,118, 68,maxColorValue=255),  # P4
                rgb( 29,118, 68,maxColorValue=255),  # PZ
                rgb( 64, 35,113,maxColorValue=255),  # T3
                rgb( 64, 35,113,maxColorValue=255),  # T4
                rgb( 64, 35,113,maxColorValue=255),  # T5
                rgb( 64, 35,113,maxColorValue=255),  # T3
                rgb( 50,116,  0,maxColorValue=255),  # LOG
                rgb( 50,116,  0,maxColorValue=255),  # ROG
                rgb( 31, 62,119,maxColorValue=255)  # EMG
                )
if(stam){
  vec_colores = vec_colores[orden_stam]
}

#################################################
# inicia guardado de los graficos
if(grabar){
  tag = ''
  if(stam){
    tag = '_stam'
  }
  
  setwd(dir_res)
  png(
  #pdf(
    paste0(etiqueta,'_epoca',tag,'_',tag_tiempo,
           #'.pdf'),width=8.5,height=5)
           '.png'),units='in',res=150,width=8.5,height=5)
}

#################################################
# graficacion
par(#bg = 'NA', 
    bg = 'white',
    mar = c(1,1.5,0,0))
par(cex.axis=.75, cex.lab=.8, cex.main=1)

plot(c(1,max(ind_pt)),
  type='o',col='white',
  ylim=c(.5*escala_mv,(n_canales-.5)*escala_mv),
  xlim=c(1,max(ind_pt)),
  #xlab = 'Tiempo [hh:mm:ss]',
  ylab = '',
  yaxt='n',xaxt='n',
  #main=paste0('Sujeto : ',etiqueta,anotaciones),
  main='',
  mgp=c(2,1,0),
  bty='n'
)
for(i in ind_linea){
 abline(v=i,col='gray')
}

#################################################
# inicio ciclo que recorre canales
setwd(dir_datos)
for(ch in 1:n_canales){
  # cargar los datos
  ch_actual = canales[ch]
  nom_ar    = paste0(nombre,'_',ch_actual,'_',hora_t,'.txt')
  DATA      = scan(nom_ar)
  if(zoom){
    DATA    = DATA[str_pt:end_pt]
  }
  agregado = (n_canales-ch)*escala_mv+escala_mv/2 - mean(DATA)
  lines(ind_pt,DATA[ind_pt] + agregado,
        type='l',col=vec_colores[ch])
  axis(2,at=(n_canales-ch)*escala_mv+escala_mv/2,
       labels=ch_actual,las=1,#col.axis=vec_colores[ch],
       mgp=c(0,0,-1.75),tick=F)
}
# fin ciclo que recorre canales
#################################################

par(mgp=c(0,0,0))
for(i in 1:length(ind_t)){
  axis(1,at=ind_t[i]*salto,labels=txt_t[i],tick = T)
}
#axis(3,at=c(min(ind_t),max(ind_t))*salto,labels=F)

lines(sep_lineas*fr_muestreo*c(.5,1.5),
      escala_mv*c(1,1),
      type='l',lwd=1.5)
lines(sep_lineas*fr_muestreo*.5*c(1,1),
      escala_mv+c(0,10),
      type='l',lwd=1.5)

if(grabar){
  dev.off()
}
# inicia guardado de los graficos
#################################################