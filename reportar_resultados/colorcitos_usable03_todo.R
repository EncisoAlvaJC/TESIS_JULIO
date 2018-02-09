###############################################################################
# directorio de trabajo
dir_actual = '~/TESIS/TESIS/reportar_resultados'
dir_graf   = '~/TESIS/TESIS/reportar_resultados'
dir_datos  = '~/TESIS/graf_datos/estacionariedad_sinfiltro/'
dir_epocas = '~/TESIS/graf_datos/epocas_dfa/'

###############################################################################
# parametros
#sujeto     = 2
grabar_tot = F

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

h_ini = c( 1, 1, 0, 3, 2, 1, 1, 1, 3, 2, 1, 1)
m_ini = c(55,28,51,14,13,19,57,53, 0,14,55,37)
s_ini = c( 0, 0, 0, 0,30,30,30, 0,30,30,30,30)

h_fin = c( 2, 1, 1, 3, 2, 1, 2, 2, 3, 2, 2, 1)
m_fin = c(15,48,11,34,33,39,17,13,20,34,15,57)
s_fin = c( 0, 0, 0, 0,30,30,30, 0,30,30,30,30)

###############################################################################
# parametros del script
nombre      = v.nombres[sujeto]
etiqueta    = v.etiqueta[sujeto]
dir_res_mid = paste0(dir_datos,v.directorio[sujeto])
fr_muestreo  = frecuenciasss[sujeto]

stam         = T

grabar      = F
anotaciones = ''

reemplazar  = TRUE
canales      = 'PSG'
#canales     = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
#                'P3','P4','PZ','T3','T4','T5','T6')

binario = T
#p.vales = c(.05,.01,.005)
escala  = F

zoom           = T
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(0,0,0)
min_hms        = c(h_ini[sujeto],m_ini[sujeto],s_ini[sujeto])
max_hms        = c(h_fin[sujeto],m_fin[sujeto],s_fin[sujeto])
#unidad_par_t   = 'puntos'
#ajuste_ini_epo = 0
#min_epo        = epoca_ini[sujeto]
#max_epo        = epoca_fin[sujeto]-1
#min_epo        = 0

# parametros de dibujo
paso    = 15*2

#################################################
# libreria especifica para el grafico tipo matriz
require('squash')

#################################################
# procesamiento parametros opcionales
if(reemplazar){
  if(canales=='10-20'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','T3','T4','T5','T6')
    if(stam){
      orden_stam  = c(9,8,7,6,5,4,17,16,2,1,19,18,14,13,12,11,10,3,15)
      canales = canales[orden_stam]
    }
  }
  if(canales=='PSG'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','T3','T4','T5','T6','LOG','ROG','EMG')
    if(stam){
      orden_stam  = c(9,8,7,6,5,4,17,16,2,1,19,18,14,13,12,11,10,3,15,20,21,22)
      canales = canales[orden_stam]
    }
  }
}
if(length(canales)<1){
  stop('ERROR: Lista de canales tiene longitud cero')
}

#################################################
# ajustes del zoom
dur.min = 30/32
if(zoom){
  validar.zoom = F
  if(unidad_par_t=='tiempo'){
    s.ini = sum(min_hms*c(60*60,60,1))
    s.fin = sum(max_hms*c(60*60,60,1))
    
    e.ini = s.ini/dur.min + 1
    e.fin = s.fin/dur.min
    
    n.epo = e.fin-e.ini
    
    validar.zoom = T
  }
}

#################################################
# parametros que dependen del sujeto
if(grupo_de[sujeto]==0){
  grupo = 'Nn'
}
if(grupo_de[sujeto]==1){
  grupo = 'Mn'
}
if(grupo_de[sujeto]==-1){
  grupo = 'ex'
}

#################################################
# inicia grafico
k = 1.5
setwd(dir_actual)
if(grabar_tot){
  setwd(dir_actual)
  #pdf(
  png(
    paste0(nombre,'_comp_est_',
           #'.pdf'),width=5.941*k,height=1*k)
           '.png'),units='in',res=300,width=5.941*k,height=9*k)
}

###############################################################################
# meta-graficacion

# grafico principal
par(oma=c(0,0,0,0),
    mar=c(.25, 2+1.5, .25, 2),
    mgp=c(1.5,.5,0))

# contadores
qq   = .925/8
cont = .025

setwd(dir_actual)
dur_epoca = 30/(2**5)
par(fig=c(0,1,cont,cont+qq), new=FALSE)
source('~/TESIS/TESIS/reportar_resultados/colorcitos_usable04_parte.R')

# contadores
qq   = .925/8
cont = .025

for(expon in -5:2){
  setwd(dir_actual)
  dur_epoca = 30*(2**expon)
  par(fig=c(0,1,cont,cont+qq), new=TRUE)
  source('~/TESIS/TESIS/reportar_resultados/colorcitos_usable04_parte.R')
  cont = cont + qq
}

# el titulo
par(oma=c(0,0,0,0),
    mar=c(0, 2, 2.5, 2),
    mgp=c(0,.5,0),
    fig=c(0,1,.95,1), new=TRUE)
title(paste0('Sujeto : ',etiqueta,'  | Grupo :  ',grupo),cex.main=2)

setwd(dir_actual)
if(grabar_tot){
  setwd(dir_actual)
  dev.off()
}
# fin guardado automatico del grafico
#################################################