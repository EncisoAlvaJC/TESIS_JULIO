###############################################################################
# directorio de trabajo
dir_actual  = '~/TESIS/TESIS/img_resultados'
dir_graf    = '~/TESIS/TESIS/img_resultados'
dir_res_mid = '~/TESIS/graf_datos/espectro_new_15s'

###############################################################################
# parametros
#sujeto     = 2
grabar_tot = T

no_relativo = T

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

dur_epoca   = 15
if(fr_muestreo==512){
  dur_epo_reg = 30
}
if(fr_muestreo==200){
  dur_epo_reg = 10
}

stam         = T
orden_stam  = c(9,8,7,6,5,4,17,16,2,1,19,18,14,13,12,11,10,3,15,20,21,22)

grabar      = F
anotaciones = ''

reemplazar  = TRUE
canales      = 'PSG'
#canales     = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
#                'P3','P4','PZ','T3','T4','T5','T6')

binario = F
escala  = F

etiquetas_tiempo = F

zoom           = F
#unidad_par_t   = 'tiempo'
#ajuste_ini_hms = c(0,0,0)
#min_hms        = c(3,17,20)
#max_hms        = c(3,19,10)
unidad_par_t   = 'puntos'
ajuste_ini_epo = 0
min_epo        = 0
max_epo        = 0

# parametros de dibujo
paso    = 15*4

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

###############################################################################

#################################################
# inicia grafico
k = 1.5
setwd(dir_actual)
if(grabar_tot){
  if(no_relativo){
    tag = 'total'
  }else{
    tag = 'relativo'
  }
  
  setwd(dir_actual)
  #pdf(
  png(
    paste0(nombre,'_espectral_',tag,
           #'.pdf'),width=5.941*k,height=1*k)
           '.png'),units='in',res=300,width=5.941*k,height=9*k)
}

###############################################################################
# meta-graficacion

# contadores
qq   = .925/7
cont = .025+6*qq

# grafico principal
par(oma=c(0,0,0,0),
    mar=c(.25, 2+1.5, .25, 3+1),
    mgp=c(1.5,.5,0))

setwd(dir_actual)
par(fig=c(0,1,cont,cont+qq), new=FALSE) #new=FALSE)
que.banda = 1
source('~/TESIS/TESIS/img_resultados/graf_espectro_integrado04_parte.R')

# graficacion de epocas
par(fig=c(0,1,.02,.955), new=TRUE,
    mar=c(0,2+1.5,0,3+1),
    mgp=c(0,-.4,0),
    oma=c(0,0,0,0))
setwd(dir_epocas)
arch_indice_e = paste0('epocas_mor_',nombre,'.txt')
indice_e      = scan(arch_indice_e)
factor.extra = 1
if(fr_muestreo==200){
  factor.extra = 3
}
#plot(0,type='n',xaxt='n',yaxt='n',xlab='',ylab='',#bty='n',
#     xlim=c(0,n_epocas)+.5,ylim=c(0,1))
te = t(RES[rev(1:n_canales),])*0-1
te[1,1]=.2
te[1,2]=.2
colorgram(z=te,outlier='white',
          colFn = grayscale,
          bty='n',axes=F,
          xlab='Tiempo [hh]',ylab='',zlab='',
          breaks=seq(0,1),
          key=0)
for(i in indice_e){
  rect(i/(factor.extra/2),-1,
       (i+1)/(factor.extra/2),25,
       col='green',
       border=NA)
}
skip = seq(1,n_epocas+1,by=paso*4)+.5
skap = seq(1,length(txt_t),by=4*15*60/dur_epoca)
axis(1,at=skip-1,labels=txt_t[skap],las=1,tick=F)

# grafico principal
par(oma=c(0,0,0,0),
    mar=c(.25, 2+1.5, .25, 3+1),
    mgp=c(1.5,.5,0))

cont = cont + qq
que.banda = 0
for(qb in 1:5){
  cont = cont - qq
  setwd(dir_actual)
  que.banda = que.banda + 1 
  par(fig=c(0,1,cont,cont+qq), new=TRUE)
  source('~/TESIS/TESIS/img_resultados/graf_espectro_integrado04_parte.R')
}

# potencia total
cont = cont - qq
setwd(dir_actual)
par(fig=c(0,1,cont,cont+qq), new=TRUE)
source('~/TESIS/TESIS/img_resultados/graf_espectro_integrado04_varianza.R')

# pseudo-color
cont = cont - qq
setwd(dir_actual)
par(fig=c(0,1,cont,cont+qq), new=TRUE)
#source('~/TESIS/TESIS/img_resultados/graf_espectro_integrado04_exponente.R')
source('~/TESIS/TESIS/img_resultados/graf_espectro_integrado04_slow.R')

#qq = qq*(2/3)

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