###############################################################################
# directorio de trabajo
dir_actual  = '~/TESIS/TESIS/img_resultados'
dir_graf    = '~/TESIS/TESIS/img_art_dfa'
#dir_res_mid = '~/TESIS/graf_datos/espectro_new_15s'
dir_res_mid = '~/TESIS/graf_datos/espectro_integrado_15s'
info_dir    = '~/TESIS/TESIS/articulo_dfa'

###############################################################################
# parametros
#sujeto     = 2
grabar_tot = T

no_relativo = T

grabar      = F
anotaciones = ''

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

orden_stam = T

###############################################################################
# librerias
require('readxl')

require('ggplot2')
require('ggpubr')

require('Rmisc')
require('reshape')

require('scales')

require('squash')

require('hms')

###############################################################################
# datos generales
info     = read_excel(paste0(info_dir,'/info_tecnico.xlsx'))

orden_k  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(info_dir,'/info_canales_alterno.xlsx'))
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

banda.n = c('Delta','Theta','Alfa','Beta','Gamma','Potencia total',
            'Ondas lentas fuera de rango','Ondas rapidas fuera de rango')
banda   = c('DELTA','THETA','ALFA','BETA','GAMMA','TOTAL','SUB','SUPER')
nbandas = length(banda.n)

###############################################################################
# parametros del script
nombre      = info$Nombre_archivo[sujeto]
etiqueta    = info$Nombre[sujeto]
fr_muestreo = info$Fr_muestreo[sujeto]

if(info$Grupo_n[sujeto]==0){
  grupo = 'CTL'
}
if(info$Grupo_n[sujeto]==1){
  grupo = 'PDC'
}
if(info$Grupo_n[sujeto]==-1){
  grupo = 'EX'
}

dur_epoca   = 15
if(fr_muestreo==512){
  dur_epo_reg = 30
}
if(fr_muestreo==200){
  dur_epo_reg = 10
}

###############################################################################
# recoleccion de datos
RES.collect = data.frame(Indice=as.POSIXct(character()),
                         Canal_var=character(),
                         Banda.nombre=character(),
                         Potencia=integer(),
                         stringsAsFactors=F)

for(qb in 1:5){
  que.banda = qb
  source('~/TESIS/TESIS/img_resultados/graf_espectro_integrado05_parte.R')
}

RES.collect$Banda.nombre[RES.collect$Banda.nombre=='Delta'] = 1
RES.collect$Banda.nombre[RES.collect$Banda.nombre=='Theta'] = 2
RES.collect$Banda.nombre[RES.collect$Banda.nombre=='Alfa']  = 3
RES.collect$Banda.nombre[RES.collect$Banda.nombre=='Beta']  = 4
RES.collect$Banda.nombre[RES.collect$Banda.nombre=='Gamma'] = 5

RES.collect$Banda.nombre = factor(RES.collect$Banda.nombre,
                                  labels=c('Delta','Theta','Alfa','Beta','Gamma'))

###############################################################################
# ajustes graficos
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", 
                                 "#7FFF7F", "yellow", "#FF7F00", "red", 
                                 "#7F0000"))

summary(RES.collect$Potencia)

mm = mean(RES.collect$Potencia,na.rm = T)
de = 3*sd(RES.collect$Potencia,na.rm = T)

RES.collect$Potencia[RES.collect$Potencia>(mm+de)] = mm+de
RES.collect$Potencia[RES.collect$Potencia<(mm-de)] = mm-de

###############################################################################
# meta-graficacion
ggplot(RES.collect,aes(x=Indice,y=Canal_var,fill=exp(Potencia))) +
  geom_raster() +
  xlab('Tiempo [hh:mm]') + ylab(NULL) +
  theme_bw() +
  scale_x_datetime(expand=c(0,0),labels=date_format("%H:%M"),
                   breaks = date_breaks("20 min"))+
  scale_y_discrete(expand=c(0,0),
                   limits=rev(levels(RES.largo$Canal_var))) +
  #scale_fill_distiller(palette='Spectral')+
  scale_fill_gradientn(colors = jet.colors(7)) +
  labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
  #labs(subtitle=paste('Potencia total en banda:',banda.n[que.banda])) +
  theme(legend.position='bottom') +
  #theme(legend.title=element_blank()) +
  labs(fill='Potencia absoluta') +
  facet_grid(Banda.nombre~.)+
  theme(strip.text.y = element_text(size = 12)) +
rotate_x_text(angle = 45)

ggsave(filename = paste0(nombre,'_espectral_','total','.png'),
       units='in',dpi=600,width=6,height=8,device='png',
       path=dir_graf,scale=1.5)

stop('Debug 1')

# potencia total
cont = cont - qq
setwd(dir_actual)
par(fig=c(0,1,cont,cont+qq), new=TRUE)
source('~/TESIS/TESIS/img_resultados/graf_espectro_integrado04_varianza.R')

# pseudo-color
cont = cont - qq
setwd(dir_actual)
par(fig=c(0,1,cont,cont+qq), new=TRUE)
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