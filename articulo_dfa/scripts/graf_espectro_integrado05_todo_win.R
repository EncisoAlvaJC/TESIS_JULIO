###############################################################################
# directorio de trabajo
dir_actual = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_graf   = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/test'
#dir_res_mid = '~/TESIS/graf_datos/espectro_new_15s'
dir_res_mid = 'C:/Users/EQUIPO 1/Desktop/julio/espectro_171124'
info_dir    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'

source('C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts/utileria.R')

###############################################################################
# parametros
#sujeto     = 2
grabar_tot = F

no_relativo = T

grabar      = F
anotaciones = ''

zoom           = T
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(0,0,0)
#min_hms        = c(3,17,20)
#max_hms        = c(3,19,10)
#unidad_par_t   = 'puntos'
#ajuste_ini_epo = 0
#min_epo        = 0
#max_epo        = 0

orden_stam = T

###############################################################################
# librerias
require('readxl')

require('ggplot2')
require('ggpubr')

require('Rmisc')
require('reshape')

require('scales')

#require('squash')

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

factor.extra = 1
if(fr_muestreo==200){
  factor.extra = 3
}

epo_0 = 30*(epo_0-1)
epo_f = 30*epo_f

min_hms = t2hms(dur_epoca*epo_0/factor.extra)
max_hms = t2hms(dur_epoca*epo_f/factor.extra)

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
  source('C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts/graf_espectro_integrado05_parte_win.R')
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
G2 = ggplot(RES.collect,aes(x=Indice,y=Canal_var,fill=(Potencia))) +
  geom_raster(hjust = 1) +
  xlab('Tiempo [hh:mm]') + ylab(NULL) +
  theme_bw() +
  scale_x_datetime(expand=c(0,0),labels=date_format("%H:%M:%S"),
                   breaks = date_breaks("30 sec"))+
  scale_y_discrete(expand=c(0,0),
                   limits=rev(levels(RES.largo$Canal_var))) +
  #scale_fill_distiller(palette='Spectral')+
  scale_fill_gradientn(colors = jet.colors(7)) +
  #labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
  #labs(subtitle=paste('Potencia total en banda:',banda.n[que.banda])) +
  theme(legend.position='bottom') +
  #theme(legend.title=element_blank()) +
  labs(fill='Potencia absoluta') +
  facet_grid(Banda.nombre~.)+
  theme(strip.text.y = element_text(size = 12)) +
  rotate_x_text(angle = 45)