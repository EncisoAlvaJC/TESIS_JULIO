###############################################################################
# directorio de trabajo
dir_actual = '~/TESIS/TESIS/img_ejemplos'
dir_graf   = '~/TESIS/TESIS/img_art_dfa'
info_dir    = '~/TESIS/TESIS/articulo_dfa'
dir_datos  = '~/TESIS/graf_datos/estacionariedad_sf/'
dir_epocas = '~/TESIS/graf_datos/epocas3/'

###############################################################################
# parametros
#sujeto     = 2
grabar_tot  = F
grabar      = F
anotaciones = ''

orden_stam = T

# parametros de dibujo
paso    = 15*2

###############################################################################
# librerias
require('readxl')

require('ggplot2')
require('ggpubr')

require('Rmisc')
require('reshape')

require('scales')

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

###############################################################################
# parametros del script
nombre      = info$Nombre_archivo[sujeto]
etiqueta    = info$Nombre[sujeto]
dir_res_mid = dir_datos
fr_muestreo = info$Fr_muestreo[sujeto]

binario = T

zoom           = F
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(0,0,0)
#min_hms        = c(3,17,20)
#max_hms        = c(3,19,10)
#unidad_par_t   = 'puntos'
#ajuste_ini_epo = 0
#min_epo        = epoca_ini[sujeto]
#max_epo        = epoca_fin[sujeto]-1
#min_epo        = 0

#################################################
# parametros que dependen del sujeto
if(info$Grupo_n[sujeto]==0){
  grupo = 'CTL'
}
if(info$Grupo_n[sujeto]==1){
  grupo = 'PDC'
}
if(info$Grupo_n[sujeto]==-1){
  grupo = 'EX'
}

###############################################################################
# meta-graficacion

#source('~/TESIS/TESIS/img_ejemplos/colorcitos_usable04_parte.R')
RES.collect = data.frame(Indice=as.POSIXct(character()),
                         Canal_var=character(),
                         Estacionario=character(),
                         D_chunk=integer(),
                         stringsAsFactors=F)

stop('Espacio para usar otros scripts')

for(expon in c(2,0,-2)){
  setwd(dir_actual)
  dur_epoca = 30*(2**expon)
  source('~/TESIS/TESIS/img_ejemplos/colorcitos_usable04_parte.R')
  
}

RES.collect$D_chunk = log2(RES.collect$D_chunk/30)
escala_labeller = function(variable,value){
  return(paste0('30*2^',value))
}

print(
  ggplot(RES.collect,aes(x=Indice,y=Canal_var,fill=Estacionario)) +
    geom_raster() +
    xlab('Tiempo [hh:mm]') + ylab(NULL) +
    theme_bw() +
    #scale_x_discrete(expand=c(0,0)) +
    scale_x_datetime(expand=c(0,0),labels=date_format("%H:%M"),
                     breaks = date_breaks("20 min"))+
    scale_y_discrete(expand=c(0,0),
                     limits=rev(levels(RES.extenso$Canal_var))) +
    #scale_fill_manual(values=c('white','black','#acff81','#077813'))+
    scale_fill_manual(values=c('white','black'))+
    #labs(title=paste('Época =',toString(dur_epoca),'s'),
    #     subtitle=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
    labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
    #theme(legend.position=c(1,1),legend.direction = 'horizontal',
    #      legend.justification=c(1,0))+
    theme(legend.position='bottom') +
    theme(legend.title=element_blank()) +
    facet_grid(D_chunk~.,as.table = T,
               labeller=label_bquote(rows=30%.%2^.(D_chunk)))+
    rotate_x_text(angle = 45)
)

if(grabar_tot){
  ggsave(filename = paste0(nombre,'_comp_est_','.png'),
         path = dir_graf,units='in',dpi=600,width=6,height=8)
}
# fin guardado automatico del grafico
#################################################