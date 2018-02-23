###############################################################################
# directorio de trabajo
dir_actual = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/Tesis/scripts_graf'
dir_graf   = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/Tesis/scripts_graf_res'
info_dir   = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/Tesis/scripts_graf'
dir_datos  = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/estacionariedad_sf'

###############################################################################
# parametros
#sujeto     = 2

orden_stam = T

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
kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'),
                      sheet='Alfabetico')
if(orden_stam){
  kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'),
                        sheet='Stam')
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

###############################################################################
# parametros del script
setwd(dir_actual)
source('utileria.R')

###############################################################################
# parametros del script
nombre      = info$Nombre_archivo[sujeto]
etiqueta    = info$Nombre[sujeto]
dir_res_mid = dir_datos
fr_muestreo = info$Fr_muestreo[sujeto]

binario = T

zoom           = T
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(0,0,0)
min_hms        = c(0,0,0)
max_hms        = c(info$hh_ff[sujeto],info$mm_ff[sujeto],0)
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

#################################################
# epocas de suenno MOR
ar_indice = read_excel(paste0(info_dir,'/info_tecnico.xlsx'),
                       sheet='EpocasTesis')
indice    = ar_indice[,etiqueta]
indice    = as.numeric(unlist(indice))
indice    = indice[!is.na(indice)]

if(fr_muestreo==200){
  indixe = ceiling(indice/3)
  indixe = unique(indixe)
  indixe = sort(indixe)
  indice = indixe
}

RES.MOR         = rep(0,hms2t(max_hms)/30)
RES.MOR[indice] = 1

RES.MOR         = as.data.frame(RES.MOR)
colnames(RES.MOR)[1] = 'Etapa'
RES.MOR$Indice  = (1:length(RES.MOR$Etapa) -1)*30
RES.MOR$Relleno = RES.MOR$Etapa*0
RES.MOR$Etapa   = factor(RES.MOR$Etapa,labels = c('NMOR','MOR'))
RES.MOR$Relleno = factor(RES.MOR$Relleno,
                         labels=c('Etapa'))

RES.MOR$Indice = (RES.MOR$Indice-1)
RES.MOR$Indice = as.POSIXct(as.hms(RES.MOR$Indice))

MOR.graf =  ggplot(RES.MOR,aes(x=Indice,y=Relleno,fill=Etapa)) +
  geom_raster() +
  xlab('Tiempo [hh:mm]') + ylab(NULL) +
  theme_bw() +
  scale_x_datetime(expand=c(0,0),labels=date_format("%H:%M"),
                   breaks = date_breaks("20 min"))+
  #scale_x_datetime(expand=c(0,0),breaks = NULL)+
  scale_y_discrete(expand=c(0,0)) +
  scale_fill_manual(values=c('#acff81','#077813'))+
  #labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
  theme(legend.position='bottom') +
  facet_grid(Relleno~.)+
  theme(legend.title=element_blank()) +
  theme(strip.background = element_blank(),
        strip.text.y = element_text(color = 'white'))+
  #theme(axis.ticks = element_blank(),axis.text.x = NULL)+
  rotate_x_text(angle = 45)

###############################################################################
# meta-graficacion
RES.collect = data.frame(Indice=as.POSIXct(character()),
                         Canal_var=character(),
                         Estacionario=character(),
                         D_chunk=integer(),
                         stringsAsFactors=F)

#for(expon in c(2,0,-2)){
#for(expon in (-5):-2){
for(expon in (-1):(2)){
  setwd(dir_actual)
  dur_epoca = 30*(2**expon)
  setwd(dir_actual)
  source('colorcitos_usable05_parte.R')
}

RES.collect$D_chunk = log2(RES.collect$D_chunk/30)

EST.graf = ggplot(RES.collect,aes(x=Indice,y=Canal_var,fill=Estacionario)) +
  geom_raster() +
  xlab(NULL) + ylab(NULL) +
  theme_bw() +
  #scale_x_datetime(expand=c(0,0),labels=date_format("%H:%M"),
  #                 breaks = date_breaks("20 min"))+
  scale_x_datetime(expand=c(0,0),breaks = NULL)+
  scale_y_discrete(expand=c(0,0),
                   limits=rev(levels(RES.extenso$Canal_var))) +
  scale_fill_manual(values=c('white','black'))+
  labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
  theme(legend.position='bottom') +
  theme(legend.title=element_blank()) +
  theme(legend.key = element_rect(color = 'black')) +
  facet_grid(D_chunk~.,as.table = T,
             labeller=label_bquote(rows=30%.%2^.(D_chunk)))+
  rotate_x_text(angle = 45)

ggarrange(EST.graf,MOR.graf,
          ncol=1,nrow=2,align = 'v',common.legend = TRUE,
          heights = c(.9,.1),legend = 'bottom')

ggsave(filename = paste0(nombre,'_comp_est_part1.png'),
       path = dir_graf,units='cm',dpi=600,width=21,height=29.7)

###############################################################################
# meta-graficacion
RES.collect = data.frame(Indice=as.POSIXct(character()),
                         Canal_var=character(),
                         Estacionario=character(),
                         D_chunk=integer(),
                         stringsAsFactors=F)

#for(expon in c(2,0,-2)){
for(expon in (-5):-2){
#for(expon in (-1):(2)){
  setwd(dir_actual)
  dur_epoca = 30*(2**expon)
  setwd(dir_actual)
  source('colorcitos_usable05_parte.R')
}

RES.collect$D_chunk = log2(RES.collect$D_chunk/30)

EST.graf = ggplot(RES.collect,aes(x=Indice,y=Canal_var,fill=Estacionario)) +
  geom_raster() +
  xlab(NULL) + ylab(NULL) +
  theme_bw() +
  #scale_x_datetime(expand=c(0,0),labels=date_format("%H:%M"),
  #                 breaks = date_breaks("20 min"))+
  scale_x_datetime(expand=c(0,0),breaks = NULL)+
  scale_y_discrete(expand=c(0,0),
                   limits=rev(levels(RES.extenso$Canal_var))) +
  scale_fill_manual(values=c('white','black'))+
  labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
  theme(legend.position='bottom') +
  theme(legend.title=element_blank()) +
  theme(legend.key = element_rect(color = 'black')) +
  facet_grid(D_chunk~.,as.table = T,
             labeller=label_bquote(rows=30%.%2^.(D_chunk)))+
  rotate_x_text(angle = 45)

ggarrange(EST.graf,MOR.graf,
          ncol=1,nrow=2,align = 'v',common.legend = TRUE,
          heights = c(.9,.1),legend = 'bottom')

ggsave(filename = paste0(nombre,'_comp_est_part2.png'),
       path = dir_graf,units='cm',dpi=600,width=21,height=29.7)

# fin
###############################################################################

stop()

RES.algo = RES.collect[is.element(RES.collect$Canal_var,
                                  c('P4','P3','PZ','LOG','ROG','EMG')),]
RES.algo$Canal_var = droplevels(RES.algo$Canal_var)

EST.graf = 
  ggplot(RES.algo,aes(x=Indice,y=Banda.nombre,fill=(Potencia))) +
  geom_raster() +
  xlab(NULL) + ylab(NULL) +
  theme_bw() +
  scale_x_datetime(expand=c(0,0),breaks = NULL)+
  scale_y_discrete(expand=c(0,0),
                   limits=rev(levels(RES.algo$Banda.nombre))) +
  #scale_fill_distiller(palette='Spectral')+
  scale_fill_gradientn(colors = jet.colors(7)) +
  labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
  theme(legend.position='bottom') +
  labs(fill='Log(Área bajo la curva)') +
  facet_grid(Canal_var~.)+
  theme(strip.text.y = element_text(size = 12)) +
  rotate_x_text(angle = 45)

ggarrange(SPEC.graf,MOR.graf,
          ncol=1,nrow=2,align = 'v',common.legend = TRUE,
          heights = c(.9,.1),legend = 'bottom')

ggsave(filename = paste0(nombre,'_espectral_EOG_EMG_antes.png'),
       path = dir_graf,units='cm',dpi=600,width=21,height=29.7,
       scale=1)

SPEC.graf = ggplot(RES.algo,aes(x=Indice,y=Canal_var,fill=(Potencia))) +
  geom_raster() +
  xlab(NULL) + ylab(NULL) +
  theme_bw() +
  scale_x_datetime(expand=c(0,0),breaks = NULL)+
  scale_y_discrete(expand=c(0,0),
                   limits=rev(levels(RES.algo$Canal_var))) +
  #scale_fill_distiller(palette='Spectral')+
  scale_fill_gradientn(colors = jet.colors(7)) +
  labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
  theme(legend.position='bottom') +
  labs(fill='Log(Área bajo la curva)') +
  facet_grid(Banda.nombre~.)+
  theme(strip.text.y = element_text(size = 12)) +
  rotate_x_text(angle = 45)

ggarrange(SPEC.graf,MOR.graf,
          ncol=1,nrow=2,align = 'v',common.legend = TRUE,
          heights = c(.9,.1),legend = 'bottom')

ggsave(filename = paste0(nombre,'_espectral_EOG_EMG_nuevo.png'),
       path = dir_graf,units='cm',dpi=600,width=21,height=29.7,
       scale=1)
