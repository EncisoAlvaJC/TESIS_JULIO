###############################################################################
# carpeta central
data_dir    = '~/TESIS/graf_datos/estacionariedad_sf/'
central_dir = '~/TESIS/TESIS/img_art_dfa'
info_dir    = '~/TESIS/TESIS/articulo_dfa'
g_dir       = '~/TESIS/TESIS/img_art_dfa'
setwd(central_dir)

###############################################################################
# librerias
require('readxl')

require('ggplot2')
require('ggpubr')

require('Rmisc')
require('reshape')

require('scales')

#################################################
# constantes generales
info     = read_excel(paste0(info_dir,'/info_tecnico.xlsx'),
                      sheet='Tesis_graf')
info     = info[1:10,]
info$Grupo_n = factor(info$Grupo_n,labels=c('CTL','PDC'))

ctl = info[1:5,]
pdc = info[6:10,]

t.test(ctl$Edad,pdc$Edad)

cor.test(info$Edad,info$Neuropsi,method='spearman')

ggplot(info,aes(x=Edad,y=Neuropsi,color=Grupo_n,shape=Grupo_n)) +
  geom_point() +
  scale_color_discrete(direction = -1) +
  theme_bw()

ggplot(info,aes(x=Edad,y=Escolaridad,color=Grupo_n,shape=Grupo_n)) +
  geom_point() +
  scale_color_discrete(direction = -1) +
  theme_bw()

ggplot(info,aes(x=Escolaridad,y=Neuropsi)) +
  geom_point()