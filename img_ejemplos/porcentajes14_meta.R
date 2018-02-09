###############################################################################
# carpeta central
data_dir    = '~/TESIS/graf_datos/estacionariedad_sf/'
central_dir = '~/TESIS/TESIS/img_art_dfa'
info_dir    = '~/TESIS/TESIS/articulo_dfa'
g_dir       = '~/TESIS/TESIS/img_art_dfa'
setwd(central_dir)

#################################################
# parametros del script

dur_chunk = 30/(2**exponente)

p.val = .01

grabar.gral = T

graf.indv   = T
grabar.indv = F

grabar.ast  = T
p.ast  = c(.05,.01,.005,.001)
ast    = c(' ','*','**','***','****')

guardar_archivo = F
nombre_archivo  = paste0('asteriscos_',toString(dur_chunk),'.csv')

orden_stam = T

quienes = 1:10

###############################################################################
# librerias
require('readxl')
#require('xlsx')

require('ggplot2')
require('ggpubr')
#require('colo')

require('Rmisc')
require('reshape')

require('scales')
require('hms')

#################################################
# constantes generales
info     = read_excel(paste0(info_dir,'/info_tecnico.xlsx'))

orden_k  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(info_dir,'/info_canales_alterno.xlsx'))
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

#n.participantes = length(info$Nombre)
n.participantes = length(quienes)

#################################################
# contenedores de datos
dif_significativas            = matrix(nrow=n.canales,
                                       ncol=n.participantes)
colnames(dif_significativas)  = info$Nombre[quienes]
row.names(dif_significativas) = kanales$Etiqueta

matriz_mor  = matrix(nrow=n.canales,ncol=n.participantes+2)
matriz_nmor = matrix(nrow=n.canales,ncol=n.participantes+2)
matriz_tot  = matrix(nrow=n.canales,ncol=n.participantes+2)

colnames(matriz_mor)   = c(info$Nombre[quienes],'Canal_var','Etapa')
colnames(matriz_nmor)  = c(info$Nombre[quienes],'Canal_var','Etapa')
colnames(matriz_tot)   = c(info$Nombre[quienes],'Canal_var','Etapa')

matriz_mor[,'Canal_var']  = 1:n.canales
matriz_nmor[,'Canal_var'] = 1:n.canales
matriz_tot[,'Canal_var']  = 1:n.canales

matriz_mor[,'Etapa']  = rep('MOR',n.canales)
matriz_nmor[,'Etapa'] = rep('NMOR',n.canales)
matriz_tot[,'Etapa']  = rep('Total',n.canales)

#################################################
# cargar los datos
for(sujeto in 1:n.participantes){
  setwd(central_dir)
  source('~/TESIS/TESIS/img_ejemplos/porcentajes14.R')
}

#################################################
# diferencias significativas MOR VS NMOR
if(grabar.ast){
  setwd(g_dir)
  write.csv(dif_significativas,file=nombre_archivo)
}

#################################################
# separacion de grupos para comparar
matriz  = rbind(matriz_mor,matriz_nmor)
matriz  = as.data.frame(matriz)
matriz2 = melt(matriz,id=c('Canal_var','Etapa'))

matriz2$value = as.numeric(as.character(matriz2$value))

# parche
grupos  = as.data.frame(c(rep(0,44*5),rep(1,44*5)))
matriz2 = cbind(matriz2,grupos)
matriz2 = cbind(matriz2,grupos)

colnames(matriz2) = c('Canal_var','Etapa','Participante',
                      'Proporcion','Grupo','GrupoEtapa')

#droplevels(matriz2$Proporcion)
#matriz2$Proporcion = droplevels(matriz2$Proporcion)

matriz2$GrupoEtapa = 2*matriz2$Grupo + 1*(matriz2$Etapa=='MOR')
matriz2$Canal_var  = factor(matriz2$Canal_var,
                            labels=kanales$Etiqueta)
matriz2$Grupo      = factor(matriz2$Grupo,
                            labels=c('CTL','PDC'))
matriz2$GrupoEtapa = factor(matriz2$GrupoEtapa,
                            labels=c('CTL NMOR','CTL MOR',
                                     'PDC NMOR','PDC MOR'))

ggplot(matriz2,aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa))+
  geom_boxplot() +
  xlab(NULL) + ylab('Épocas estacionarias [%]') +
  theme_bw() +
  scale_y_continuous(labels=percent) +
  scale_fill_brewer(palette='Paired') +
  theme(legend.position='bottom') +
  labs(fill=NULL) +
  labs(title=paste('Época =',toString(dur_chunk),'s')) +
  facet_grid(Grupo~.) +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns = T,paired = F)+
  #stat_compare_means(label = 'p.format',method='wilcox.test',
  #                  hide.ns = T)+
  rotate_x_text(angle = 45)
if(grabar.gral){
  ggsave(filename=paste0('Comparacion_gpos_CTL_PDC_v3_',
                         toString(dur_chunk),'.pdf'),
         path=g_dir,device='pdf',
         width=6,height=5,unit='in')
}

ggplot(matriz2,aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa))+
  geom_boxplot() +
  xlab(NULL) + ylab('Épocas estacionarias [%]') +
  theme_bw() +
  scale_y_continuous(labels=percent) +
  scale_fill_brewer(palette='Paired') +
  theme(legend.position='bottom') +
  labs(fill=NULL) +
  labs(title=paste('Época =',toString(dur_chunk),'s')) +
  facet_grid(Etapa~.) +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns = T)+
  rotate_x_text(angle = 45)
if(grabar.gral){
  ggsave(filename=paste0('Comparacion_gpos_MOR_NMOR_v3_',
                         toString(dur_chunk),'.pdf'),
         path=g_dir,device='pdf',
         width=6,height=5,unit='in')
}

#################################################
# parametros graficos
#rojito    = rgb(255, 64, 64,maxColorValue=255)
#verdecito = rgb( 64,255, 64,maxColorValue=255)
#azulito   = rgb( 64, 64,255,maxColorValue=255)
#gricesito = rgb(128,128,128,maxColorValue=255)