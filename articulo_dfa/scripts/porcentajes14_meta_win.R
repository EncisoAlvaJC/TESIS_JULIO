###############################################################################
# carpeta central
data_dir    = 'C:/Users/EQUIPO 1/Desktop/julio/estacionariedad_171118'
central_dir = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/stat'
info_dir    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
g_dir       = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/stat'
setwd(central_dir)

#################################################
# parametros del script

dur_chunk = 30/(2**0)

p.val = .05

grabar.gral = T

graf.indv   = F
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
require('xlsx')

require('ggplot2')
require('ggpubr')
#require('colo')

require('Rmisc')
require('reshape')

require('scales')

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

matriz_mor[,'Etapa']  = rep('REM',n.canales)
matriz_nmor[,'Etapa'] = rep('NREM',n.canales)
matriz_tot[,'Etapa']  = rep('Total',n.canales)

#################################################
# cargar los datos
for(sujeto in 1:n.participantes){
  setwd(central_dir)
  source('C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/stat/porcentajes14.R')
}

#################################################
# diferencias significativas MOR VS NMOR
#if(grabar.ast){
#  setwd(g_dir)
#  write.csv(dif_significativas,file=nombre_archivo)
#}

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

matriz2$GrupoEtapa = 2*matriz2$Grupo + 1*(matriz2$Etapa=='REM')
matriz2$Canal_var  = factor(matriz2$Canal_var,
                            labels=kanales$Etiqueta)
matriz2$Grupo      = factor(matriz2$Grupo,
                            labels=c('CTRL','PMCI'))
matriz2$GrupoEtapa = factor(matriz2$GrupoEtapa,
                            labels=c('CTRL NREM','CTRL REM',
                                     'PMCI NREM','PMCI REM'))

ggplot(matriz2,aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa,
                   color=GrupoEtapa))+
  geom_boxplot() +
  xlab(NULL) + ylab('Stationary epoche [%]') +
  theme_classic2() +
  scale_y_continuous(labels=percent) +
  #scale_fill_brewer(palette='Paired') +
  #scale_fill_grey(start = 1, end = 0) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  theme(legend.position = 'top')+
  labs(fill=NULL) +
  labs(title=paste('Epoch =',toString(dur_chunk),
                   's | Whole night')) +
  facet_grid(Grupo~.) +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns = T)+
  rotate_x_text(angle = 45)
if(grabar.gral){
  ggsave(filename=paste0('completo_Comparacion_gpos_CTL_PDC_',
                         toString(dur_chunk),'.png'),
         path=g_dir,device='png',
         width=8,height=5,unit='in',dpi=400)
  ggsave(filename=paste0('completo_Comparacion_gpos_CTL_PDC_',
                         toString(dur_chunk),'.eps'),
         path=g_dir,device='eps',
         width=8,height=5,unit='in')
}

ggplot(matriz2,aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa))+
  geom_boxplot() +
  xlab(NULL) + ylab('Stationary epoche [%]') +
  theme_classic2() +
  scale_y_continuous(labels=percent) +
  #scale_fill_brewer(palette='Paired') +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  #theme(legend.position=c(1,1),legend.direction = 'horizontal',
  #      legend.justification=c(1,0))+
  theme(legend.position = 'top')+
  labs(fill=NULL) +
  labs(title=paste('Epoch =',toString(dur_chunk),
                   's | Whole night')) +
  facet_grid(Etapa~.) +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns = T)+
  rotate_x_text(angle = 45)
if(grabar.gral){
  ggsave(filename=paste0('completo_Comparacion_gpos_MOR_NMOR_',
                         toString(dur_chunk),'.png'),
         path=g_dir,device='png',
         width=8,height=5,unit='in',dpi=400)
  ggsave(filename=paste0('completo_Comparacion_gpos_MOR_NMOR_',
                         toString(dur_chunk),'.eps'),
         path=g_dir,device='eps',
         width=8,height=5,unit='in',dpi=400)
}

#################################################
# parametros graficos
matriz2 = melt(matriz,id=c('Canal_var','Etapa'))

matriz2$value = as.numeric(as.character(matriz2$value))

matriz2 = cbind(matriz2,grupos)
matriz2 = cbind(matriz2,grupos)



colnames(matriz2) = c('Canal_var','Etapa','Participante',
                      'Proporcion','Grupo','GrupoEtapa')

matriz2$GrupoEtapa = 2*matriz2$Grupo + 1*(matriz2$Etapa=='REM')



#droplevels(matriz2$Proporcion)
#matriz2$Proporcion = droplevels(matriz2$Proporcion)

#matriz2$Grupo = 1*(matriz2$Grupo=='CTRL')
#matriz2$Etapa = 1*(matriz2$Etapa=='REM')

matriz2$Canal_var  = factor(matriz2$Canal_var,
                            labels=kanales$Etiqueta)
matriz2$Grupo      = factor(matriz2$Grupo,
                            labels=c('CTRL','PMCI'))
matriz2$GrupoEtapa = factor(matriz2$GrupoEtapa,
                            labels=c('CTRL NREM','CTRL REM',
                                     'PMCI NREM','PMCI REM'))

promedio = summarySE(matriz2,measurevar='Proporcion',
                     groupvars=c('Canal_var','GrupoEtapa'))

promedio$Grupo = 1*(promedio$GrupoEtapa=='CTRL REM')+
  1*(promedio$GrupoEtapa=='CTRL NREM')
promedio$Etapa = 1*(promedio$GrupoEtapa=='CTRL REM')+
  1*(promedio$GrupoEtapa=='PMCI REM')

promedio$Grupo = factor(promedio$Grupo,
                        labels=c('PMCI','CTRL'))
promedio$Etapa = factor(promedio$Etapa,
                        labels=c('NREM','REM'))

ggplot(promedio,aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa)) +
  xlab(NULL) + ylab('Stationary epoche [%]') +
  labs(title=paste('Epoch =',toString(dur_chunk),
                   's | Whole night')) +
  theme_classic2() +
  labs(fill=NULL) +
  scale_y_continuous(labels=percent) +
  #geom_bar(stat='identity',position=position_dodge(),color='black')+
  geom_bar(stat='identity',position=position_dodge())+
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  #scale_fill_grey(start = 1, end = 0) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  #scale_y_continuous(expand=c(0,0))+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  coord_cartesian(ylim=c(0,.5))+
  stat_compare_means(data = matriz2,inherit.aes=F,
                     aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa),label = 'p.signif',method='wilcox.test',
                     hide.ns = T,
                     label.y =.5)+
  rotate_x_text(angle = 45)
if(grabar.gral){
  ggsave(filename=paste0('completo_Comparacion_gpos_CTL_PDC_',
                         toString(dur_chunk),
                         '_barra.png'),
         path=g_dir,device='png',
         width=8,height=5,unit='in',dpi=400)
  ggsave(filename=paste0('completo_Comparacion_gpos_CTL_PDC_',
                         toString(dur_chunk),
                         '_barra.eps'),
         path=g_dir,device='eps',
         width=8,height=5,unit='in')
}

ggplot(promedio,aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa)) +
  xlab(NULL) + ylab('Stationary epoche [%]') +
  labs(title=paste('Epoch =',toString(dur_chunk),
                   's | Whole night')) +
  theme_classic2() +
  labs(fill=NULL) +
  scale_y_continuous(labels=percent) +
  #geom_bar(stat='identity',position=position_dodge(),color='black')+
  geom_bar(stat='identity',position=position_dodge())+
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  #scale_fill_grey(start = 1, end = 0) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  #scale_y_continuous(expand=c(0,0))+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  coord_cartesian(ylim=c(0,.5))+
  stat_compare_means(data = matriz2,inherit.aes=F,
                     aes(x=Canal_var,y=Proporcion,fill=GrupoEtapa),label = 'p.signif',method='wilcox.test',
                     hide.ns = T,
                     label.y =.5)+
  rotate_x_text(angle = 45)
if(grabar.gral){
  ggsave(filename=paste0('completo_Comparacion_gpos_MOR_NMOR_',
                         toString(dur_chunk),
                         '_barra.png'),
         path=g_dir,device='png',
         width=8,height=5,unit='in',dpi=400)
  ggsave(filename=paste0('completo_Comparacion_gpos_MOR_NMOR_',
                         toString(dur_chunk),
                         '_barra.eps'),
         path=g_dir,device='eps',
         width=8,height=5,unit='in')
}

setwd(g_dir)
write.csv(matriz2,file='completo_estacionariedad_crudo.csv',
          row.names=F)


# guardar para la tesis
ggplot(RES.MOR,aes(x=Neuropsi,y=Proporcion,
                   shape=Grupo,color=Grupo))+
  ylab('Stationary epoche [%]') +
  labs(title='At REM')+
  labs(color=NULL,shape=NULL) +
  facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  #geom_smooth(method=lm,
  #            mapping=aes(x=Neuropsi,y=Proporcion),
  #            inherit.aes=F,
  #            se=F,color='black') +
  labs(color='Grupo')+
  stat_cor(data=RES.MOR,inherit.aes=F,
           aes(x=Neuropsi,y=Proporcion),method='spearman') +
  #scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/correlacion_estacionariedad_30_Neuropsi.png',path=dir_graf,
       device='png',units='cm',width=30,height=30,dpi=400)
ggsave(filename='/correlacion_estacionariedad_30_Neuropsi.eps',path=dir_graf,
       device='eps',units='cm',width=30,height=30,dpi=300)

ggplot(RES.MOR,aes(x=Age,y=Proporcion,
                   shape=Grupo,color=Grupo))+
  ylab('Stationary epoche [%]') +
  labs(title='At REM')+
  labs(color=NULL,shape=NULL) +
  facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  #geom_smooth(method=lm,
  #            mapping=aes(x=Neuropsi,y=Proporcion),
  #            inherit.aes=F,
  #            se=F,color='black') +
  labs(color='Grupo')+
  stat_cor(data=RES.MOR,inherit.aes=F,
           aes(x=Age,y=Proporcion),method='spearman') +
  #scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/correlacion_estacionariedad_30_edad.png',path=dir_graf,
       device='png',units='cm',width=30,height=30,dpi=400)
ggsave(filename='/correlacion_estacionariedad_30_edad.eps',path=dir_graf,
       device='eps',units='cm',width=30,height=30,dpi=300)