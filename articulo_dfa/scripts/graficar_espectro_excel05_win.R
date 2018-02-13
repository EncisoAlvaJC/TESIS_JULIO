###############################################################################
# parametros
potencia.total = F
orden_stam     = TRUE

usar.log = T
if(!potencia.total){
  usar.log = F
}

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk   = 30

quitar.artefactos = TRUE

zoom           = T
unidad_par_t   = 'tiempo'
#unidad_par_t   = 'epocas'

###############################################################################
# directorios de trabajo
#
#     gral : de uso general
#     info : detalles de los participantes
#  scripts : sub-rutinas, en caso de haberlas
#  res_pre : resultados previos, solo para analizar y/o graficar
#   epocas : epocas para resaltar, por ahora solo MOR
#     graf : donde guardar los graficos, en caso de producirse

dir_gral    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_info    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_scripts = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
#dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/espectro_integrado_171018'
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/espectro_tmp'
dir_epocas  = 'C:/Users/EQUIPO 1/Desktop/julio/epocas_dfa'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/graf_171123'

###############################################################################
# librerias
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_65')
require(readxl)
require(xlsx)

require(ggplot2)
require(ggpubr)

require(readr)

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,'/info_tecnico.xlsx'))
bandas   = read_excel(paste0(dir_info,'/info_bandas.xlsx'))

n.bandas = length(bandas$Banda)

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales = length(kanales$Etiqueta)

sujetos = 1:10

if(potencia.total){
  agregado = 'total'
}else{
  agregado = 'relativo'
}

RES.nuevo = c()
for(sujeto in sujetos){
  #tt = read_csv(paste0(dir_graf,'/espectro',info$Nombre_archivo[sujeto],
  #                     '_',agregado,'_promedios.csv'))
  tt = read_csv(paste0(dir_res_pre,'/espectro',info$Nombre_archivo[sujeto],
                       '_',agregado,'.csv'))
  RES.nuevo = rbind(RES.nuevo,tt)
}

RES.nuevo = cbind(RES.nuevo,
                  (RES.nuevo$Delta+RES.nuevo$Theta)/(RES.nuevo$Alfa+RES.nuevo$Beta),
                  2*RES.nuevo$Grupo+RES.nuevo$Etapa)
colnames(RES.nuevo)[14] = 'Ratio'
colnames(RES.nuevo)[15] = 'GrupoEtapa'

if(usar.log){
  RES.nuevo$Delta    = log(RES.nuevo$Delta)
  RES.nuevo$Theta    = log(RES.nuevo$Theta)
  RES.nuevo$Alfa     = log(RES.nuevo$Alfa)
  RES.nuevo$Beta     = log(RES.nuevo$Beta)
  RES.nuevo$Gamma    = log(RES.nuevo$Gamma)
  RES.nuevo$Varianza = log(RES.nuevo$Varianza)
}

RES.nuevo$Ratio    = log(RES.nuevo$Ratio)

RES.nuevo$Participante = factor(RES.nuevo$Participante,
                                labels = info$Nombre[sujetos])
RES.nuevo$Canal        = factor(RES.nuevo$Canal,
                                labels=kanales$Etiqueta)
RES.nuevo$Grupo        = factor(RES.nuevo$Grupo,
                                labels=c('CTRL','PMCI'))
RES.nuevo$Etapa        = factor(RES.nuevo$Etapa,labels=c('NREM','REM'))
RES.nuevo$GrupoEtapa   = factor(RES.nuevo$GrupoEtapa,
                                labels=c('CTRL NREM','CTRL REM',
                                         'PMCI NREM','PMCI REM'))

letrero.log = ''
if(usar.log){
  letrero.log = 'Log of'
}
if(potencia.total){
  letrero.potencia = 'Absolute'
}else{
  letrero.potencia = 'Relative'
}

###############################################################################
# MOR vs NMOR
bnd = 1

setwd(dir_graf)

ggplot(RES.nuevo,aes(x=Canal,y=Delta,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Delta band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Delta_',agregado,'.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Delta_',agregado,'.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Theta,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Theta band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Theta_',agregado,'.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Theta_',agregado,'.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Alfa,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Alfa band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Alfa_',agregado,'.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Alfa_',agregado,'.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Beta,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Beta band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Beta_',agregado,'.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Beta_',agregado,'.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Gamma,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Gamma band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Gamma_',agregado,'.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Gamma_',agregado,'.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Varianza,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Total') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Total_',agregado,'.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Total_',agregado,'.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Ratio,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab('Log of (D+T)/(A+B) Ratio') +
  labs(title='Ratio (D+T)/(A+B)') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Grupo~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('Ratio_',agregado,'.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('Ratio_',agregado,'.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

###############################################################################
###############################################################################

setwd(dir_graf)

ggplot(RES.nuevo,aes(x=Canal,y=Delta,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Delta band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Delta_',agregado,'_2.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Delta_',agregado,'_2.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Theta,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Theta band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Theta_',agregado,'_2.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Theta_',agregado,'_2.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Alfa,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Alfa band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Alfa_',agregado,'_2.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Alfa_',agregado,'_2.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Beta,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Beta band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Beta_',agregado,'_2.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Beta_',agregado,'_2.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Gamma,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Gamma band') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Gamma_',agregado,'_2.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Gamma_',agregado,'_2.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Varianza,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab(paste(letrero.log,letrero.potencia,'Power')) +
  labs(title='Total') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('ondas_Total_',agregado,'_2.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('ondas_Total_',agregado,'_2.png'),
       device='png',units='cm',width=18,height=7,dpi=400)

ggplot(RES.nuevo,aes(x=Canal,y=Ratio,fill=GrupoEtapa)) +
  scale_fill_manual(values=c('#ffff00','#009f3c',
                             '#da785f','#4886a5'))+
  geom_boxplot() +
  theme_classic2()+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  labs(fill=NULL)+
  xlab(NULL) + 
  ylab('Log of (D+T)/(A+B) Ratio') +
  labs(title='Ratio (D+T)/(A+B)') +
  stat_compare_means(label = 'p.signif',method='wilcox.test',
                     hide.ns=T)+
  facet_grid(Etapa~.)+
  rotate_x_text(angle = 45)
ggsave(filename=paste0('Ratio_',agregado,'_2.eps'),
       device='eps',units='cm',width=18,height=7)
ggsave(filename=paste0('Ratio_',agregado,'_2.png'),
       device='png',units='cm',width=18,height=7,dpi=400)



