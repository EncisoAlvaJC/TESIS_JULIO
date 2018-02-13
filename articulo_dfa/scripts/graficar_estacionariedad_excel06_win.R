
###############################################################################
# parametros
grabar         = FALSE
orden_stam     = TRUE

usar.log = F

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk   = 30

zoom           = T
unidad_par_t   = 'tiempo'
#unidad_par_t   = 'epocas'

quienes = 1:10

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
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/stat'
dir_epocas  = 'C:/Users/EQUIPO 1/Desktop/julio/epocas_dfa'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/stat'

###############################################################################
# librerias
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_65')
require(readxl)
#require(xlsx)

require(ggplot2)
require(ggpubr)

require(readr)

require('Rmisc')

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,
                             '/info_tecnico.xlsx'),sheet='General')

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales = length(kanales$Etiqueta)

#################################################
# datos sobre estacionariedad
setwd(dir_res_pre)
RES.nuevo = as.data.frame(matrix(NA,ncol=13,nrow=44*9))

sujeto = 1
tt = read_csv(paste0(dir_graf,'/estacionariedad_',info$Nombre[sujeto],
                     '_',toString(dur.chunk),'_promedios.csv'))
ward = colnames(tt)
colnames(RES.nuevo) = c(ward,'Age','Neuropsi','MMSE')

for(sujeto in quienes){
  tt = read_csv(paste0(dir_graf,'/estacionariedad_',info$Nombre[sujeto],
                       '_',toString(dur.chunk),'_promedios.csv'))
  RES.nuevo[(sujeto-1)*44 + 1:44,ward] = tt
}

npsi = info$Neuropsi
mmse = info$MMSE
edad = info$Edad

for(sujeto in quienes){
  RES.nuevo[44*(sujeto-1)+1:44,'Neuropsi'] = rep(npsi[sujeto],44)
  RES.nuevo[44*(sujeto-1)+1:44,'MMSE']     = rep(mmse[sujeto],44)
  RES.nuevo[44*(sujeto-1)+1:44,'Age']     = rep(edad[sujeto],44)
}

RES.nuevo$Participante = factor(RES.nuevo$Participante,
                                labels = info$Nombre[quienes])
RES.nuevo$Canal        = factor(RES.nuevo$Canal/2,
                                labels=c(kanales$Etiqueta))
RES.nuevo$Grupo        = factor(RES.nuevo$Grupo,
                                labels=c('CTRL','PMCI'))
RES.nuevo$Etapa        = factor(RES.nuevo$Etapa,labels=c('NREM','REM'))

RES.MOR = RES.nuevo[grep('^REM',RES.nuevo$Etapa),]

promedios = summarySE(RES.nuevo,measurevar='Proporcion',
                      groupvars=c('Grupo','Canal','Etapa'),na.rm=T)
promedios.MOR = summarySE(RES.MOR,measurevar='Proporcion',
                      groupvars=c('Grupo','Canal','Etapa'),na.rm=T)


###############################################################################
# edad vs estacionariedad
ggplot(RES.MOR,aes(x=Neuropsi,y=Proporcion,
                      shape=Grupo,color=Grupo))+
  ylab('Stationary epoche [%]') +
  labs(title='A')+
  labs(color=NULL,shape=NULL) +
  #facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Proporcion),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Grupo')+
  stat_cor(data=RES.MOR,inherit.aes=F,
           aes(x=Neuropsi,y=Proporcion),method='spearman') +
  #scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
#ggsave(filename='/estacionariedad_30_Neuropsi.png',path=dir_graf,
#       device='png',units='cm',width=12,height=7,dpi=300)

ggplot(RES.MOR,aes(x=Age,y=Proporcion,
                     shape=Grupo,color=Grupo))+
  ylab('Stationary epoche [%]') +
  labs(title='A')+
  labs(color=NULL,shape=NULL) +
  #facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Age,y=Proporcion),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Grupo')+
  stat_cor(data=RES.MOR,inherit.aes=F,
           aes(x=Age,y=Proporcion),method='spearman') +
  #scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
#ggsave(filename='/estacionariedad_30_Edad.png',path=dir_graf,
#       device='png',units='cm',width=12,height=7,dpi=300)

ggplot(RES.MOR,aes(x=MMSE,y=Proporcion,
                     shape=Grupo,color=Grupo))+
  ylab('Stationary epoche [%]') +
  labs(title='A')+
  labs(color=NULL,shape=NULL) +
  #facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=MMSE,y=Proporcion),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Grupo')+
  stat_cor(data=RES.MOR,inherit.aes=F,
           aes(x=MMSE,y=Proporcion),method='spearman') +
  #scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
#ggsave(filename='/estacionariedad_30_MMSE.png',path=dir_graf,
#       device='png',units='cm',width=12,height=7,dpi=300)

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
ggsave(filename='/comparacion_30_Neuropsi_REMvsNREM.png',path=dir_graf,
       device='png',units='cm',width=30,height=30,dpi=300)

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
ggsave(filename='/comparacion_30_Age_REMvsNREM.png',path=dir_graf,
       device='png',units='cm',width=30,height=30,dpi=300)



###############################################################################
# NMOR vs MOR
ggplot(RES.nuevo,aes(x=Neuropsi,y=Proporcion,
                   shape=Grupo,color=Etapa))+
  ylab('Stationary epoche [%]') +
  labs(title='A')+
  labs(color=NULL,shape=NULL) +
  facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  #geom_smooth(method=lm,
  #            mapping=aes(x=Neuropsi,y=Proporcion),
  #            inherit.aes=F,
  #            se=F,color='black') +
  labs(color='Grupo')+
  stat_cor(data=RES.nuevo,inherit.aes=F,
           aes(x=Neuropsi,y=Proporcion),method='spearman') +
  #scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
#ggsave(filename='/estacionariedad_30_Neuropsi_multi.png',path=dir_graf,
#       device='png',units='cm',width=20,height=20,dpi=300)

ggplot(promedios.MOR,aes(x=Canal,y=Proporcion,fill=Grupo)) +
  xlab(NULL) + ylab('Stationary epoche [%]') +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  #labs(title=paste0('Stationarity (',toString(dur.chunk),'s)')) +
  geom_bar(stat='identity',position=position_dodge(),
           color='black')+
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  scale_fill_grey(start = 1, end = 0) +
  #geom_signif()
  stat_compare_means(data=RES.MOR,inherit.aes=F,
                     mapping=aes(x=Canal,y=Proporcion),
                     label='p.signif',method='t.test',
                     hide.ns=T)+
  facet_grid(Etapa~.)+
  #rotate_x_text(angle = 45)
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  scale_y_continuous(expand=c(0,0))+
  coord_cartesian(ylim=c(.3,1.1))+
  rotate_x_text(angle = 0)

#coord_cartesian(ylim=c(.5,1.7))+
ggsave(filename='/estacionariedad_30_REMvsNREM.png',path=dir_graf,
       device='png',units='cm',width=20,height=7,dpi=300)


#guadalupe victoria 28

ggplot(promedios,aes(x=Canal,y=Proporcion,fill=Etapa)) +
  xlab(NULL) + ylab('Stationary epoche [%]') +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  #labs(title=paste0('Stationarity (',toString(dur.chunk),'s)')) +
  geom_bar(stat='identity',position=position_dodge(),
           color='black')+
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  scale_fill_grey(start = 1, end = 0) +
  #geom_signif()
  stat_compare_means(data=RES.nuevo,inherit.aes=F,
                     mapping=aes(x=Canal,y=Proporcion,
                                 fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.1)+
  facet_grid(Grupo~.)+
  #rotate_x_text(angle = 45)
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  scale_y_continuous(expand=c(0,0))+
  #coord_cartesian(ylim=c(.5,1))+
  rotate_x_text(angle = 0)

  #coord_cartesian(ylim=c(.5,1.7))+
ggsave(filename='/estacionariedad_30_REMvsNREM.png',path=dir_graf,
       device='png',units='cm',width=20,height=7,dpi=300)

ggplot(promedios,aes(x=Canal,y=Proporcion,fill=Grupo)) +
  xlab(NULL) + ylab('Stationary epoche [%]') +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  #labs(title=paste0('Stationarity (',toString(dur.chunk),'s)')) +
  geom_bar(stat='identity',position=position_dodge(),
           color='black')+
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  scale_fill_grey(start = 1, end = 0) +
  scale_y_continuous(expand=c(0,0))+
  #geom_signif()
  #stat_compare_means(data=RES.nuevo,inherit.aes=F,
  #                   mapping=aes(x=Canal,y=Proporcion,
  #                               fill=Grupo),
  #                   label='p.signif',method='t.test',
  #                   hide.ns=T,label.y=1.1)+
  facet_grid(Etapa~.)+
  #rotate_x_text(angle = 45)
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  #coord_cartesian(ylim=c(.5,1))+
  rotate_x_text(angle = 0)

#coord_cartesian(ylim=c(.5,1.7))+
ggsave(filename='/estacionariedad_30_CTRLvsPMCI.png',path=dir_graf,
       device='png',units='cm',width=20,height=7,dpi=300)

ggplot(RES.nuevo,aes(x=Canal,y=Proporcion*100,fill=Etapa)) +
  #geom_bar(stat='identity',position=position_dodge(),
  #         color='black')+
  geom_boxplot()+
  xlab(NULL) + ylab('Stationary epoche [%]') +
  theme_classic2() +
  labs(fill=NULL) +
  labs(title=paste0('Stationarity (',toString(dur.chunk),'s)')) +
  theme(legend.position='bottom') +
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(label = 'p.signif',method='t.test')+
  facet_grid(Grupo~.)+
  #rotate_x_text(angle = 45)
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 0)
ggsave(filename='/estacionariedad_30_CTRLvsPMCI.png',path=dir_graf,
       device='png',units='cm',width=20,height=7,dpi=300)

ggplot(RES.nuevo,aes(x=Canal,y=Proporcion*100,fill=Grupo)) +
  #geom_bar(stat='identity',position=position_dodge(),
  #         color='black')+
  geom_boxplot()+
  xlab(NULL) + ylab('Stationary epoche [%]') +
  theme_classic2() +
  labs(fill=NULL) +
  labs(title=paste0('Stationarity (',toString(dur.chunk),'s)')) +
  theme(legend.position='bottom') +
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(label = 'p.signif',method='t.test')+
  facet_grid(Etapa~.)+
  #rotate_x_text(angle = 45)
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 0)
ggsave(filename='/estacionariedad_30_REMvsNREM.png',path=dir_graf,
       device='png',units='cm',width=20,height=7,dpi=300)

###############################################################################
###############################################################################
