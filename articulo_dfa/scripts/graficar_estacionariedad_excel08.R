
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
quienes  = 1:10
n.sujetos = length(quienes)

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
dir_scripts = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts'
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/stat'
dir_epocas  = 'C:/Users/EQUIPO 1/Desktop/julio/epocas_dfa'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/graf_def'

###############################################################################
# librerias
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_65')
require('readxl')
#require(xlsx)

require('ggplot2')
require('ggpubr')

require('readr')
require('reshape')

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
raw  = read_excel(paste0(dir_res_pre,'/stat_asdataframe.xlsx'),
                  sheet='mor')
raw = as.data.frame(raw)
RES.MOR  = melt(raw,id=c('Sujeto','Edad','Neuropsi','MMSE',
                         'Escolaridad','Grupo','Etapa'))
RES.MOR  = as.data.frame(RES.MOR)
colnames(RES.MOR)[c(8,9)] = c('Canal_var','Proporcion')

raw  = read_excel(paste0(dir_res_pre,'/stat_asdataframe.xlsx'),
                  sheet='nmor')
raw = as.data.frame(raw)
RES.NMOR  = melt(raw,id=c('Sujeto','Edad','Neuropsi','MMSE',
                         'Escolaridad','Grupo','Etapa'))
RES.NMOR  = as.data.frame(RES.NMOR)
colnames(RES.NMOR)[c(8,9)] = c('Canal_var','Proporcion')

RES.MOR$Grupo  = factor(RES.MOR$Grupo,labels = c('CTRL','PMCI'))
RES.NMOR$Grupo = factor(RES.NMOR$Grupo,labels = c('CTRL','PMCI'))

promedios.MOR  = summarySE(RES.MOR,measurevar='Proporcion',
                           groupvars=c('Grupo','Canal_var'),na.rm=T)
promedios.NMOR = summarySE(RES.NMOR,measurevar='Proporcion',
                          groupvars=c('Grupo','Canal_var'),na.rm=T)

###############################################################################
# Neuropsi vs estacionariedad
# MOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  stat.tmp = RES.MOR[grep(ch.actual,RES.MOR$Canal_var),]
  
  a = unlist(stat.tmp$Neuropsi)
  b = unlist(stat.tmp$Proporcion)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(RES.MOR,aes(x=Neuropsi,y=Proporcion*100,
                   shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Proporcion*100),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_stat_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  stat.tmp = RES.NMOR[grep(ch.actual,RES.NMOR$Canal_var),]
  
  a = unlist(stat.tmp$Neuropsi)
  b = unlist(stat.tmp$Proporcion)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(RES.NMOR,aes(x=Neuropsi,y=Proporcion*100,
                   shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Proporcion*100),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_stat_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

###############################################################################
# Edad vs esacionariedad
# MOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  stat.tmp = RES.MOR[grep(ch.actual,RES.MOR$Canal_var),]
  
  a = unlist(stat.tmp$Edad)
  b = unlist(stat.tmp$Proporcion)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(RES.MOR,aes(x=Edad,y=Proporcion*100,
                   shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Proporcion*100),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_stat_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  stat.tmp = RES.NMOR[grep(ch.actual,RES.NMOR$Canal_var),]
  
  a = unlist(stat.tmp$Edad)
  b = unlist(stat.tmp$Proporcion)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(RES.NMOR,aes(x=Edad,y=Proporcion*100,
                   shape=Grupo,color=Grupo))+
  ylab('Stationary epochs [%]') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Proporcion*100),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_stat_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

###############################################################################
# estacionariedad x canales
# MOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  RES.tmp = RES.MOR[grep(ch.actual,RES.MOR$Canal_var),]
  
  a = RES.tmp[grep('CTRL',RES.tmp$Grupo),]
  a = unlist(a$Proporcion)
  b = RES.tmp[grep('PMCI',RES.tmp$Grupo),]
  b = unlist(b$Proporcion)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

ggplot(promedios.MOR,aes(x=Canal_var,y=Proporcion,fill=Grupo)) +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Stationary epochs [%]') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  #coord_cartesian(ylim=c(.5,1.6))+
  scale_y_continuous(expand=c(0,0)) +
  #scale_fill_grey(start = 1, end = 0) +
  #'#da785f','#4886a5'
  #'#ffff00','#009f3c'
  #scale_fill_manual(values=c('#4886a5','#da785f'))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=RES.MOR,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Proporcion,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_stat_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

# NMOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  RES.tmp = RES.NMOR[grep(ch.actual,RES.NMOR$Canal_var),]
  
  a = RES.tmp[grep('CTRL',RES.tmp$Grupo),]
  a = unlist(a$Proporcion)
  b = RES.tmp[grep('PMCI',RES.tmp$Grupo),]
  b = unlist(b$Proporcion)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

ggplot(promedios.NMOR,aes(x=Canal_var,y=Proporcion,fill=Grupo)) +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Stationary epochs [%]') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  #coord_cartesian(ylim=c(.5,1.6))+
  scale_y_continuous(expand=c(0,0)) +
  #scale_fill_grey(start = 1, end = 0) +
  #'#da785f','#4886a5'
  #'#ffff00','#009f3c'
  #scale_fill_manual(values=c('#4886a5','#da785f'))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=RES.NMOR,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Proporcion,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_stat_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

###############################################################################
# Intercanales
RES.MOR$Etapa  = rep('REM',length(RES.MOR$Etapa))
RES.NMOR$Etapa = rep('NREM',length(RES.MOR$Etapa))
RES.todo = rbind(RES.MOR,RES.NMOR)

promedios.MOR$Etapa  = rep('REM',length(promedios.MOR$Grupo))
promedios.NMOR$Etapa = rep('NREM',length(promedios.NMOR$Grupo))
promedios.todo = rbind(promedios.MOR,promedios.NMOR)

ggplot(promedios.todo,aes(x=Canal_var,y=Proporcion,fill=Grupo)) +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Stationary epochs [%]') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(0,1.15))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=RES.todo,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Proporcion,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y = 1.1)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_stat_uno_CTRL_PMCI.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# grafico nuevo, REM vs NREM
comparaciones.CTL     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.CTL[,1] = kanales$Etiqueta
colnames(comparaciones.CTL) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  RES.tmp = RES.todo[grep(ch.actual,RES.todo$Canal_var),]
  RES.tmp = RES.tmp[grep('CTRL',RES.tmp$Grupo),]
  
  a = RES.tmp[grep('^REM',RES.tmp$Etapa),]
  a = unlist(a$Proporcion)
  b = RES.tmp[grep('NREM',RES.tmp$Etapa),]
  b = unlist(b$Proporcion)
  
  k = t.test(a,b,na.action(na.omit),paired = T)
  
  comparaciones.CTL[ch,'p']  = k$p.value
  comparaciones.CTL[ch,'dF'] = k$parameter
  comparaciones.CTL[ch,'t']  = k$statistic
  #comparaciones.CTL[ch,'m1'] = k$estimate[1]
  #comparaciones.CTL[ch,'m2'] = k$estimate[2]
}

comparaciones.PDC     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.PDC[,1] = kanales$Etiqueta
colnames(comparaciones.PDC) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  RES.tmp = RES.todo[grep(ch.actual,RES.todo$Canal_var),]
  RES.tmp = RES.tmp[grep('PMCI',RES.tmp$Grupo),]
  
  a = RES.tmp[grep('^REM',RES.tmp$Etapa),]
  a = unlist(a$Proporcion)
  b = RES.tmp[grep('NREM',RES.tmp$Etapa),]
  b = unlist(b$Proporcion)
  
  k = t.test(a,b,na.action(na.omit),paired = T)
  
  comparaciones.PDC[ch,'p']  = k$p.value
  comparaciones.PDC[ch,'dF'] = k$parameter
  comparaciones.PDC[ch,'t']  = k$statistic
  #comparaciones.PDC[ch,'m1'] = k$estimate[1]
  #comparaciones.PDC[ch,'m2'] = k$estimate[2]
}

ggplot(promedios.todo,aes(x=Canal_var,y=Proporcion,fill=Etapa)) +
  labs(title='B') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Stationary epochs [%]') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  #geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(0,1.15))+
  #scale_fill_grey(start = 1, end = 0) +
  #scale_fill_manual(values = c('#2b83ba','#d7191c'))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=RES.todo,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Proporcion,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.1,paired=T)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_stat_uno_REM_NREM.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# significativos
promedios.REM.signif = promedios.todo[grep(c('^REM'),
                                            promedios.todo$Etapa),]
patch = is.element(promedios.REM.signif$Canal_var,c('C3'))
promedios.REM.signif = promedios.REM.signif[patch,]
RES.REM.signif = RES.todo[grep(c('CTRL'),RES.todo$Etapa),]
patch = is.element(RES.REM.signif$Canal_var,c('C3'))
RES.REM.signif = RES.REM.signif[patch,]

ggplot(promedios.REM.signif,aes(x=Canal_var,y=Proporcion,fill=Grupo)) +
  labs(title='B') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Stationary epochs [%]') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  #geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  #coord_cartesian(ylim=c(0,.6))+
  #scale_fill_grey(start = 1, end = 0) +
  #scale_fill_manual(values = c('#2b83ba','#d7191c'))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=RES.REM.signif,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Proporcion,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,paired=T)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  rotate_x_text(angle = 0)

ggsave(filename='/comparacion_stat_whole_signif_2.png',path=dir_graf,
       device='png',units='cm',width=3,height=3,dpi=400,scale=2)
