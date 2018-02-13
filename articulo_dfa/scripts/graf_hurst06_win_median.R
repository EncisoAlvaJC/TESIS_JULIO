###############################################################################
# parametros
orden_stam     = TRUE
zoom           = F
unidad_par_t   = 'tiempo'
#unidad_par_t   = 'epocas'
usar.log       = F

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk   = 1

archivo.excel = '/pvalores_Hurst.xlsx'

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
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/Hurst'
dir_epocas  = 'C:/Users/EQUIPO 1/Desktop/julio/epocas_dfa_10'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/graf_def'

###############################################################################
# librerias
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_65')
require('readxl')
#require('xlsx')

require('ggplot2')
require('ggpubr')

require('Rmisc')

require('reshape')

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,'/info_tecnico.xlsx'))

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

###############################################################################
# cargar los datos
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe.xlsx'),sheet='uno')
raw = as.data.frame(raw)

Hurst.MOR           = melt(raw,id=c('Sujeto','Grupo','Edad',
                                    'MMSE','Neuropsi',
                                    'MORn','Epoca','Etapa'))
colnames(Hurst.MOR) = c('Sujeto','Grupo','Edad','MMSE','Neuropsi',
                         'MORn','Epoca','Etapa','Canal_var','Hurst')
Hurst.MOR           = Hurst.MOR[!is.na(Hurst.MOR$Hurst),]
Hurst.MOR$Canal_var = as.numeric(Hurst.MOR$Canal_var)
Hurst.MOR$Sujeto_n  = factor(Hurst.MOR$Sujeto,labels = info$Nombre[1:10])
Hurst.MOR$Etapa     = rep(1,length(Hurst.MOR$Sujeto))

Hurst.MOR.promedio = c()
for(suj in 1:10){
  tmp       = Hurst.MOR[grep(info$Nombre[suj],Hurst.MOR$Sujeto_n),]
  promedios = aggregate(tmp,by=list(tmp$Canal_var),mean)
  Hurst.MOR.promedio = rbind(Hurst.MOR.promedio,promedios)
}

Hurst.MOR.promedio$Sujeto_n = info$Nombre[Hurst.MOR.promedio$Sujeto]

if(usar.log){
  Hurst.MOR$Hurst     = log(Hurst.MOR$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}

# problemas con etiquetas
Hurst.MOR$Canal_var          = factor(Hurst.MOR$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.MOR$Grupo              = factor(Hurst.MOR$Grupo,
                                      labels=c('CTRL','PMCI'))
Hurst.MOR.promedio$Canal_var = factor(Hurst.MOR.promedio$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.MOR.promedio$Grupo     = factor(Hurst.MOR.promedio$Grupo,
                                      labels=c('CTRL','PMCI'))
Hurst.MOR.promedio = as.data.frame(Hurst.MOR.promedio)
promedios.MOR      = summarySE(Hurst.MOR,measurevar='Hurst',
                               groupvars=c('Grupo','Canal_var'),na.rm=T)

###############################################################################
# lo mismo para NMOR
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe.xlsx'),sheet='uno_pre')
raw = as.data.frame(raw)

Hurst.NMOR           = melt(raw,id=c('Sujeto','Grupo','Edad',
                                    'MMSE','Neuropsi',
                                    'MORn','Epoca','Etapa'))
colnames(Hurst.NMOR) = c('Sujeto','Grupo','Edad','MMSE','Neuropsi',
                        'MORn','Epoca','Etapa','Canal_var','Hurst')
Hurst.NMOR           = Hurst.NMOR[!is.na(Hurst.NMOR$Hurst),]
Hurst.NMOR$Canal_var = as.numeric(Hurst.NMOR$Canal_var)
Hurst.NMOR$Sujeto_n  = factor(Hurst.NMOR$Sujeto,labels = info$Nombre[1:10])
Hurst.NMOR$Etapa     = rep(0,length(Hurst.NMOR$Sujeto))

Hurst.NMOR.promedio = c()
for(suj in 1:10){
  tmp       = Hurst.NMOR[grep(info$Nombre[suj],Hurst.NMOR$Sujeto_n),]
  promedios = aggregate(tmp,by=list(tmp$Canal_var),mean)
  Hurst.NMOR.promedio = rbind(Hurst.NMOR.promedio,promedios)
}

Hurst.NMOR.promedio$Sujeto_n = info$Nombre[Hurst.NMOR.promedio$Sujeto]

if(usar.log){
  Hurst.NMOR$Hurst     = log(Hurst.NMOR$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}

# problemas con etiquetas
Hurst.NMOR$Canal_var          = factor(Hurst.NMOR$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.NMOR$Grupo              = factor(Hurst.NMOR$Grupo,
                                      labels=c('CTRL','PMCI'))
Hurst.NMOR.promedio$Canal_var = factor(Hurst.NMOR.promedio$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.NMOR.promedio$Grupo     = factor(Hurst.NMOR.promedio$Grupo,
                                      labels=c('CTRL','PMCI'))
Hurst.NMOR.promedio = as.data.frame(Hurst.NMOR.promedio)
promedios.NMOR      = summarySE(Hurst.NMOR,measurevar='Hurst',
                                groupvars=c('Grupo','Canal_var'),na.rm=T)

###############################################################################
# Neuropsi vs MMSE
cor.test(Hurst.MOR.promedio$Neuropsi,Hurst.MOR.promedio$MMSE,
         method='spearman')

ggplot(Hurst.MOR.promedio,aes(x=Neuropsi,y=MMSE,shape=Grupo,
                          add='reg.line')) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=MMSE),
              inherit.aes=F,
              se=F,color='gray2') +
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  scale_shape_discrete(name=NULL)+
  geom_point()
ggsave(filename='/Neuropsi_MMSE.png',path=dir_graf,
       device='png',units='cm',width=6,height=4.5,dpi=400,
       scale=2)

###############################################################################
# Neuropsi vs Edad
cor.test(Hurst.MOR.promedio$Edad,Hurst.MOR.promedio$Neuropsi,
         method='spearman')

ggplot(Hurst.MOR.promedio,aes(x=Edad,y=Neuropsi,shape=Grupo,
                          add='reg.line')) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Neuropsi),
              inherit.aes=F,
              se=F,color='gray2') +
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  scale_shape_discrete(name=NULL)+
  geom_point()
ggsave(filename='/Neuropsi_Edad.png',path=dir_graf,
       device='png',units='cm',width=6,height=4.5,dpi=400,
       scale=2)

###############################################################################
# Neuropsi vs Hurst
# MOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.MOR.promedio,aes(x=Neuropsi,y=Hurst,
                              shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_hurst_uno_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.NMOR.promedio,aes(x=Neuropsi,y=Hurst,
                              shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_hurst_uno_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

###############################################################################
# Edad vs Hurst
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.MOR.promedio,aes(x=Edad,y=Hurst,
                              shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_hurst_uno_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.NMOR.promedio,aes(x=Edad,y=Hurst,
                               shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_hurst_uno_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# Significativos
cuales = c(10,18)
signif = c()
for(ch in cuales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  signif = rbind(signif,Hurst.tmp)
}

#ggplot(signif,aes(x=Edad,y=Hurst,
#                              shape=Grupo,color=Grupo))+
#  ylab('Hurst Exponent') + xlab('Age') +
#  labs(shape=NULL,color=NULL) +
#  facet_wrap(~Canal_var,ncol=6) +
#  theme_classic2() +
#  geom_smooth(method=lm,
#              mapping=aes(x=Edad,y=Hurst),
#              inherit.aes=F,
#              se=F,color='black') +
#  scale_colour_discrete(guide = FALSE) +
#  rotate_x_text(angle = 45)+
#  theme(legend.position='none')+
#  geom_point()
#ggsave(filename='/edad_hurst_uno_mor_significativos.png',path=dir_graf,
#       device='png',units='cm',width=6,height=4,dpi=400,scale=2)

ggplot(signif,aes(x=Edad,y=Hurst,
                  shape=Grupo))+
  ylab('Hurst Exponent') + xlab('Age') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='gray60') +
  scale_colour_discrete(guide = FALSE) +
  #rotate_x_text(angle = 45)+
  theme(strip.background = element_blank())+
  theme(legend.position='top')+
  coord_cartesian(xlim=c(59,79))+
  geom_point()
ggsave(filename='/Fig02_edad_hurst.png',path=dir_graf,
       device='png',units='cm',width=6,height=4,dpi=400,scale=2)

###############################################################################
# Hurst x Canal
# MOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR[grep(ch.actual,Hurst.MOR$Canal_var),]
  
  a = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

ggplot(promedios.MOR,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.MOR,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_uno_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

# NMOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR[grep(ch.actual,Hurst.NMOR$Canal_var),]
  
  a = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

ggplot(promedios.NMOR,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.NMOR,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_uno_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

###############################################################################
# Intercanales
Hurst.MOR$Etapa  = rep('REM',length(Hurst.MOR$Etapa))
Hurst.NMOR$Etapa = rep('NREM',length(Hurst.NMOR$Etapa))
Hurst.todo = rbind(Hurst.MOR,Hurst.NMOR)

promedios.MOR$Etapa  = rep('REM',length(promedios.MOR$Grupo))
promedios.NMOR$Etapa = rep('NREM',length(promedios.NMOR$Grupo))
promedios.todo = rbind(promedios.MOR,promedios.NMOR)

ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.todo,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_hurst_uno_CTRL_PMCI.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# grafico nuevo, REM vs NREM
# usando los promedios
comparaciones.CTL     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.CTL[,1] = kanales$Etiqueta
colnames(comparaciones.CTL) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = promedios.todo[grep(ch.actual,promedios.todo$Canal_var),]
  Hurst.tmp = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  
  a = Hurst.tmp[grep('^REM',Hurst.tmp$Etapa),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('NREM',Hurst.tmp$Etapa),]
  b = unlist(b$Hurst)
  
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
  Hurst.tmp = promedios.todo[grep(ch.actual,promedios.todo$Canal_var),]
  Hurst.tmp = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  
  a = Hurst.tmp[grep('^REM',Hurst.tmp$Etapa),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('NREM',Hurst.tmp$Etapa),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit),paired = T)
  
  comparaciones.PDC[ch,'p']  = k$p.value
  comparaciones.PDC[ch,'dF'] = k$parameter
  comparaciones.PDC[ch,'t']  = k$statistic
  #comparaciones.PDC[ch,'m1'] = k$estimate[1]
  #comparaciones.PDC[ch,'m2'] = k$estimate[2]
}

# usando todas las epocas
comparaciones.CTL     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.CTL[,1] = kanales$Etiqueta
colnames(comparaciones.CTL) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo[grep(ch.actual,Hurst.todo$Canal_var),]
  Hurst.tmp = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  
  a = Hurst.tmp[grep('^REM',Hurst.tmp$Etapa),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('NREM',Hurst.tmp$Etapa),]
  b = unlist(b$Hurst)
  
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
  Hurst.tmp = Hurst.todo[grep(ch.actual,Hurst.todo$Canal_var),]
  Hurst.tmp = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  
  a = Hurst.tmp[grep('^REM',Hurst.tmp$Etapa),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('NREM',Hurst.tmp$Etapa),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit),paired = T)
  
  comparaciones.PDC[ch,'p']  = k$p.value
  comparaciones.PDC[ch,'dF'] = k$parameter
  comparaciones.PDC[ch,'t']  = k$statistic
  #comparaciones.PDC[ch,'m1'] = k$estimate[1]
  #comparaciones.PDC[ch,'m2'] = k$estimate[2]
}

ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Etapa)) +
  labs(title='B') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  #geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  #scale_fill_grey(start = 1, end = 0) +
  #scale_fill_manual(values = c('#2b83ba','#d7191c'))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=Hurst.todo,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6,paired=T)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_hurst_uno_REM_NREM.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

stop()

promedios.m.MOR      = summarySE(Hurst.MOR.promedio,measurevar='Hurst',
                                 groupvars=c('Grupo','Canal_var'),na.rm=T)
promedios.m.NMOR      = summarySE(Hurst.NMOR.promedio,measurevar='Hurst',
                                  groupvars=c('Grupo','Canal_var'),na.rm=T)

###############################################################################
###############################################################################

#    INTERCANALES

###############################################################################
###############################################################################
kanales  = read_excel(paste0(dir_info,'/info_intercanales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_intercanales_alterno.xlsx'))
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

# cargar los datos
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe.xlsx'),sheet='multi')
raw = as.data.frame(raw)

Hurst.MOR           = melt(raw,id=c('Sujeto','Grupo','Edad',
                                    'MMSE','Neuropsi',
                                    'MORn','Epoca','Etapa'))
colnames(Hurst.MOR) = c('Sujeto','Grupo','Edad','MMSE','Neuropsi',
                        'MORn','Epoca','Etapa','Canal_var','Hurst')
Hurst.MOR           = Hurst.MOR[!is.na(Hurst.MOR$Hurst),]
Hurst.MOR$Canal_var = as.numeric(Hurst.MOR$Canal_var)
Hurst.MOR$Sujeto_n  = factor(Hurst.MOR$Sujeto,labels = info$Nombre[1:10])
Hurst.MOR$Etapa     = rep(1,length(Hurst.MOR$Sujeto))

Hurst.MOR.promedio = c()
for(suj in 1:10){
  tmp       = Hurst.MOR[grep(info$Nombre[suj],Hurst.MOR$Sujeto_n),]
  promedios = aggregate(tmp,by=list(tmp$Canal_var),mean)
  Hurst.MOR.promedio = rbind(Hurst.MOR.promedio,promedios)
}

Hurst.MOR.promedio$Sujeto_n = info$Nombre[Hurst.MOR.promedio$Sujeto]

if(usar.log){
  Hurst.MOR$Hurst     = log(Hurst.MOR$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}

# problemas con etiquetas
Hurst.MOR$Canal_var          = factor(Hurst.MOR$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.MOR$Grupo              = factor(Hurst.MOR$Grupo,
                                      labels=c('CTRL','PMCI'))
Hurst.MOR.promedio$Canal_var = factor(Hurst.MOR.promedio$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.MOR.promedio$Grupo     = factor(Hurst.MOR.promedio$Grupo,
                                      labels=c('CTRL','PMCI'))
Hurst.MOR.promedio = as.data.frame(Hurst.MOR.promedio)
promedios.MOR      = summarySE(Hurst.MOR,measurevar='Hurst',
                               groupvars=c('Grupo','Canal_var'),na.rm=T)

###############################################################################
# lo mismo para NMOR
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe.xlsx'),sheet='multi_pre')
raw = as.data.frame(raw)

Hurst.NMOR           = melt(raw,id=c('Sujeto','Grupo','Edad',
                                     'MMSE','Neuropsi',
                                     'MORn','Epoca','Etapa'))
colnames(Hurst.NMOR) = c('Sujeto','Grupo','Edad','MMSE','Neuropsi',
                         'MORn','Epoca','Etapa','Canal_var','Hurst')
Hurst.NMOR           = Hurst.NMOR[!is.na(Hurst.NMOR$Hurst),]
Hurst.NMOR$Canal_var = as.numeric(Hurst.NMOR$Canal_var)
Hurst.NMOR$Sujeto_n  = factor(Hurst.NMOR$Sujeto,labels = info$Nombre[1:10])
Hurst.NMOR$Etapa     = rep(0,length(Hurst.NMOR$Sujeto))

Hurst.NMOR.promedio = c()
for(suj in 1:10){
  tmp       = Hurst.NMOR[grep(info$Nombre[suj],Hurst.NMOR$Sujeto_n),]
  promedios = aggregate(tmp,by=list(tmp$Canal_var),mean)
  Hurst.NMOR.promedio = rbind(Hurst.NMOR.promedio,promedios)
}

Hurst.NMOR.promedio$Sujeto_n = info$Nombre[Hurst.NMOR.promedio$Sujeto]

if(usar.log){
  Hurst.NMOR$Hurst     = log(Hurst.NMOR$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}

# problemas con etiquetas
Hurst.NMOR$Canal_var          = factor(Hurst.NMOR$Canal_var,
                                       labels=kanales$Etiqueta)
Hurst.NMOR$Grupo              = factor(Hurst.NMOR$Grupo,
                                       labels=c('CTRL','PMCI'))
Hurst.NMOR.promedio$Canal_var = factor(Hurst.NMOR.promedio$Canal_var,
                                       labels=kanales$Etiqueta)
Hurst.NMOR.promedio$Grupo     = factor(Hurst.NMOR.promedio$Grupo,
                                       labels=c('CTRL','PMCI'))
Hurst.NMOR.promedio = as.data.frame(Hurst.NMOR.promedio)
promedios.NMOR      = summarySE(Hurst.NMOR,measurevar='Hurst',
                                groupvars=c('Grupo','Canal_var'),na.rm=T)

###############################################################################
# Neuropsi vs Hurst
# MOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.MOR.promedio,aes(x=Neuropsi,y=Hurst,
                              shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_hurst_multi_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.NMOR.promedio,aes(x=Neuropsi,y=Hurst,
                               shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_hurst_multi_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

###############################################################################
# Edad vs Hurst
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.MOR.promedio,aes(x=Edad,y=Hurst,
                              shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_hurst_multi_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.NMOR.promedio,aes(x=Edad,y=Hurst,
                               shape=Grupo,color=Grupo))+
  ylab('Hurst Exponent') +
  labs(shape=NULL,color=NULL) +
  facet_wrap(~Canal_var,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,.05),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_hurst_multi_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

###############################################################################
# Hurst x Canal
# MOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR[grep(ch.actual,Hurst.MOR$Canal_var),]
  
  a = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

ggplot(promedios.MOR,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  labs(title='B') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.MOR,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_multi_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

# NMOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR[grep(ch.actual,Hurst.NMOR$Canal_var),]
  
  a = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit))
  
  comparaciones[ch,'p']  = k$p.value
  comparaciones[ch,'dF'] = k$parameter
  comparaciones[ch,'t']  = k$statistic
  comparaciones[ch,'m1'] = k$estimate[1]
  comparaciones[ch,'m2'] = k$estimate[2]
}

ggplot(promedios.NMOR,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  labs(title='B') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.NMOR,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_multi_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

###############################################################################
# Intercanales
Hurst.MOR$Etapa  = rep('REM',length(Hurst.MOR$Etapa))
Hurst.NMOR$Etapa = rep('NREM',length(Hurst.NMOR$Etapa))
Hurst.todo = rbind(Hurst.MOR,Hurst.NMOR)

promedios.MOR$Etapa  = rep('REM',length(promedios.MOR$Grupo))
promedios.NMOR$Etapa = rep('NREM',length(promedios.NMOR$Grupo))
promedios.todo = rbind(promedios.MOR,promedios.NMOR)

ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.todo,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_multi_CTRL_PMCI.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# grafico nuevo, REM vs NREM
comparaciones.CTL     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.CTL[,1] = kanales$Etiqueta
colnames(comparaciones.CTL) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo[grep(ch.actual,Hurst.todo$Canal_var),]
  Hurst.tmp = Hurst.tmp[grep('CTRL',Hurst.tmp$Grupo),]
  
  a = Hurst.tmp[grep('^REM',Hurst.tmp$Etapa),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('NREM',Hurst.tmp$Etapa),]
  b = unlist(b$Hurst)
  
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
  Hurst.tmp = Hurst.todo[grep(ch.actual,Hurst.todo$Canal_var),]
  Hurst.tmp = Hurst.tmp[grep('PMCI',Hurst.tmp$Grupo),]
  
  a = Hurst.tmp[grep('^REM',Hurst.tmp$Etapa),]
  a = unlist(a$Hurst)
  b = Hurst.tmp[grep('NREM',Hurst.tmp$Etapa),]
  b = unlist(b$Hurst)
  
  k = t.test(a,b,na.action(na.omit),paired = T)
  
  comparaciones.PDC[ch,'p']  = k$p.value
  comparaciones.PDC[ch,'dF'] = k$parameter
  comparaciones.PDC[ch,'t']  = k$statistic
  #comparaciones.PDC[ch,'m1'] = k$estimate[1]
  #comparaciones.PDC[ch,'m2'] = k$estimate[2]
}

ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Etapa)) +
  labs(title='B') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  #geom_bar(stat='identity',position=position_dodge()) +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  #scale_fill_grey(start = 1, end = 0) +
  #scale_fill_manual(values = c('#2b83ba','#d7191c'))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=Hurst.todo,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6,paired=T)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  rotate_x_text(angle = 45)
ggsave(filename='/comparacion_multi_REM_NREM.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)


promedios.m.MOR      = summarySE(Hurst.MOR.promedio,measurevar='Hurst',
                               groupvars=c('Grupo','Canal_var'),na.rm=T)
promedios.m.NMOR      = summarySE(Hurst.NMOR.promedio,measurevar='Hurst',
                                 groupvars=c('Grupo','Canal_var'),na.rm=T)
