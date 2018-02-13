###############################################################################
# parametros
potencia.total = T
orden_stam     = TRUE
zoom           = T
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
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/Hurst'

###############################################################################
# librerias
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_65')
require('readxl')
require('xlsx')

require('ggplot2')
require('ggpubr')

require('Rmisc')

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
raw = read_excel(paste0(dir_res_pre,'/Source_vale2.xlsx'))

Hurst.todo = matrix(0,ncol=9,nrow=100*n.canales)
colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst','Estado',
                         'Neuropsi','Edad','MMSE')
Hurst.todo = as.data.frame(Hurst.todo)

for(i in 1:100){
  step = n.canales*(i-1)
  channels = 1:n.canales
  Hurst.todo[step+channels,'Participante'] = raw$Sujeto[i]
  Hurst.todo[step+channels,'MOR_n']        = raw$MOR_n[i]
  Hurst.todo[step+channels,'Epoca_n']      = raw$Epoca_n[i]
  Hurst.todo[step+channels,'Canal']        = channels
  Hurst.todo[step+channels,'Estado']       = 1*(raw$Sujeto[i]>5)
  Hurst.todo[step+channels,'Edad']         = raw$Edad[i]
  Hurst.todo[step+channels,'Neuropsi']     = raw$Neuropsi_gral[i]
  Hurst.todo[step+channels,'MMSE']         = raw$MMSE[i]
  
  for(ch in 1:n.canales){
    Hurst.todo[step+ch,'Hurst'] = unlist(raw[canales.arch[ch]])[i]
  }
}

Hurst.promedio = c()
for(sujeto in 1:10){
  tmp       = Hurst.todo[grep(sujeto,Hurst.todo$Participante),]
  promedios = aggregate(tmp,by=list(tmp$Canal),mean)
  Hurst.promedio = rbind(Hurst.promedio,promedios)
}

if(usar.log){
  Hurst.todo$Hurst     = log(Hurst.todo$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}

# problemas con etiquetas
Hurst.todo$Canal       = factor(Hurst.todo$Canal,
                                labels=kanales$Etiqueta)
Hurst.todo$Estado      = factor(Hurst.todo$Estado,
                                labels=c('CTRL','PMCI'))
Hurst.promedio$Canal   = factor(Hurst.promedio$Canal,
                                labels=kanales$Etiqueta)
Hurst.promedio$Estado  = factor(Hurst.promedio$Estado,
                                labels=c('CTRL','PMCI'))
Hurst.promedio = as.data.frame(Hurst.promedio)
promedios = summarySE(Hurst.todo,measurevar='Hurst',
                      groupvars=c('Estado','Canal'),na.rm=T)



###############################################################################
# graficos sobre diferencias
if(FALSE){
  ggplot(promedios,aes(x=Canal,y=Hurst,fill=Estado)) +
    labs(title='Exponente de Hurst') +
    xlab(NULL) + ylab('Exp. de Hurst') +
    geom_bar(stat='identity',position=position_dodge(),
             color='black') +
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                  position=position_dodge(.9),width=.5) +theme_classic() +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_grey(start = 1, end = 0) +
    stat_compare_means(
      label = 'p.signif',method='t.test',
      hide.ns=T)+
    #rotate_x_text(angle = 45) +
    theme(legend.position='bottom')
}

ggplot(promedios,aes(x=Canal,y=Hurst,fill=Estado)) +
  #labs(title='Comparisons between groups on REM') +
  labs(title='A') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.7))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.todo,inherit.aes=F,
                     mapping=aes(x=Canal,y=Hurst,fill=Estado),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))
ggsave(filename='/comparacion_uno.png',path=dir_graf,
       device='png',units='cm',width=20,height=7,dpi=300)

ggplot(Hurst.todo,aes(x=Edad,y=Neuropsi,shape=Estado,
                      add='reg.line')) +
  labs(title='A') +
  xlab('Age') + labs(shape=NULL) +
  theme_classic2() +
  stat_cor(inherit.aes=F,aes(x=Edad,y=Neuropsi),method='spearman') +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Neuropsi),
              inherit.aes=F,
              se=F,color='gray2') +
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_neuropsi.png',path=dir_graf,
       device='png',units='cm',width=15,height=12,dpi=300)

ggplot(Hurst.todo,aes(x=MMSE,y=Neuropsi,shape=Estado,
                      add='reg.line')) +
  labs(title='A') +
  labs(shape=NULL) +
  theme_classic2() +
  stat_cor(inherit.aes=F,aes(x=MMSE,y=Neuropsi),method='spearman') +
  geom_smooth(method=lm,
              mapping=aes(x=MMSE,y=Neuropsi),
              inherit.aes=F,
              se=F,color='black') +
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/MMSE_neuropsi.png',path=dir_graf,
       device='png',units='cm',width=15,height=12,dpi=300)

###############################################################################
# comparaciones usando wilcox
raw.ctl = rbind(raw[grep(1,raw$Sujeto),],
                raw[grep(2,raw$Sujeto),],
                raw[grep(3,raw$Sujeto),],
                raw[grep(4,raw$Sujeto),],
                raw[grep(5,raw$Sujeto),]
            )

raw.pdc = rbind(raw[grep(6,raw$Sujeto),],
                raw[grep(7,raw$Sujeto),],
                raw[grep(8,raw$Sujeto),],
                raw[grep(9,raw$Sujeto),]
)

stop('Añadir Kolmogorov-Smirnov')

pvalores           = matrix(2,nrow=n.canales,ncol=8)
colnames(pvalores) = c('canal','t.p','T','dF',
                       'k.p.ctl','d.ctl')
pvalores[,'canal'] = kanales$Etiqueta

for(i in 1:n.canales){
  a = unlist(raw.ctl[kanales$Nombre_archivo[i]])
  b = unlist(raw.pdc[kanales$Nombre_archivo[i]])
  
  ks.test(a,'pnorm')
  
  ttt = t.test(a,b,var.equal = F)
  pvalores[i,2] = ttt$p.value
  pvalores[i,3] = ttt$statistic
  pvalores[i,4] = ttt$parameter
}

guardar = cbind(promedios[1:n.canales,],
                promedios[n.canales+1:n.canales,],
                pvalores)

write.xlsx(guardar,file=paste0(dir_graf,archivo.excel),
           sheetName='CTL_PDC uno',
           col.names=TRUE,row.names=TRUE,append=FALSE)

###############################################################################
# correlaciones
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')

Hurst.todo$Participante    = factor(Hurst.todo$Participante,
                                    labels=info$Nombre[1:10])
Hurst.promedio$Participante = factor(Hurst.promedio$Participante,
                                    labels=info$Nombre[1:10])

# todas las observaciones
# Hurst vs Neuropsi
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo[grep(ch.actual,Hurst.todo$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Neuropsi)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.todo,aes(x=Neuropsi,y=Hurst,
                      shape=Estado,color=Estado))+
  ylab('Hurst Exponent') +
  labs(title='A')+
  labs(color=NULL,shape=NULL) +
  facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Group')+
  stat_cor(data=Hurst.todo,inherit.aes=F,
           aes(x=Neuropsi,y=Hurst),method='spearman') +
  scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_hurst_uno.png',path=dir_graf,
       device='png',units='cm',width=18,height=15,dpi=300)

write.xlsx(correlaciones,file=paste0(dir_graf,archivo.excel),
           sheetName='H_Neuropsi uno',
           col.names=TRUE,row.names=TRUE,append=TRUE)

# Hurst vs Edad
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo[grep(ch.actual,Hurst.todo$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Edad)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.todo,aes(x=Edad,y=Hurst,
                      shape=Estado,color=Estado))+
  ylab('Hurst Exponent') + xlab('Age') +
  labs(title='A')+
  labs(color=NULL,shape=NULL) +
  facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Group')+
  stat_cor(data=Hurst.todo,inherit.aes=F,
           aes(x=Edad,y=Hurst),method='spearman') +
  scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_hurst_uno.png',path=dir_graf,
       device='png',units='cm',width=18,height=15,dpi=300)

write.xlsx(correlaciones,file=paste0(dir_graf,archivo.excel),
           sheetName='H_Edad uno',
           col.names=TRUE,row.names=TRUE,append=TRUE)

# promedio por sujeto
# Hurst vs Neuropsi
for(ch in 1:n.canales){
  print(ch)
  
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.promedio[grep(ch.actual,Hurst.promedio$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Neuropsi)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,
                    shape=Estado,color=Estado))+
  ylab('Hurst Exponent') +
  labs(title='A')+
  labs(color=NULL) +
  facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Group')+
  stat_cor(data=Hurst.promedio,inherit.aes=F,
           aes(x=Neuropsi,y=Hurst),method='spearman') +
  scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_hurst_promedio_uno.png',path=dir_graf,
       device='png',units='cm',width=18,height=15,dpi=300)

write.xlsx(correlaciones,file=paste0(dir_graf,archivo.excel),
           sheetName='H_Neuropsi_promedio uno',
           col.names=TRUE,row.names=TRUE,append=TRUE)

# Hurst vs Edad
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.promedio[grep(ch.actual,Hurst.promedio$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Edad)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.promedio,aes(x=Edad,y=Hurst,
                    shape=Estado,color=Estado))+
  ylab('Hurst Exponent') + xlab('Age') +
  labs(title='A')+
  labs(color=NULL) +
  facet_wrap(~Canal,ncol=6) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Group')+
  stat_cor(data=Hurst.promedio,inherit.aes=F,
           aes(x=Edad,y=Hurst),method='spearman') +
  scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_hurst_promedio_uno.png',path=dir_graf,
       device='png',units='cm',width=18,height=15,dpi=300)

write.xlsx(correlaciones,file=paste0(dir_graf,archivo.excel),
           sheetName='H_Edad_promedio uno',
           col.names=TRUE,row.names=TRUE,append=TRUE)

#################################################
#################################################
#################################################
#################################################

kanales  = read_excel(paste0(dir_info,'/info_intercanales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_intercanales_alterno.xlsx'))
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

###############################################################################
# cargar los datos
raw = read_excel(paste0(dir_res_pre,'/Source_vale_multi2.xlsx'))

Hurst.todo = matrix(0,ncol=9,nrow=90*n.canales)
colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst','Estado',
                         'Neuropsi','Edad','MMSE')
Hurst.todo = as.data.frame(Hurst.todo)

for(i in 1:90){
  step = n.canales*(i-1)
  channels = 1:n.canales
  Hurst.todo[step+channels,'Participante'] = raw$Sujeto[i]
  Hurst.todo[step+channels,'MOR_n']        = raw$MOR_n[i]
  Hurst.todo[step+channels,'Epoca_n']      = raw$Epoca_n[i]
  Hurst.todo[step+channels,'Canal']        = channels
  Hurst.todo[step+channels,'Estado']       = 1*(raw$Sujeto[i]>5)
  Hurst.todo[step+channels,'Edad']         = raw$Edad[i]
  Hurst.todo[step+channels,'Neuropsi']     = raw$Neuropsi_gral[i]
  Hurst.todo[step+channels,'MMSE']         = raw$MMSE[i]
  
  for(ch in 1:n.canales){
    Hurst.todo[step+ch,'Hurst'] = unlist(raw[canales.arch[ch]])[i]
  }
}

Hurst.promedio = c()
for(sujeto in 1:9){
  tmp       = Hurst.todo[grep(sujeto,Hurst.todo$Participante),]
  promedios = aggregate(tmp,by=list(tmp$Canal),mean)
  Hurst.promedio = rbind(Hurst.promedio,promedios)
}

if(usar.log){
  Hurst.todo$Hurst     = log(Hurst.todo$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}

# problemas con etiquetas
Hurst.todo$Canal       = factor(Hurst.todo$Canal,
                                labels=kanales$Etiqueta)
Hurst.todo$Estado      = factor(Hurst.todo$Estado,
                                labels=c('CTRL','PMCI'))
Hurst.promedio$Canal   = factor(Hurst.promedio$Canal,
                                labels=kanales$Etiqueta)
Hurst.promedio$Estado  = factor(Hurst.promedio$Estado,
                                labels=c('CTRL','PMCI'))
Hurst.promedio = as.data.frame(Hurst.promedio)
promedios = summarySE(Hurst.todo,measurevar='Hurst',
                      groupvars=c('Estado','Canal'),na.rm=T)

###############################################################################
# graficos sobre diferencias
ggplot(promedios,aes(x=Canal,y=Hurst,fill=Estado)) +
  #labs(title='Comparisons between groups on REM') +
  labs(title='B') +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.7))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.todo,inherit.aes=F,
                     mapping=aes(x=Canal,y=Hurst,fill=Estado),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))
ggsave(filename='/comparacion_multi.png',path=dir_graf,
       device='png',units='cm',width=20,height=7,dpi=300)

###############################################################################
# comparaciones usando wilcox
raw.ctl = rbind(raw[grep(1,raw$Sujeto),],
                raw[grep(2,raw$Sujeto),],
                raw[grep(3,raw$Sujeto),],
                raw[grep(4,raw$Sujeto),],
                raw[grep(5,raw$Sujeto),]
)

raw.pdc = rbind(raw[grep(6,raw$Sujeto),],
                raw[grep(7,raw$Sujeto),],
                raw[grep(8,raw$Sujeto),],
                raw[grep(9,raw$Sujeto),]
)

pvalores           = matrix(2,nrow =n.canales,ncol=4)
colnames(pvalores) = c('canal','p','T','dF')
pvalores[,'canal'] = kanales$Etiqueta

for(i in 1:n.canales){
  a = unlist(raw.ctl[kanales$Nombre_archivo[i]])
  b = unlist(raw.pdc[kanales$Nombre_archivo[i]])
  ttt = t.test(a,b,var.equal = F)
  pvalores[i,2] = ttt$p.value
  pvalores[i,3] = ttt$statistic
  pvalores[i,4] = ttt$parameter
}

guardar = cbind(promedios[1:n.canales,],
                promedios[n.canales+1:n.canales,],
                pvalores)

write.xlsx(guardar,file=paste0(dir_graf,archivo.excel),
           sheetName='CTL_PDC multi',
           col.names=TRUE,row.names=TRUE,append=TRUE)

###############################################################################
# correlaciones
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')

Hurst.todo$Participante    = factor(Hurst.todo$Participante,
                                    labels=info$Nombre[1:9])
Hurst.promedio$Participante = factor(Hurst.promedio$Participante,
                                     labels=info$Nombre[1:9])

# todas las observaciones
# Hurst vs Neuropsi
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo[grep(ch.actual,Hurst.todo$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Neuropsi)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.todo,aes(x=Neuropsi,y=Hurst,
                      shape=Estado,color=Estado))+
  ylab('Hurst Exponent') +
  labs(title='A')+
  labs(color=NULL,shape=NULL) +
  facet_wrap(~Canal,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Group')+
  stat_cor(data=Hurst.todo,inherit.aes=F,
           aes(x=Neuropsi,y=Hurst),method='spearman') +
  scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_hurst_multi.png',path=dir_graf,
       device='png',units='cm',width=18,height=15,dpi=300)

write.xlsx(correlaciones,file=paste0(dir_graf,archivo.excel),
           sheetName='H_Neuropsi multi',
           col.names=TRUE,row.names=TRUE,append=TRUE)

# Hurst vs Edad
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo[grep(ch.actual,Hurst.todo$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Edad)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.todo,aes(x=Edad,y=Hurst,
                      shape=Estado,color=Estado))+
  ylab('Hurst Exponent') + xlab('Age') +
  labs(title='A')+
  labs(color=NULL,shape=NULL) +
  facet_wrap(~Canal,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Group')+
  stat_cor(data=Hurst.todo,inherit.aes=F,
           aes(x=Edad,y=Hurst),method='spearman') +
  scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_hurst_multi.png',path=dir_graf,
       device='png',units='cm',width=18,height=15,dpi=300)

write.xlsx(correlaciones,file=paste0(dir_graf,archivo.excel),
           sheetName='H_Edad multi',
           col.names=TRUE,row.names=TRUE,append=TRUE)

# promedio por sujeto
# Hurst vs Neuropsi
for(ch in 1:n.canales){
  print(ch)
  
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.promedio[grep(ch.actual,Hurst.promedio$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Neuropsi)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,
                          shape=Estado,color=Estado))+
  ylab('Hurst Exponent') +
  labs(title='A')+
  labs(color=NULL) +
  facet_wrap(~Canal,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Neuropsi,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Group')+
  stat_cor(data=Hurst.promedio,inherit.aes=F,
           aes(x=Neuropsi,y=Hurst),method='spearman') +
  scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/neuropsi_hurst_promedio_multi.png',path=dir_graf,
       device='png',units='cm',width=18,height=15,dpi=300)

write.xlsx(correlaciones,file=paste0(dir_graf,archivo.excel),
           sheetName='H_Neuropsi_promedio multi',
           col.names=TRUE,row.names=TRUE,append=TRUE)

# Hurst vs Edad
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.promedio[grep(ch.actual,Hurst.promedio$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Edad)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.promedio,aes(x=Edad,y=Hurst,
                          shape=Estado,color=Estado))+
  ylab('Hurst Exponent') + xlab('Age') +
  labs(title='A')+
  labs(color=NULL) +
  facet_wrap(~Canal,ncol=5) +
  theme_classic2() +
  geom_smooth(method=lm,
              mapping=aes(x=Edad,y=Hurst),
              inherit.aes=F,
              se=F,color='black') +
  labs(color='Group')+
  stat_cor(data=Hurst.promedio,inherit.aes=F,
           aes(x=Edad,y=Hurst),method='spearman') +
  scale_y_continuous(expand=c(0,0))+
  scale_colour_discrete(guide = FALSE) +
  rotate_x_text(angle = 45)+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_point()
ggsave(filename='/edad_hurst_promedio_multi.png',path=dir_graf,
       device='png',units='cm',width=18,height=15,dpi=300)

write.xlsx(correlaciones,file=paste0(dir_graf,archivo.excel),
           sheetName='H_Edad_promedio multi',
           col.names=TRUE,row.names=TRUE,append=TRUE)
