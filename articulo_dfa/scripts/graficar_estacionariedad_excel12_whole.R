
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
                             '/info_tecnico2.xlsx'),sheet='General')

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales = length(kanales$Etiqueta)

#################################################
# datos sobre estacionariedad
setwd(dir_res_pre)
raw  = read_excel(paste0(dir_res_pre,'/stat_whole_asdataframe.xlsx'),
                  sheet='mor')
raw = as.data.frame(raw)
RES.MOR  = melt(raw,id=c('Sujeto','Edad','Neuropsi','MMSE',
                         'Escolaridad','Grupo','Etapa'))
RES.MOR  = as.data.frame(RES.MOR)
colnames(RES.MOR)[c(8,9)] = c('Canal_var','Proporcion')

RES.MOR$Proporcion = RES.MOR$Proporcion*100 

raw  = read_excel(paste0(dir_res_pre,'/stat_whole_asdataframe.xlsx'),
                  sheet='nmor')
raw = as.data.frame(raw)
RES.NMOR  = melt(raw,id=c('Sujeto','Edad','Neuropsi','MMSE',
                         'Escolaridad','Grupo','Etapa'))
RES.NMOR  = as.data.frame(RES.NMOR)
colnames(RES.NMOR)[c(8,9)] = c('Canal_var','Proporcion')

RES.NMOR$Proporcion = RES.NMOR$Proporcion*100

RES.MOR$Grupo  = factor(RES.MOR$Grupo,labels = c('CTRL','PMCI'))
RES.NMOR$Grupo = factor(RES.NMOR$Grupo,labels = c('CTRL','PMCI'))

#RES.MOR$Proporcion  = 100 - RES.MOR$Proporcion
#RES.NMOR$Proporcion = 100 - RES.NMOR$Proporcion

promedios.MOR  = summarySE(RES.MOR,measurevar='Proporcion',
                           groupvars=c('Grupo','Canal_var'),na.rm=T)
promedios.NMOR = summarySE(RES.NMOR,measurevar='Proporcion',
                          groupvars=c('Grupo','Canal_var'),na.rm=T)

###############################################################################
# Intercanales
RES.MOR$Etapa  = rep('REM',length(RES.MOR$Etapa))
RES.NMOR$Etapa = rep('NREM',length(RES.MOR$Etapa))
RES.todo = rbind(RES.MOR,RES.NMOR)

promedios.MOR$Etapa  = rep('REM',length(promedios.MOR$Grupo))
promedios.NMOR$Etapa = rep('NREM',length(promedios.NMOR$Grupo))
promedios.todo = rbind(promedios.MOR,promedios.NMOR)

A = ggplot(promedios.todo,aes(x=Canal_var,y=Proporcion,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Stationary epochs [%]') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(0,50))+
  #scale_fill_grey(start = 1, end = 0) +
  scale_fill_manual(values = c('#e6e6e6ff','#4d4d4dff'))+
  stat_compare_means(data=RES.todo,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Proporcion,fill=Grupo),
                     label='p.signif',method='wilcox.test',
                     hide.ns=T,label.y = 45)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels=NULL)
ggsave(filename='/comparacion_whole_stat_CTRL_PMCI.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

A = ggplot(promedios.todo,aes(x=Canal_var,y=Proporcion,fill=Etapa)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Stationary epochs [%]') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Proporcion-se,ymax=Proporcion+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(0,50))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=RES.todo,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Proporcion,fill=Etapa),
                     label='p.signif',method='wilcox.test',
                     hide.ns=T,label.y = 45)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  theme(strip.background = element_blank())+
  facet_grid(Grupo~.) +
  rotate_x_text(angle = 45)
ggarrange(A,labels=NULL)
ggsave(filename='/comparacion_whole_stat_MOR_NMOR.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

################################################################################
big.summary= c()
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  #tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp   = RES.todo[grep(ch.actual,RES.todo$Canal),]
  tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]
  
  aov   = aov(Proporcion ~ (Grupo) + (Etapa) +(Grupo:Etapa),
              data=tmp)
  
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  p = ggplot(tmp.m,aes(x=Etapa,y=Proporcion,linetype=Grupo))+
    theme_classic2() +
    labs(linetype=ch.actual)+
    #coord_cartesian(ylim=c(0.3,1.75))+
    geom_line(aes(group=Grupo))+
    geom_errorbar(aes(ymin=Proporcion-sd,ymax=Proporcion+sd),width=.1,
                  color='grey40') +
    geom_point()
  print(p)
  ggsave(path=dir_graf,device='png',units='cm',
         width=8,height=4,dpi=400,scale=2,
         file=paste0(ch.actual,'.png'))
  
  qs  = summarySE(data=tmp,groupvars=c('Grupo','Etapa'),
                  measurevar='Proporcion')
  qs2 = unlist(t(qs))
  qs2 = as.list((qs2))
  qs2 = unlist(t(qs2))
  big.summary = rbind(big.summary,qs2)
  
  View(k)
  invisible(readline(prompt="Presion [enter] para continuar"))
}
