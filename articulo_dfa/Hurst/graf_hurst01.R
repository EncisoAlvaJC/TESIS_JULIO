###############################################################################
# parametros
grabar         = FALSE
potencia.total = T
#sujeto         = 2
orden_stam     = TRUE

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk   = 1

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
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/espectro_integrado_171018'
dir_epocas  = 'C:/Users/EQUIPO 1/Desktop/julio/epocas_dfa_10'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/espectro_extendido'

###############################################################################
# librerias
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_65')
require(readxl)
require(xlsx)

require(ggplot2)

require(ggpubr)

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,'/info_participantes.xlsx'))
bandas   = read_excel(paste0(dir_info,'/info_bandas.xlsx'))

n.bandas = length(bandas$Banda)

canales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  canales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales = length(canales$Etiqueta)




library(readxl)
raw = read_excel("C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/Hurst/Source_vale2.xlsx")

usar.log = F

Hurst.todo = matrix(0,ncol=5,nrow=90*22)

colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst')

Hurst.todo = as.data.frame(Hurst.todo)

for(i in 1:90){
  step = 22*(i-1)
  channels = 1:22
  Hurst.todo[step+channels,1] = raw$Sujeto[i]
  Hurst.todo[step+channels,2] = raw$MOR_n[i]
  Hurst.todo[step+channels,3] = raw$Epoca_n[i]
  Hurst.todo[step+channels,4] = channels
  
  Hurst.todo[step+ 1,5] = raw$FP2[i]
  Hurst.todo[step+ 2,5] = raw$FP1[i]
  Hurst.todo[step+ 3,5] = raw$F8[i]
  Hurst.todo[step+ 4,5] = raw$F7[i]
  Hurst.todo[step+ 5,5] = raw$F4[i]
  Hurst.todo[step+ 6,5] = raw$F3[i]
  Hurst.todo[step+ 7,5] = raw$T4[i]
  Hurst.todo[step+ 8,5] = raw$T3[i]
  Hurst.todo[step+ 9,5] = raw$C4[i]
  Hurst.todo[step+10,5] = raw$C3[i]
  Hurst.todo[step+11,5] = raw$T6[i]
  Hurst.todo[step+12,5] = raw$T5[i]
  Hurst.todo[step+13,5] = raw$P4[i]
  Hurst.todo[step+14,5] = raw$P3[i]
  Hurst.todo[step+15,5] = raw$O2[i]
  Hurst.todo[step+16,5] = raw$O1[i]
  Hurst.todo[step+17,5] = raw$FZ[i]
  Hurst.todo[step+18,5] = raw$CZ[i]
  Hurst.todo[step+19,5] = raw$PZ[i]
  Hurst.todo[step+20,5] = raw$LOG[i]
  Hurst.todo[step+21,5] = raw$ROG[i]
  Hurst.todo[step+22,5] = raw$EMG[i]
  
}

if(usar.log){
  Hurst.todo$Hurst = log(Hurst.todo$Hurst)
}

Hurst.todo$Canal        = factor(Hurst.todo$Canal,
                                 labels=canales$Etiqueta)

Hurst.control = Hurst.todo[Hurst.todo$Participante<6,]

Hurst.control$Participante = factor(Hurst.control$Participante,
                                 labels = info$Etiqueta[1:5])

gpo = 'CTL'

ggplot(Hurst.control,aes(x=Canal,y=Hurst,fill=Participante)) +
  #geom_boxplot(notch = T) +
  geom_boxplot() +
  xlab(NULL) + ylab('Exponente de Hurst') +
  labs(title=paste('Grupo',gpo)) +
  theme(legend.position='bottom')+
  stat_compare_means(label = 'p.signif',method='kruskal.test')+
  rotate_x_text(angle = 45)



Hurst.pdc = Hurst.todo[Hurst.todo$Participante>5,]

Hurst.pdc$Participante = factor(Hurst.pdc$Participante,
                                    labels = info$Etiqueta[6:9])

gpo = 'PDC'

ggplot(Hurst.pdc,aes(x=Canal,y=Hurst,fill=Participante)) +
  #geom_boxplot(notch = T) +
  geom_boxplot() +
  xlab(NULL) + ylab('Exponente de Hurst') +
  labs(title=paste('Grupo',gpo)) +
  theme(legend.position='bottom')+
  stat_compare_means(label = 'p.signif',method='kruskal.test')+
  rotate_x_text(angle = 45)






gpo = 'Todos'

ggplot(Hurst.todo,aes(x=Canal,y=Hurst,fill=Participante)) +
  #geom_boxplot(notch = T) +
  geom_boxplot() +
  xlab(NULL) + ylab('Área bajo la curva') +
  labs(title=paste('Grupo',gpo,'| Proporcion enlentecimiento')) +
  theme(legend.position='bottom')+
  stat_compare_means(label = 'p.signif',method='kruskal.test')+
  rotate_x_text(angle = 45)


Hurst.todo$Participante = factor(Hurst.todo$Participante,
                                 labels = info$Etiqueta[1:9])

i = 1

Hurst.uno = Hurst.todo[grep(i,Hurst.todo$Participante,
                       ignore.case=T),]

ggplot(Hurst.uno,aes(x=Epoca_n,y=Canal,fill=Hurst)) +
  scale_x_continuous(breaks = pretty(Hurst.uno$Epoca_n, n = 10),
                     expand=c(0,0)) +
  labs(title=info$Etiqueta[i])+
  geom_raster(hjust=.5,vjust=.5,interpolate=F) +
  xlab('Número de época') +
  theme(legend.position='left')+
  scale_fill_gradient2(low='white',high='saddlebrown',mid='pink',
                       midpoint=1)


print(54)

































library(readxl)
raw = read_excel("C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/Hurst/Source_vale_multi2.xlsx")

Hurst.todo = matrix(0,ncol=5,nrow=90*13)

colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst')

Hurst.todo = as.data.frame(Hurst.todo)

for(i in 1:90){
  step = 13*(i-1)
  channels = 1:13
  Hurst.todo[step+channels,1] = raw$Sujeto[i]
  Hurst.todo[step+channels,2] = raw$MOR_n[i]
  Hurst.todo[step+channels,3] = raw$Epoca_n[i]
  Hurst.todo[step+channels,4] = channels
  
  Hurst.todo[step+ 1,5] = raw$FP1_FP2[i]
  Hurst.todo[step+ 2,5] = raw$F7_F8[i]
  Hurst.todo[step+ 3,5] = raw$F3_F4[i]
  Hurst.todo[step+ 4,5] = raw$T3_T4[i]
  Hurst.todo[step+ 5,5] = raw$C3_C4[i]
  Hurst.todo[step+ 6,5] = raw$T5_T6[i]
  Hurst.todo[step+ 7,5] = raw$P3_P4[i]
  Hurst.todo[step+ 8,5] = raw$O1_O2[i]
  Hurst.todo[step+ 9,5] = raw$LOG_ROG[i]
  Hurst.todo[step+10,5] = raw$FP2_P4[i]
  Hurst.todo[step+11,5] = raw$FP1_P3[i]
  Hurst.todo[step+12,5] = raw$O2_P4_T4[i]
  Hurst.todo[step+13,5] = raw$O1_P3_T3[i]
  
}

Hurst.todo$Canal = factor(Hurst.todo$Canal,
                          labels=c('Fp1-Fp2',
                                   'F7-F8',
                                   'F3-F4',
                                   'T3-T4',
                                   'C3-C4',
                                   'T5-T6',
                                   'P3-P4',
                                   'O1-O2',
                                   'LOG-ROG',
                                   'Fp2-P4',
                                   'Fp1-P3',
                                   'O2-P4-T4',
                                   'O1-P3-T3'))

Hurst.control = Hurst.todo[Hurst.todo$Participante<6,]

Hurst.control$Participante = factor(Hurst.control$Participante,
                                    labels = info$Etiqueta[1:5])

gpo = 'CTL'

ggplot(Hurst.control,aes(x=Canal,y=Hurst,fill=Participante)) +
  #geom_boxplot(notch = T) +
  geom_boxplot() +
  xlab(NULL) + ylab('Exponente de Hurst') +
  labs(title=paste('Grupo',gpo)) +
  theme(legend.position='bottom')+
  stat_compare_means(label = 'p.signif',method='kruskal.test')+
  rotate_x_text(angle = 45)




Hurst.pdc = Hurst.todo[Hurst.todo$Participante>5,]

Hurst.pdc$Participante = factor(Hurst.pdc$Participante,
                                labels = info$Etiqueta[6:9])

gpo = 'PDC'

ggplot(Hurst.pdc,aes(x=Canal,y=Hurst,fill=Participante)) +
  #geom_boxplot(notch = T) +
  geom_boxplot() +
  xlab(NULL) + ylab('Exponente de Hurst') +
  labs(title=paste('Grupo',gpo)) +
  theme(legend.position='bottom')+
  stat_compare_means(label = 'p.signif',method='kruskal.test')+
  rotate_x_text(angle = 45)
