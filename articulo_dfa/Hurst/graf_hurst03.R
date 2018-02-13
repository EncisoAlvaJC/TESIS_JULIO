# Estadistica no parametrica
# Aplicada a las ceincias de la conducta
# Sidney Siegel
# N John Castellanm

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

Hurst.todo = matrix(0,ncol=5+1,nrow=90*22)

colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst','Estado')

Hurst.todo = as.data.frame(Hurst.todo)

for(i in 1:90){
  step = 22*(i-1)
  channels = 1:22
  Hurst.todo[step+channels,1] = raw$Sujeto[i]
  Hurst.todo[step+channels,2] = raw$MOR_n[i]
  Hurst.todo[step+channels,3] = raw$Epoca_n[i]
  Hurst.todo[step+channels,4] = channels
  Hurst.todo[step+channels,6] = 1*(raw$Sujeto[i]>5)
  
  for(ch in 1:22){
    Hurst.todo[step+ch,5] = unlist(raw[canales$Nombre_archivo[ch]])[i]
  }
}

if(usar.log){
  Hurst.todo$Hurst = log(Hurst.todo$Hurst)
}

Hurst.todo$Canal   = factor(Hurst.todo$Canal,
                            labels=canales$Etiqueta)
Hurst.todo$Estado  = factor(Hurst.todo$Estado,
                            labels=c('CTL','PDC'))

ggplot(Hurst.todo,aes(x=Canal,y=Hurst,fill=Estado)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Exp. de Hurst') +
  labs(title='Exponente de Hurst') +
  theme(legend.position='bottom')+
  #stat_compare_means(label = 'p.format',method='t.test')+
  stat_compare_means(label = 'p.signif',method='t.test')+
  rotate_x_text(angle = 45)

ggplot(Hurst.todo,aes(x=Canal,y=Hurst,fill=Estado)) +
  geom_bar() +
  xlab(NULL) + ylab('Exp. de Hurst') +
  labs(title='Exponente de Hurst') +
  theme(legend.position='bottom')+
  #stat_compare_means(label = 'p.format',method='t.test')+
  stat_compare_means(label = 'p.signif',method='t.test')+
  rotate_x_text(angle = 45)


##################################
##################################

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

pvalores = matrix(2,nrow = 22,ncol=4)
colnames(pvalores) = c('canal','p','T','df')
pvalores[,1] = canales$Etiqueta

for(i in 1:22){
  a = unlist(raw.ctl[canales$Nombre_archivo[i]])
  b = unlist(raw.pdc[canales$Nombre_archivo[i]])
  #print(canales$Nombre_archivo[i])
  ttt = t.test(a,b,var.equal = F)
  pvalores[i,2] = ttt$p.value
  pvalores[i,3] = ttt$statistic
  pvalores[i,4] = ttt$parameter
  #print(t.test(a,b,var.equal = T))
}

# Hurst.todo$Participante = factor(Hurst.todo$Participante,
#                                  labels = info$Etiqueta[1:9])
# 
# Hurst.uno = Hurst.todo[grep(i,Hurst.todo$Participante,
#                        ignore.case=T),]
# 
# ggplot(Hurst.uno,aes(x=Epoca_n,y=Canal,fill=Hurst)) +
#   scale_x_continuous(breaks = pretty(Hurst.uno$Epoca_n, n = 10),
#                      expand=c(0,0)) +
#   labs(title=info$Etiqueta[i])+
#   geom_raster(hjust=.5,vjust=.5,interpolate=F) +
#   xlab('Número de época') +
#   theme(legend.position='left')+
#   scale_fill_gradient2(low='white',high='saddlebrown',mid='pink',
#                        midpoint=1)

Hurst.todo = matrix(0,ncol=5+1+2,nrow=90*22)

colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst','Estado',
                         'Edad','Neuropsi')

Hurst.todo = as.data.frame(Hurst.todo)

for(i in 1:90){
  step = 22*(i-1)
  channels = 1:22
  Hurst.todo[step+channels,1] = raw$Sujeto[i]
  Hurst.todo[step+channels,2] = raw$MOR_n[i]
  Hurst.todo[step+channels,3] = raw$Epoca_n[i]
  Hurst.todo[step+channels,4] = channels
  Hurst.todo[step+channels,6] = 1*(raw$Sujeto[i]>5)
  Hurst.todo[step+channels,7] = raw$Edad[i]
  Hurst.todo[step+channels,8] = raw$Neuropsi_gral[i]
  
  for(ch in 1:22){
    Hurst.todo[step+ch,5] = unlist(raw[canales$Nombre_archivo[ch]])[i]
  }
}

if(usar.log){
  Hurst.todo$Hurst = log(Hurst.todo$Hurst)
}

Hurst.todo = as.data.frame(Hurst.todo)


Hurst.promedio = c()

for(sujeto in 1:9){
  tmp       = Hurst.todo[grep(sujeto,Hurst.todo$Participante),]
  promedios = aggregate(tmp,by=list(tmp$Canal),mean)
  Hurst.promedio = rbind(Hurst.promedio,promedios)
}

Hurst.promedio$Canal   = factor(Hurst.promedio$Canal,
                            labels=canales$Etiqueta)
Hurst.promedio$Estado  = factor(Hurst.promedio$Estado,
                            labels=c('CTL','PDC'))

Hurst.promedio = as.data.frame(Hurst.promedio)

correlaciones = matrix(0,nrow=22,ncol=2)
colnames(correlaciones) = c('Canal','Corr')

correlaciones[,1] = canales$Etiqueta

for(ch in 1:22){
  Hurst.tmp = Hurst.todo[grep(canales$Etiqueta[ch],Hurst.todo$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Neuropsi)
  
  correlaciones[ch,2] = cor(a,b,use='pairwise.complete.obs')
}

correlaciones10 = correlaciones

cor(Hurst.todo$Neuropsi,Hurst.todo$Hurst,use='pairwise.complete.obs')

correlaciones = matrix(0,nrow=22,ncol=2)
colnames(correlaciones) = c('Canal','Corr')

correlaciones[,1] = canales$Etiqueta

for(ch in 1:22){
  Hurst.tmp = Hurst.promedio[grep(canales$Etiqueta[ch],Hurst.promedio$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Neuropsi)
  
  correlaciones[ch,2] = cor(a,b,use='pairwise.complete.obs')
}

correlaciones1 = correlaciones

ch = 12
Hurst.tmp = Hurst.promedio[grep(canales$Etiqueta[ch],Hurst.promedio$Canal),]
#ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,color=Estado))+
ggplot(Hurst.tmp,aes(x=Neuropsi,y=Hurst))+
  #xlab(NULL) + 
  ylab('Exponente de Hurst') +
  theme(legend.position='bottom')+
  geom_smooth(method=lm) +
  rotate_x_text(angle = 45)+
  facet_grid(Canal~.) +
  geom_point()


print(2017)




Hurst.todo$Canal   = factor(Hurst.todo$Canal,
                            labels=canales$Etiqueta)
Hurst.todo$Estado  = factor(Hurst.todo$Estado,
                            labels=c('CTL','PDC'))
Hurst.todo$Participante  = factor(Hurst.todo$Participante,
                            labels=info$Etiqueta[1:9])

ch = 21
Hurst.tmp = Hurst.todo[grep(canales$Nombre_archivo[ch],Hurst.todo$Canal),]
#ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,color=Estado))+
ggplot(Hurst.tmp,aes(x=Neuropsi,y=Hurst))+
  #xlab(NULL) + 
  ylab('Exponente de Hurst') +
  theme(legend.position='bottom')+
  geom_smooth(method=lm) +
  rotate_x_text(angle = 45)+
  facet_grid(Canal~.) +
  geom_point()


print(54)










ch = 9
canales$Etiqueta[ch]
Hurst.tmp = Hurst.promedio[grep(canales$Nombre_archivo[ch],Hurst.promedio$Canal),]
mean(Hurst.tmp$Hurst,na.rm = T)
range(Hurst.tmp$Hurst,na.rm=T)

ch = 12
canales$Etiqueta[ch]
Hurst.tmp = Hurst.promedio[grep(canales$Nombre_archivo[ch],Hurst.promedio$Canal),]
mean(Hurst.tmp$Hurst,na.rm = T)
range(Hurst.tmp$Hurst,na.rm=T)

ch = 20
canales$Etiqueta[ch]
Hurst.tmp = Hurst.promedio[grep(canales$Nombre_archivo[ch],Hurst.promedio$Canal),]
mean(Hurst.tmp$Hurst,na.rm = T)
range(Hurst.tmp$Hurst,na.rm=T)

ggplot(Hurst.promedio,aes(x=Participante,y=Hurst,color=Epoca_n))+
  facet_grid(.~Canal)+
  geom_point()

ch = 21
Hurst.tmp = Hurst.promedio[grep(canales$Nombre_archivo[ch],Hurst.promedio$Canal),]
mean(Hurst.tmp$Hurst,na.rm = T)
range(Hurst.tmp$Hurst,na.rm=T)


ch = 10
canales$Etiqueta[ch]
Hurst.tmp = Hurst.promedio[grep(canales$Nombre_archivo[ch],Hurst.promedio$Canal),]
mean(Hurst.tmp$Hurst,na.rm = T)
range(Hurst.tmp$Hurst,na.rm=T)

ch = 11
canales$Etiqueta[ch]
Hurst.tmp = Hurst.promedio[grep(canales$Nombre_archivo[ch],Hurst.promedio$Canal),]
mean(Hurst.tmp$Hurst,na.rm = T)
range(Hurst.tmp$Hurst,na.rm=T)

ch = 18
canales$Etiqueta[ch]
Hurst.tmp = Hurst.promedio[grep(canales$Nombre_archivo[ch],Hurst.promedio$Canal),]
mean(Hurst.tmp$Hurst,na.rm = T)
range(Hurst.tmp$Hurst,na.rm=T)

ch = 21
Hurst.tmp = Hurst.promedio[grep(canales$Nombre_archivo[ch],Hurst.promedio$Canal),]
mean(Hurst.tmp$Hurst,na.rm = T)
range(Hurst.tmp$Hurst,na.rm=T)

print(54)




















library(readxl)
raw = read_excel("C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/Hurst/Source_vale_multi2.xlsx")

Hurst.todo = matrix(0,ncol=5+1+2,nrow=90*13)

colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst','Estado',
                         'Edad','Neuropsi')

Hurst.todo = as.data.frame(Hurst.todo)

intercanales_sub = c('FP1_FP2',
                 'F7_F8',
                 'F3_F4',
                 'T3_T4',
                 'C3_C4',
                 'T5_T6',
                 'P3_P4',
                 'O1_O2',
                 'LOG_ROG',
                 'FP2_P4',
                 'FP1_P3',
                 'O2_P4_T4',
                 'O1_P3_T3')

intercanales    = c('Fp1-Fp2',
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
                 'O1-P3-T3')

for(i in 1:90){
  step = 13*(i-1)
  channels = 1:13
  Hurst.todo[step+channels,1] = raw$Sujeto[i]
  Hurst.todo[step+channels,2] = raw$MOR_n[i]
  Hurst.todo[step+channels,3] = raw$Epoca_n[i]
  Hurst.todo[step+channels,4] = intercanales
  Hurst.todo[step+channels,6] = 1*(raw$Sujeto[i]>5)
  Hurst.todo[step+channels,7] = raw$Edad[i]
  Hurst.todo[step+channels,8] = raw$Neuropsi_gral[i]
  
  for(ch in 1:13){
    Hurst.todo[step+ch,5] = unlist(raw[intercanales_sub[ch]])[i]
  }
}





Hurst.promedio = c()

for(sujeto in 1:9){
  tmp       = Hurst.todo[grep(sujeto,Hurst.todo$Participante),]
  promedios = aggregate(tmp,by=list(tmp$Canal),mean)
  Hurst.promedio = rbind(Hurst.promedio,promedios)
}

correlaciones = matrix(0,nrow=13,ncol=2)
colnames(correlaciones) = c('Canal','Corr')

correlaciones[,1] = intercanales

for(ch in 1:13){
  Hurst.tmp = Hurst.todo[grep(intercanales[ch],Hurst.todo$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Neuropsi)
  
  correlaciones[ch,2] = cor(a,b,use='pairwise.complete.obs')
}

correlaciones10 = correlaciones

cor(Hurst.todo$Neuropsi,Hurst.todo$Hurst,use='pairwise.complete.obs')

correlaciones = matrix(0,nrow=22,ncol=2)
colnames(correlaciones) = c('Canal','Corr')

correlaciones[,1] = canales$Etiqueta

for(ch in 1:22){
  Hurst.tmp = Hurst.promedio[grep(canales$Etiqueta[ch],Hurst.promedio$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Neuropsi)
  
  correlaciones[ch,2] = cor(a,b,use='pairwise.complete.obs')
}

correlaciones1 = correlaciones

Hurst.todo$Canal = factor(Hurst.todo$Canal,
                          labels=intercanales)

Hurst.promedio$Estado  = factor(Hurst.promedio$Estado,
                                labels=c('CTL','PDC'))

Hurst.promedio = as.data.frame(Hurst.promedio)

ch = 2
Hurst.tmp = Hurst.promedio[grep(intercanales[ch],Hurst.promedio$Group.1),]
#ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,color=Estado))+
ggplot(Hurst.tmp,aes(x=Neuropsi,y=Hurst))+
  #xlab(NULL) + 
  ylab('Exponente de Hurst') +
  theme(legend.position='bottom')+
  geom_smooth(method=lm) +
  rotate_x_text(angle = 45)+
  facet_grid(Group.1~.) +
  geom_point()


print(2017)




Hurst.todo$Canal   = factor(Hurst.todo$Canal,
                            labels=intercanales)
Hurst.todo$Estado  = factor(Hurst.todo$Estado,
                            labels=c('CTL','PDC'))
Hurst.todo$Participante  = factor(Hurst.todo$Participante,
                                  labels=info$Etiqueta[1:9])

ch = 4
Hurst.tmp = Hurst.todo[grep(intercanales[ch],Hurst.todo$Canal),]
#ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,color=Estado))+
ggplot(Hurst.tmp,aes(x=Neuropsi,y=Hurst),color=Estado)+
  #xlab(NULL) + 
  ylab('Exponente de Hurst') +
  theme_pubclean()+
  theme(legend.position='bottom')+
  geom_smooth(method=lm) +
  rotate_x_text(angle = 45)+
  facet_grid(Canal~.) +
  geom_point()


print(54)



ggplot(Hurst.tmp,aes(x=Edad,y=Neuropsi,color=Estado))+
  theme_pubclean()+
  geom_point()

###############################################################################33
##################################################################################
#####################################################################################
#########################################################################33333333


# Estadistica no parametrica
# Aplicada a las ceincias de la conducta
# Sidney Siegel
# N John Castellanm

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

Hurst.todo = matrix(0,ncol=5+1,nrow=90*22)

colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst','Estado')

Hurst.todo = as.data.frame(Hurst.todo)

for(i in 1:90){
  step = 22*(i-1)
  channels = 1:22
  Hurst.todo[step+channels,1] = raw$Sujeto[i]
  Hurst.todo[step+channels,2] = raw$MOR_n[i]
  Hurst.todo[step+channels,3] = raw$Epoca_n[i]
  Hurst.todo[step+channels,4] = channels
  Hurst.todo[step+channels,6] = 1*(raw$Sujeto[i]>5)
  
  for(ch in 1:22){
    Hurst.todo[step+ch,5] = unlist(raw[canales$Nombre_archivo[ch]])[i]
  }
}

if(usar.log){
  Hurst.todo$Hurst = log(Hurst.todo$Hurst)
}

Hurst.todo$Canal   = factor(Hurst.todo$Canal,
                            labels=canales$Etiqueta)
Hurst.todo$Estado  = factor(Hurst.todo$Estado,
                            labels=c('CTL','PDC'))

ggplot(Hurst.todo,aes(x=Canal,y=Hurst,fill=Estado)) +
  geom_boxplot() +
  xlab(NULL) + ylab('Exp. de Hurst') +
  labs(title='Exponente de Hurst') +
  theme(legend.position='bottom')+
  #stat_compare_means(label = 'p.format',method='t.test')+
  stat_compare_means(label = 'p.signif',method='t.test')+
  rotate_x_text(angle = 45)


##################################
##################################

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

pvalores = matrix(2,nrow = 22,ncol=4)
colnames(pvalores) = c('canal','p','T','df')
pvalores[,1] = canales$Etiqueta

for(i in 1:22){
  a = unlist(raw.ctl[canales$Nombre_archivo[i]])
  b = unlist(raw.pdc[canales$Nombre_archivo[i]])
  #print(canales$Nombre_archivo[i])
  ttt = t.test(a,b,var.equal = F)
  pvalores[i,2] = ttt$p.value
  pvalores[i,3] = ttt$statistic
  pvalores[i,4] = ttt$parameter
  #print(t.test(a,b,var.equal = T))
}

# Hurst.todo$Participante = factor(Hurst.todo$Participante,
#                                  labels = info$Etiqueta[1:9])
# 
# Hurst.uno = Hurst.todo[grep(i,Hurst.todo$Participante,
#                        ignore.case=T),]
# 
# ggplot(Hurst.uno,aes(x=Epoca_n,y=Canal,fill=Hurst)) +
#   scale_x_continuous(breaks = pretty(Hurst.uno$Epoca_n, n = 10),
#                      expand=c(0,0)) +
#   labs(title=info$Etiqueta[i])+
#   geom_raster(hjust=.5,vjust=.5,interpolate=F) +
#   xlab('Número de época') +
#   theme(legend.position='left')+
#   scale_fill_gradient2(low='white',high='saddlebrown',mid='pink',
#                        midpoint=1)

Hurst.todo = matrix(0,ncol=5+1+2,nrow=90*22)

colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst','Estado',
                         'Edad','Neuropsi')

Hurst.todo = as.data.frame(Hurst.todo)

for(i in 1:90){
  step = 22*(i-1)
  channels = 1:22
  Hurst.todo[step+channels,1] = raw$Sujeto[i]
  Hurst.todo[step+channels,2] = raw$MOR_n[i]
  Hurst.todo[step+channels,3] = raw$Epoca_n[i]
  Hurst.todo[step+channels,4] = channels
  Hurst.todo[step+channels,6] = 1*(raw$Sujeto[i]>5)
  Hurst.todo[step+channels,7] = raw$Edad[i]
  Hurst.todo[step+channels,8] = raw$Neuropsi_gral[i]
  
  for(ch in 1:22){
    Hurst.todo[step+ch,5] = unlist(raw[canales$Nombre_archivo[ch]])[i]
  }
}

if(usar.log){
  Hurst.todo$Hurst = log(Hurst.todo$Hurst)
}

Hurst.todo = as.data.frame(Hurst.todo)


Hurst.promedio = c()

for(sujeto in 1:9){
  tmp       = Hurst.todo[grep(sujeto,Hurst.todo$Participante),]
  promedios = aggregate(tmp,by=list(tmp$Canal),mean)
  Hurst.promedio = rbind(Hurst.promedio,promedios)
}

Hurst.promedio$Canal   = factor(Hurst.promedio$Canal,
                                labels=canales$Etiqueta)
Hurst.promedio$Estado  = factor(Hurst.promedio$Estado,
                                labels=c('CTL','PDC'))

Hurst.promedio = as.data.frame(Hurst.promedio)

correlaciones = matrix(0,nrow=22,ncol=2)
colnames(correlaciones) = c('Canal','Corr')

correlaciones[,1] = canales$Etiqueta

for(ch in 1:22){
  Hurst.tmp = Hurst.todo[grep(ch,Hurst.todo$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Edad)
  
  correlaciones[ch,2] = cor(a,b,use='pairwise.complete.obs')
}

correlaciones10_edad = correlaciones

cor(Hurst.todo$Neuropsi,Hurst.todo$Hurst,use='pairwise.complete.obs')

correlaciones = matrix(0,nrow=22,ncol=2)
colnames(correlaciones) = c('Canal','Corr')

correlaciones[,1] = canales$Etiqueta

for(ch in 1:22){
  Hurst.tmp = Hurst.promedio[grep(canales$Etiqueta[ch],Hurst.promedio$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Edad)
  
  correlaciones[ch,2] = cor(a,b,use='pairwise.complete.obs')
}

correlaciones1_edad = correlaciones

ch = 22
Hurst.tmp = Hurst.promedio[grep(canales$Etiqueta[ch],Hurst.promedio$Canal),]
#ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,color=Estado))+
ggplot(Hurst.tmp,aes(x=Edad,y=Hurst,color=Estado))+
  #xlab(NULL) + 
  ylab('Exponente de Hurst') +
  theme(legend.position='bottom')+
  geom_smooth(method=lm) +
  theme_pubclean()+
  rotate_x_text(angle = 45)+
  facet_grid(Canal~.) +
  geom_point()
# WARD

print(2017)




Hurst.todo$Canal   = factor(Hurst.todo$Canal,
                            labels=canales$Etiqueta)
Hurst.todo$Estado  = factor(Hurst.todo$Estado,
                            labels=c('CTL','PDC'))
Hurst.todo$Participante  = factor(Hurst.todo$Participante,
                                  labels=info$Etiqueta[1:9])

ch = 21
Hurst.tmp = Hurst.todo[grep(canales$Nombre_archivo[ch],Hurst.todo$Canal),]
#ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,color=Estado))+
ggplot(Hurst.tmp,aes(x=Neuropsi,y=Hurst))+
  #xlab(NULL) + 
  ylab('Exponente de Hurst') +
  theme(legend.position='bottom')+
  geom_smooth(method=lm) +
  rotate_x_text(angle = 45)+
  facet_grid(Canal~.) +
  geom_point()


print(54)

































library(readxl)
raw = read_excel("C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/Hurst/Source_vale_multi2.xlsx")

Hurst.todo = matrix(0,ncol=5+1+2,nrow=90*13)

colnames(Hurst.todo) = c('Participante','MOR_n','Epoca_n','Canal','Hurst','Estado',
                         'Edad','Neuropsi')

Hurst.todo = as.data.frame(Hurst.todo)

intercanales_sub = c('FP1_FP2',
                     'F7_F8',
                     'F3_F4',
                     'T3_T4',
                     'C3_C4',
                     'T5_T6',
                     'P3_P4',
                     'O1_O2',
                     'LOG_ROG',
                     'FP2_P4',
                     'FP1_P3',
                     'O2_P4_T4',
                     'O1_P3_T3')

intercanales    = c('Fp1-Fp2',
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
                    'O1-P3-T3')

for(i in 1:90){
  step = 13*(i-1)
  channels = 1:13
  Hurst.todo[step+channels,1] = raw$Sujeto[i]
  Hurst.todo[step+channels,2] = raw$MOR_n[i]
  Hurst.todo[step+channels,3] = raw$Epoca_n[i]
  Hurst.todo[step+channels,4] = intercanales
  Hurst.todo[step+channels,6] = 1*(raw$Sujeto[i]>5)
  Hurst.todo[step+channels,7] = raw$Edad[i]
  Hurst.todo[step+channels,8] = raw$Neuropsi_gral[i]
  
  for(ch in 1:13){
    Hurst.todo[step+ch,5] = unlist(raw[intercanales_sub[ch]])[i]
  }
}





Hurst.promedio = c()

for(sujeto in 1:9){
  tmp       = Hurst.todo[grep(sujeto,Hurst.todo$Participante),]
  promedios = aggregate(tmp,by=list(tmp$Canal),mean)
  Hurst.promedio = rbind(Hurst.promedio,promedios)
}

correlaciones = matrix(0,nrow=13,ncol=2)
colnames(correlaciones) = c('Canal','Corr')

correlaciones[,1] = intercanales

for(ch in 1:13){
  Hurst.tmp = Hurst.todo[grep(intercanales[ch],Hurst.todo$Canal),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Edad)
  
  correlaciones[ch,2] = cor(a,b,use='pairwise.complete.obs')
}

correlaciones10 = correlaciones

cor(Hurst.todo$Neuropsi,Hurst.todo$Hurst,use='pairwise.complete.obs')

correlaciones = matrix(0,nrow=13,ncol=2)
colnames(correlaciones) = c('Canal','Corr')

correlaciones[,1] = intercanales

for(ch in 1:13){
  Hurst.tmp = Hurst.promedio[grep(intercanales[ch],Hurst.promedio$Group.1),]
  
  a = unlist(Hurst.tmp$Hurst)
  b = unlist(Hurst.tmp$Edad)
  
  correlaciones[ch,2] = cor(a,b,use='pairwise.complete.obs')
}

correlaciones1 = correlaciones

Hurst.todo$Canal = factor(Hurst.todo$Canal,
                          labels=intercanales)

Hurst.promedio$Estado  = factor(Hurst.promedio$Estado,
                                labels=c('CTL','PDC'))

Hurst.promedio = as.data.frame(Hurst.promedio)

ch = 2
Hurst.tmp = Hurst.promedio[grep(intercanales[ch],Hurst.promedio$Group.1),]
#ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,color=Estado))+
ggplot(Hurst.tmp,aes(x=Neuropsi,y=Hurst))+
  #xlab(NULL) + 
  ylab('Exponente de Hurst') +
  theme(legend.position='bottom')+
  geom_smooth(method=lm) +
  rotate_x_text(angle = 45)+
  facet_grid(Group.1~.) +
  geom_point()


print(2017)




Hurst.todo$Canal   = factor(Hurst.todo$Canal,
                            labels=intercanales)
Hurst.todo$Estado  = factor(Hurst.todo$Estado,
                            labels=c('CTL','PDC'))
Hurst.todo$Participante  = factor(Hurst.todo$Participante,
                                  labels=info$Etiqueta[1:9])

ch = 4
Hurst.tmp = Hurst.todo[grep(intercanales[ch],Hurst.todo$Canal),]
#ggplot(Hurst.promedio,aes(x=Neuropsi,y=Hurst,color=Estado))+
ggplot(Hurst.tmp,aes(x=Neuropsi,y=Hurst),color=Estado)+
  #xlab(NULL) + 
  ylab('Exponente de Hurst') +
  theme_pubclean()+
  theme(legend.position='bottom')+
  geom_smooth(method=lm) +
  rotate_x_text(angle = 45)+
  facet_grid(Canal~.) +
  geom_point()


print(54)



ggplot(Hurst.tmp,aes(x=Edad,y=Neuropsi,color=Estado))+
  theme_pubclean()+
  geom_point()
