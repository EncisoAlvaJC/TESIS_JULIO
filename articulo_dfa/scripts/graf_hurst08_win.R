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

n.mors = 10

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
info     = read_excel(paste0(dir_info,'/info_tecnico2.xlsx'))

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

###############################################################################
# cargar los datos
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe_cut.xlsx'),sheet='uno')
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

Hurst.MOR = Hurst.MOR[Hurst.MOR$MORn<(n.mors+1),]

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
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe_cut.xlsx'),sheet='uno_pre')
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

Hurst.NMOR = Hurst.NMOR[Hurst.NMOR$MORn>(10-n.mors),]

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

Hurst.MOR$Etapa  = rep('REM',length(Hurst.MOR$Etapa))
Hurst.NMOR$Etapa = rep('NREM',length(Hurst.NMOR$Etapa))
Hurst.todo = rbind(Hurst.MOR,Hurst.NMOR)

promedios.MOR$Etapa  = rep('REM',length(promedios.MOR$Grupo))
promedios.NMOR$Etapa = rep('NREM',length(promedios.NMOR$Grupo))
promedios.todo = rbind(promedios.MOR,promedios.NMOR)

####################################################################33

colnames(Hurst.MOR.promedio)[1]  = 'Canal'
Hurst.MOR.promedio$Canal         = factor(Hurst.MOR.promedio$Canal,
                                          labels = kanales$Etiqueta)
colnames(Hurst.NMOR.promedio)[1] = 'Canal'
Hurst.NMOR.promedio$Canal         = factor(Hurst.NMOR.promedio$Canal,
                                          labels = kanales$Etiqueta)

Hurst.todo.promedio = rbind(Hurst.MOR.promedio,
                            Hurst.NMOR.promedio)
Hurst.todo.promedio$Etapa = factor(Hurst.todo.promedio$Etapa,
                                   labels = c('NREM','REM'))

####################################################################33
# analisis ANOVA

#stop()

big.m.Grupo = c()
big.m.Etapa = c()
big.m.Inter = c()

big.s.Grupo = c()
big.s.Etapa = c()
big.s.Inter = c()

big.summary = c()

for(ch in 1:22){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]
  
  aov   = aov(Hurst ~ factor(Grupo) + factor(Etapa) + factor(Grupo:Etapa),
              data=tmp)
  #aov
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  #pos_hoc  = TukeyHSD(x=aov.EEG,conf.level=0.95)
  #ph_etapa = pos_hoc[['Etapa']]
  #ph_grupo = pos_hoc[['Grupo']]
  #ph_inter = pos_hoc[['Grupo:Etapa']]
  
  p = ggplot(tmp.m,aes(x=Etapa,y=Hurst,linetype=Grupo))+
    theme_classic2() +
  labs(linetype=ch.actual)+
    geom_line(aes(group=Grupo))+
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                  color='grey40') +
    geom_point()
  print(p)
  
  #q  = model.tables(aov.EEG,'means')
  #q  = model.tables(aov,'means',se=T)
  #qt = q[["tables"]]
  #qs = q[["se"]]
  
  #qt.Grupo = qt$`factor(Grupo)`
  #qt.Etapa = qt$`factor(Etapa)`
  #qt.Inter = qt$`factor(Grupo:Etapa)`
  
  #big.m.Grupo = rbind(big.m.Grupo,qt.Grupo)
  #big.m.Etapa = rbind(big.m.Etapa,qt.Etapa)
  #big.m.Inter = rbind(big.m.Inter,qt.Inter)
  
  qs  = summarySE(data=tmp,groupvars=c('Grupo','Etapa'),
                  measurevar='Hurst')
  qs2 = unlist(t(qs))
  qs2 = as.list((qs2))
  qs2 = unlist(t(qs2))
  
  big.summary = rbind(big.summary,qs2)
  
  invisible(readline(prompt="Presion [enter] para continuar"))
}

#big.Grupo = as.data.frame(big.Grupo)
#big.Etapa = as.data.frame(big.Etapa)
#big.Inter = as.data.frame(big.Inter)

#big.Grupo$Canal = kanales$Etiqueta
#big.Etapa$Canal = kanales$Etiqueta
#big.Inter$Canal = kanales$Etiqueta

##stop()

biggr = c()

ch = 21
ch.actual = kanales$Etiqueta[ch]
print(ch.actual)
tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]

aov.EEG = aov(Hurst ~ factor(Grupo) + factor(Etapa) + factor(Grupo:Etapa),
              data=tmp)
aov.EEG
k = summary(aov.EEG)
k = k[[1]]
#plot(aov.EEG)
q  = model.tables(aov.EEG)
qt = q[["tables"]]
q.Grupo = qt$`factor(Grupo)`
q.Etapa = qt$`factor(Etapa)`
q.Inter = qt$`factor(Grupo:Etapa)`

#pos_hoc  = TukeyHSD(x=aov.EEG,conf.level=0.95)
#ph_etapa = pos_hoc[['Etapa']]
#ph_grupo = pos_hoc[['Grupo']]
#ph_inter = pos_hoc[['Grupo:Etapa']]

ggplot(tmp.m,aes(x=Grupo,y=Hurst,linetype=Etapa))+
  labs(title=ch.actual)+
  theme_classic2() +
  ylab('Hurst Exponent') + xlab(NULL)+
  labs(linetype=NULL)+
  #coord_cartesian(ylim=c(1.2,1.5))+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_line(aes(group=Etapa))+
  geom_point()
ggsave(filename = 'ANOVAS_1.png',device='png',height = 4,width = 4,
       path=dir_graf)

biggr = rbind(biggr,tmp.m)

ch = 20
ch.actual = kanales$Etiqueta[ch]
print(ch.actual)
tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]

aov.EEG = aov(Hurst ~ factor(Grupo) + factor(Etapa) + factor(Grupo:Etapa),
              data=tmp)
aov.EEG
print(summary(aov.EEG))
#plot(aov.EEG)
model.tables(aov.EEG)

pos_hoc  = TukeyHSD(x=aov.EEG,conf.level=0.95)
ph_etapa = pos_hoc[['Etapa']]
ph_grupo = pos_hoc[['Grupo']]
ph_inter = pos_hoc[['Grupo:Etapa']]

ggplot(tmp.m,aes(x=Grupo,y=Hurst,linetype=Etapa))+
  labs(title=ch.actual)+
  theme_classic2() +
  ylab('Hurst Exponent') + xlab(NULL)+
  labs(linetype=NULL)+
  #coord_cartesian(ylim=c(1.2,1.5))+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_line(aes(group=Etapa))+
  geom_point()
ggsave(filename = 'ANOVAS_2.png',device='png',height = 4,width = 4,
       path=dir_graf)

biggr = rbind(biggr,tmp.m)

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
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe_cut.xlsx'),sheet='multi')
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

Hurst.MOR = Hurst.MOR[Hurst.MOR$MORn<(n.mors+1),]

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
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe_cut.xlsx'),sheet='multi_pre')
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

Hurst.NMOR = Hurst.NMOR[Hurst.NMOR$MORn>(10-n.mors),]

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
# Intercanales
Hurst.MOR$Etapa  = rep('REM',length(Hurst.MOR$Etapa))
Hurst.NMOR$Etapa = rep('NREM',length(Hurst.NMOR$Etapa))
Hurst.todo = rbind(Hurst.MOR,Hurst.NMOR)

promedios.MOR$Etapa  = rep('REM',length(promedios.MOR$Grupo))
promedios.NMOR$Etapa = rep('NREM',length(promedios.NMOR$Grupo))
promedios.todo = rbind(promedios.MOR,promedios.NMOR)

colnames(Hurst.MOR.promedio)[1]  = 'Canal'
Hurst.MOR.promedio$Canal         = factor(Hurst.MOR.promedio$Canal,
                                          labels = kanales$Etiqueta)
colnames(Hurst.NMOR.promedio)[1] = 'Canal'
Hurst.NMOR.promedio$Canal         = factor(Hurst.NMOR.promedio$Canal,
                                           labels = kanales$Etiqueta)

Hurst.todo.promedio = rbind(Hurst.MOR.promedio,
                            Hurst.NMOR.promedio)
Hurst.todo.promedio$Etapa = factor(Hurst.todo.promedio$Etapa,
                                   labels = c('NREM','REM'))

####################################################################33
# analisis ANOVA

if(FALSE){
emg = is.element(Hurst.todo.promedio$Canal_var,c('EMG'))
eog = is.element(Hurst.todo.promedio$Canal_var,c('LOG','ROG'))
eeg = !is.element(Hurst.todo.promedio$Canal_var,c('EMG','LOG','ROG'))
Hurst.m.EEG = Hurst.todo.promedio[eeg,]
Hurst.m.EOG = Hurst.todo.promedio[eog,]
Hurst.m.EMG = Hurst.todo.promedio[emg,]

ch = 1
for(ch in 1:22){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]
  
  aov.EEG = aov(Hurst ~ factor(Grupo) + factor(Etapa) + factor(Grupo:Etapa),
                data=tmp)
  aov.EEG
  print(summary(aov.EEG))
  #plot(aov.EEG)
  model.tables(aov.EEG)
  
  pos_hoc  = TukeyHSD(x=aov.EEG,conf.level=0.95)
  ph_etapa = pos_hoc[['Etapa']]
  ph_grupo = pos_hoc[['Grupo']]
  ph_inter = pos_hoc[['Grupo:Etapa']]
  
  p = ggplot(tmp.m,aes(x=Grupo,y=Hurst,linetype=Etapa))+
    theme_classic2() +
    labs(linetype=ch.actual)+
    geom_line(aes(group=Etapa))+
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                  color='grey40') +
    geom_point()
  
  print(p)
  
  invisible(readline(prompt="Presion [enter] para continuar"))
}
}

big.m.Grupo = c()
big.m.Etapa = c()
big.m.Inter = c()

big.s.Grupo = c()
big.s.Etapa = c()
big.s.Inter = c()

big.summary = c()

for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]
  
  aov   = aov(Hurst ~ factor(Grupo) + factor(Etapa) + factor(Grupo:Etapa),
              data=tmp)
  #aov
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  #pos_hoc  = TukeyHSD(x=aov.EEG,conf.level=0.95)
  #ph_etapa = pos_hoc[['Etapa']]
  #ph_grupo = pos_hoc[['Grupo']]
  #ph_inter = pos_hoc[['Grupo:Etapa']]
  
  p = ggplot(tmp.m,aes(x=Grupo,y=Hurst,linetype=Etapa))+
    theme_classic2() +
    labs(linetype=ch.actual)+
    geom_line(aes(group=Etapa))+
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                  color='grey40') +
    
    geom_point()
  print(p)
  
  #q  = model.tables(aov.EEG,'means')
  #q  = model.tables(aov,'means',se=T)
  #qt = q[["tables"]]
  #qs = q[["se"]]
  
  #qt.Grupo = qt$`factor(Grupo)`
  #qt.Etapa = qt$`factor(Etapa)`
  #qt.Inter = qt$`factor(Grupo:Etapa)`
  
  #big.m.Grupo = rbind(big.m.Grupo,qt.Grupo)
  #big.m.Etapa = rbind(big.m.Etapa,qt.Etapa)
  #big.m.Inter = rbind(big.m.Inter,qt.Inter)
  
  qs  = summarySE(data=tmp,groupvars=c('Grupo','Etapa'),
                  measurevar='Hurst')
  qs2 = unlist(t(qs))
  qs2 = as.list((qs2))
  qs2 = unlist(t(qs2))
  
  big.summary = rbind(big.summary,qs2)
  
  invisible(readline(prompt="Presion [enter] para continuar"))
}

ch = 9
ch.actual = kanales$Etiqueta[ch]
print(ch.actual)
tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]

aov.EEG = aov(Hurst ~ factor(Grupo) + factor(Etapa) + factor(Grupo:Etapa),
              data=tmp)
aov.EEG
print(summary(aov.EEG))
#plot(aov.EEG)
model.tables(aov.EEG)

pos_hoc  = TukeyHSD(x=aov.EEG,conf.level=0.95)
ph_etapa = pos_hoc[['Etapa']]
ph_grupo = pos_hoc[['Grupo']]
ph_inter = pos_hoc[['Grupo:Etapa']]

ggplot(tmp.m,aes(x=Grupo,y=Hurst,linetype=Etapa))+
  labs(title=ch.actual)+
  theme_classic2() +
  ylab('Hurst Exponent') + xlab(NULL)+
  labs(linetype=NULL)+
  #coord_cartesian(ylim=c(1.2,1.5))+
  theme(legend.position=c(1,1),legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  geom_line(aes(group=Etapa))+
  geom_point()
ggsave(filename = 'ANOVAS_3.png',device='png',height = 4,width = 4,
       path=dir_graf)

biggr = rbind(biggr,tmp.m)

stop()

ggplot(biggr,aes(x=Grupo,y=Hurst,linetype=Etapa))+
  #labs(title=ch.actual)+
  #labs(title=' ')+
  theme_classic2() +
  ylab('Hurst Exponent') + xlab(NULL)+
  labs(linetype=NULL)+
  coord_cartesian(ylim=c(1.2,1.6))+
  #theme(legend.position=c(1,1),legend.direction = 'horizontal',
  #      legend.justification=c(1,0))+
  theme(legend.position = 'top')+
  facet_grid(.~Canal_var) +
  theme(strip.background = element_blank())+
  geom_line(aes(group=Etapa))+
  geom_point()
ggsave(filename = 'ANOVAS_ojos.png',device='png',height = 4,width = 6,
       path=dir_graf,dpi=400)

#ggplot(biggr,aes(x=Etapa,y=Hurst,linetype=Grupo))+
#  #labs(title=ch.actual)+
#  #labs(title=' ')+
#  theme_classic2() +
#  ylab('Hurst Exponent') + xlab(NULL)+
#  labs(linetype=NULL)+
#  coord_cartesian(ylim=c(1.2,1.6))+
#  #theme(legend.position=c(1,1),legend.direction = 'horizontal',
#  #      legend.justification=c(1,0))+
#  theme(legend.position = 'top')+
#  facet_grid(.~Canal_var) +
#  theme(strip.background = element_blank())+
#  geom_line(aes(group=Grupo))+
#  geom_point()
#ggsave(filename = 'ANOVAS_ojos_2.png',device='png',height = 4,width = 6,
#       path=dir_graf,dpi=400)

ggplot(biggr,aes(x=Etapa,y=Hurst))+
  theme_classic2() +
  ylab('Hurst Exponent') + xlab(NULL)+
  labs(linetype=NULL)+
  coord_cartesian(ylim=c(1.2,1.6))+
  theme(legend.position = 'top')+
  facet_grid(.~Canal_var) +
  theme(strip.background = element_blank())+
  geom_line(aes(group=Grupo,linetype=Grupo))+
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                color='grey40') +
  geom_point()
ggsave(filename = 'Fig03_ANOVAS.png',device='png',height = 6,width = 8,
       unit='cm',
       path=dir_graf,dpi=400,scale=2)

#stop()

big.Grupo = c()
big.Etapa = c()
big.Inter = c()

ch = 1
for(ch in 1:length(kanales$Etiqueta)){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]
  
  aov.EEG = aov(Hurst ~ factor(Grupo) + factor(Etapa) + factor(Grupo:Etapa),
                data=tmp)
  aov.EEG
  #print(summary(aov.EEG))
  #plot(aov.EEG)
  model.tables(aov.EEG)
  
  k = summary(aov.EEG)
  k = k[[1]]
  
  print(k)
  
  pos_hoc  = TukeyHSD(x=aov.EEG,conf.level=0.95)
  ph_etapa = pos_hoc[['Etapa']]
  ph_grupo = pos_hoc[['Grupo']]
  ph_inter = pos_hoc[['Grupo:Etapa']]
  
  p = ggplot(tmp.m,aes(x=Etapa,y=Hurst,linetype=Grupo))+
    theme_classic2() +
    labs(linetype=ch.actual)+
    geom_line(aes(group=Grupo))+
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                  color='grey40') +
    geom_point()
  
  print(p)
  
  q  = model.tables(aov.EEG,'means')
  qt = q[["tables"]]
  q.Grupo = qt$`factor(Grupo)`
  q.Etapa = qt$`factor(Etapa)`
  q.Inter = qt$`factor(Grupo:Etapa)`
  
  big.Grupo = rbind(big.Grupo,q.Grupo)
  big.Etapa = rbind(big.Etapa,q.Etapa)
  big.Inter = rbind(big.Inter,q.Inter)
  
  invisible(readline(prompt="Presion [enter] para continuar"))
}

big.Grupo = as.data.frame(big.Grupo)
big.Etapa = as.data.frame(big.Etapa)
big.Inter = as.data.frame(big.Inter)

big.Grupo$Canal = kanales$Etiqueta
big.Etapa$Canal = kanales$Etiqueta
big.Inter$Canal = kanales$Etiqueta

