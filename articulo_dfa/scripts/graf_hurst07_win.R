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

emg = is.element(Hurst.todo.promedio$Canal_var,c('EMG'))
eog = is.element(Hurst.todo.promedio$Canal_var,c('LOG','ROG'))
eeg = !is.element(Hurst.todo.promedio$Canal_var,c('EMG','LOG','ROG'))
Hurst.m.EEG = Hurst.todo.promedio[eeg,]
Hurst.m.EOG = Hurst.todo.promedio[eog,]
Hurst.m.EMG = Hurst.todo.promedio[emg,]

ch = 1
{
ch.actual = kanales$Etiqueta[ch]
print(ch.actual)
tmp   = Hurst.m.EEG[grep(ch.actual,Hurst.m.EEG$Canal),]
tmp.m = promedios.todo.EEG[grep(ch.actual,promedios.todo.EEG$Canal),]

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
  theme_classic2() +
  labs(linetype=ch.actual)+
  geom_line(aes(group=Etapa))+
  geom_point()

invisible(readline(prompt="Presion [enter] para continuar"))
} 

stop()
interaction.plot(Grupo,Etapa,Hurst,fun='mean',data=tmp)

aov.EEG = aov(Hurst ~ factor(Grupo) + factor(Etapa) + factor(Grupo:Etapa),
                     data=Hurst.m.EEG)
aov.EEG
summary(aov.EEG)
plot(aov.EEG)

pos_hoc  = TukeyHSD(x=aov.EEG.etapas,conf.level=0.95)
ph_etapa = pos_hoc[['Etapa']]
ph_grupo = pos_hoc[['Grupo']]
ph_inter = pos_hoc[['Grupo:Etapa']]

stop()

emg = is.element(Hurst.todo$Canal_var,c('EMG'))
eog = is.element(Hurst.todo$Canal_var,c('LOG','ROG'))
eeg = !is.element(Hurst.todo$Canal_var,c('EMG','LOG','ROG'))
Hurst.todo.EEG = Hurst.todo[eeg,]
Hurst.todo.EOG = Hurst.todo[eog,]
Hurst.todo.EMG = Hurst.todo[emg,]

emg = is.element(Hurst.todo$Canal_var,c('EMG'))
eog = is.element(Hurst.todo$Canal_var,c('LOG','ROG'))
eeg = !is.element(Hurst.todo$Canal_var,c('EMG','LOG','ROG'))
Hurst.todo.EEG = Hurst.todo[eeg,]
Hurst.todo.EOG = Hurst.todo[eog,]
Hurst.todo.EMG = Hurst.todo[emg,]

emg = is.element(promedios.todo$Canal_var,c('EMG'))
eog = is.element(promedios.todo$Canal_var,c('LOG','ROG'))
eeg = !is.element(promedios.todo$Canal_var,c('EMG','LOG','ROG'))
promedios.todo.EEG = promedios.todo[eeg,]
promedios.todo.EOG = promedios.todo[eog,]
promedios.todo.EMG = promedios.todo[emg,]

#aov.EEG.etapas = aov(Hurst ~ Grupo + Etapa + Etapa:Grupo,
#                 data=Hurst.todo.EEG)
aov.EEG.etapas = aov(Hurst ~ Grupo + Etapa + Etapa:Grupo,
                     data=promedios.todo.EEG)
aov.EEG.etapas
summary(aov.EEG.etapas)
plot(aov.EEG.etapas)

pos_hoc  = TukeyHSD(x=aov.EEG.etapas,conf.level=0.95)
ph_etapa = pos_hoc[['Etapa']]
ph_grupo = pos_hoc[['Grupo']]
ph_inter = pos_hoc[['Grupo:Etapa']]

stop('Hasta aqui bien, luego ya no')

####################################################################33
# analisis ANOVA

aov = aov( Hurst ~ Canal_var + MORn + Canal_var:MORn,data=Hurst.MOR)
aov
summary(aov)
plot(aov)

emg = is.element(Hurst.MOR$Canal_var,c('EMG'))
eog = is.element(Hurst.MOR$Canal_var,c('LOG','ROG'))
eeg = !is.element(Hurst.MOR$Canal_var,c('EMG','LOG','ROG'))
Hurst.MOR.EEG = Hurst.MOR[eeg,]
Hurst.MOR.EOG = Hurst.MOR[eog,]
Hurst.MOR.EMG = Hurst.MOR[emg,]

emg = is.element(promedios.MOR$Canal_var,c('EMG'))
eog = is.element(promedios.MOR$Canal_var,c('LOG','ROG'))
eeg = !is.element(promedios.MOR$Canal_var,c('EMG','LOG','ROG'))
promedios.MOR.EEG = promedios.MOR[eeg,]
promedios.MOR.EOG = promedios.MOR[eog,]
promedios.MOR.EMG = promedios.MOR[emg,]

aov.EEG = aov( Hurst ~ Canal_var + Grupo,
               data=promedios.MOR.EEG)
aov.EEG
summary(aov.EEG)
#plot(aov.EEG)

aov.EEG = aov( Hurst ~ Canal_var + Grupo,
               data=Hurst.MOR.EEG)
aov.EEG
summary(aov.EEG)
#plot(aov.EEG)

aov.EEG = aov( Hurst ~ Canal_var + Grupo + Canal_var:Grupo,
               data=Hurst.MOR.EEG)
aov.EEG
summary(aov.EEG)
#plot(aov.EEG)

phh = TukeyHSD(x=aov.EEG,conf.level=0.95)

cump = phh[["Canal_var"]]
chidos = cump[,4]<.05
reportar = cump[chidos,]

cump = phh[["Canal_var:Grupo"]]
chidos = cump[,4]<.05
reportar = cump[chidos,]

stop()

aov.EEG2 = aov( Hurst ~ MORn + Grupo + MORn*Grupo,
               data=Hurst.MOR.EEG)
aov.EEG
summary(aov.EEG)
#plot(aov.EEG)

aov.EOG = aov( Hurst ~ Canal_var + Grupo + Canal_var:Grupo,
               data=Hurst.MOR.EOG)
aov.EOG
summary(aov.EOG)
#plot(aov.EEG)

aov.EMG = aov( Hurst ~ Canal_var + Grupo + Canal_var:Grupo,
               data=Hurst.MOR.EMG)
aov.EMG
summary(aov.EMG)
#plot(aov.EEG)

stop()

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
# Intercanales
Hurst.MOR$Etapa  = rep('REM',length(Hurst.MOR$Etapa))
Hurst.NMOR$Etapa = rep('NREM',length(Hurst.NMOR$Etapa))
Hurst.todo = rbind(Hurst.MOR,Hurst.NMOR)

promedios.MOR$Etapa  = rep('REM',length(promedios.MOR$Grupo))
promedios.NMOR$Etapa = rep('NREM',length(promedios.NMOR$Grupo))
promedios.todo = rbind(promedios.MOR,promedios.NMOR)