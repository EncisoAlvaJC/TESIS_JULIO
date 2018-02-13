###############################################################################
# parametros
orden_stam     = TRUE
usar.log       = F

###############################################################################
# directorios de trabajo

dir_gral    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_info    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_scripts = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts'
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/Hurst'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/graf_def'

###############################################################################
# librerias
require('readxl')

require('ggplot2')
require('ggpubr')

require('Rmisc')

require('reshape')

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,'/info_tecnico2.xlsx'))

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'),
                      sheet='Alfabetico')
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'),
                        sheet='Stam')
}
n.canales    = length(kanales$Etiqueta)

###############################################################################
# cargar los datos
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe.xlsx'),
                 sheet='uno')
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
Hurst.MOR$xx        = kanales$x[Hurst.MOR$Canal_var]
Hurst.MOR$yy        = kanales$y[Hurst.MOR$Canal_var]

if(usar.log){
  Hurst.MOR$Hurst     = log(Hurst.MOR$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}
Hurst.MOR.promedio = c()
for(suj in 1:10){
  tmp       = Hurst.MOR[grep(info$Nombre[suj],Hurst.MOR$Sujeto_n),]
  promedios = aggregate(tmp,by=list(tmp$Canal_var),mean)
  Hurst.MOR.promedio = rbind(Hurst.MOR.promedio,promedios)
}

# problemas con etiquetas
Hurst.MOR$Canal_var          = factor(Hurst.MOR$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.MOR$Grupo              = factor(Hurst.MOR$Grupo,
                                      labels=c('CTRL','PMCI'))

Hurst.MOR.promedio           = as.data.frame(Hurst.MOR.promedio)
Hurst.MOR.promedio$Canal_var = factor(Hurst.MOR.promedio$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.MOR.promedio$Grupo     = factor(Hurst.MOR.promedio$Grupo,
                                      labels=c('CTRL','PMCI'))
Hurst.MOR.promedio$Sujeto_n = info$Nombre[Hurst.MOR.promedio$Sujeto]
promedios.MOR               = summarySE(Hurst.MOR,na.rm=T,
                                        measurevar='Hurst',
                                        groupvars=c('Grupo','Canal_var'))

###############################################################################
# lo mismo para NMOR
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe.xlsx'),
                 sheet='uno_pre')
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
Hurst.NMOR$xx        = kanales$x[Hurst.NMOR$Canal_var]
Hurst.NMOR$yy        = kanales$y[Hurst.NMOR$Canal_var]

if(usar.log){
  Hurst.NMOR$Hurst     = log(Hurst.NMOR$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}
Hurst.NMOR.promedio = c()
for(suj in 1:10){
  tmp       = Hurst.NMOR[grep(info$Nombre[suj],Hurst.NMOR$Sujeto_n),]
  promedios = aggregate(tmp,by=list(tmp$Canal_var),mean)
  Hurst.NMOR.promedio = rbind(Hurst.NMOR.promedio,promedios)
}

# problemas con etiquetas
Hurst.NMOR$Canal_var          = factor(Hurst.NMOR$Canal_var,
                                       labels=kanales$Etiqueta)
Hurst.NMOR$Grupo              = factor(Hurst.NMOR$Grupo,
                                       labels=c('CTRL','PMCI'))
Hurst.NMOR.promedio           = as.data.frame(Hurst.NMOR.promedio)
Hurst.NMOR.promedio$Canal_var = factor(Hurst.NMOR.promedio$Canal_var,
                                       labels=kanales$Etiqueta)
Hurst.NMOR.promedio$Grupo     = factor(Hurst.NMOR.promedio$Grupo,
                                       labels=c('CTRL','PMCI'))
Hurst.NMOR.promedio$Sujeto_n = info$Nombre[Hurst.NMOR.promedio$Sujeto]
promedios.NMOR               = summarySE(Hurst.NMOR,na.rm=T,
                                         measurevar='Hurst',
                                         groupvars=c('Grupo','Canal_var'))

# combinacion
Hurst.MOR$Etapa  = rep('REM',length(Hurst.MOR$Etapa))
Hurst.NMOR$Etapa = rep('NREM',length(Hurst.NMOR$Etapa))
Hurst.todo       = rbind(Hurst.MOR,Hurst.NMOR)

Hurst.MOR.promedio$Etapa  = rep('REM',length(Hurst.MOR.promedio$Etapa))
Hurst.NMOR.promedio$Etapa = rep('NREM',length(Hurst.NMOR.promedio$Etapa))
Hurst.todo.promedio       = rbind(Hurst.MOR.promedio,Hurst.NMOR.promedio)

promedios.MOR$Etapa  = rep('REM',length(promedios.MOR$Grupo))
promedios.NMOR$Etapa = rep('NREM',length(promedios.NMOR$Grupo))
promedios.todo       = rbind(promedios.MOR,promedios.NMOR)

#rm(Hurst.MOR,Hurst.NMOR,Hurst.todo,promedios,raw,tmp)

###############################################################################
###############################################################################

#stop('Los datos estan cargados')



if(FALSE){
  require('lemon')
  
  promedios.todo$xx = kanales$x[promedios.todo$Canal_var]
  promedios.todo$yy = kanales$y[promedios.todo$Canal_var]
  
  ggplot(promedios.todo,aes(x=Etapa,y=Hurst,linetype=Grupo))+
    theme_classic2() +
    ylab('Hurst exponent')+ xlab('Sleep stage')+
    geom_line(aes(group=Grupo))+
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=0.05,
                  color='grey40') +
    scale_x_discrete(expand = c(.25,0)) +
    #theme(legend.position='left',legend.direction = 'horizontal')+
    theme(legend.position='left')+
    labs(linetype='Group') +
    facet_rep_grid((-yy)~xx)+
    geom_text(data=promedios.todo,aes(x=-Inf,y=-Inf,label=Canal_var),
              hjust=-.1,vjust=-1,colour='gray')+
    theme(strip.text.y = element_blank()) +
    theme(strip.text.x = element_blank()) +
    geom_point(size=.1)
  ggsave(filename='/cabeza_ANOVA_Hurst.png',path=dir_graf,
         device='png',units='cm',width=8,height=7,dpi=400,scale=1.9)
  ggsave(filename='/cabeza_ANOVA_Hurst.eps',path=dir_graf,
         device='eps',units='cm',width=8,height=7,dpi=400,scale=1.9)
}

big.summary = c()

for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  #tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp   = Hurst.todo[grep(ch.actual,Hurst.todo$Canal),]
  tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]
  
  aov   = aov(Hurst ~ (Grupo) + (Etapa) +(Grupo:Etapa),
              data=tmp)
  
  k = summary(aov)
  k = k[[1]]
  
  #print(k)
  
  p = ggplot(tmp.m,aes(x=Etapa,y=Hurst,linetype=Grupo))+
    theme_classic2() +
    labs(linetype=ch.actual)+
    coord_cartesian(ylim=c(0.3,1.75))+
    geom_line(aes(group=Grupo))+
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                  color='grey40') +
    geom_point()
  print(p)
  
  #ggsave(path=dir_graf,device='png',units='cm',
  #       width=8,height=4,dpi=400,scale=2,
  #       file=paste0(ch.actual,'.png'))
  
  qs  = summarySE(data=tmp,groupvars=c('Grupo','Etapa'),
                  measurevar='Hurst')
  qs2 = unlist(t(qs))
  qs2 = as.list((qs2))
  qs2 = unlist(t(qs2))
  big.summary = rbind(big.summary,qs2)
  
  #View(k)
  #invisible(readline(prompt="Presion [enter] para continuar"))
}

stop()

min(promedios.todo$Hurst- promedios.todo$sd)
max(promedios.todo$Hurst+ promedios.todo$sd)

View(big.summary)
invisible(readline(prompt="Presion [enter] para continuar"))

biggr = c()

for(ch in 20:21){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]
  
  aov   = aov(Hurst ~ (Grupo) + (Etapa) +(Grupo:Etapa),
              data=tmp)
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  p = ggplot(tmp.m,aes(x=Etapa,y=Hurst,linetype=Grupo))+
    theme_classic2() +
    labs(linetype=ch.actual)+
    geom_line(aes(group=Grupo))+
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                  color='grey40') +
    geom_point()
  print(p)
  
  qs  = summarySE(data=tmp,groupvars=c('Grupo','Etapa'),
                  measurevar='Hurst')
  qs2 = unlist(t(qs))
  qs2 = as.list((qs2))
  qs2 = unlist(t(qs2))
  big.summary = rbind(big.summary,qs2)
  
  #View(k)
  #invisible(readline(prompt="Presion [enter] para continuar"))
  biggr = rbind(biggr,tmp.m)
}

# nuevo: anova de medidas repetidas
big.summary = c()

promedios2.MOR = summarySE(Hurst.MOR,na.rm=T,measurevar='Hurst',
                           groupvars=c('Grupo','MORn','Canal_var'))
min(promedios2.MOR$Hurst- promedios2.MOR$sd)
max(promedios2.MOR$Hurst+ promedios2.MOR$sd)

for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  #tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp   = Hurst.MOR[grep(ch.actual,Hurst.MOR$Canal),]
  #tmp.m = promedios.MOR[grep(ch.actual,promedios.MOR$Canal),]
  tmp.m = summarySE(tmp,na.rm=T,measurevar='Hurst',
                    groupvars=c('Grupo','MORn'))
  
  aov   = aov(Hurst ~ (Grupo) + (MORn) +(Grupo:MORn),
              data=tmp)
  
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  p = ggplot(tmp.m,aes(x=MORn,y=Hurst,linetype=Grupo))+
    theme_classic2() +
    labs(linetype=ch.actual)+
    coord_cartesian(ylim=c(0.3,2.25))+
    geom_line(aes(group=Grupo))+
    geom_errorbar(aes(ymin=Hurst-sd,ymax=Hurst+sd),width=.1,
                  color='grey40') +
    geom_point()
  print(p)
  
  ggsave(path=dir_graf,device='png',units='cm',
         width=8,height=4,dpi=400,scale=2,
         file=paste0('anova_etapasMOR_',ch.actual,'.png'))
  
  qs  = summarySE(data=tmp,groupvars=c('Grupo','MORn'),
                  measurevar='Hurst')
  qs2 = unlist(t(qs))
  qs2 = as.list((qs2))
  qs2 = unlist(t(qs2))
  big.summary = rbind(big.summary,qs2)
  
  View(k)
  invisible(readline(prompt="Presion [enter] para continuar"))
}

###############################################################################
###############################################################################

#    INTERCANALES

###############################################################################
###############################################################################
kanales  = read_excel(paste0(dir_info,'/info_intercanales.xlsx'),
                      sheet='Alfabetico')
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_intercanales.xlsx'),
                        sheet='Stam')
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

###############################################################################
# cargar los datos
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe.xlsx'),
                 sheet='multi')
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

if(usar.log){
  Hurst.MOR$Hurst     = log(Hurst.MOR$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}
Hurst.MOR.promedio = c()
for(suj in 1:10){
  tmp       = Hurst.MOR[grep(info$Nombre[suj],Hurst.MOR$Sujeto_n),]
  promedios = aggregate(tmp,by=list(tmp$Canal_var),mean)
  Hurst.MOR.promedio = rbind(Hurst.MOR.promedio,promedios)
}

# problemas con etiquetas
Hurst.MOR$Canal_var          = factor(Hurst.MOR$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.MOR$Grupo              = factor(Hurst.MOR$Grupo,
                                      labels=c('CTRL','PMCI'))

Hurst.MOR.promedio           = as.data.frame(Hurst.MOR.promedio)
Hurst.MOR.promedio$Canal_var = factor(Hurst.MOR.promedio$Canal_var,
                                      labels=kanales$Etiqueta)
Hurst.MOR.promedio$Grupo     = factor(Hurst.MOR.promedio$Grupo,
                                      labels=c('CTRL','PMCI'))
Hurst.MOR.promedio$Sujeto_n = info$Nombre[Hurst.MOR.promedio$Sujeto]
promedios.MOR               = summarySE(Hurst.MOR.promedio,na.rm=T,
                                        measurevar='Hurst',
                                        groupvars=c('Grupo','Canal_var'))

###############################################################################
# lo mismo para NMOR
raw = read_excel(paste0(dir_res_pre,'/dfa_asdataframe.xlsx'),
                 sheet='multi_pre')
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

if(usar.log){
  Hurst.NMOR$Hurst     = log(Hurst.NMOR$Hurst)
  Hurst.promedio$Hurst = log(Hurst.promedio$Hurst)
}
Hurst.NMOR.promedio = c()
for(suj in 1:10){
  tmp       = Hurst.NMOR[grep(info$Nombre[suj],Hurst.NMOR$Sujeto_n),]
  promedios = aggregate(tmp,by=list(tmp$Canal_var),mean)
  Hurst.NMOR.promedio = rbind(Hurst.NMOR.promedio,promedios)
}

# problemas con etiquetas
Hurst.NMOR$Canal_var          = factor(Hurst.NMOR$Canal_var,
                                       labels=kanales$Etiqueta)
Hurst.NMOR$Grupo              = factor(Hurst.NMOR$Grupo,
                                       labels=c('CTRL','PMCI'))
Hurst.NMOR.promedio           = as.data.frame(Hurst.NMOR.promedio)
Hurst.NMOR.promedio$Canal_var = factor(Hurst.NMOR.promedio$Canal_var,
                                       labels=kanales$Etiqueta)
Hurst.NMOR.promedio$Grupo     = factor(Hurst.NMOR.promedio$Grupo,
                                       labels=c('CTRL','PMCI'))
Hurst.NMOR.promedio$Sujeto_n = info$Nombre[Hurst.NMOR.promedio$Sujeto]
promedios.NMOR               = summarySE(Hurst.NMOR.promedio,na.rm=T,
                                         measurevar='Hurst',
                                         groupvars=c('Grupo','Canal_var'))

# combinacion
Hurst.MOR$Etapa  = rep('REM',length(Hurst.MOR$Etapa))
Hurst.NMOR$Etapa = rep('NREM',length(Hurst.NMOR$Etapa))
Hurst.todo       = rbind(Hurst.MOR,Hurst.NMOR)

Hurst.MOR.promedio$Etapa  = rep('REM',length(Hurst.MOR.promedio$Etapa))
Hurst.NMOR.promedio$Etapa = rep('NREM',length(Hurst.NMOR.promedio$Etapa))
Hurst.todo.promedio       = rbind(Hurst.MOR.promedio,Hurst.NMOR.promedio)

promedios.MOR$Etapa  = rep('REM',length(promedios.MOR$Grupo))
promedios.NMOR$Etapa = rep('NREM',length(promedios.NMOR$Grupo))
promedios.todo       = rbind(promedios.MOR,promedios.NMOR)

#rm(Hurst.MOR,Hurst.NMOR,Hurst.todo,promedios,raw,tmp)

###############################################################################

big.summary = c()

for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  #tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp   = Hurst.todo[grep(ch.actual,Hurst.todo$Canal),]
  tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]
  
  aov   = aov(Hurst ~ (Grupo) + (Etapa) +(Grupo:Etapa),
              data=tmp)
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  p = ggplot(tmp.m,aes(x=Etapa,y=Hurst,linetype=Grupo))+
    theme_classic2() +
    labs(linetype=ch.actual)+
    geom_line(aes(group=Grupo))+
    coord_cartesian(ylim=c(0.9,1.65))+
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                  color='grey40') +
    geom_point()
  print(p)
  
  ggsave(path=dir_graf,device='png',units='cm',
         width=8,height=4,dpi=400,scale=2,
         file=paste0(ch.actual,'.png'))
  
  qs  = summarySE(data=tmp,groupvars=c('Grupo','Etapa'),
                  measurevar='Hurst')
  qs2 = unlist(t(qs))
  qs2 = as.list((qs2))
  qs2 = unlist(t(qs2))
  big.summary = rbind(big.summary,qs2)
  
  #View(k)
  #invisible(readline(prompt="Presion [enter] para continuar"))
}

min(promedios.todo$Hurst- promedios.todo$sd)
max(promedios.todo$Hurst+ promedios.todo$sd)

View(big.summary)
invisible(readline(prompt="Presion [enter] para continuar"))

for(ch in 9){
  ch.actual = kanales$Etiqueta[ch]
  print(ch.actual)
  tmp   = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal),]
  tmp.m = promedios.todo[grep(ch.actual,promedios.todo$Canal),]
  
  aov   = aov(Hurst ~ (Grupo) + (Etapa) +(Grupo:Etapa),
              data=tmp)
  k = summary(aov)
  k = k[[1]]
  
  print(k)
  
  p = ggplot(tmp.m,aes(x=Etapa,y=Hurst,linetype=Grupo))+
    theme_classic2() +
    labs(linetype=ch.actual)+
    geom_line(aes(group=Grupo))+
    geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                  color='grey40') +
    geom_point()
  print(p)
  
  qs  = summarySE(data=tmp,groupvars=c('Grupo','Etapa'),
                  measurevar='Hurst')
  qs2 = unlist(t(qs))
  qs2 = as.list((qs2))
  qs2 = unlist(t(qs2))
  big.summary = rbind(big.summary,qs2)
  
  #View(k)
  #invisible(readline(prompt="Presion [enter] para continuar"))
  biggr = rbind(biggr,tmp.m)
}


p = ggplot(biggr,aes(x=Etapa,y=Hurst,linetype=Grupo))+
  theme_classic2() +
  labs(linetype=NULL) +
  xlab(NULL)+
  geom_line(aes(group=Grupo))+
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),width=.1,
                color='grey40') +
  facet_grid(.~Canal_var) +
  theme(strip.background = element_blank())+
  theme(legend.position='top',legend.direction = 'horizontal')+
  geom_point()
print(p)
ggsave(filename='/comparacion_hurst_uno_REM_NREM_signif.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)
