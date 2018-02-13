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

stop()

Hurst_un = dcast(Hurst.todo,MORn+Sujeto_n~Canal_var,value.var ='Hurst',
                 fun.aggregate = mean)

Hurst_un = dcast(Hurst.todo,Sujeto_n+Canal_var~MORn,value.var ='Hurst',
                 fun.aggregate = mean)

###############################################################################
# Neuropsi vs MMSE
cor.test(Hurst.MOR.promedio$Neuropsi,Hurst.MOR.promedio$MMSE,
         method='spearman')

cor.test(Hurst.MOR.promedio$Neuropsi,Hurst.MOR.promedio$MMSE,
         method='pearson')

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

cor.test(Hurst.MOR.promedio$Edad,Hurst.MOR.promedio$Neuropsi,
         method='pearson')

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
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
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

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
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

# ambos
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

ggplot(Hurst.todo.promedio,aes(x=Neuropsi,y=Hurst,
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

###############################################################################
# Edad vs Hurst
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','rho_Spearman','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'rho_Spearman'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}




correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR[grep(ch.actual,Hurst.MOR$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','rho_Spearman','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR[grep(ch.actual,Hurst.MOR$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'rho_Spearman'] = k$estimate
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

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','rho_Spearman','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'rho_Spearman'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}




correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR[grep(ch.actual,Hurst.NMOR$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
}

correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','rho_Spearman','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR[grep(ch.actual,Hurst.NMOR$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='spearman',na.action(na.omit))
  
  correlaciones[ch,'rho_Spearman'] = k$estimate
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

# Significativos
cuales = c(18,19)
signif = c()
for(ch in cuales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  signif = rbind(signif,Hurst.tmp)
}

A = ggplot(signif,aes(x=Edad,y=Hurst,
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
  theme(strip.background = element_blank())+
  theme(legend.position='top')+
  coord_cartesian(xlim=c(59,79))+
  facet_grid(Etapa~Canal_var) +
  geom_point()

cuales = c(20,21)
signif = c()
for(ch in cuales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  signif = rbind(signif,Hurst.tmp)
}

B = ggplot(signif,aes(x=Edad,y=Hurst,
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
  theme(strip.background = element_blank())+
  theme(legend.position='top')+
  coord_cartesian(xlim=c(59,79))+
  facet_grid(Etapa~Canal_var) +
  geom_point()
ggarrange(A,B,ncol=1,nrow=2,labels='AUTO',common.legend=TRUE)

ggsave(filename='/Fig02_edad_hurst.png',path=dir_graf,
       device='png',units='cm',width=7,height=7,dpi=400,scale=2)

ggarrange(A,labels=NULL)
ggsave(filename='/Fig_age_hurst.png',path=dir_graf,
       device='png',units='cm',width=4,height=4,dpi=400,scale=2)

###############################################################################
# Hurst x Canal
# MOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  #Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
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

stop()

A = ggplot(promedios.MOR,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.MOR.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggarrange(A,labels='AUTO')
ggsave(filename='/comparacion_uno_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

# NMOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  #Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
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

A = ggplot(promedios.NMOR,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.NMOR.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggarrange(A,labels='B')
ggsave(filename='/comparacion_uno_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

# ambos
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
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

###############################################################################
# Intercanales
A = ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.todo.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels='A')
ggsave(filename='/comparacion_hurst_uno_CTRL_PMCI.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# grafico nuevo, REM vs NREM
# usando los promedios
comparaciones.CTL     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.CTL[,1] = kanales$Etiqueta
colnames(comparaciones.CTL) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  #Hurst.tmp = Hurst.todo.promedio[grep(ch.actual,
  #                                     Hurst.todo.promedio$Canal_var),]
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
  comparaciones.CTL[ch,'m'] = k$estimate[1]
}

comparaciones.PDC     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.PDC[,1] = kanales$Etiqueta
colnames(comparaciones.PDC) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  #Hurst.tmp = Hurst.todo.promedio[grep(ch.actual,
  #                                     Hurst.todo.promedio$Canal_var),]
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
  comparaciones.PDC[ch,'m'] = k$estimate[1]
}

A = ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Etapa)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=Hurst.todo.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6,paired=T)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels='B')
ggsave(filename='/comparacion_hurst_uno_REM_NREM.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)
stop()
# Significativos
cuales   = c(4,5,7,9,11,13:17)
signif   = c()
signif.m = c()
for(ch in cuales){
  ch.actual    = kanales$Etiqueta[ch]
  Hurst.tmp    = Hurst.todo.promedio[grep(ch.actual,
                                          Hurst.todo.promedio$Canal_var),]
  promedio.tmp = promedios.todo[grep(ch.actual,
                                     promedios.todo$Canal_var),]
  
  signif   = rbind(signif,Hurst.tmp)
  signif.m = rbind(signif.m,promedio.tmp)
}

A = ggplot(signif.m,aes(x=Canal_var,y=Hurst,fill=Etapa)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=signif,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6,paired=T)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels='C')
ggsave(filename='/comparacion_hurst_uno_REM_NREM_signif.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

stop()

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
stop()

###############################################################################
# Neuropsi vs Hurst
# MOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
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

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
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

# ambos
correlaciones     = matrix(0,nrow=n.canales,ncol=4)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Spear','p','S')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo.promedio[grep(ch.actual,Hurst.todo.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Neuropsi)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Spear'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'S']       = k$statistic
}

ggplot(Hurst.todo.promedio,aes(x=Neuropsi,y=Hurst,
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

###############################################################################
# Edad vs Hurst
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
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

# NMOR
correlaciones     = matrix(0,nrow=n.canales,ncol=5)
correlaciones[,1] = kanales$Etiqueta
colnames(correlaciones) = c('Canal','R_Pearson','p','t','dF')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
  a = unlist(Hurst.tmp$Edad)
  b = unlist(Hurst.tmp$Hurst)
  k = cor.test(a,b,method='pearson',na.action(na.omit))
  
  correlaciones[ch,'R_Pearson'] = k$estimate
  correlaciones[ch,'p']       = k$p.value
  correlaciones[ch,'t']       = k$statistic
  correlaciones[ch,'dF']      = k$parameter
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

###############################################################################
# Hurst x Canal
# MOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  #Hurst.tmp = Hurst.MOR.promedio[grep(ch.actual,Hurst.MOR.promedio$Canal_var),]
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

A = ggplot(promedios.MOR,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.MOR.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggarrange(A,labels='AUTO')
ggsave(filename='/comparacion_multi_mor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

# NMOR
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  #Hurst.tmp = Hurst.NMOR.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
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

A = ggplot(promedios.NMOR,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.NMOR.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  rotate_x_text(angle = 45)
ggarrange(A,labels='B')
ggsave(filename='/comparacion_multi_nmor.png',path=dir_graf,
       device='png',units='cm',width=8,height=4,dpi=400,scale=2)

# ambos
comparaciones     = matrix(0,nrow=n.canales,ncol=6)
comparaciones[,1] = kanales$Etiqueta
colnames(comparaciones) = c('Canal','p','dF','t','m1','m2')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  Hurst.tmp = Hurst.todo.promedio[grep(ch.actual,Hurst.NMOR.promedio$Canal_var),]
  
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

###############################################################################
# Intercanales
A = ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Grupo)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_grey(start = 1, end = 0) +
  stat_compare_means(data=Hurst.todo.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Grupo),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Etapa~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels='A')
ggsave(filename='/comparacion_hurst_multi_CTRL_PMCI.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# grafico nuevo, REM vs NREM
# usando los promedios
comparaciones.CTL     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.CTL[,1] = kanales$Etiqueta
colnames(comparaciones.CTL) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  #Hurst.tmp = Hurst.todo.promedio[grep(ch.actual,
  #                                     Hurst.todo.promedio$Canal_var),]
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
  comparaciones.CTL[ch,'m'] = k$estimate[1]
}

comparaciones.PDC     = matrix(0,nrow=n.canales,ncol=5)
comparaciones.PDC[,1] = kanales$Etiqueta
colnames(comparaciones.PDC) = c('Canal','p','dF','t','m')
for(ch in 1:n.canales){
  ch.actual = kanales$Etiqueta[ch]
  #Hurst.tmp = Hurst.todo.promedio[grep(ch.actual,
  #                                     Hurst.todo.promedio$Canal_var),]
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
  comparaciones.PDC[ch,'m'] = k$estimate[1]
}

A = ggplot(promedios.todo,aes(x=Canal_var,y=Hurst,fill=Etapa)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=Hurst.todo.promedio,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6,paired=T)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels='B')
ggsave(filename='/comparacion_hurst_multi_REM_NREM.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

# Significativos
cuales   = c(7,8)
signif   = c()
signif.m = c()
for(ch in cuales){
  ch.actual    = kanales$Etiqueta[ch]
  Hurst.tmp    = Hurst.todo.promedio[grep(ch.actual,
                                          Hurst.todo.promedio$Canal_var),]
  promedio.tmp = promedios.todo[grep(ch.actual,
                                     promedios.todo$Canal_var),]
  
  signif   = rbind(signif,Hurst.tmp)
  signif.m = rbind(signif.m,promedio.tmp)
}

A = ggplot(signif.m,aes(x=Canal_var,y=Hurst,fill=Etapa)) +
  theme_classic2() +
  labs(fill=NULL) +
  xlab(NULL) + ylab('Hurst Exponent') +
  geom_bar(stat='identity',position=position_dodge(),color='black') +
  geom_errorbar(aes(ymin=Hurst-se,ymax=Hurst+se),
                position=position_dodge(.9),width=.5) +
  coord_cartesian(ylim=c(.5,1.6))+
  scale_fill_manual(values = c('#bebada','#fb8072'))+
  stat_compare_means(data=signif,inherit.aes=F,
                     mapping=aes(x=Canal_var,y=Hurst,fill=Etapa),
                     label='p.signif',method='t.test',
                     hide.ns=T,label.y=1.6,paired=T)+
  theme(legend.position='top',legend.direction = 'horizontal',
        legend.justification=c(1,0))+
  facet_grid(Grupo~.) +
  theme(strip.background = element_blank())+
  rotate_x_text(angle = 45)
ggarrange(A,labels='C')
ggsave(filename='/comparacion_hurst_multi_REM_NREM_signif.png',path=dir_graf,
       device='png',units='cm',width=8,height=6,dpi=400,scale=2)

###############################################################################
###############################################################################