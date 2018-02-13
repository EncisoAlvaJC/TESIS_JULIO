###############################################################################
# parametros
potencia.total = F
orden_stam     = TRUE

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk      = 1

quitar.artefactos = TRUE

zoom           = T
unidad_par_t   = 'tiempo'

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
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/espectro_171124'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/espectro_171129'

###############################################################################
# librerias
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_65')
require(readxl)
#require(xlsx)

require(ggplot2)

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,'/info_tecnico.xlsx'))
bandas   = read_excel(paste0(dir_info,'/info_bandas.xlsx'))

n.bandas = length(bandas$Banda)

canales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  canales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales = length(canales$Etiqueta)

###############################################################################
# parametros del script
nombre      = info$Nombre_archivo[sujeto]
etiqueta    = info$Nombre[sujeto]
fr_muestreo = info$Fr_muestreo[sujeto]

ajuste_ini_hms = c(info$hh_0[sujeto],info$mm_0[sujeto],info$ss_0[sujeto])
min_hms        = c(info$hh_0[sujeto],info$mm_0[sujeto],info$ss_0[sujeto])
max_hms        = c(info$hh_f[sujeto],info$mm_f[sujeto],info$ss_f[sujeto])

grupo = info$Grupo_n[sujeto]
if(grupo== 0){grupo.etiqueta = 'CTRL'}
if(grupo== 1){grupo.etiqueta = 'PMCI'}
if(grupo==-1){grupo.etiqueta = 'EXCL'}

#################################################
# ajustes del zoom
if(zoom){
  validar.zoom = F
  if(unidad_par_t=='tiempo'){
    s.ini = hms2t(min_hms)
    s.fin = hms2t(max_hms)
    
    e.ini = s.ini/30+1
    e.fin = s.fin/30+1
    
    n.epo = e.fin - e.ini
    
    validar.zoom = T
  }
  if(unidad_par_t=='epocas'){
    # agregar posteriormente
  }
}

###############################################################################
# leer epocas
factor.extra = 1
if(fr_muestreo==200){
  factor.extra = 3
}
diez = 10*factor.extra

epocas.mor    = read_excel(paste0(dir_info,'/info_tecnico.xlsx'),
                           sheet='EpocasMOR')
indice.epocas = epocas.mor[,etiqueta]
indice.epocas = as.numeric(unlist(indice.epocas))
indice.epocas = indice.epocas[!is.na(indice.epocas)]
indice.epocas = sort(unique(indice.epocas))
epo0          = indice.epocas[1]-diez
indice.epocas = c(epo0-1+1:diez,indice.epocas)

indice.chunk   = indice.epocas*30/factor.extra - s.ini #epoca->segundos
indice.chunk   = indice.chunk/dur.chunk                #segundos->chunks
indice.chunk.t = c()
for(k in 1:(30/factor.extra)){
  indice.chunk.t = c(indice.chunk.t,indice.chunk+k)
}
indice.chunk = sort(unique(indice.chunk.t))

#################################################
# informacion preeliminar en el archivo
tag = rbind(c('Participante',etiqueta),
            c('Etiqueta',nombre),
            c('Grupo',grupo.etiqueta),
            c('Fr. muestreo',fr_muestreo),
            c('Dur. epoca',30/factor.extra))

gral           = matrix(ncol=5,nrow=length(indice.epocas))
colnames(gral) = c('Etapa','[#]','Epoca [#]','Inicio [hhmmss]','Fin [hhmmss]')

#for(i in 1:10){
#  hhmmss1  = t2hms((indice.epocas[i]-1)*30/factor.extra)
#  hhmmss2  = corregir.hms(corregir.hms(hhmmss1+c(0,0,30/factor.extra)))
#  gral[i,] = c('NMOR',i,indice.epocas[i],hms2txt(hhmmss1),hms2txt(hhmmss2))
#}

#for(i in 11:length(indice.epocas)){
##  hhmmss1  = t2hms((indice.epocas[i]-1)*30/factor.extra)
#  hhmmss2  = corregir.hms(hhmmss1+c(0,0,30/factor.extra))
#  gral[i,] = c('MOR',i-10,indice.epocas[i],hms2txt(hhmmss1),hms2txt(hhmmss2))
#}

if(potencia.total){
  agregado = 'total'
}else{
  agregado = 'relativo'
}

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_pre)
ch        = 19
ch.actual = canales$Nombre_archivo[ch]
n.archivo = paste0('SP_INT_',nombre,'_',ch.actual,'_TOTAL.txt')
datos     = scan(n.archivo)
n.chunks  = length(datos)

indice.chunk = indice.chunk[indice.chunk<=n.chunks]

i.d    = length(indice.chunk)
i.e    = length(indice.epocas)
indize = 1:i.d

RES.todo = as.data.frame(matrix(0,ncol=8+n.bandas,nrow=n.canales*i.d))
RES.mean = as.data.frame(matrix(0,ncol=8+n.bandas,nrow=n.canales*i.e))
colnames(RES.todo) = c('Participante','Canal','IndiceEpoca','EpocaMOR',
                       'Delta','Theta','Alfa','Beta','Gamma','Varianza',
                       'Grupo','Etapa','Indice')
colnames(RES.todo) = c('Participante','Canal','IndiceEpoca','EpocaMOR',
                       'Delta','Theta','Alfa','Beta','Gamma','Varianza',
                       'Grupo','Etapa','Indice')

RES.todo$Participante = rep(sujeto,n.canales*i.d)
RES.todo$Grupo        = rep(grupo,n.canales*i.d)

archivo.csv = paste0('espectro',nombre,'_',agregado,'.csv')

c.epoca    = info$Dur_epoca[sujeto]/dur.chunk    #chunk/epoca
#c.analizar = c.epoca*i.e                         #chunk de epocas consideradas
c.10       = c.epoca*10                          #chunk de las primeras 10 epocas

#################################################
# inicio ciclo que recorre todos los canales
for(banda.actual in 1:n.bandas){
  for(ch in 1:n.canales){
    setwd(dir_res_pre)
    
    # cargar los datos
    ch.actual = canales$Nombre_archivo[ch]
    n.archivo = paste0('SP_INT_',nombre,'_',ch.actual,'_',
                       bandas$Nombre_archivo[banda.actual],'.txt')
    datos     = scan(n.archivo)
    if(zoom){
      datos    = datos[indice.chunk]
    }
    
    step.ch  = i.d*(ch-1)
    
    bnd = bandas$Banda[banda.actual]
    
    RES.todo[step.ch+1:i.d,bnd]       = datos
    RES.todo[step.ch+1:i.d,'Canal']   = rep(ch,i.d)
    RES.todo[step.ch + 1:i.d,'Etapa'] = c(rep(0,c.10),rep(1,i.d-c.10))
    RES.todo[step.ch+1:i.d,'Indice']  = 
      indize - c(rep(0,c.10),rep(1,i.d-c.10))*c.10
    
    RES.todo[step.ch+1:i.d,'IndiceEpoca'] = sort(rep(c(1:i.e),c.epoca))
  } 
}
RES.todo$EpocaMOR = RES.todo$IndiceEpoca - 10*RES.todo$Etapa

###############################################################################
# normalizacion
suma = RES.todo$Delta + RES.todo$Theta + RES.todo$Alfa + 
  RES.todo$Beta + RES.todo$Gamma

RES.todo$Delta = RES.todo$Delta/suma
RES.todo$Theta = RES.todo$Theta/suma
RES.todo$Alfa  = RES.todo$Alfa/suma
RES.todo$Beta  = RES.todo$Beta/suma
RES.todo$Gamma = RES.todo$Gamma/suma

if(potencia.total){
  setwd(dir_res_pre)
  
  n.archivo = paste0('VAR_',nombre,'_',ch.actual,'.txt')
  potencia  = scan(n.archivo)
  potencia  = potencia[indice.chunk]
  
  RES.todo$Delta = RES.todo$Delta*potencia
  RES.todo$Theta = RES.todo$Theta*potencia
  RES.todo$Alfa  = RES.todo$Alfa*potencia
  RES.todo$Beta  = RES.todo$Beta*potencia
  RES.todo$Gamma = RES.todo$Gamma*potencia
  
  if(quitar.artefactos){
    kk = 3
    
    for(banda.a in 1:n.bandas){
      bnd = bandas$Banda[banda.actual]
      
      q1  = quantile(RES.todo[,bnd],.25,na.rm=T)
      q3  = quantile(RES.todo[,bnd],.75,na.rm=T)
      riq = kk*(q3-q1)
      RES.todo$Delta = pmin(RES.todo[,bnd],q3+riq)
      RES.todo$Delta = pmax(RES.todo[,bnd],q1-riq)
    }
  }
}
###############################################################################

for(banda.actual in 1:n.bandas){
  setwd(dir_graf)
  
  archivo.excel = paste0(nombre,'_',bandas$Nombre_archivo[banda.actual],
                         '_',agregado,'.xlsx')
  
#  write.xlsx(tag,file=archivo.excel,
#             row.names=FALSE,col.names=FALSE,
#             sheetName='General',append=FALSE,showNA=TRUE)
#  write.xlsx(gral,file=archivo.excel,
#             row.names=FALSE,col.names=TRUE,
#             sheetName='Epocas',append=TRUE,showNA=TRUE)
  
  setwd(dir_graf)
  
  RES = matrix(0,nrow=n.canales,ncol=length(indice.chunk))
  row.names(RES) = canales$Etiqueta
  
  RES.todo$Canal = factor(RES.todo$Canal,labels=canales$Etiqueta)
  
  for(ch in 1:n.canales){
    k        = RES.todo[grep(canales$Etiqueta[ch],RES.todo$Canal),]
    RES[ch,] = k[,bandas$Banda[banda.actual]]
  }
  
  
  parche = character(30/factor.extra)
  for(i in 1:(30/factor.extra)){
    parche[i] = toString(i)
  }
  
  for(i in 1:10){
    tmp = matrix(0,nrow=n.canales,ncol=30/factor.extra+2)
    row.names(tmp) = canales$Etiqueta
    colnames(tmp) = c('Promedio','Des. std.',parche)
    for(ch in 1:n.canales){
      tmp[ch,1] = mean(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
      tmp[ch,2] =   sd(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
    }
    tmp[,1:(30/factor.extra)+2] = 
      RES[,(((i-1)*30/factor.extra+1):(i*30/factor.extra))]
#    write.xlsx(tmp,file=archivo.excel,sheetName=paste('pre-MOR',toString(i)),
#               col.names=TRUE,row.names=TRUE,
#               append=TRUE)
  }
  for(i in 11:(length(indice.epocas))){
    tmp = matrix(0,nrow=n.canales,ncol=30/factor.extra+2)
    row.names(tmp) = canales$Etiqueta
    colnames(tmp) = c('Promedio','Des. std.',parche)
    for(ch in 1:n.canales){
      tmp[ch,1] = mean(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
      tmp[ch,2] =   sd(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
    }
    tmp[,1:(30/factor.extra)+2] = 
      RES[,(((i-1)*30/factor.extra+1):(i*30/factor.extra))]
#    write.xlsx(tmp,file=archivo.excel,sheetName=paste('MOR',toString(i)),
#               col.names=TRUE,row.names=TRUE,
#               append=TRUE)
  }
}

###############################################################################
###############################################################################

{
  archivo.excel = paste0(nombre,'_VARIANZA',
                         '_',agregado,'.xlsx')
  
#  write.xlsx(tag,file=archivo.excel,
#             row.names=FALSE,col.names=FALSE,
#             sheetName='General',append=FALSE,showNA=TRUE)
#  write.xlsx(gral,file=archivo.excel,
#             row.names=FALSE,col.names=TRUE,
#             sheetName='Epocas',append=TRUE,showNA=TRUE)
  
  #################################################
  # contenedores de los datos
  RES = matrix(0,nrow=n.canales,ncol=length(indice.chunk))
  row.names(RES) = canales$Etiqueta
  
  #################################################
  # inicio ciclo que recorre todos los canales
  for(ch in 1:n.canales){
    setwd(dir_res_pre)
    # cargar los datos
    ch.actual = canales$Nombre_archivo[ch]
    n.archivo = paste0('SP_INT_',nombre,'_',ch.actual,'_',
                       bandas$Nombre_archivo[1],'.txt')
    datos     = scan(n.archivo)
    
    if(zoom){
      datos    = datos[indice.chunk]
    }
    
    # organizacion de los datos en una matriz
    RES[ch,] = datos
  }
  
  for(ch in 1:n.canales){
    setwd(dir_res_pre)
    step.ch  = i.d*(ch-1)
    
    RES.todo[step.ch+(1:i.d),10] = RES[ch,]
  }
  
  RES[is.na(RES)*1==1] = 0
  # fin ciclo que recorre canales
  #################################################
  
  setwd(dir_graf)
  
  parche = character(30/factor.extra)
  for(i in 1:(30/factor.extra)){
    parche[i] = toString(i)
  }
  
  for(i in 1:10){
    tmp = matrix(0,nrow=n.canales,ncol=30/factor.extra+2)
    row.names(tmp) = canales$Etiqueta
    colnames(tmp) = c('Promedio','Des. std.',parche)
    for(ch in 1:n.canales){
      tmp[ch,1] = mean(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
      tmp[ch,2] =   sd(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
    }
    tmp[,1:(30/factor.extra)+2] = 
      RES[,(((i-1)*30/factor.extra+1):(i*30/factor.extra))]
#    write.xlsx(tmp,file=archivo.excel,sheetName=paste('pre-MOR',toString(i)),
#               col.names=TRUE,row.names=TRUE,
#               append=TRUE)
  }
  for(i in 11:(length(indice.epocas))){
    tmp = matrix(0,nrow=n.canales,ncol=30/factor.extra+2)
    row.names(tmp) = canales$Etiqueta
    colnames(tmp) = c('Promedio','Des. std.',parche)
    for(ch in 1:n.canales){
      tmp[ch,1] = mean(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
      tmp[ch,2] =   sd(RES[ch,(((i-1)*30/factor.extra+1):(i*30/factor.extra))])
    }
    tmp[,1:(30/factor.extra)+2] = 
      RES[,(((i-1)*30/factor.extra+1):(i*30/factor.extra))]
#    write.xlsx(tmp,file=archivo.excel,sheetName=paste('MOR',toString(i)),
#               col.names=TRUE,row.names=TRUE,
#               append=TRUE)
  }
}

setwd(dir_graf)
write.csv(RES.todo,file=archivo.csv,row.names=F)

###############################################################################
prom0 = RES.todo[grep(0,RES.todo$Etapa),]
mean0 = aggregate(prom0[,c(1,5:12)],
                  list(prom0$Canal),mean)
prom1 = RES.todo[grep(1,RES.todo$Etapa),]
mean1 = aggregate(prom1[,c(1,5:12)],
                  list(prom1$Canal),mean)

promedios = rbind(mean1,mean0)

colnames(promedios)[1] = 'Canal'

archivo.csv2 = paste0('espectro',nombre,'_',agregado,
                      '_promedios.csv')
write.csv(promedios,file=archivo.csv2,row.names=F)

###############################################################################
# graficos de espectrogramas
require('ggpubr')
require('ggthemes')
require('ggplot2')
require('Rmisc')

# color chevere
jet.colors = colorRampPalette(c('#00007F','blue','#007FFF',
                                'cyan','#7FFF7F','yellow', 
                                '#FF7F00', 'red','#7F0000'))

RES.todo$Etapa  = factor(RES.todo$Etapa,labels=c('NMOR','MOR'))

banda.actual = 1
bnd       = bandas$Banda[banda.actual]
promedios = summarySE(RES.todo,measurevar=bnd,
                      groupvars=c('Etapa','Canal'))

p = ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Delta)) + 
  #scale_fill_gradient(palette='Spectral')+
  #scale_fill_gradientn(colors=jet.colors(7)) +
  scale_fill_gradient(low='white',high='black') +
  labs(title=etiqueta,
       subtitle=paste('Espectrograma |',bnd,'| Potencia',agregado))+
  xlab(NULL)+ylab(NULL)+
  geom_raster()+
  theme_classic() +
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))
print(p)
ggsave(filename=paste0('plot_espectro_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)

p = ggplot(promedios,aes(x=Canal,y=Delta,fill=Etapa)) +
  labs(title=etiqueta,
       subtitle=paste('Comparación banda',
                      bandas$Banda[banda.actual]))+
  theme_classic() +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Delta-se,ymax=Delta+se),
                position=position_dodge(.9),width=.5) +
  stat_compare_means(label='p.signif',method='wilcox.test',hide.ns=T)+
  xlab(NULL) + ylab('Área bajo la curva') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_grey(start = 1, end = 0) +
  rotate_x_text(angle = 45)+
  theme(legend.position='bottom')
print(p)
ggsave(filename=paste0('plot_espectro_NR_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)

banda.actual = 2
bnd       = bandas$Banda[banda.actual]
promedios = summarySE(RES.todo,measurevar=bnd,
                      groupvars=c('Etapa','Canal'))

p = ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Theta)) + 
  #scale_fill_gradient(palette='Spectral')+
  #scale_fill_gradientn(colors=jet.colors(7)) +
  scale_fill_gradient(low='white',high='black') +
  labs(title=etiqueta,
       subtitle=paste('Espectrograma |',bnd,'| Potencia',agregado))+
  xlab(NULL)+ylab(NULL)+
  geom_raster()+
  theme_classic() +
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))
print(p)
ggsave(filename=paste0('plot_espectro_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)

p = ggplot(promedios,aes(x=Canal,y=Theta,fill=Etapa)) +
  labs(title=etiqueta,
       subtitle=paste('Comparación banda',
                      bandas$Banda[banda.actual]))+
  theme_classic() +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Theta-se,ymax=Theta+se),
                position=position_dodge(.9),width=.5) +
  stat_compare_means(label='p.signif',method='wilcox.test',hide.ns=T)+
  xlab(NULL) + ylab('Área bajo la curva') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_grey(start = 1, end = 0) +
  rotate_x_text(angle = 45)+
  theme(legend.position='bottom')
print(p)
ggsave(filename=paste0('plot_espectro_NR_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)


banda.actual = 3
bnd       = bandas$Banda[banda.actual]
promedios = summarySE(RES.todo,measurevar=bnd,
                      groupvars=c('Etapa','Canal'))

p = ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Alfa)) + 
  #scale_fill_gradient(palette='Spectral')+
  #scale_fill_gradientn(colors=jet.colors(7)) +
  scale_fill_gradient(low='white',high='black') +
  labs(title=etiqueta,
       subtitle=paste('Espectrograma |',bnd,'| Potencia',agregado))+
  xlab(NULL)+ylab(NULL)+
  geom_raster()+
  theme_classic() +
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))
print(p)
ggsave(filename=paste0('plot_espectro_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)

p = ggplot(promedios,aes(x=Canal,y=Alfa,fill=Etapa)) +
  labs(title=etiqueta,
       subtitle=paste('Comparación banda',
                      bandas$Banda[banda.actual]))+
  theme_classic() +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Alfa-se,ymax=Alfa+se),
                position=position_dodge(.9),width=.5) +
  stat_compare_means(label='p.signif',method='wilcox.test',hide.ns=T)+
  xlab(NULL) + ylab('Área bajo la curva') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_grey(start = 1, end = 0) +
  rotate_x_text(angle = 45)+
  theme(legend.position='bottom')
print(p)
ggsave(filename=paste0('plot_espectro_NR_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)


banda.actual = 4
bnd       = bandas$Banda[banda.actual]
promedios = summarySE(RES.todo,measurevar=bnd,
                      groupvars=c('Etapa','Canal'))

p = ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Beta)) + 
  #scale_fill_gradient(palette='Spectral')+
  #scale_fill_gradientn(colors=jet.colors(7)) +
  scale_fill_gradient(low='white',high='black') +
  labs(title=etiqueta,
       subtitle=paste('Espectrograma |',bnd,'| Potencia',agregado))+
  xlab(NULL)+ylab(NULL)+
  geom_raster()+
  theme_classic() +
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))
print(p)
ggsave(filename=paste0('plot_espectro_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)

p = ggplot(promedios,aes(x=Canal,y=Beta,fill=Etapa)) +
  labs(title=etiqueta,
       subtitle=paste('Comparación banda',
                      bandas$Banda[banda.actual]))+
  theme_classic() +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Beta-se,ymax=Beta+se),
                position=position_dodge(.9),width=.5) +
  stat_compare_means(label='p.signif',method='wilcox.test',hide.ns=T)+
  xlab(NULL) + ylab('Área bajo la curva') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_grey(start = 1, end = 0) +
  rotate_x_text(angle = 45)+
  theme(legend.position='bottom')
print(p)
ggsave(filename=paste0('plot_espectro_NR_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)

banda.actual = 5
bnd       = bandas$Banda[banda.actual]
promedios = summarySE(RES.todo,measurevar=bnd,
                      groupvars=c('Etapa','Canal'))

p = ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Beta)) + 
  #scale_fill_gradient(palette='Spectral')+
  #scale_fill_gradientn(colors=jet.colors(7)) +
  scale_fill_gradient(low='white',high='black') +
  labs(title=etiqueta,
       subtitle=paste('Espectrograma |',bnd,'| Potencia',agregado))+
  xlab(NULL)+ylab(NULL)+
  geom_raster()+
  theme_classic() +
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))
print(p)
ggsave(filename=paste0('plot_espectro_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)

p = ggplot(promedios,aes(x=Canal,y=Gamma,fill=Etapa)) +
  labs(title=etiqueta,
       subtitle=paste('Comparación banda',
                      bandas$Banda[banda.actual]))+
  theme_classic() +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Gamma-se,ymax=Gamma+se),
                position=position_dodge(.9),width=.5) +
  stat_compare_means(label='p.signif',method='wilcox.test',hide.ns=T)+
  xlab(NULL) + ylab('Área bajo la curva') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_grey(start = 1, end = 0) +
  rotate_x_text(angle = 45)+
  theme(legend.position='bottom')
print(p)
ggsave(filename=paste0('plot_espectro_NR_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)


bnd       = 'Varianza'
promedios = summarySE(RES.todo,measurevar=bnd,
                      groupvars=c('Etapa','Canal'))

p = ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Beta)) + 
  #scale_fill_gradient(palette='Spectral')+
  #scale_fill_gradientn(colors=jet.colors(7)) +
  scale_fill_gradient(low='white',high='black') +
  labs(title=etiqueta,
       subtitle=paste('Espectrograma |',bnd,'| Potencia',agregado))+
  xlab(NULL)+ylab(NULL)+
  geom_raster()+
  theme_classic() +
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))
print(p)
ggsave(filename=paste0('plot_espectro_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)

p = ggplot(promedios,aes(x=Canal,y=Varianza,fill=Etapa)) +
  labs(title=etiqueta,
       subtitle=paste('Comparación banda',
                      bandas$Banda[banda.actual]))+
  theme_classic() +
  geom_bar(stat='identity',position=position_dodge(),
           color='black') +
  geom_errorbar(aes(ymin=Varianza-se,ymax=Varianza+se),
                position=position_dodge(.9),width=.5) +
  stat_compare_means(label='p.signif',method='wilcox.test',hide.ns=T)+
  xlab(NULL) + ylab('Área bajo la curva') +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_grey(start = 1, end = 0) +
  rotate_x_text(angle = 45)+
  theme(legend.position='bottom')
print(p)
ggsave(filename=paste0('plot_espectro_NR_',nombre,'_',bnd,'.png'),
       device='png',path=dir_graf)
