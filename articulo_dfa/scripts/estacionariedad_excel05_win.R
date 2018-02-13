###############################################################################
# parametros
grabar         = FALSE
#sujeto         = 2
orden_stam     = TRUE

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk   = 30

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
dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/estacionariedad_171118'
#dir_res_pre = 'C:/Users/EQUIPO 1/Desktop/julio/estacionariedad_sf'
#dir_epocas  = 'C:/Users/EQUIPO 1/Desktop/julio/epocas_dfa_10'
dir_graf    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/stat'

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
info     = read_excel(paste0(dir_info,'/info_tecnico.xlsx'),
                      sheet='General')

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales = length(kanales$Etiqueta)

###############################################################################
# parametros del script
nombre      = info$Nombre[sujeto]
fr_muestreo = info$Fr_muestreo[sujeto]
nombre.ar   = info$Nombre_archivo[sujeto]

ajuste_ini_hms = c(0,0,0)
min_hms        = c(info$hh_0[sujeto],info$mm_0[sujeto],info$ss_0[sujeto])
max_hms        = c(info$hh_f[sujeto],info$mm_f[sujeto],info$ss_f[sujeto])

grupo = info$Grupo_n[sujeto]
if(grupo== 0){grupo = 'CTL'}
if(grupo== 1){grupo = 'PDC'}
if(grupo==-1){grupo = 'EX'}

factor.extra = 1
if(fr_muestreo==200){
  factor.extra = 3
}

diez = 10*factor.extra

#################################################
# ajustes del zoom
# zoom = F
# if(zoom){
#   validar.zoom = F
   if(unidad_par_t=='tiempo'){
     s.ini = hms2t(ajuste_ini_hms)
     s.fin = hms2t(max_hms)
     
     #e.ini = s.ini/30+1
     #e.fin = s.fin/30+1
     
     #n.epo = e.fin - e.ini
     
     #validar.zoom = T
   }
#   if(unidad_par_t=='epocas'){
#     # agregar posteriormente
#   }
# }

###############################################################################
# leer epocas
archivo.epocas = read_excel(paste0(dir_info,'/info_tecnico.xlsx'),
                           sheet='parche_EpocasMOR')
indice.epocas  = as.numeric(unlist(archivo.epocas[,nombre]))
epo0           = indice.epocas[1] - diez
indice.epocas  = c(epo0+(1:diez)-1,indice.epocas)
indice.epocas  = sort(unique(indice.epocas))

indice.chunk   = indice.epocas*30/factor.extra - s.ini #epoca->segundos
indice.chunk   = indice.chunk/dur.chunk               #segundos->chunks
indice.chunk   = ceiling(indice.chunk)
indice.chunk.t = c()
for(k in 1:(30/(factor.extra*dur.chunk))){
  indice.chunk.t = c(indice.chunk.t,indice.chunk+k)
}
indice.chunk = sort((indice.chunk.t))

#################################################
# informacion preeliminar en el archivo
tag = rbind(c('Participante',nombre),
            c('Etiqueta',nombre),
            c('Estado',info$Grupo_n[sujeto]),
            c('Fr. muestreo',fr_muestreo),
            c('Dur. epoca',30/factor.extra))

gral           = matrix(ncol=5,nrow=length(indice.epocas))
colnames(gral) = c('Etapa','[#]','Epoca [#]','Inicio [hhmmss]','Fin [hhmmss]')

for(i in 1:diez){
  hhmmss1  = t2hms((indice.epocas[i]-1)*30/factor.extra)
  hhmmss2  = corregir.hms(corregir.hms(hhmmss1+c(0,0,30/factor.extra)))
  gral[i,] = c('NMOR',i,indice.epocas[i],hms2txt(hhmmss1),hms2txt(hhmmss2))
}

for(i in (diez+1):length(indice.epocas)){
  hhmmss1  = t2hms((indice.epocas[i]-1)*30/factor.extra)
  hhmmss2  = corregir.hms(hhmmss1+c(0,0,30/factor.extra))
  gral[i,] = c('MOR',i-diez,indice.epocas[i],hms2txt(hhmmss1),hms2txt(hhmmss2))
}

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_pre)
ch        = 19
ch.actual = kanales$Nombre_archivo[ch]
n.archivo = paste0('EST_',nombre.ar,'_',ch.actual,'_T_',
                   toString(dur.chunk),'.txt')
datos.t   = scan(n.archivo)
n.chunks  = length(datos.t)

indice.chunk = indice.chunk[indice.chunk<=n.chunks]

i.d = length(indice.chunk)
indize = 1:i.d

RES.todo = as.data.frame(matrix(0,ncol=9,nrow=n.canales*i.d))
colnames(RES.todo) = c('Participante','Canal','EpocaMOR','NumEpoca',
                       'Valido','Estacionario',
                       'Grupo','Etapa','Indice')

RES.suma = as.data.frame(matrix(0,ncol=9,nrow=n.canales*2))
colnames(RES.suma) = c('Participante','Canal','EpocaMOR','NumEpoca',
                       'Epocas','Epocas_estacionarias',
                       'Grupo','Etapa','Proporcion')

RES.todo[,'Participante'] = rep(sujeto,n.canales*i.d)
RES.suma[,'Participante'] = rep(sujeto,n.canales*2)
if(sujeto<6){
  RES.todo[,'Grupo'] = rep(0,n.canales*i.d)
  RES.suma[,'Grupo'] = rep(0,n.canales*2)
}else{
  RES.todo[,'Grupo'] = rep(1,n.canales*i.d)
  RES.suma[,'Grupo'] = rep(1,n.canales*2)
}


#################################################
# contenedores de los datos
RES.t   = matrix(0,nrow=n.canales,ncol=length(indice.chunk))
RES.res = matrix(0,nrow=n.canales,ncol=4)

row.names(RES.t)   = kanales$Etiqueta

row.names(RES.res) = kanales$Etiqueta
colnames(RES.res)  = c('Total_NMOR','Estacionarios_NMOR',
                       'Total_MOR','Estacionarios_MOR')

#################################################
# inicio ciclo que recorre todos los canales
for(ch in 1:n.canales){
  setwd(dir_res_pre)
  
  # cargar los datos
  ch.actual = kanales$Nombre_archivo[ch]
  n.archivo = paste0('EST_',nombre.ar,'_',ch.actual,'_T_',
                     toString(dur.chunk),'.txt')
  datos.t   = scan(n.archivo)
  n.archivo = paste0('EST_',nombre.ar,'_',ch.actual,'_TIR_',
                     toString(dur.chunk),'.txt')
  #datos.tir = scan(n.archivo)
  datos.tir = datos.t
  
  if(zoom){
    datos.t   = datos.t[indice.chunk]
    datos.tir = datos.tir[indice.chunk]
  }
  
  validos  = as.logical(pmin(1*(!is.na(datos.t)),1*(!is.na(datos.tir))))
  validos1 = c(validos[1:(i.d/2)],rep(F,i.d/2))
  validos2 = c(rep(F,i.d/2),validos[(i.d/2+1):i.d])
  
  # organizacion de los datos en una matriz
  RES.res[ch,1]   = sum(1*validos1)
  RES.res[ch,3]   = sum(1*validos2)
  
  datos.t[is.na(datos.t)]     = 1
  datos.tir[is.na(datos.tir)] = 1
  
  RES.t[ch,] = pmin(1*(datos.t<.05),1*(datos.tir<.05))
  
  RES.res[ch,2] = sum(RES.t[ch,validos1])
  RES.res[ch,4] = sum(RES.t[ch,validos2])

  
  step.ch  = i.d*(ch-1)
  
  RES.todo[step.ch+(1:i.d),5] = 1*validos
  RES.todo[step.ch+(1:i.d),6] = RES.t[ch,]
  RES.todo[step.ch+1:i.d,2] = rep(ch,i.d)
  RES.todo[step.ch+1:i.d,9] = indize
  
  RES.suma[ch,5] = RES.res[ch,1]
  RES.suma[ch,6] = RES.res[ch,2]
  RES.suma[ch+n.canales,5] = RES.res[ch,3]
  RES.suma[ch+n.canales,6] = RES.res[ch,4]
  RES.suma[2*ch -1:0,2] = rep(ch,2)
  RES.suma[2*ch-1,8] = 0
  RES.suma[2*ch  ,8] = 1
  
  RES.todo[step.ch+(1:diez),8]   = 
    rep(0,diez)
  RES.todo[step.ch+((diez+1):i.d),8] = 
    rep(1,i.d-diez)
}
# fin ciclo que recorre canales
###############################################################################

setwd(dir_graf)

archivo.excel = paste0(nombre,'_estacionariedad_',toString(dur.chunk),'.xlsx')

#write.xlsx(tag,file=archivo.excel,
#           row.names=FALSE,col.names=FALSE,
#           sheetName='General',append=FALSE,showNA=TRUE)
#write.xlsx(gral,file=archivo.excel,
#           row.names=FALSE,col.names=TRUE,
#           sheetName='Epocas',append=TRUE,showNA=TRUE)

RES.todo$Canal = factor(RES.todo$Canal,labels=kanales$Etiqueta)

parche = character(i.d)
for(i in 1:i.d){
  parche[i] = paste('c',toString(i))
}

# for(i in 1:diez){
#   tmp = matrix(0,nrow=n.canales,ncol=diez+2)
#   tmp[,3:length(tmp[1,])] = RES.t[,1:diez]
#   
#   row.names(tmp) = kanales$Etiqueta
#   colnames(tmp)  = c('Promedio','Des. std.',parche[1:diez])
#   for(ch in 1:n.canales){
#     tmp[ch,1] = mean(RES.t[ch,((i-1)*1*30/dur.chunk+1):(i*1*30/dur.chunk)])
#     tmp[ch,2] =   sd(RES.t[ch,(((i-1)*1*30/dur.chunk+1):(i*1*30/dur.chunk))])
#   }
#   write.xlsx(tmp,file=archivo.excel,sheetName=paste('pre-MOR',toString(i)),
#              col.names=TRUE,row.names=TRUE,
#              append=TRUE)
# }
# for(i in (diez+1):length(RES.t[1,])){
#   tmp = matrix(0,nrow=n.canales,ncol=(i.d-diez+2))
#   tmp[,3:length(tmp[1,])] = RES.t[,(diez+1):i.d]
#   
#   row.names(tmp) = kanales$Etiqueta
#   colnames(tmp)  = c('Promedio','Des. std.',parche[(diez+1):i.d])
#   for(ch in 1:n.canales){
#     tmp[ch,1] = mean(RES.t[ch,((i-1)*1*30/dur.chunk+1):(i*1*30/dur.chunk)])
#     tmp[ch,2] =   sd(RES.t[ch,(((i-1)*1*30/dur.chunk+1):(i*1*30/dur.chunk))])
#   }
#   write.xlsx(tmp,file=archivo.excel,sheetName=paste('MOR',
#                                                     toString(i-diez)),
#              col.names=TRUE,row.names=TRUE,
#              append=TRUE)
# }
  tmp = matrix(0,nrow=n.canales,ncol=diez+2)
  tmp[,3:length(tmp[1,])] = RES.t[,1:diez]
  
  row.names(tmp) = kanales$Etiqueta
  colnames(tmp)  = c('Promedio','Des. std.',parche[1:diez])
  for(ch in 1:n.canales){
    tmp[ch,1] = mean(RES.t[ch,])
    tmp[ch,2] =   sd(RES.t[ch,])
  }
#  write.xlsx(tmp,file=archivo.excel,sheetName='pre-MOR',
#             col.names=TRUE,row.names=TRUE,
#             append=TRUE)

  tmp = matrix(0,nrow=n.canales,ncol=(i.d-diez+2))
  tmp[,3:length(tmp[1,])] = RES.t[,(diez+1):i.d]
  
  row.names(tmp) = kanales$Etiqueta
  colnames(tmp)  = c('Promedio','Des. std.',parche[(diez+1):i.d])
  for(ch in 1:n.canales){
    tmp[ch,1] = mean(RES.t[ch,])
    tmp[ch,2] =   sd(RES.t[ch,])
  }
#  write.xlsx(tmp,file=archivo.excel,sheetName='MOR',
#             col.names=TRUE,row.names=TRUE,
#             append=TRUE)

###############################################################################
###############################################################################

setwd(dir_graf)

archivo.csv = paste0('estacionariedad_',nombre,'_',toString(dur.chunk),'.csv')

write.csv(RES.todo,file=archivo.csv,row.names=F)

###############################################################################
archivo.csv2 = paste0('estacionariedad_',nombre,'_',toString(dur.chunk),
                      '_promedios.csv')

RES.suma$Proporcion = RES.suma$Epocas_estacionarias/RES.suma$Epocas

write.csv(RES.suma,file=archivo.csv2,row.names=F)

###############################################################################

require(ggpubr)

RES.todo$Etapa = factor(RES.todo$Etapa,labels=c('NMOR','MOR'))
RES.todo$Indice = RES.todo$Indice - (RES.todo$Etapa=='MOR')*diez

print(
ggplot(RES.todo,aes(x=Indice,y=Canal,fill=Estacionario)) + 
  #scale_x_continuous(breaks = pretty((indice.epocas-indice.epocas[1])*30/factor.extra, n = 20),
  #                   expand=c(0,0)) +
  scale_fill_gradient2(low = 'white',high='black',midpoint=.5)+
  labs(title=nombre,
       subtitle='Épocas que son estacionarias')+
  geom_raster()+
  theme_classic2()+
  #geom_vline(xintercept=10*30/factor.extra)
  facet_grid(.~Etapa) +
  scale_x_continuous(expand=c(0,0))
)
setwd(dir_graf)
ggsave(filename=paste0(nombre,'_panorama_estacionariedad_30.png'),
       device='png')


#print(
#ggplot(RES.todo,aes(x=Canal,y=Estacionario,fill=Etapa)) + 
#  labs(title=nombre,
#       subtitle='Comparación MOR vs NMOR para estacionariedad')+
#  geom_boxplot()+
#  theme(legend.position='bottom')+
#  stat_compare_means(label = 'p.signif',method='t.test')+
#  theme(axis.title.x=element_blank())+
#  rotate_x_text(angle = 45)
#)
#################################################