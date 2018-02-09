# libreria especifica para el grafico tipo matriz
library(plotrix)

#####

# nombres de las carpetas
#       data  directorio donde estan los datos
#    central  directorio donde guardar los graficos, debe
#             contener el subdirectorio con las epocas
data_dir    = 'C:/Users/Erika/Desktop/Julio161213/scripts170405/estacionariedad_'
#central_dir = 'C:/Users/Erika/Desktop/Julio161213/scripts170330'

#####
#####
#####

duracionnn = c(30,10,60,2.5)
durrr_nomb = c('30','10','60','2_5')

#duracion_tal=1

duracion = duracionnn[duracion_tal]
dur_nomb = durrr_nomb[duracion_tal]

frecuenciasss = c(200,
                  512,512,
                  200,200,
                  512,512,
                  200,
                  512,512,
                  512,512)

frecuencia = frecuenciasss[sujeto]

# control manual, paso intermedio a la automatizacion
#sujeto  = 1     # numero de sujeto, en orden alfabetico 
p.val   = .05   # p-valor de rechazo para la hipotesis
#grabar  = F     # guardado automatico de los graficos
porcent = T     # cantidad total o proporcional
escala  = F     # el porcentaje se grafica entre 0 y 1

#####
#####
#####

# automatizacion de directorio datos segun sujeto
nombre_abreviado = nomb_facil[sujeto]
nombre           = nomb_arch[sujeto]
nom_dir          = nomb_dir[sujeto]

# directorios de trabajo, nombres abreviados
d_dir   = paste0(data_dir,dur_nomb,'s/',nom_dir)      # d(atos)
e_dir   = paste0(central_dir,'/epocas3') # e(pocas)
r_dir   = central_dir                   # r(esultados)
#g_dir   = paste0(central_dir,
#                 '/datos')# g(raficos)

#####

# nombre del archivo que contiene las epocas MOR
setwd(e_dir)
ar_indice = paste0('epocas_mor_',nombre,'.txt')
indice    = scan(ar_indice)

if(frecuencia==512){
  if(duracion==60){
    # 60 s  : 2 epocas por bloque
    indixe = ceiling(indice/2)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 1
  }
  if(duracion==30){
    # 30 s  : 1 epoca por bloque
    # no se hace nada
    
    epo_s_min = 2
  }
  if(duracion==10){
    # 10 s  : 1 epoca 3 bloques
    indixe = c( 3*indice  ,
                3*indice-1,
                3*indice-2)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 6
  }
  if(duracion==2.5){
    # 2.5 s : 1 epoca 12 bloques
    indixe = c( 12*indice  ,
                12*indice- 1,
                12*indice- 2,
                12*indice- 3,
                12*indice- 4,
                12*indice- 5,
                12*indice- 6,
                12*indice- 7,
                12*indice- 8,
                12*indice- 9,
                12*indice-10,
                12*indice-11)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 24
  }
}
if(frecuencia==200){
  if(duracion==60){
    # 60 s  : 6 epocas en 1 bloque
    indixe = ceiling(indice/6)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 1
  }
  if(duracion==30){
    # 30 s  : 3 epocas en 1 bloque
    indixe = ceiling(indice/3)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 2
  }
  if(duracion==10){
    # 10 s  : 1 epoca por bloque
    # no se hace nada
    
    epo_s_min = 6
  }
  if(duracion==2.5){
    # 2.5 s : 1 epoca 4 bloques
    indixe = c( 4*indice  ,
                4*indice-1,
                4*indice-2,
                4*indice-3)
    
    indixe = unique(indixe)
    indixe = sort(indixe)
    
    indice = indixe
    
    epo_s_min = 24
  }
}

#####

# contenedores de los datos
RES_T   = c()
max_epo = c()

#####

#if(nuevo){
  tag  = 'EST'
  lain = '_' 
#}
#if(!nuevo){
#  tag  = 'RES'
#  lain = ''
#}

setwd(d_dir)

# ciclo que recorre los 22 canales
for(ch in 1:22){
  # forma el nombre del archivo con daos
  canal  = channel[ch]
  ar_t   = paste0(tag,'_',nombre,
                  lain,canal,'_T.csv'  )
  #ar_t   = paste0('EST_',nombre,'_',canal,'_T.csv'  )
  #ar_t   = paste0('RES_',nombre,canal,'_T.csv'  )
  
  # carga los datos
  pv_t_pre = read.csv( ar_t,row.names=1 )
  pv_t     = as.numeric(unlist(pv_t_pre))
  
  # pone los datos en una matriz
  RES_T   = do.call(rbind,list(RES_T  ,pv_t ))
  max_epo = append(max_epo,length(pv_t))
}

for(ii in 1:22){
  for(jj in 1:length(RES_T[1,])){
    if(is.na(RES_T[ii,jj])){
      RES_T[ii,jj] = 0
    }
  }
}

IND_T = 1:min(max_epo)

# variable auxiliar, numero de epocas totales
n.epo = length(IND_T)

# numero total de epocas y numero de epocas MOR
print(paste0('Total : ', toString(n.epo)))
print(paste0('  MOR : ', toString(length(indice) )))

####

####

# nombre de archivo acorde a argumentos opcionales
#if(porcent){
#  tag = 'porcentaje'
#}
#if(!porcent){
#  tag = 'total'
#}

# guardado automatico del grafico resultante
# if(grabar){
#   setwd(g_dir)
#   pdf(paste0(nombre,'_',toString(length(indice)),
#              #png(paste0(nombre,'_',toString(length(indice)),
#              '_',toString(n.epo),
#              '_',toString(100*p.val),
#              '_bar',
#              '_',tag,
#              '.pdf'),width=12,height=6)
#   #'.png'),units='in',res=150,width=12,height=6)
# }

####
####
####

# nombres un poco mas sencillos
mor   = indice
n.mor = setdiff(1:n.epo,mor)

# se cuentan las epocas estacionarias en 3 categorias
#      tot : todas las epocas del registro
#      mor : las epocas mor
#     nmor : 

# contenedores de resultado
res_tot  = rep(0,22)
res_mor  = rep(0,22)
res_nmor = rep(0,22)

# conteo de epocas estacionarias
for(ch in 1:22){
  res_tot[ch]  = sum((RES_T[ch,]>p.val)*1)
  res_nmor[ch] = sum((RES_T[ch,n.mor]>p.val)*1)
  res_mor[ch]  = sum((RES_T[ch,mor]>p.val)*1)
}

##########################################
##########################################

for(ch in 1:22){
  TOTAL_MOR[ch,sujeto]  = res_mor[ch]
  TOTAL_NMOR[ch,sujeto] = res_nmor[ch]
  TOTAL_TOT[ch,sujeto]  = res_tot[ch]
}

TOTAL_MOR[23,sujeto]  = length(mor)
TOTAL_NMOR[23,sujeto] = length(n.mor)
TOTAL_TOT[23,sujeto]  = length(RES_T[1,])

##########################################
##########################################

# participacion relativa
#if(porcent){
  res_tot  =  res_tot/length(RES_T[1,])
  res_nmor = res_nmor/length(n.mor)
  res_mor  =  res_mor/length(mor)
#}
  
##########################################
##########################################

for(ch in 1:22){
  PORCE_MOR[ch,sujeto]  = res_mor[ch]
  PORCE_NMOR[ch,sujeto] = res_nmor[ch]
  PORCE_TOT[ch,sujeto]  = res_tot[ch]
}

##########################################
##########################################  

# # matriz con todos los datos
# ress = rbind(res_tot,res_nmor,res_mor)
# #ress = rbind(res_tot,res_mor)
# #ress = t(ress)
# 
# # fijar el maximo del grafico
# max_r = (max(ress))
# if(escala){
#   max_r = 1
# }
# 
# # el mensaje cambia si es total o proporcion
# if(porcent){
#   yl = 'Proporcion'
# }
# if(!porcent){
#   yl = 'Total'
# }
# 
# # se grafican las cantidades
# barplot(ress,space=c(0,.5),
#         ylim=c(0,max_r),
#         names.arg=channel,
#         col=c('black','gray','green'),
#         #col=c('black','green'),
#         border=NA,xpd=F,beside=T,
#         main=paste0(nombre_abreviado,' , *=',
#                     toString(p.val)),
#         ylab=paste0(yl,' de epocas estacionarias'))
# 
# legend('topleft',
#        c('Total','no-MOR','MOR'),
#        fill=c('black','gray','green'),
#        #c('Total','MOR'),
#        #fill=c('black','green'),
#        bty='o',y.intersp=1,
#        xjust=1,yjust=0,
#        cex=1)
# 
# ####
# ####
# 
# # guardado automatizado de los resultados
# if(grabar){
#   setwd(g_dir)
#   dev.off()
# }
# 
# ####