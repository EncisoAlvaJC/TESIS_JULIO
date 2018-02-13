###############################################################################
# parametros
orden_stam   = T

grabar       = T
zoom         = T
unidad_par_t = 'tiempo'

#################################################
# parametros de dibujo
escala_mv  = 15
salto      = 8
sep_lineas = 30
n_tiempos  = 5

###############################################################################
# directorios de trabajo
#
#     gral : de uso general
#     info : detalles de los participantes
#  scripts : sub-rutinas, en caso de haberlas
#  res_pre : resultados previos, solo para analizar y/o graficar
#   epocas : epocas para resaltar, por ahora solo MOR
#     graf : donde guardar los graficos, en caso de producirse

dir_gral       = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_info       = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_scripts    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts'
dir_registro   = 'C:/Users/EQUIPO 1/Desktop/julio/DATOS_corregido/'
dir_resultados = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/graf_registros'

###############################################################################
# librerias
require('beepr')
require('readxl')

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,'/info_tecnico.xlsx'))

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
vec_colores = matrix(0,nrow=1,ncol=22)
colnames(vec_colores) = kanales$Etiqueta
vec_colores[1,] =  c(rgb(132,120, 32,maxColorValue=255),  # C3
                     rgb(132,120, 32,maxColorValue=255),  # C4
                     rgb(132,120, 32,maxColorValue=255),  # CZ
                     rgb(166, 58, 40,maxColorValue=255),  # F3
                     rgb(166, 58, 40,maxColorValue=255),  # F4
                     rgb(166, 58, 40,maxColorValue=255),  # F7
                     rgb(166, 58, 40,maxColorValue=255),  # F8
                     rgb(128,  0,128,maxColorValue=255),  # FP1
                     rgb(128,  0,128,maxColorValue=255),  # FP2
                     rgb(166, 58, 40,maxColorValue=255),  # FZ
                     rgb( 72, 64, 65,maxColorValue=255),  # O1
                     rgb( 72, 64, 65,maxColorValue=255),  # O1
                     rgb( 29,118, 68,maxColorValue=255),  # P3
                     rgb( 29,118, 68,maxColorValue=255),  # P4
                     rgb( 29,118, 68,maxColorValue=255),  # PZ
                     rgb( 64, 35,113,maxColorValue=255),  # T3
                     rgb( 64, 35,113,maxColorValue=255),  # T4
                     rgb( 64, 35,113,maxColorValue=255),  # T5
                     rgb( 64, 35,113,maxColorValue=255),  # T3
                     rgb( 50,116,  0,maxColorValue=255),  # LOG
                     rgb( 50,116,  0,maxColorValue=255),  # ROG
                     rgb( 31, 62,119,maxColorValue=255)  # EMG
)
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales = length(kanales$Etiqueta)

nom_dir   = info$Nombre_directorio
nom_arch  = info$Nombre_archivo
nom_facil = info$Nombre

frecuenciasss = info$Fr_muestreo
grupo_de      = info$Grupo_n

h_ini = info$hh_0
m_ini = info$mm_0
s_ini = info$ss_0

h_fin = info$hh_f
m_fin = info$mm_f
s_fin = info$ss_f

beep()

nombre   = nom_arch[sujeto]
etiqueta = nom_facil[sujeto]

dir_datos = paste0(dir_registro,nom_dir[sujeto])
dir_res   = dir_resultados

fr_muestreo = frecuenciasss[sujeto]

min_hms = c(h_ini[sujeto],m_ini[sujeto],s_ini[sujeto])
max_hms = c(h_fin[sujeto],m_fin[sujeto],s_fin[sujeto])

if(fr_muestreo==512){
  dur.chunk = 30
}
if(fr_muestreo==200){
  dur.chunk = 10
}

#################################################
# parametros que dependen de los datos
ventana   = dur.chunk*fr_muestreo
ajuste_ini_hms = c(0,0,0)

#################################################
# procesamiento parametros opcionales (ajuste)
if(unidad_par_t =='tiempo'){
  ini_t   = hms2t(ajuste_ini_hms)
  ini_epo = ini_t/dur.chunk
  ini_pt  = floor(ini_t*fr_muestreo)
}
if(unidad_par_t =='epoca'){
  ini_epo = ajuste_ini_epo
  ini_t   = ini_epo*dur.chunk
  ini_pt  = ini_epo*ventana
}
str_t   = 0
str_epo = 0
str_pt  = 1

# ini : cuando inicia el archivo, util si es un fragmento
# str : cuando inicia el zoom, evita la confusion provocada por ini

# procesamiento parametros opcionales (zoom)
if(zoom){
  confirma_zoom = FALSE
  if(unidad_par_t == 'tiempo'){
    min_t = hms2t(min_hms)
    max_t = hms2t(max_hms)
    
    min_e  = (min_t+ini_t)/dur.chunk -ini_epo
    max_e  = (max_t+ini_t)/dur.chunk -ini_epo
    
    confirma_zoom = TRUE
  }
  if(unidad_par_t == 'puntos'){
    min_t = min_epo*dur.chunk -ini_t
    max_t = max_epo*dur.chunk -ini_t
    
    min_e = (min_epo+ini_t) -ini_epo
    max_e = (max_epo+ini_t) -ini_epo
    
    confirma_zoom = TRUE
  }
  if(!confirma_zoom){
    warning('WARNING: Indique unidad de tiempo para zoom (epocas o segundos)')
  }
  min_pt = floor(min_t*fr_muestreo)
  max_pt = ceiling(max_t*fr_muestreo)
  
  str_t   = max(min_t, 0)
  str_epo = max(min_e, 0)
  str_pt  = max(min_pt,1)
}

#################################################
# inicia guardado de los graficos
if(grabar){
  hh.txt = toString(min_hms[1])
  if(min_hms[1]<10){
    hh.txt = paste0('0',hh.txt)
  }
  mm.txt = toString(min_hms[2])
  if(min_hms[2]<10){
    mm.txt = paste0('0',mm.txt)
  }
  ss.txt = toString(min_hms[3])
  if(min_hms[3]<10){
    ss.txt = paste0('0',ss.txt)
  }
  
  setwd(dir_res)
  png(
  #pdf(
    paste0('general_',etiqueta,'_',
           hh.txt,mm.txt,ss.txt,
           #toString(cual),
           #'.pdf'),width=6,height=3.5)
           '.png'),units='in',res=150,width=6,height=3.5)
}

#################################################
# modo seguro: revisa si estan los archivos
correctos = rep(FALSE,n.canales)
n_datos   = Inf

setwd(dir_datos)
for(ch in 1:n.canales){
  ch_actual = kanales$Nombre_archivo[ch]
  if(file.exists(paste0(nombre,'_',ch_actual,'.txt'))){
    correctos[ch] =TRUE
  }else{
    warning('ERROR: En canal ',ch_actual,'no se encontro el archivo ',
            paste0(nombre,'_',ch_actual,'.txt'))
  }
}
canales   = kanales$Nombre_archivo[correctos]
n.canales = length(canales)

#################################################
# graficacion

# carga los datos para inicializar la ventana
setwd(dir_datos)
ch        = 1
ch_actual = canales[ch]
nom_arch  = paste0(nombre,'_',ch_actual,'.txt')
DATA      = scan(nom_arch)
n_datos   = length(DATA)

# ajustes en el tiempo
if(zoom){
  end_t   = min(max_t, n_datos/fr_muestreo)
  end_epo = min(max_e, n_datos/ventana)
  end_pt  = min(max_pt,n_datos)
  
  DATA    = DATA[str_pt:end_pt]
  
  ini_t   = ini_t   + str_t
  ini_epo = ini_epo + str_epo
  ini_pt  = ini_pt  + str_pt
  n_datos   = length(DATA)
} 
# end : cuando termina el zoom, evita la confusion provocada por ini

# miscelanea de indices para detalles tecnicos
n_puntos  = floor(n_datos/salto)
ind_t     = floor(seq(0,n_puntos,by=n_puntos/n_tiempos))+1
ind_pt    = seq(0,n_datos,by=salto)
ind_pt[1] = 1
ind_linea = seq(0,n_datos,by=sep_lineas*fr_muestreo)
ind_linea[1] = 1

# creacion etiquetas de tiempo
num_t  = (0:n_puntos)*(salto/fr_muestreo) + ini_t
num_t  = num_t[ind_t]
num_hh = floor(num_t/(60*60))
num_mm = floor( (num_t - num_hh*60*60)/60 )
num_ss = floor(  num_t - num_hh*60*60 - num_mm*60 )
num_ms = num_t - num_hh*60*60 - num_mm*60 - num_ss
txt_t  = character(length(num_t))
for(i in 1:(length(num_t))){
  txt_mm = toString(num_mm[i])
  if(num_mm[i]<10){
    txt_mm = paste0('0',num_mm[i])
  }
  txt_ss = toString(num_ss[i])
  if(num_ss[i]<10){
    txt_ss = paste0('0',num_ss[i])
  }
  txt_t[i] = paste0(toString(num_hh[i]),':',txt_mm,':',txt_ss)
}

#################################################
# graficacion
#par(bg = 'NA', mar = c(0,1.5,0,0))
par(bg = 'white', mar = c(0,1.5,0,0))
par(cex.axis=.75, cex.lab=.8, cex.main=1)

plot(c(1,max(ind_pt)),
  type='o',col='white',
  ylim=c(.5*escala_mv,(n.canales-.5)*escala_mv),
  xlim=c(1,max(ind_pt)),
  ylab = '',
  yaxt='n',xaxt='n',
  main='',
  mgp=c(2,1,0),
  bty='n'
)
for(i in ind_linea){
 abline(v=i,col='gray')
}

#################################################
# inicio ciclo que recorre canales
setwd(dir_datos)
for(ch in 1:n.canales){
  # cargar los datos
  ch_actual = canales[ch]
  nom_ar    = paste0(nombre,'_',ch_actual,'.txt')
  DATA      = scan(nom_ar)
  if(zoom){
    DATA    = DATA[str_pt:end_pt]
  }
  agregado = (n.canales-ch)*escala_mv+escala_mv/2 - mean(DATA)
  lines(ind_pt,DATA[ind_pt] + agregado,
        type='l',col=vec_colores[,kanales$Etiqueta[ch]])
  axis(2,at=(n.canales-ch)*escala_mv+escala_mv/2,
       labels=ch_actual,las=1,#col.axis=vec_colores[ch],
       mgp=c(0,0,-1.75),tick=F)
}
# fin ciclo que recorre canales
#################################################

lines(fr_muestreo*(.5*sep_lineas+c(0,30)),
      escala_mv*c(1,1),
      type='l',lwd=1.5)
lines(sep_lineas*fr_muestreo*.5*c(1,1),
      escala_mv+c(0,10),
      type='l',lwd=1.5)

if(grabar){
  setwd(dir_res)
  dev.off()
}
# inicia guardado de los graficos
#################################################