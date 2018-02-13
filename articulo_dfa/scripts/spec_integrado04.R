###############################################################################
# parametros
orden_stam = T

# tamano de la ventana analizada, nombre raro para evitar confusiones
dur.chunk   = c(15)
quienes     = rev(2:7)
#quienes = 10

zoom           = F
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

dir_gral       = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_info       = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa'
dir_scripts    = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts'
dir_registro   = 'C:/Users/EQUIPO 1/Desktop/julio/DATOS_corregido/'
dir_resultados = 'C:/Users/EQUIPO 1/Desktop/julio/espectro_171203'

###############################################################################
# librerias
require('beepr')
require('readxl')

# libreria que contiene la prueba de PSR
require('psd')
require('fractal')

# libreria para correr en paralelo
require('foreach')
require('doParallel')
require('parallel')

# sub-rutinas que acortan el codigo
source(paste0(dir_scripts,'/utileria.R'))

###############################################################################
# datos generales
info     = read_excel(paste0(dir_info,'/info_tecnico.xlsx'))

kanales  = read_excel(paste0(dir_info,'/info_canales.xlsx'))
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

#################################################
# inicia ciclo que recorre tamanos de ventana
for(dur_epoca in dur.chunk){
  #################################################
  # inicia ciclo que recorre sujetos
  for(sujeto in quienes){
    nombre   = nom_arch[sujeto]
    etiqueta = nom_facil[sujeto]
    
    dir_datos = paste0(dir_registro,nom_dir[sujeto])
    dir_res   = dir_resultados
    
    fr_muestreo = frecuenciasss[sujeto]
    
    #min_hms = c(h_ini[sujeto],m_ini[sujeto],s_ini[sujeto])
    #max_hms = c(h_fin[sujeto],m_fin[sujeto],s_fin[sujeto])
    
    min_hms = c(0 ,0,0)
    max_hms = c(11,0,0)
    
    setwd(dir_scripts)
    source('multi_espectro_integrado03_paralelizado.R' )
    beep()
    
    # freno de emergencia
    #stopCluster(closter)
  }
  # fin cilco que recorre sujetos
  #################################################
  beep()
}
# fin cilco que recorre tamanos de ventana
#################################################