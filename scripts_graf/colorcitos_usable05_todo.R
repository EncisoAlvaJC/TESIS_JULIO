###############################################################################
# directorio de trabajo
dir_actual = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/Tesis/scripts_graf'
dir_graf   = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/Tesis/scripts_graf_res'
info_dir   = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/Tesis/scripts_graf'
dir_datos  = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/estacionariedad_sf'

###############################################################################
# parametros
#sujeto     = 2

orden_stam = T

###############################################################################
# librerias
require('readxl')

require('ggplot2')
require('ggpubr')

require('Rmisc')
require('reshape')

require('scales')

require('hms')

###############################################################################
# datos generales
info     = read_excel(paste0(info_dir,'/info_tecnico.xlsx'))

orden_k  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'),
                      sheet='Alfabetico')
if(orden_stam){
  kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'),
                        sheet='Stam')
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

###############################################################################
# parametros del script
setwd(dir_actual)
source('utileria.R')

###############################################################################
# parametros del script
nombre      = info$Nombre_archivo[sujeto]
etiqueta    = info$Nombre[sujeto]
dir_res_mid = dir_datos
fr_muestreo = info$Fr_muestreo[sujeto]

binario = T

zoom           = T
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(0,0,0)
min_hms        = c(0,0,0)
max_hms        = c(info$hh_ff[sujeto],info$mm_ff[sujeto],0)
#unidad_par_t   = 'puntos'
#ajuste_ini_epo = 0
#min_epo        = epoca_ini[sujeto]
#max_epo        = epoca_fin[sujeto]-1
#min_epo        = 0

#################################################
# parametros que dependen del sujeto
if(info$Grupo_n[sujeto]==0){
  grupo = 'CTL'
}
if(info$Grupo_n[sujeto]==1){
  grupo = 'PDC'
}
if(info$Grupo_n[sujeto]==-1){
  grupo = 'EX'
}

###############################################################################
# meta-graficacion

#stop('Espacio para usar otros scripts')

#for(expon in c(2,0,-2)){
for(expon in (-5):3){
  setwd(dir_actual)
  dur_epoca = 30*(2**expon)
  setwd(dir_actual)
  source('colorcitos_usable05_parte.R')
}

# fin
###############################################################################