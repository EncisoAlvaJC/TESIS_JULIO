###############################################################################
# carpeta central
data_dir    = '~/TESIS/graf_datos/estacionariedad_sf/'
central_dir = '~/TESIS/TESIS/img_art_dfa'
info_dir    = '~/TESIS/TESIS/articulo_dfa'
g_dir       = '~/TESIS/TESIS/img_art_dfa'
setwd(central_dir)

dur_chunk = 30

p.val = .05

guardar_archivo = F
nombre_archivo  = paste0('porcentajes_',toString(dur_chunk),'.xlsx')

###############################################################################
# librerias
require('readxl')
require('xlsx')

require('ggplot2')
require('ggpubr')

require('Rmisc')

###############################################################################
# datos generales
info     = read_excel(paste0(info_dir,'/info_tecnico.xlsx'))

kanales  = read_excel(paste0(info_dir,'/info_canales.xlsx'))
if(orden_stam){
  kanales  = read_excel(paste0(dir_info,'/info_canales_alterno.xlsx'))
}
n.canales    = length(kanales$Etiqueta)
canales.arch = kanales$Nombre_archivo

n.participantes = length(info$Nombre)

###############################################################################
# contenedores de datos
PORCE_MOR  = matrix(nrow=n.canales,ncol=n.participantes)
PORCE_NMOR = matrix(nrow=n.canales,ncol=n.participantes)
PORCE_TOT  = matrix(nrow=n.canales,ncol=n.participantes)

TOTAL_MOR  = matrix(nrow=n.canales+1,ncol=n.participantes)
TOTAL_NMOR = matrix(nrow=n.canales+1,ncol=n.participantes)
TOTAL_TOT  = matrix(nrow=n.canales+1,ncol=n.participantes)

colnames(PORCE_MOR)  = info$Nombre
colnames(PORCE_NMOR) = info$Nombre
colnames(PORCE_TOT)  = info$Nombre

row.names(PORCE_MOR)  = kanales$Etiqueta
row.names(PORCE_NMOR) = kanales$Etiqueta
row.names(PORCE_TOT)  = kanales$Etiqueta

colnames(TOTAL_MOR)  = info$Nombre
colnames(TOTAL_NMOR) = info$Nombre
colnames(TOTAL_TOT)  = info$Nombre

row.names(TOTAL_MOR)  = c(kanales$Etiqueta,'Total')
row.names(TOTAL_NMOR) = c(kanales$Etiqueta,'Total')
row.names(TOTAL_TOT)  = c(kanales$Etiqueta,'Total')

###############################################################################
# ejecucion per se
for(sujeto in 1:n.participantes){
  source('~/TESIS/TESIS/img_ejemplos/acomodar_porcentajes04.R')
}

if(guardar_archivo){
  setwd(g_dir)

  write.xlsx(TOTAL_MOR,sheetName ='total_MOR',
             file=nombre_archivo,append=F)
  write.xlsx(TOTAL_NMOR,sheetName='total_NMOR',
             file=nombre_archivo,append=T)
  write.xlsx(TOTAL_TOT,sheetName ='total_TODO',
             file=nombre_archivo,append=T)
  
  write.xlsx(PORCE_MOR,sheetName ='porcentaje_MOR',
             file=nombre_archivo,append=T)
  write.xlsx(PORCE_NMOR,sheetName='porcentaje_NMOR',
             file=nombre_archivo,append=T)
  write.xlsx(PORCE_TOT,sheetName ='porcentaje_TODO',
             file=nombre_archivo,append=T)
}