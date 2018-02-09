###############################################################################
# carpeta central
data_dir    = '~/TESIS/graf_datos/estacionariedad_sinfiltro/'
central_dir = '~/TESIS/TESIS/img_ejemplos'
g_dir       = central_dir
setwd(central_dir)

p.val = .05

guardar_archivo = F
nombre_archivo  = 'porcentajes.xlsx'

###############################################################################
# datos generales
nomb_dir  = c('VCNNS',
              'MJNNVIGILOScCanal',
              'JANASUE_revisado',
              'GH',
              'GURM_revisado',
              'CLMN10SUE',
              'RLMN',
              'RRMNS_2',
              'JGMN6SUE',
              'FGH_EEGdescompuesto',
              'MGNA',
              'EMNN')
nomb_arch = c('VCNNS1',
              'MJNNVIGILOS',
              'JANASUE',
              'GH24031950SUE?O',
              'GURM251148SUE',
              'CLMN10SUE',
              'RLMN10SUE',
              'RRMNS',
              'JGMN6SUE',
              'FGHSUE',
              'MGNA5SUE',
              'EMNNS')
nomb_facil = c('VCR',
               'MJH',
               'JAE',
               'GHA',
               'MFGR',
               'CLO',
               'RLO',
               'RRU',
               'JGZ', 
               'FGH',
               'MGG',
               'EMT')
channel   = c('C3','C4','CZ',
              'F3','F4','F7','F8',
              'FP1','FP2','FZ',
              'O1','O2','P3','P4','PZ',
              'T3','T4','T5','T6',
              'LOG','ROG',
              'EMG')
frecuenciasss = c(200,
                  512,512,
                  200,
                  200,#segun valeria GUR=200 Hz
                  #512, #segun la libreta GURM=512
                  512,512,
                  200,#solo tiene 3 horas
                  512,
                  512,512,
                  200)
grupo_de = c(0,0,0,0,0,1,1,1,1,-1,-1,-1)
#grupo_de = c(-1,0,0,-1,-1,1,1,-1,-1,-1,-1,-1)
#grupo_de =  c(0,-1,-1,0,0,-1,-1,1,1,-1,-1,-1)
#grupo_de = c(1,1,1,1,1,1,1,1,1,-1,0,0)

###############################################################################
# para grabar en formato de excel
require(xlsx)

# archivo contenedor
matriz = matrix(nrow=2,ncol=2)
matriz[1,1]=0
matriz[1,2]=0
matriz[2,1]=0
matriz[2,2]=0
rownames(matriz)=NULL
colnames(matriz)=NULL
#write.xlsx(matriz,file='porcentajes')

# contenedores de datos
PORCE_MOR  = matrix(nrow=22,ncol=12)
PORCE_NMOR = matrix(nrow=22,ncol=12)
PORCE_TOT  = matrix(nrow=22,ncol=12)

TOTAL_MOR  = matrix(nrow=23,ncol=12)
TOTAL_NMOR = matrix(nrow=23,ncol=12)
TOTAL_TOT  = matrix(nrow=23,ncol=12)

colnames(PORCE_MOR)  = nomb_facil
colnames(PORCE_NMOR) = nomb_facil
colnames(PORCE_TOT)  = nomb_facil

row.names(PORCE_MOR)  = channel
row.names(PORCE_NMOR) = channel
row.names(PORCE_TOT)  = channel

colnames(TOTAL_MOR)  = nomb_facil
colnames(TOTAL_NMOR) = nomb_facil
colnames(TOTAL_TOT)  = nomb_facil

row.names(TOTAL_MOR)  = c(channel,'Total')
row.names(TOTAL_NMOR) = c(channel,'Total')
row.names(TOTAL_TOT)  = c(channel,'Total')

###############################################################################
# ejecucion per se
for(sujeto in 1:12){
  source('~/TESIS/TESIS/img_ejemplos/acomodar_porcentajes02.R')
}

if(guardar_archivo){
  setwd(g_dir)
  write.xlsx(matriz,file=nombre_archivo)
  
  write.xlsx(TOTAL_MOR,sheetName ='total_MOR',
             file=nombre_archivo,append=T)
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