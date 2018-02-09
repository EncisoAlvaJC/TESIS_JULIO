###############################################################################
# parametros del script
porcent = T     # cantidad total o proporcional
escala  = F     # el porcentaje se grafica entre 0 y 1

graf_individuales = T
grabar_individual = F

###############################################################################
# parametros que dependen de los datos
frecuencia   = info$Fr_muestreo[sujeto]
nombre_corto = info$Nombre[sujeto]
nombre       = info$Nombre_archivo[sujeto]
nom_dir      = info$Nombre_directorio[sujeto]

# directorios de trabajo
d_dir   = data_dir
r_dir   = g_dir

###############################################################################
# cargar las epocas MOR
ar_indice = read_excel(paste0(info_dir,'/info_tecnico.xlsx'),
                       sheet='EpocasTesis')
indice    = ar_indice[,nombre_corto]
indice    = as.numeric(unlist(indice))
indice    = indice[!is.na(indice)]

epo_s_min = 2
if(frecuencia==200){
  # 30 s  : 3 epocas en 1 bloque
  indixe = ceiling(indice/3)
  
  indixe = unique(indixe)
  indixe = sort(indixe)
  
  indice = indixe
}

###############################################################################
# contenedores de los datos
RES_T   = c()
RES_TIR = c()
max_epo = c()

setwd(d_dir)
#################################################
# inicia ciclo que recorre canales
for(ch in 1:n.canales){
  # cargar los datos
  canal    = kanales$Nombre_archivo[ch]
  ar_t     = paste0('EST_',nombre,'_',canal,'_T.txt'  )
  pv_t_pre = scan( ar_t)
  pv_t     = as.numeric(unlist(pv_t_pre))
  
  # pone los datos en una matriz
  RES_T   = do.call(rbind,list(RES_T  ,pv_t ))
  max_epo = append(max_epo,length(pv_t))
}
# fin ciclo que recorre canales
#################################################

#################################################
# proteccion contra NA
for(ii in 1:22){
  for(jj in 1:length(RES_T[1,])){
    if(is.na(RES_T[ii,jj])){
      RES_T[ii,jj] = 0
    }
  }
}

#################################################
# variables auxiliares
IND_T = 1:min(max_epo)
n.epo = length(IND_T)

if(porcent){
  tag = 'porcentaje'
}
if(!porcent){
  tag = 'total'
}

# numero total de epocas y numero de epocas MOR
#print(paste0('Total : ', toString(n.epo)))
#print(paste0('  MOR : ', toString(length(indice) )))

#################################################
# seleccionar solo algunas epocas
mor   = indice
n.mor = setdiff(1:n.epo,mor)

# contenedores de resultado
res_tot  = rep(0,22)
res_mor  = rep(0,22)
res_nmor = rep(0,22)

# conteo
for(ch in 1:22){
  res_tot[ch]  = sum((RES_T[ch,]     >p.val)*1)
  res_nmor[ch] = sum((RES_T[ch,n.mor]>p.val)*1)
  res_mor[ch]  = sum((RES_T[ch,mor]  >p.val)*1)
}
for(ch in 1:22){
  TOTAL_MOR[ch,sujeto]  = res_mor[ch]
  TOTAL_NMOR[ch,sujeto] = res_nmor[ch]
  TOTAL_TOT[ch,sujeto]  = res_tot[ch]
}
TOTAL_MOR[23,sujeto]  = length(mor)
TOTAL_NMOR[23,sujeto] = length(n.mor)
TOTAL_TOT[23,sujeto]  = length(RES_T[1,])

#################################################
# proporcion o total
if(porcent){
  res_tot  =  res_tot/length(RES_T[1,])
  res_nmor = res_nmor/length(n.mor)
  res_mor  =  res_mor/length(mor)
}

for(ch in 1:22){
  PORCE_MOR[ch,sujeto]  = res_mor[ch]
  PORCE_NMOR[ch,sujeto] = res_nmor[ch]
  PORCE_TOT[ch,sujeto]  = res_tot[ch]
}

#################################################
# inicia grafico 1
if(graf_individuales){
  if(grabar_individual){
    setwd(g_dir)
    pdf(
      #png(
      paste0(nombre,'_',toString(length(indice)),         
             '_',toString(n.epo),
             '_',toString(100*p.val),
             '_bar',
             '_',tag,
             '.pdf'),width=12,height=6)
    #'.png'),units='in',res=150,width=12,height=6)
  }
  
  # grafico per se
  ress = rbind(res_tot,res_nmor,res_mor)
  
  # fijar el maximo del grafico
  max_r = (max(ress))
  if(escala){
    max_r = 1
  }
  
  # el mensaje cambia si es total o proporcion
  if(porcent){
    yl = 'Proporcion'
  }
  if(!porcent){
    yl = 'Total'
  }
  
  # se grafican las cantidades
  barplot(ress,space=c(0,.5),
          ylim=c(0,max_r),
          names.arg=channel,
          col=c('black','gray','green'),
          #col=c('black','green'),
          border=NA,xpd=F,beside=T,
          main=paste0(nombre_abreviado,' , *=',
                      toString(p.val)),
          ylab=paste0(yl,' de epocas estacionarias'))
  
  legend('topleft',
         c('Total','no-MOR','MOR'),
         fill=c('black','gray','green'),
         #c('Total','MOR'),
         #fill=c('black','green'),
         bty='o',y.intersp=1,
         xjust=1,yjust=0,
         cex=1)
  
  if(grabar){
    setwd(g_dir)
    dev.off()
  }
}
# fin grafico 1
#################################################