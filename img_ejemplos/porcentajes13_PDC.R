#################################################
# parametros del script
porcent = T     # proporcion o tiempo
escala  = F     # se grafica entre 0 y 1

#################################################
# parametros para retirar
binario = T   # contraste acepta/rechaza estacionariedad

#################################################
# parametros del sujeto
frecuencia       = frecuenciasss[sujeto]
nombre_abreviado = nomb_facil[sujeto]
nombre           = nomb_arch[sujeto]
nom_dir          = nomb_dir[sujeto]
d_dir            = paste0(data_dir,nom_dir)

#################################################
# carga las epocas
setwd(e_dir)
ar_indice = paste0('epocas_mor_',nombre,'.txt')
indice    = scan(ar_indice)

if(frecuencia==512){
  epo_s_min = 2
}
if(frecuencia==200){
  indixe = ceiling(indice/3)
  indixe = unique(indixe)
  indixe = sort(indixe)
  indice = indixe
  
  epo_s_min = 2
}

#################################################
# cargar los datos
RES_T    = c()
RES_TIR  = c()
max_epo  = rep(0,n.canales)

setwd(d_dir)
for(ch in 1:22){
  canal  = channel[ch]
  ar_t   = paste0('EST_',nombre,'_',canal,'_T.txt'  )
  pv_t   = scan(ar_t)
  ar_tir = paste0('EST_',nombre,'_',canal,'_TIR.txt')
  pv_tir = scan(ar_tir)
  
  # datos en una matriz
  RES_T   = do.call(rbind,list(RES_T  ,pv_t  ))
  RES_TIR = do.call(rbind,list(RES_TIR,pv_tir))
  max_epo[ch] = length(pv_t)
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
mor   = indice
n.mor = setdiff(1:n.epo,mor)

#################################################
# proteccion contra NA
buen_t   = matrix(0,nrow=n.canales,ncol=3)
buen_tir = matrix(0,nrow=n.canales,ncol=3)
buen_ok  = matrix(0,nrow=n.canales,ncol=3)

for(ch in 1:n.canales){
  buen_t[ch,1]   = length(RES_T[ch,]  )
  buen_tir[ch,1] = length(RES_TIR[ch,])
  buen_t[ch,2]   = length(n.mor)
  buen_tir[ch,2] = length(n.mor)
  buen_t[ch,3]   = length(  mor)
  buen_tir[ch,3] = length(  mor)
  for(jj in mor){
    if(is.na(RES_T[ch,jj])){
      buen_t[ch,2] = buen_t[ch,2] - 1
    }
    if(is.na(RES_TIR[ch,jj])){
      buen_tir[ch,2]   = buen_tir[ch,2] - 1
    }
  }
  for(jj in n.mor){
    if(is.na(RES_T[ch,jj])){
      buen_t[ch,3] = buen_t[ch,3] - 1
    }
    if(is.na(RES_TIR[ch,jj])){
      buen_tir[ch,3]   = buen_tir[ch,3] - 1
    }
  }
  for(jj in 1:length(RES_T[1,])){
    if(is.na(RES_T[ch,jj])){
      RES_T[ch,jj] = 0
      buen_t[ch,1] = buen_t[ch,1] - 1
    }
  }
  for(jj in 1:length(RES_TIR[1,])){
    if(is.na(RES_TIR[ch,jj])){
      RES_TIR[ch,jj] = 0
      buen_tir[ch,1]   = buen_tir[ch,1] - 1
    }
  }
}
buen_ok = pmin(buen_t,buen_tir)
buen_ok = pmax(buen_ok,rep(1,n.canales))

#################################################
# seleccion epocas MOR
# contenedores de resultado
res_tot  = rep(0,22)
res_mor  = rep(0,22)
res_nmor = rep(0,22)
significados = rep(0,22)

# conteo y comparacion
for(ch in 1:22){
  res_tot[ch]  = sum(pmax((RES_T[ch,]  >p.val)*1,
                          (RES_TIR[ch,]>p.val)*1))
  res_nmor[ch] = sum(pmax((RES_T[ch,n.mor]  >p.val)*1,
                          (RES_TIR[ch,n.mor]>p.val)*1))
  res_mor[ch]  = sum(pmax((RES_T[ch,mor]  >p.val)*1,
                          (RES_TIR[ch,mor]>p.val)*1))
  
  tu = prop.test(x=c(res_tot[ch],res_mor[ch]),
                 n=c(length(RES_T[ch,]),length(mor)),
                 correct=T)
  significados[ch] = as.numeric(tu['p.value'])
  if(is.nan(significados[ch])){
    significados[ch] = 1
  }
}

if(porcent){
  res_tot  =   res_tot/buen_ok[,1]
  res_nmor =  res_nmor/buen_ok[,2]
  res_mor  =   res_mor/buen_ok[,3]
}

# matriz con todos los datos
ress = rbind(res_tot,res_nmor,res_mor)
#ress = rbind(res_tot,res_mor)
#ress = t(ress)

#################################################
# inicia grafico
if(graf.indv){
  if(grabar.indv){
    setwd(g_dir)
    pdf(
      #png(
      paste0(nomb_facil[sujeto],'_',
             toString(length(indice)),
             #toString(100*p.val),
             '_',tag,
             '.pdf'),width=10.3/1.5,height=8/1.5)
             #'.png'),units='in',res=150,width=12,height=6)
  }
  
  #################################################
  # parametros graficos
  max_r = (max(ress))
  if(escala){
    max_r = 1
  }
  if(porcent){
    yl = 'Proporcion'
  }
  if(!porcent){
    yl = 'Total'
  }
  MINI = floor(20*min(c(ress[1,],ress[3,])))/20
  MAXI = ceiling(20*max(c(ress[1,],ress[3,])))/20
  
  #################################################
  # graficacion per se
  par(mar=c(3,4,2,0))
  plot(100*ress[2,],xaxt='n',
       ylim=100*c(MINI,MAXI),
       xlab='',ylab='% épocas estacionarias',
       main=paste0('Sujeto : ',nombre_abreviado),
       type='l',col='black',lwd=1,las=2,bty='n',lty=2)
  lines(100*ress[2,],xaxt='n',
        ylim=c(MINI,MAXI),
        type='p',col='black',pch=16)
  lines(100*ress[3,],xaxt='n',
        ylim=c(MINI,MAXI),
        type='l',col='green4',lwd=1)
  lines(100*ress[3,],xaxt='n',
        ylim=c(MINI,MAXI),
        type='p',col='green4',pch=18)
  axis(1,at=1:22,labels=(channel),las=2,
       tick=T)
  legend('topleft',
         legend=c('NMOR','MOR'),
         col=c('black','green4'),
         lty=1,lwd=2,cex=1,bty='n')
  
  #################################################
  # asteriscos de significancia
  strst=(MAXI-MINI)/40
  suma = rep(0,22)
  
  ch_lin = 1:22
  equis  = (significados<.05)
  suma   = suma + equis*1
  lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2,col='red')
  equis  = (significados<.01)
  suma   = suma + equis*1
  lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  equis  = (significados<.005)
  suma   = suma + equis*1
  lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
        type='p',pch='*',lwd=0,cex=2)
  
  # guardado automatizado de los resultados
  if(grabar.indv){
    setwd(g_dir)
    dev.off()
  }
}

#################################################
# resultados generales
for(ch in 1:22){
  matriz_mor[sujeto,ch]  = ress[3,ch]
  matriz_nmor[sujeto,ch] = ress[2,ch]
  matriz_tot[sujeto,ch]  = ress[1,ch]
  dif_significativas[ch,sujeto] = ast[suma[ch]+1]
}

