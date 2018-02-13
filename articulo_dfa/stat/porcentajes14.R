#################################################
# parametros del script
porcent = T     # proporcion o tiempo
escala  = F     # se grafica entre 0 y 1

binario = T   # contraste acepta/rechaza estacionariedad

#################################################
# parametros del sujeto
frecuencia       = info$Fr_muestreo[sujeto]
nombre_abreviado = info$Nombre[sujeto]
nombre           = info$Nombre_archivo[sujeto]
nom_dir          = info$Nombre_directorio[sujeto]
d_dir            = data_dir

#################################################
# carga las epocas
#ar_indice = read_excel(paste0(info_dir,'/info_tecnico.xlsx'),
#                       sheet='Epocas_todas')
ar_indice = read_excel(paste0(info_dir,'/info_tecnico.xlsx'),
                       sheet='EpocasTesis')
indice    = ar_indice[,nombre_abreviado]
indice    = as.numeric(unlist(indice))
indice    = indice[!is.na(indice)]

epo_s_min = 2
if(frecuencia==200){
  indixe = ceiling(indice/3)
  indixe = unique(indixe)
  indixe = sort(indixe)
  indice = indixe
}

factor.escala = 30/dur_chunk
if(factor.escala<1){
  indixe = ceiling(indixe*factor.escala)
  indixe = unique(indixe)
  indixe = sort(indixe)
  indice = indixe
}
if(factor.escala>1){
  indixe = c()
  for(k in 1:factor.escala){
    indixe = c(indixe,indice*factor.escala-k+1)
  }
  indixe = unique(indixe)
  indixe = sort(indixe)
  indice = indixe
}
epo_s_min = 2*factor.escala

#################################################
# cargar los datos
RES_T    = c()
RES_TIR  = c()
max_epo  = rep(0,n.canales)

setwd(d_dir)
for(ch in 1:n.canales){
  #canal  = kanales$Nombre_archivo[ch]
  canal  = orden_k$Nombre_archivo[ch]
  ar_t   = paste0('EST_',nombre,'_',canal,'_T_',
                  toString(dur_chunk),'.txt'  )
  pv_t   = scan(ar_t)
  ar_tir = paste0('EST_',nombre,'_',canal,'_TIR_',
                  toString(dur_chunk),'.txt')
  pv_tir = scan(ar_tir)
  
  # datos en una matriz
  RES_T   = do.call(rbind,list(RES_T  ,pv_t  ))
  RES_TIR = do.call(rbind,list(RES_TIR,pv_tir))
  max_epo[ch] = length(pv_t)
}

#parche
#print(length(indice)*30)
#print(t2hms(length(indice)*30))
#print(length(pv_t)*30)
#print(t2hms(length(pv_t)*30))
#stop('Hoy no necesito el grafico')

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
res_tot  = rep(0,n.canales)
res_mor  = rep(0,n.canales)
res_nmor = rep(0,n.canales)
significados = rep(0,n.canales)

# conteo y comparacion
for(ch in 1:n.canales){
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
#ress = t(rbind(res_tot,res_nmor,res_mor,1:n.canales))
#colnames(ress) = c('Total','NMOR','MOR','Canal_var')
ress           = t(rbind(res_nmor,res_mor,1:n.canales))
colnames(ress) = c('NMOR','MOR','Canal_var')
ress           = as.data.frame(ress)

ress$Canal_var = factor(ress$Canal_var,labels=kanales$Etiqueta)

ress2           = melt(ress,id='Canal_var')
colnames(ress2) = c('Canal_var','Etapa','Porcentaje')

#################################################
# inicia grafico
if(graf.indv){
  #################################################
  # graficacion per se
  p = ggplot(ress2,aes(x=Canal_var,y=Porcentaje*100,group=Etapa,
                       linetype=Etapa,shape=Etapa,colour=Etapa))+
    theme_pubclean() +
    scale_colour_manual(values=c('black','green4')) +
    ylab('Épocas estacionarias [%]') +
    xlab(NULL)+
    labs(title=paste0('Participante : ',nombre_abreviado)) +
    rotate_x_text(angle = 45) + 
    stat_compare_means(label='p.signif',method='wilcox.test',
                       hide.ns=T)+
    geom_point() +
    geom_line()
  print(p)
  if(grabar.indv){
    setwd(g_dir)
    ggsave(filename=paste0(nombre_abreviado,'_',tag,'.pdf'),
           width=10.3/1.5,height=8/1.5,device='pdf')
  }
}

#################################################
# asteriscos de significancia
suma = rep(0,n.canales)

for(i in 1:length(p.ast)){
  suma = suma + 1*(significados<p.ast[i])
}

ress = t(rbind(res_tot,res_nmor,res_mor,1:n.canales))
ress           = as.data.frame(ress)
rownames(ress) = kanales$Etiqueta
colnames(ress) = c('Total','NMOR','MOR','Canal_var')

ress$Canal_var = factor(ress$Canal_var,labels=kanales$Etiqueta)

#################################################
# resultados generales
matriz_mor[,nombre_abreviado]  = ress[,'MOR']
matriz_nmor[,nombre_abreviado] = ress[,'NMOR']
matriz_tot[,nombre_abreviado]  = ress[,'Total']

dif_significativas[,nombre_abreviado] = ast[suma+1]