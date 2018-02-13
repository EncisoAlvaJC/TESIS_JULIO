#################################################
# parametros dependientes de los datos
n_canales = length(kanales$Etiqueta)
ventana   = dur_epoca*fr_muestreo

#################################################
# procesamiento parametros opcionales (ajuste)
if(unidad_par_t =='tiempo'){
  ini_t   = hms2t(ajuste_ini_hms)
  ini_epo = ini_t/dur_epoca
  ini_pt  = floor(ini_t*fr_muestreo)
}
if(unidad_par_t =='puntos'){
  ini_epo = ajuste_ini_epo
  ini_t   = ini_epo*dur_epoca
  ini_pt  = ini_epo*ventana
}
str_t   = 0
str_epo = 1
str_pt  = 1

# ini : cuando inicia el archivo, util si es un fragmento
# str : cuando inicia el zoom, evita la confusion provocada por ini

min_e = 1

# procesamiento parametros opcionales (zoom)
if(zoom){
  confirma_zoom = FALSE
  if(unidad_par_t == 'tiempo'){
    min_t  = hms2t(min_hms) -ini_t
    max_t  = hms2t(max_hms) -ini_t
    
    min_e  = floor((min_t+ini_t)/dur_epoca -ini_epo)
    max_e  = ceiling((max_t+ini_t)/dur_epoca -ini_epo)
    
    confirma_zoom = TRUE
  }
  if(unidad_par_t == 'puntos'){
    min_t = min_epo*dur_epoca -ini_t
    max_t = max_epo*dur_epoca -ini_t
    
    min_e = floor((min_epo+ini_t) -ini_epo)
    max_e = ceiling((max_epo+ini_t) -ini_epo)
    
    confirma_zoom = TRUE
  }
  if(!confirma_zoom){
    warning('WARNING: Indique unidad de tiempo para zoom (epocas o segundos)')
  }
  min_pt = floor(min_e*ventana)
  max_pt = ceiling(max_e*ventana)
  
  str_t   = max(min_t, 0)
  str_epo = max(min_e, 1)
  str_pt  = max(min_pt,1)
}

#################################################
# optimizacion: lee el tamano de los datos el contenedor
setwd(dir_res_mid)
ch        = 1
ch_actual = kanales$Nombre_archivo[ch]
nom_arch  = paste0('SP_INT_',nombre,'_',ch_actual,'_SUB.txt')
pv_t      = scan(nom_arch)
n_epocas  = length(pv_t)
#max_e     = n_epocas

lab_epo_ini = ini_epo
lab_epo_fin = ini_epo + length(pv_t)*dur_epoca/dur_epoca

# ajustes en el tiempo

if(zoom){
  end_t    = min(max_t, n_epocas*dur_epoca)
  end_epo  = min(max_e, n_epocas)
  end_pt   = min(max_pt,n_epocas)
  
  ini_t    = ini_t   + str_t
  ini_epo  = ini_epo + str_epo
  ini_pt   = ini_pt  + str_pt
  n_epocas = length(str_epo:end_epo)
} 
# end : cuando termina el zoom, evita la confusion provocada por ini

#################################################

# contenedores de los datos
RES = matrix(nrow=n_canales,ncol=n_epocas)

#################################################
# inicio ciclo que recorre todos los canales
for(ch in 1:n_canales){
  # cargar los datos
  ch_actual = kanales$Nombre_archivo[ch]
  nom_arch  = nom_arch  = paste0('SP_INT_',nombre,'_',ch_actual,'_',
                                 banda[que.banda],'.txt')
  pv_t      = scan(nom_arch)
  nom_arch  = nom_arch  = paste0('SP_INT_',nombre,'_',ch_actual,'_',
                                 banda[nbandas-2],'.txt')
  norm      = scan(nom_arch)
  pv_t      = pv_t/norm
  
  if(no_relativo){
    nom_arch  = nom_arch  = paste0('VAR_',nombre,'_',
                                   ch_actual,'.txt')
    norm = scan(nom_arch)
    
    norm = pv_t*norm
    
    norm = log(norm)
    summary(norm)
    
    for(re in 1:15){
      mm = mean(norm,na.rm = T)
      de = 3*sd(norm,na.rm = T)
      
      norm[norm>(mm+de)] = mm+de
      norm[norm<(mm-de)] = mm-de
    }
    norm[is.na(norm)] = mm+de
    #norm = exp(norm)
    #norm = log(norm)
    #pv_t      = pv_t*norm
    pv_t      = norm
  }
  
  if(zoom){
    pv_t    = pv_t[min_e:max_e]
  }
  
  # organizacion de los datos en una matriz
  RES[ch,] = pv_t[1:n_epocas]
}
# fin ciclo que recorre canales
#################################################

#################################################
# inicio grafico
RES = as.data.frame(t(RES))
colnames(RES) = kanales$Etiqueta
RES$Indice = 1:length(RES[,1]) + min_t/dur_epoca -1

RES.largo = melt(RES,id='Indice')
colnames(RES.largo) = c('Indice','Canal_var','Potencia')

RES.largo$Indice = (RES.largo$Indice)*dur_epoca
RES.largo$Indice = as.POSIXct(as.hms(RES.largo$Indice))

RES.largo$Banda.nombre = rep(banda.n[que.banda],length(RES.largo$Indice))

RES.collect = rbind(RES.collect,RES.largo)

#################################################
# inicio grafico
if(FALSE){
  ggplot(RES.largo,aes(x=Indice,y=Canal_var,fill=Potencia)) +
    geom_raster(hjust = 1) +
    xlab('Tiempo [hh:mm]') + ylab(NULL) +
    theme_bw() +
    scale_x_datetime(expand=c(0,0),labels=date_format("%H:%M:%S"),
                     breaks = date_breaks("1 sec"))+
    scale_y_discrete(expand=c(0,0),
                     limits=rev(levels(RES.largo$Canal_var))) +
    scale_fill_distiller(palette='Spectral')+
    labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
    labs(subtitle=paste('Potencia total en banda:',banda.n[que.banda])) +
    theme(legend.position='bottom') +
    theme(legend.title=element_blank()) +
    #facet_grid(.) +
    rotate_x_text(angle = 45)
  
  ggsave(filename = paste0(etiqueta,'_bandaPROP_',
                           banda[que.banda],'.png'),
         path=dir_actual,device='png',
         units='in',res=600,width=8,height=1.5)
}
