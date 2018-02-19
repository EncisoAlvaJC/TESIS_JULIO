#################################################
# directorios
dir_datos  = '~/TESIS/graf_datos/estacionariedad_sf/'
dir_graf   = '~/TESIS/TESIS_JULIO/scripts_graf_res'
info_dir   = '~/TESIS/TESIS_JULIO/scripts_graf'

#################################################
# parametros

duraciones  = 30*(2**c(-5:3))
p.val       = c(.05)

orden_stam  = T

###############################################################################
# librerias
require('readxl')

require('ggplot2')
require('ggpubr')

require('Rmisc')
require('reshape')

require('scales')

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

#################################################
# parametro grafico
c.fp = rgb(170, 68,136,maxColorValue=255)
c.ff = rgb(170, 68, 85,maxColorValue=255)
c.tt = rgb(170,170, 68,maxColorValue=255)
c.cc = rgb(170,119, 68,maxColorValue=255)
c.pp = rgb( 68,119,170,maxColorValue=255)
c.oo = rgb( 68,170,170,maxColorValue=255)
c.ll = rgb( 17, 68,119,maxColorValue=255)
c.ee = rgb(128,  0,  0,maxColorValue=255)

vec_colores = c(c.fp,c.fp,  # FP2 FP2
                c.ff,c.ff,  # F8  F7
                c.ff,c.ff,  # F4  F3
                c.tt,c.tt,  # T4  T3
                c.cc,c.cc,  # C4  C3
                c.tt,c.tt,  # T6  T5
                c.pp,c.pp,  # P4  P3
                c.oo,c.oo,  # O2  01
                c.ff,c.cc,c.pp, # FZ  CZ PZ
                c.ll,c.ll, # LOG ROG
                c.ee  # EMG
)

#################################################
# parametros dependientes del sujeto
nombre      = info$Nombre_archivo[sujeto]
etiqueta    = info$Nombre[sujeto]
dir_res_mid = dir_datos
fr_muestreo = info$Fr_muestreo[sujeto]
grupo.n     = info$Grupo_n[sujeto]

if(grupo.n==0){
  grupo = 'CTL'
}
if(grupo.n==1){
  grupo = 'PDC'
}
if(grupo.n==-1){
  grupo = 'EX'
}

#################################################
# paramtros que dependen de los datos
n.dur       = length(duraciones)
P           = length(p.val)
setwd(dir_res_mid)

#################################################
# cargar epocas suenno MOR
ar_indice = read_excel(paste0(info_dir,'/info_tecnico.xlsx'),
                       sheet='EpocasTesis')
indice    = ar_indice[,etiqueta]
indice    = as.numeric(unlist(indice))
indice    = indice[!is.na(indice)]

if(fr_muestreo==200){
  indixe = ceiling(indice/3)
  indixe = unique(indixe)
  indixe = sort(indixe)
  indice = indixe
}

GR = c()

#################################################
# inicia ciclo recorre canales
for(ch in 1:n.canales){
  # contenedor de datos
  porcentajes     = matrix(0,nrow=n.dur,ncol=P)
  porcentajes_mor = matrix(0,nrow=n.dur,ncol=P)
  
  #################################################
  # inicia ciclo que recorre tamanos de epoca
  setwd(dir_res_mid)
  
  for(d in 1:n.dur){
    dur_epoca = duraciones[d]
    f.escala  = 30/dur_epoca
    
    # seleccion de epoca MOR
    indice_k = indice
    if(f.escala<1){
      indice_k = sort(unique(ceiling(indice*f.escala)))
    }
    if(f.escala>1){
      k = matrix(nrow=f.escala,ncol=length(indice))
      for(i in 1:f.escala){
        for(j in 1:length(indice)){
          k[i,j] = indice[j]*f.escala - i + 1
        }
      }
      indice_k = sort(unique(as.numeric(k)))
    }
    
    canal  = kanales$Nombre_archivo[ch]
    ar_t   = paste0('EST_',nombre,'_',canal,
                    '_T_',toString(dur_epoca),'.txt'  )
    pv_t   = scan(ar_t)
    ar_tir = paste0('EST_',nombre,'_',canal,
                    '_TIR_',toString(dur_epoca),'.txt')
    pv_tir = scan(ar_tir)
    
    # se retiran las epocas no-clasificadas
    ok_t   = length(pv_t  )
    ok_tir = length(pv_tir)
    for(i in 1:length(pv_t)){
      if(is.na(pv_t[i])){
        pv_t[i] = 0
        ok_t    = ok_t-1
      }
    }
    for(i in 1:length(pv_tir)){
      if(is.na(pv_tir[i])){
        pv_tir[i] = 0
        ok_tir    = ok_tir-1
      }
    }
    ok = min(ok_t,ok_tir)
    ok = max(ok,1)
    
    for(pp in 1:P){
      porcentajes[d,pp] = sum(pmax((pv_t  >p.val[pp])*1,
                                   (pv_tir>p.val[pp])*1))/ok
    }
    
    # el proceso se repite para epocas mor
    pv_t   =   pv_t[indice_k]
    pv_tir = pv_tir[indice_k]
    # se retiran las epocas no-clasificadas
    ok_t   = length(pv_t  )
    ok_tir = length(pv_tir)
    for(i in 1:length(pv_t)){
      if(is.na(pv_t[i])){
        pv_t[i] = 0
        ok_t    = ok_t-1
      }
    }
    for(i in 1:length(pv_tir)){
      if(is.na(pv_tir[i])){
        pv_tir[i] = 0
        ok_tir    = ok_tir-1
      }
    }
    ok = min(ok_t,ok_tir)
    ok = max(ok,1)
    
    for(pp in 1:P){
      porcentajes_mor[d,pp] = sum(pmax((pv_t  >p.val[pp])*1,
                                       (pv_tir>p.val[pp])*1))/ok
    }
  }
  # fin ciclo recorre tamnos de epoca
  #################################################
  
  porc = cbind(rbind(porcentajes,porcentajes_mor),
               c(rep(0,n.dur),rep(1,n.dur)),
               rep(1:n.dur,2),
               rep(ch,2*n.dur))
  
  GR = rbind(GR,porc)
}
# fin ciclo recorre canales
#################################################

GR = as.data.frame(GR)
colnames(GR) = c('Porcentaje','Etapa','D_epoca','Canal_var')

GR$xx = kanales$x[GR$Canal_var]
GR$yy = 5-kanales$y[GR$Canal_var]

GR$Canal_var = factor(GR$Canal_var,labels=kanales$Etiqueta )
GR$Etapa = factor(GR$Etapa,labels=c('NMOR + Vigilia','MOR'))

ggplot(GR,aes(x=duraciones[D_epoca]/30,y=100*Porcentaje,
              linetype=Etapa,shape=Etapa,
              color=Canal_var))+
  scale_color_manual(values=vec_colores)+
  ylab('Épocas estacionarias [%]') + xlab('Tamaño de ventana / 30 [s]') +
  labs(title=paste0('Participante: ',etiqueta,' | Grupo: ',grupo))+
  facet_grid(yy~xx)+
  expand_limits(y=c(0,100))+
  scale_x_continuous(trans = 'log2',
                     breaks = trans_breaks('log2',inv = function(x) 2**x ,
                                           n=n.dur), 
                     labels=trans_format('log2',
                                         math_format(2^.x)))+
  theme_bw()+
  theme(strip.text.x = element_blank(),strip.background = element_blank())+
  theme(strip.text.y = element_blank())+
  geom_line()+
  geom_point()+
  guides(color=FALSE)+
  geom_text(aes(label=Canal_var,x=5,y=80),color='gray')+
  rotate_x_text(angle = 45)+
  theme(legend.position='bottom')

k = 1.2
ggsave(file=paste0(nombre,'_cabeza_epocas_v2.pdf'),device='pdf',
       width=6,height=6,scale=k,path=dir_graf)

# fin grafico
#################################################