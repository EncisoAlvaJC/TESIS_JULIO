#################################################
# directorios
dir_datos = '~/TESIS/graf_datos/estacionariedad_sinfiltro/'
dir_graf  = '~/TESIS/TESIS/img_resultados'
dir_epocas  = '~/TESIS/graf_datos/epocas3/'

#################################################
# parametros
# sujeto = 6

grabar      = T
duraciones  = 30*(2**c(-5:2))
p.val       = c(.05)

reemplazar  = TRUE
canales     = 'PSG'

#################################################
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
              'GH24031950SUEÑO',
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
frecuenciasss = c(200,
                  512,512,
                  200,200,
                  512,512,
                  200,
                  512,512,
                  512,512)
grupo_de = c(0,0,0,0,0,1,1,1,1,-1,-1,-1)
#grupo_de = c(-1,0,0,-1,-1,1,1,-1,-1,-1,-1,-1)
#grupo_de =  c(0,-1,-1,0,0,-1,-1,1,1,-1,-1,-1)
#grupo_de = c(1,1,1,1,1,1,1,1,1,-1,0,0)

vec_colores = c(rgb(132,120, 32,maxColorValue=255),  # C3
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

#################################################
# procesamiento parametros opcionales
if(reemplazar){
  if(canales=='10-20'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','T3','T4','T5','T6')
  }
  if(canales=='PSG'){
    canales = c('C3','C4','CZ','F3','F4','F7','F8','FP1','FP2','FZ','O1','O2',
                'P3','P4','PZ','T3','T4','T5','T6','LOG','ROG','EMG')
  }
}

#################################################
# parametros dependientes del sujeto
#sujeto = 6
nombre      = nomb_arch[sujeto]
etiqueta    = nomb_facil[sujeto]
dir_res_mid = paste0(dir_datos,nomb_dir[sujeto])
fr_muestreo = frecuenciasss[sujeto]

#################################################
# paramtros que dependen de los datos
n.dur       = length(duraciones)
P           = length(p.val)
setwd(dir_res_mid)

#################################################
# cargar epocas
setwd(dir_epocas)
arch_indice_e = paste0('epocas_mor_',nombre,'.txt')
indice_e      = scan(arch_indice_e)
if(fr_muestreo==200){
  indice_e = sort(unique(ceiling(indice_e/3)))
}

#################################################
# inicia grafico
k = 1.5
setwd(dir_graf)
if(grabar){
  setwd(dir_graf)
  pdf(
  #png(
    paste0(nombre,'_cabeza_epocas_',
           '.pdf'),width=6*k,height=6*k)
           #'.png'),units='in',res=300,width=6*k,height=6*k)
}

#################################################
# ajustes graficos
#require(png)
#cab.ar = readPNG('~/TESIS/TESIS/img_resultados/cabeza_fondo.png')
#cab    = as.raster((cab.ar))
#par(oma=c(0,0,0,0),
#    mar=c(0,0,0,0))
#plot(0,type='n',xlim=c(0,1),ylab=c(0,1),xaxt='n',yaxt='n',bty='n')
#rasterImage(cab.ar,0,-1,1,1)

par(oma=c(0,0,0,0),
    mar=c(0,0,0,0),
    bg = 'white')

dx = .9/5
dy = .9/6

x0 = .05
y0 = .025

xc = c(2,4,3,2,4,1,5,2.5,3.5,3,2.5,3.5,2,4,3,1,5,1,5,1.5,4.5,  3)
yc = c(3,3,3,4,4,4,4,  5,  5,4,  1,  1,2,2,2,3,3,2,2,6.3,6.3,6.4)

#################################################
# inicia ciclo recorre canales
for(ch in 1:22){
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
    indice_k = indice_e
    if(f.escala<1){
      indice_k = sort(unique(ceiling(indice_e*f.escala)))
    }
    if(f.escala>1){
      k = matrix(nrow=f.escala,ncol=length(indice_e))
      for(i in 1:f.escala){
        for(j in 1:length(indice_e)){
          k[i,j] = indice_e[j]*f.escala - i + 1
        }
      }
    }
    indice_k = sort(unique(as.numeric(k)))
    
    canal  = canales[ch]
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
  
  par(fig=c(x0+dx*(xc[ch]-.8),x0+dx*(xc[ch]-.1),
            y0+dy*(yc[ch]-.8),y0+dy*(yc[ch]-.1)), new=TRUE)
  par(cex.axis=0.5,
      mgp=c(0,.5,0))
  
  plot(0,type='n',
       ylim=c(0,100),xlim=log(c(min(duraciones),max(duraciones))),
       xlab='',ylab='',xaxt='n',yaxt='n',main='',bty='n')
  
  axis(2,at=20*(0:5),las=2)
  axis(2,at=20*(0:5),las=2,labels=F)
  for(i in 20*(0:5)){
    abline(h=i,col='gray')
  }
  
  for(pp in 1:P){
    lines(log(duraciones),100*porcentajes[,pp],type='o',
          pch=15,lty=2,col=vec_colores[ch])
    lines(log(duraciones),100*porcentajes_mor[,pp],type='o',
          lty=3,col=vec_colores[ch])
  }
  
  
  numeritos = rep('no',n.dur)
  for(i in 1:n.dur){
    d_txt = round(10*duraciones[i])/10
    numeritos[i] = toString(d_txt)
  }
  axis(1,at=log(duraciones),label=numeritos,las = 2)
  axis(1,at=log(duraciones),label=F)
  
  legend(x='topright',bty = 'n',legend = canal,col='white')
  
  #}
}
# fin ciclo recorre canales
#################################################

par(fig=c(x0+dx*(1.5-.8),x0+dx*(1.5-.1),
          y0+dy*(1-.8),y0+dy*(1-.1)), new=TRUE,
    oma=c(0,0,0,0))
par(cex.axis=0.5,#cex.lab=0.5,
    mgp=c(0,.5,0))
plot(0,type='n',
     ylim=c(0,100),xlim=log(c(min(duraciones),max(duraciones))),
     #xlab='Duración época [s]',
     #ylab='Épocas\n estacionarias [%]',xaxt='n',yaxt='n',main=''
     xlab='',ylab='',xaxt='n',yaxt='n',bty='n'
     )
#axis(2,lab='Épocas\n est. [%]',las=3,labels=F)
legend('topright',legend=c('NMOR','MOR'),lty=c(2,3),
       bty='n',lwd=2,pch=c(15,1),col='blue')


setwd(dir_graf)
if(grabar){
  setwd(dir_graf)
  dev.off()
}
# fin grafico
#################################################