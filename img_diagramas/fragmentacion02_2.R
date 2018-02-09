#################################################
# parametros
N.base = 175
grabar = T

#################################################
# colores
azul1 = rgb( 60, 49,118,maxColorValue=255)
mora1 = rgb( 95, 40,113,maxColorValue=255)
azul2 = rgb( 40, 83,108,maxColorValue=255)

#################################################
# X es un AR que cambia de orden
setwd("~/TESIS/TESIS/img_diagramas")
set.seed(2017)

N      = N.base*12
e      = rnorm(N+10)
X      = rep(0,N)
for(grupo in 0:11){
  if(grupo-2*floor(grupo/2)>0){
    for(i in (1:(N.base))+grupo*N.base){
      if(i>1){
        X[i] = (X[i-1] + e[i])
      }
    }
  }else{
    for(i in (1:(N.base))+grupo*N.base){
      if(i>1){
        X[i] = (0.65*X[i-1] + e[i])
      }
    }
  }
}

#################################################
# inicia imagen 1
if(grabar){
  pdf(paste0('espectrosA_v2.pdf'),width=5,height=2)
}

#################################################
# parametros graficos, MUY modificados
par(cex.axis=.7, cex.lab=.8, cex.main=1,
    oma=c(0,0,0,0),
    mgp=c(1.75,.7,0),
    mar=c(2.5,2.5,1.5,.25),
    bg='white',
    las=2)

# graficos per se
plot((1:N)-1,X,type='l',col=azul1,
     #xlab='Tiempo [mm:ss]',
     xlab='',
     ylab='Amplitud [mV]',main='Sujeto: ---  |  Canal: ---',
     xlim = c(0,N-1),xaxt='n',
     bty='n')

# etiqueta con letra pequena
par(mgp=c(2,.3,0))
mtext('Tiempo [mm:ss]',side=1,las=1,line=1.5,cex=.8)

#################################################
# etiquetas en el tiempo
te        = ((1:N))*(120/N)
mm        = floor(te/60)
ss        = floor(te - 60*mm)
places    = ceiling(seq(0,N,by=(N)/8))
places[1] = 1
plaquitas = c()
for(y in 1:length(places)){
  i = places[y]
  if(ss[i]==0 || ss[i]==59){
    seg = '00'
  }else{
    seg=toString(ss[i])
  }
  plaquitas = c(plaquitas,
                paste0(toString(mm[i]+7),':',seg))
}
places[1]=0
axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
  dev.off()
}
# termina imagen 1
#################################################

#################################################
# inicia imagen 2
if(grabar){
  pdf(paste0('espectrosB_v2.pdf'),width=3.25,height=2)
}

#################################################
# parametros graficos, MUY modificados
par(cex.axis=.7, cex.lab=.8, cex.main=.8,
    oma=c(0,0,0,0),
    mgp=c(1.75,.7,0),
    mar=c(1.5,1.5,1.5,.25),
    bg='white',
    las=2)

# grafico per se
plot((1:N),X,type='l',col='white',
     #xlab='Tiempo [mm:ss]',
     xlab='',
     #ylab='Amplitud [mV]',
     ylab='',
     #main='Sujeto: ---  |  Canal: --- | d_e = 30s',
     main='Época = 30s',
     xlim = c(0,N-1),xaxt='n',
     bty='n')

# etiqueta menor
par(cex.axis=.6, cex.lab=.8, cex.main=.8)
par(mgp=c(2,.3,0))
#mtext('Tiempo [mm:ss]',side=1,las=1,line=1.5,cex=.8)

# ciclo que pinta los bloques
part = 4
for(i in 0:(part/2-1)){
  if(i>0){
    abline(v=2*i*(N/part),col='gray')
  }
  abline(v=(2*i+1)*(N/part),col='gray')
  
  sub = 2*i*(N/part)
  ind = (sub+1):(sub+N/part+1)
  lines(ind,X[ind],type='l',col=mora1,xlab='',ylab='')
  sub = (2*i+1)*(N/part)
  ind = (sub+1):(sub+N/part+1)
  lines(ind,X[ind],type='l',col=azul2,xlab='',ylab='')
}

# eje en el tiempo (previamente calculado)
axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
  dev.off()
}
# fin de la imagen 2
#################################################

#################################################
# inicia la imagen 3
if(grabar){
  pdf(paste0('espectrosC_v2.pdf'),width=3.25,height=2)
}

#################################################
# parametros graficos, MUY modificados
par(cex.axis=.7, cex.lab=.8, cex.main=.8,
    oma=c(0,0,0,0),
    mgp=c(1.75,.7,0),
    mar=c(1.5,1.5,1.5,.25),
    bg='white',
    las=2)

# grafico per se
plot((1:N),X,type='l',col='white',
     #xlab='Tiempo [mm:ss]',
     xlab='',
     #ylab='Amplitud [mV]',
     ylab='',
     #main='Sujeto: ---  |  Canal: --- | d_e = 30s',
     main='Época = 10s',
     xlim = c(0,N-1),xaxt='n',
     bty='n')

# etiqueta para el tiempo
par(cex.axis=.6, cex.lab=.8, cex.main=.8)
par(mgp=c(2,.3,0))
#mtext('Tiempo [mm:ss]',side=1,las=1,line=1.5,cex=.8)

# ciclo que pinta los bloques
part = 12
for(i in 0:(part/2-1)){
  if(i>0){
    abline(v=2*i*(N/part),col='gray')
  }
  abline(v=(2*i+1)*(N/part),col='gray')
  
  sub = 2*i*(N/part)
  ind = (sub+1):(sub+N/part+1)
  lines(ind,X[ind],type='l',col=mora1,xlab='',ylab='')
  sub = (2*i+1)*(N/part)
  ind = (sub+1):(sub+N/part+1)
  lines(ind,X[ind],type='l',col=azul2,xlab='',ylab='')
}

# eje en el tiempo (previamente calculado)
axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
 dev.off()
}
# fin de la imagen 3
#################################################

#################################################
# inicia la imagen 4
if(grabar){
  pdf(paste0('espectrosD_v2.pdf'),width=3.25,height=1.5)
}

#################################################
# parametros graficos, MUY modificados
par(cex.axis=.7, cex.lab=.8, cex.main=.8,
    oma=c(0,0,0,0),
    mgp=c(1.75,.7,0),
    mar=c(1.5,1.5,1.5,.25),
    bg='white',
    las=2,font.lab=2)

# matriz ejemplo
M = matrix(c(1,1,1,1))
M = t(M)

# grafico per se
require(plotrix)
color2D.matplot(M,axes=F,
               xlab='Tiempo (mm:ss)',
               ylab='',
               main='Sujeto: --- | Época = 30s')

# etiqueta para el canal
par(cex.axis=.6, cex.lab=.9, cex.main=.8,
    mgp=c(.5,.3,0))
axis(2,at=c(0,1),labels=F)
axis(2,at=c(0.5),labels=c('Canal: ---'),tick=F,las=3,font=2)

# etiqueta para el tiempo
axis(1,
    at=(0:(length(plaquitas)-1))*(length(M)/(length(plaquitas)-1)),
    labels=plaquitas,las=1)

if(grabar){
  dev.off()
}
# fin de la imagen 4
#################################################

#################################################
# inicia la imagen 5
if(grabar){
  pdf(paste0('espectrosE_v2.pdf'),width=3.25,height=1.5)
}

#################################################
# parametros graficos, MUY modificados
par(cex.axis=.6, cex.lab=.8, cex.main=.8,
    oma=c(0,0,0,0),
    mgp=c(1.75,.7,0),
    mar=c(1.5,1.5,1.5,.25),
    bg='white',
    las=2,font.lab=2)

# matriz ejemplo
M = matrix(c(0,1,0,1,0,1,0,1,0,1,0,1))
M = t(M)

# grafico per se
require(plotrix)
color2D.matplot(M,axes=F,
               xlab='Tiempo (mm:ss)',
               ylab='',
               main='Sujeto: --- | Época = 10s')

# etiqueta para el canal
par(cex.axis=.6, cex.lab=.9, cex.main=.8,
    mgp=c(.5,.3,0))
axis(2,at=c(0,1),labels=F)
axis(2,at=c(0.5),labels=c('Canal: ---'),tick=F,las=3,font=2)

# etiqueta para el tiempo
axis(1,
    at=(0:(length(plaquitas)-1))*(length(M)/(length(plaquitas)-1)),
    labels=plaquitas,las=1)

if(grabar){
 dev.off()
}
# termina la imagen 5
#################################################