grabar = F

setwd("~/TESIS/TESIS/img_diagramas")

set.seed(2017)

N = 6000

S = 10

P = rpois(N,S)

C = c(rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12),
      rep(0,N/12),rep(1,N/12))

for(i in 1:N){
  if(C[i]>0){
    P[i] = S
  }
}

X = rnorm(N,0,1)*P*(1/S)

if(grabar){
  #setwd(g_dir)
  pdf(paste0('espectrosA',
             '.pdf'),width=10,height=3)
  #'.png'),units='in',res=150,width=12,height=6)
}

te = ((1:N))*(120/N)
mm = floor(te/60)
ss = floor(te - 60*mm)

places = ceiling(seq(0,N,by=(N)/8))
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
                paste0(toString(mm[i]+7),
                                ':',seg))
}

par(mar=c(3,3,2,1))
par(las=2)
plot((1:N)-1,X,type='l',col='black',
     xlab='Tiempo [mm:ss]',
     ylab='mV',main='Sujeto: ---  |  Canal: ---',
     xlim = c(0,N-1),xaxt='n',
     mgp = c(2, 1, 0),bty='n')

places[1]=0
axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectrosB',
             '.pdf'),width=5.5,height=2.5)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(3,3,2,1))
par(las=2)

part = 4

plot((1:N),X,type='l',col='white',
     xlab='Tiempo (mm:ss)',
     ylab='mV',main='Sujeto: ---  |  Canal: ---  (30 s)',
     xlim = c(0,N-1),xaxt='n',
     mgp = c(2, 1, 0))

for(i in 0:(part/2-1)){
  sub = 2*i*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind,X[ind],type='l',col='cadetblue4',xlab='',ylab='')
  sub = (2*i+1)*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind,X[ind],type='l',col='chartreuse4',xlab='',ylab='')
}

axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectrosC',
             '.pdf'),width=5.5,height=2.5)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(3,3,2,1))
par(las=2)

part = 12

plot((1:N),X,type='l',col='white',
     xlab='Tiempo (mm:ss)',
     ylab='mV',main='Sujeto: ---  |  Canal: ---  (10 s)',
     xlim = c(0,N-1),xaxt='n',
     mgp = c(2, 1, 0))

for(i in 0:(part/2-1)){
  sub = 2*i*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind,X[ind],type='l',col='cadetblue4',xlab='',ylab='')
  sub = (2*i+1)*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind,X[ind],type='l',col='chartreuse4',xlab='',ylab='')
}

axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectrosD',
             '.pdf'),width=5.5,height=2)
  #'.png'),units='in',res=150,width=12,height=6)
}

library(plotrix)

M = matrix(c(1,1,1,1))
M = t(M)

par(mar=c(4,2,2,1))

color2D.matplot(M,axes=F,
                xlab='Tiempo (mm:ss)',
                ylab='',
                main='Sujeto: ---')

axis(2,at=c(0,1),labels=F)
axis(2,at=c(0.5),labels=c('Canal: ---'),tick=F)

axis(1,
     at=(0:(length(plaquitas)-1))*(length(M)/(length(plaquitas)-1)),
     labels=plaquitas,las=1)

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectrosE',
             '.pdf'),width=5.5,height=2)
  #'.png'),units='in',res=150,width=12,height=6)
}

M = matrix(c(1,0,1,0,1,0,1,0,1,0,1,0))
M = t(M)

par(mar=c(4,2,2,1))

color2D.matplot(M,axes=F,
                xlab='Tiempo (mm:ss)',
                ylab='',
                main='Sujeto: ---')

axis(2,at=c(0,1),labels=F)
axis(2,at=c(0.5),labels=c('Canal: ---'),tick=F)

axis(1,
     at=(0:(length(plaquitas)-1))*(length(M)/(length(plaquitas)-1)),
     labels=plaquitas,las=1)

if(grabar){
  dev.off()
}