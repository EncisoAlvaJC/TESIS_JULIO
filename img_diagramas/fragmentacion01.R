grabar = T

setwd("~/TESIS/TESIS/img_diagramas")

set.seed(2017)

N = 25000

S = 10

P = rpois(N,S)

C = c(rep(0,N/10),rep(1,N/10),
      rep(0,N/10),rep(1,N/10),
      rep(0,N/10),rep(1,N/10),
      rep(0,N/10),rep(1,N/10),
      rep(0,N/10),rep(1,N/10))

for(i in 1:N){
  if(C[i]){
    P[i] = S
  }
}

X = rnorm(N,0,1)*P*(1/S)

if(grabar){
  #setwd(g_dir)
  pdf(paste0('espectros1',
             '.pdf'),width=10,height=2.5)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(3,3,2,1))
par(las=2)
plot((1:N),X,type='l',col='white',
     xlab='Tiempo (mm:ss)',
     ylab='mV',main='Sujeto: ---  |  Canal: ---',
     xlim = c(0,N-1),xaxt='n',
     mgp = c(2, 1, 0)
     )

part = 10

for(i in 0:(part/2-1)){
  sub = 2*i*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind,X[ind],type='l',col='cadetblue4',xlab='',ylab='')
  sub = (2*i+1)*(N/part)
  ind = (sub+1):(sub+N/part)
  lines(ind,X[ind],type='l',col='chartreuse4',xlab='',ylab='')
}

te = ((1:N))*(5*60/N)
mm = floor(te/60)
ss = floor(te - 60*mm)

places = ceiling(seq(0,N,by=(N)/20))
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
                paste0(toString(mm[i]+4),
                       ':',seg))
}

places[1]=0
axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectros2',
             '.pdf'),width=10,height=2.5)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(3,3,2,1))
par(las=2)

ii = 2
sub = 2*ii*(N/part)
ind = (sub+1):(sub+N/part)
XX = X[ind]

NN = N/part
partt = 5

#arcoiris = rainbow(partt)
kolor = c('chartreuse4','darkgreen',
          'chartreuse4','darkgreen',
          'chartreuse4','darkgreen')

plot(XX,type='l',col='white',
     xlab='Tiempo (mm:ss)',
     ylab='mV',
     main='Sujeto: ---  |  Época: ---  |  Canal: ---',
     xaxt='n',mgp = c(2, 1, 0))

indd = 1:(length(XX))

for(i in 0:(partt-1)){
  sub = i*(NN/partt)
  indd = (sub+1):(sub+NN/partt)
  lines(indd,XX[indd],type='l',
        col=kolor[i+1])
  d = i+1
  axis(3,at=c(sub+NN/(partt*2)),
       #labels=paste0('t_',toString(i+1)),
       labels=substitute(t[d],list(d=d)),
       las=1,mgp = c(0, -1, -.5))
}

sub = 2*ii*(N/part)
ind = (sub+1):(sub+N/part)

NN = length(ind)

te = (ind)*(30/NN)+30
mm = floor(te/60)
ss = floor(te - 60*mm)

places = ceiling(seq(1,length(te),by=(length(te)-1)/10))
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
                paste0(toString(mm[i]+4),
                       ':',seg))
}

places[1]=0
axis(1,at=places,labels=plaquitas,las=1)

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectros3',
             '.pdf'),width=10,height=3)
  #'.png'),units='in',res=150,width=12,height=6)
}

par(mar=c(3,3,2,1))
par(las=1)

i = 0
sub = i*(NN/partt)
indd = (sub+1):(sub+NN/partt)
XXX = XX[ind]
K = indd*(6/length(indd))
plot(K,abs(fft(XX[indd])),col='white',
     type = 'l',ylim=c(0,480),
     xlab='Periodo (s)',
     ylab='Amplitud (dB)',
     main='Sujeto: ---  |  Época: ---  |  Canal: ---  |  Y(t,w)',
     yaxt='n',mgp = c(2, 1, 0))

for(y in 0:10){
  d = y+1
  axis(3,at=c(y*(6/10)),
       #labels=paste0('t_',toString(i+1)),
       labels=substitute(w[d],list(d=d)),
       las=1,mgp = c(0, -1, -.5))
}

for(i in 0:(partt-1)){
 sub = i*(NN/partt)
 indd = (sub+1):(sub+NN/partt)
 XXX = XX[ind]
 lines(K,abs(fft(XX[indd]))+i*80+20,type = 'l',
       col=kolor[i+1],
       lwd=2)
 d = i+1
 axis(4,at=c(40+80*i),
      #labels=paste0('t=',toString(i+1)),
      labels=substitute(t[d],list(d=d)),
      las=1,mgp = c(0, -1, -.5))
}

if(grabar){
  dev.off()
  
  #setwd(g_dir)
  pdf(paste0('espectros4',
             '.pdf'),width=10,height=2)
  #'.png'),units='in',res=150,width=12,height=6)
}

library(plotrix)

M = matrix(c(1,0,1,0,1,0,1,0,1,0))
M = t(M)

par(mar=c(4,2,2,1))

color2D.matplot(M,axes=F,
                xlab='Tiempo (mm:ss)',
                ylab='',
                main='Sujeto: ---')

axis(2,at=c(0,1),labels=F)
axis(2,at=c(0.5),labels=c('Canal: ---'),tick=F)

te = ((1:N))*(5*60/N)
mm = floor(te/60)
ss = floor(te - 60*mm)

places = ceiling(seq(0,N,by=(N)/20))
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
                paste0(toString(mm[i]+4),
                       ':',seg))
}

places[1]=0

axis(1,
     at=(0:(length(plaquitas)-1))*(length(M)/(length(plaquitas)-1)),
     labels=plaquitas,las=1)

if(grabar){
  dev.off()
}
