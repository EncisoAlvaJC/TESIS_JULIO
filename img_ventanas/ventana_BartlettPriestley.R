###########################################################
## Funcion de ventana de Bartlet-Priestley  ###############

# funcion generadora k, definicion
k.base = function(u){
  if(u==0){
    return(1)
  }
  return((3/((pi*u)**2))*((sin(pi*u))/(pi*u) - cos(pi*u)))
}

# funcion k, vectorizada
k = function(u){
  sapply(u,function(tt) k.base(tt) )
}

# funcion generadora k, truncada
k.t.base = function(u){
  if(abs(u)>1){
    return(0)
  }else{
    if(u==0){
      return(1)
    }
  }
  return((3/((pi*u)**2))*((sin(pi*u))/(pi*u) - cos(pi*u)))
}

# funcion k, vectorizada
k.t = function(u){
  sapply(u,function(tt) k.t.base(tt) )
}

# grafica de k
u = seq(0,3,by=1/500)
plot(u,k(u),type='l',bty='n',xlab='u',ylab='k(u)',
     main='Ventana de Bartlett-Priestley',sub='Dominio tiempo')
lines(u,k.t(u),type='l',col='red')

legend(x='topright',legend=c('Original','Truncada'),
       fill=c('black','red'),bty='n')



# normalizacion (numerica) de k
du       = 1/100
u        = seq(-3,3,by=du)
du       = u[2]-u[1]
ajuste   = 1/( sum(k(u)**2)*du*(2*pi) )
ajuste.t = 1/( sum(k.t(u)**2)*du*(2*pi) )
kv       = k(u)  * sqrt(ajuste)
kv.t     = k.t(u)* sqrt(ajuste.t)

# grafica de k
plot(u,kv,type='l',bty='n',xlab='u',ylab='k(u)',
     main='Ventana de Bartlett-Priestley (normalizada)',
     sub='Dominio tiempo')
lines(u,kv.t,type='l',col='red')

legend(x='topright',legend=c('Original','Truncada'),
       fill=c('black','red'),bty='n')

###########################################################
###########################################################

# transformada de Fourier de k
require(fourierin)

# la integral de fourier se hace numericamente en general
rr  = 10**4
K   = fourierin(f=k,freq_adj=1,lower_int=-100,upper_int=100,const_adj=0,
                lower_eval=-10,upper_eval=10,resol=rr)
K.t = fourierin(f=k.t,freq_adj=1,lower_int=-100,upper_int=100,const_adj=0,
                lower_eval=-10,upper_eval=10,resol=rr)

# grafica de K
mm  = min(min(Re(K$values)),min(Re(K.t$values)))
MM  = max(max(Re(K$values)),max(Re(K.t$values)))
plot(K$w,Re(K$values),type='l',bty='n',
     ylim=c(mm,MM),xlab='th',ylab='K(th)',
     main='Ventana de Bartlett-Priestley',sub='Dominio frecuencias')
lines(K.t$w,Re(K.t$values),type='l',col='red')
legend(x='topright',legend=c('Original','Truncada'),
       fill=c('black','red'),bty='n')


# normalizacion de K
th  = K$w
dth = th[2]-th[1]
K   = Re(K$values  )
K.t = Re(K.t$values)

ajuste   = 1/( sum(K**2)*dth   )
ajuste.t = 1/( sum(K.t**2)*dth )
K   = K  *sqrt(ajuste  )
K.t = K.t*sqrt(ajuste.t)

# grafica de K
mm  = min(min(K),min(K.t))
MM  = max(max(K),max(K.t))
plot(th,K,type='l',bty='n',
     ylim=c(mm,MM),xlab='th',ylab='K(th)',
     main='Ventana de Bartlett-Priestley (normalizada)',
     sub='Dominio frecuencias')
lines(th,K.t,type='l',col='red')
legend(x='topright',legend=c('Original','Truncada'),
       fill=c('black','red'),bty='n')

###########################################################
###########################################################

dt       = 1/1000
t        = seq(-150,150,by=dt)
dt       = t[2]-t[1]
ajuste   = 1/( sum(k(t)**2)*dt*(2*pi) )
ajuste.t = 1/( sum(k.t(t)**2)*dt*(2*pi) )
kv       = k(t)  * sqrt(ajuste)
kv.t     = k.t(t)* sqrt(ajuste.t)

B.g   = sum(abs(t)*kv  )*dt
B.g.t = sum(abs(t)*kv.t)*dt

print(       'Anchos de banda (g)')
print(paste0('Normal  : ',toString(B.g)))
print(paste0('Truncado: ',toString(B.g.t)))

nu   = 2/(sum(kv**2  )*dt)
nu.t = 2/(sum(kv.t**2)*dt)

print(       'Grados de libertad equivalentes')
print(paste0('Normal  : ',toString(nu)))
print(paste0('Truncado: ',toString(nu.t)))

###########################################################

rr   = 10**5
mini = -150
maxi =  150

K   = fourierin(f=k,freq_adj=1,lower_int=-100,upper_int=100,const_adj=0,
                lower_eval=mini,upper_eval=maxi,resol=rr)
K.t = fourierin(f=k.t,freq_adj=1,lower_int=-100,upper_int=100,const_adj=0,
                lower_eval=mini,upper_eval=maxi,resol=rr)

th  = K$w
dth = t[2]-t[1]
K   = Mod(K$values  )
K.t = Mod(K.t$values)

ajuste   = 1/( sum(K**2)*dt )
ajuste.t = 1/( sum(K.t**2)*dt )
K   = K  *sqrt(ajuste  )
K.t = K.t*sqrt(ajuste.t)

B.G   = sum((th**2)*(K**2  ))*dth
B.G.t = sum((th**2)*(K.t**2))*dth

print(       'Ancho de banda (GAMMA)')
print(paste0('Normal  : ',toString(B.G)))
print(paste0('Truncado: ',toString(B.G.t)))

G4   = sum(K**4  )*dth
G4.t = sum(K.t**4)*dth

print(       'Factor int G**4')
print(paste0('Normal  : ',toString(G4)))
print(paste0('Truncado: ',toString(G4.t)))

###########################################################

# el siguiente grafico es exploratorio para definir la
# separacion minima entre frecuencias, falta ser escalado
# de acuerdo a la frecuencia de muestreo

# parametros
maxi  = 2000
oo    = seq(1,maxi+1,by=50)
qq    = length(oo)
fun   = rep(0,qq)
fun.t = rep(0,qq)

ajuste   = 1/( sum(K**2)*dt )
ajuste.t = 1/( sum(K.t**2)*dt )
K   = K  *sqrt(ajuste  )
K.t = K.t*sqrt(ajuste.t)

K.M   = K**2
K.M.t = K.t**2

for(i in 1:qq){
  dd       = oo[i]
  ind      = 1:(length(K.M)-dd+1)
  fun[i]   = sum( K.M[ind]  *K.M[ind+dd-1] )*dth
  fun.t[i] = sum( K.M.t[ind]*K.M[ind+dd-1] )*dth
}

mm = min(min(fun),min(fun.t))
MM = max(max(fun),max(fun.t))

plot(oo*dth,fun,type='l',bty='n',ylim=c(mm,MM),
     xlab='d omega',ylab='Interferencia en frecuencias',
     main='Ventana de Bartlett-Priestley')
lines(oo*dth,fun.t,type='l',col='red')
legend(x='topright',legend=c('Original','Truncada'),
       fill=c('black','red'),bty='n')

abline(h=.05,col='blue')
