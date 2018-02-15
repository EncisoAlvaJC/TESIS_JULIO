###########################################################
## Funciones de ventana #####################################

###########################################################
setwd('C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/Tesis/img_ventanas')

grabar = T

# funcion generadora k, definicion

# Bartlett
#k.base = function(u){
#  if(abs(u)>1){
#    return(0)
#  }
#  return(1)
#}

# Fejer
#k.base = function(u){
#  if(abs(u)>1){
#    return(0)
#  }
#  return(1-abs(u))
#}

# Daniell
#k.base = function(u){
#  if(abs(u)==0){
#    return(1)
#  }
#  return(sin(pi*u)/(pi*u))
#}

# Parzen 1
#k.base = function(u){
#  if(abs(u)>1){
#    return(0)
#  }
#  return(1-u*u)
#}

# Parzen 2
#k.base = function(u){
#  if(abs(u)>1){
#    return(0)
#  }
#  return(1/(1+abs(u)))
#}

# Parzen 3
#k.base = function(u){
#  #if(abs(u)>1){
#  #  return(0)
#  #}
#  return(1/(1+u*u))
#}

# Parzen 4
#k.base = function(u){
#  if(abs(u)>1){
#    return(0)
#  }
#  if(abs(u)<.5){
#    return(1-6*u*u+6*abs(u)*abs(u)*abs(u))
#  }
#  return((2*((1-abs(u))**3)))
#}

# Tukey
#a = .25
#k.base = function(u){
#  if(abs(u)>1){
#    return(0)
#  }
#  return(1-2*a+2*a*cos(pi*u))
#}

# Neave
#a = .3
#b = .6
#k.base = function(u){
#  if(abs(u)>1){
#    return(0)
#  }
#  if(abs(u)<a){
#    return(1)
#  }
#  if(abs(u)<b){
#    return( (1/(1-a))*(1 -u +((b-a)/pi)*sin(((b-u)/(b-a))*pi) ) )
#  }
#  return( (1/(1-a))*(1 -u -((1-b)/pi)*sin(((u-b)/(1-b))*pi) ) )
#}

# Cuadratica
#k.base = function(u){
#  if(u==0){
#    return(1)
#  }
#  return((25/(12*((pi*u)**2)))*((sin(pi*u*6/5))/(pi*u*6/5) - cos(pi*u*6/5)))
#}

# Bartlett-Priestley
#k.base = function(u){
#  if(u==0){
#    return(1)
#  }
#return((3/((pi*u)**2))*((sin(pi*u))/(pi*u) - cos(pi*u)))
#}

# Papoulis
#k.base = function(u){
#  if(u>1){
#    return(0)
#  }
#  if(u==0){
#    return(1)
#  }
#  return(((1-u)*cos(pi*u)+sin(pi*u)/(pi*u))*.5)
#}

# Cosenoidal
#k.base = function(u){
#  if(u>1){
#    return(0)
#  }
#  return(cos(pi*u/2))
#}

# Trapezoidal
#a = .5
#k.base = function(u){
#  if(u>1){
#    return(0)
#  }
#  if(abs(u)<a){
#    return(1)
#  }
#  return((u-1)/(a-1))
#}

# Normal
s = 1/3
k.base = function(u){
  #if(u>1){
  #  return(0)
  #}
  return(exp(-(u**2)/(2*(s**2))))
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

q = 1.5

{
# grabar
if(grabar){
  pdf(paste0('ventana_.pdf'),width=1.5*q,height=.5*q)
}

# grafica de k
u = seq(0,3,by=1/500)
par(oma=c(0,0,0,0),mar=c(0,0,0,0))
#par(mgp=c(-2,-1,-.5))
plot(u,k(u),type='l',col='red',
     xaxt='n',
     yaxt='n',
     #bty='n',#xlab='u',ylab='k(u)',
     #main='Ventana de Bartlett-Priestley',sub='Dominio tiempo',
     main='',xlab='',ylab='',
     las = 1,
     bty='n'
     )
box(bty='o',col=gray(.4),lwd=2)
box(bty=']',col='white',lwd=2.2)
abline(h=0,col=gray(.4))
abline(v=1,col='blue',lty=2)
#lines(u,k.t(u),type='l',col='red')

#legend(x='topright',legend=c('Original','Truncada'),
#       fill=c('black','red'),bty='n')

lines(u,k(u),type='l',col='red')

if(grabar){
  dev.off()
}
}