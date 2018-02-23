###########################################################
## Funciones de ventana #####################################

require('ggpubr')

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

# grafica de k
u = seq(0,2,by=1/500)
u = as.data.frame(u)
colnames(u) = c('xx')
u$yy = k(u$xx)

ggplot(u,aes(x=xx,y=yy)) +
  xlab(NULL)+ylab(NULL)+
  theme_bw()+
  scale_x_continuous(expand=c(0,0))+
  scale_y_continuous(expand=c(0,0))+
  annotate('rect',#inherit.aes = F,
            xmin=1,xmax=2,ymin=-Inf,ymax=Inf,fill='gray',alpha=0.2)+
  geom_line(col='red')


# grabar
q = 2.5
ggsave(file=paste0('ventana_.pdf'),width=1.5,height=.5,
       scale=q)
