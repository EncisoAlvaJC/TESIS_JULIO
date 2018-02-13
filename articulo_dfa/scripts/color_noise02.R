# este script genera sennales que corresponden a varios
# colores de ruido, calcula su espectro de potencias y
# grafica en escala log-log
#
# usar sennales artificiales lo hace lucir mas 'realista'
# que graficar lo que son, analiticamente, los colores

# librerias para generar los ruidos
require(tuneR)
require(psd)

# PARAMETRO, guardado automatico
guardar.auto = F

# colores (exponente) para generar los ruidos, y su color grafico
col.exp = c(0,1,2,-1,-2)
col.col = c('gray','pink','red','blue','violet')

n.colores = length(col.exp)

# parametros: fr de muestreo (Hz) y duracion de la sennal (s)
f.muestreo  = 2*(10**5)
t.sennal    = 2

# contenedor de datos
esp = matrix(0,nrow=n.colores,
             ncol=t.sennal*f.muestreo/2)

# generacion de datos artificiales, usando el paquete tuneR
set.seed(2017)
for(k in 1:n.colores){
  sennal = noise(kind='power',alpha=col.exp[k],
                 duration=t.sennal,samp.rate=f.muestreo,
                 xunit='time')
  sennal.v = attr(sennal,'left')
  
  sennal.v = sennal.v - mean(sennal.v)
  sennal.v = sennal.v/sd(sennal.v)
  
  # el espectro se calcula usando el paquete psd
  espect   = pspectrum(sennal.v,Nyquist.normalize=F,
                       x.frqsamp=f.muestreo)
  esp[k,] = 10*log(espect$spec,10) #decibeles
  
  # DEBUG
  #plot(sennal.v,type='l')
  #plot(log(espect$freq),esp[k,],type='l')
}

# vector con las frecuencias
frec.v = espect$freq

esp[5,] = (esp[5,]-2*esp[3,])/3

# parche
# forzar a que la integral del epectro sea la varianza hace que su valor en cero
# pueda no ser la media, 
for(k in 1:n.colores){
  esp[k,] = esp[k,] - esp[k,1]
}


#######################################
# grafico usando plot

# parametros graficos
mmax = max(esp)
mmin = min(esp)

if(guardar.auto){
  #pdf('color_noise_plot.pdf',width=8,height=5.5)
  png('color_noise_plot.png',res=300,units='in',width=8,height=5.5)
  # las unidades son en pulgadas
}

# grafico vacio
par(mgp=c(2,.5,0))
plot(0,type='n',
     xlim=c(log(5,10),log(max(frec.v),10)),ylim=c(mmin,mmax),
     xlab = 'Frecuency [Hz]',
     ylab='Power Spectral Density [dB]\n Logarithm of spectrum',
     main='The Colors of Noise',
     xaxt='n',las=2,
     tck=.03)
axis(1,at=0:10,label=10**(0:10),tck=F)

# marcas logaritmicas de ejes
axis(1,at=log((1:10)*(10**0),10),label=F,tck=.03)
axis(1,at=log((1:10)*(10**1),10),label=F,tck=.03)
axis(1,at=log((1:10)*(10**2),10),label=F,tck=.03)
axis(1,at=log((1:10)*(10**3),10),label=F,tck=.03)
axis(1,at=log((1:10)*(10**4),10),label=F,tck=.03)

# lineas per se de los espectros
for(k in 1:n.colores){
  lines(log(frec.v,10),esp[k,],
        type='l',col=col.col[k],lwd=2)
}

if(grabar.auto){
  dev.off()
}

#######################################
# grafico usando ggplot
require(ggplot2)
require(tidyr)
require(scales)

require('ggpubr')

# arreglo de los datos
spec.d = data.frame(White   =esp[1,],
                    Pink    =esp[2,],
                    Red     =esp[3,],
                    Blue    =esp[4,],
                    Violet  =esp[5,],
                    Frecuency = frec.v)

# parche: evita error cuando se escala en log la frecuencia,
# y por estetica para evitar el efecto de puntos no-equiespaciados
spec.2 = subset(spec.d,Frecuency>5)

# parche: para escala logaritmica
esc_log = c()
for(i in 1:3){
  esc_log = c(esc_log,c(1,2,3,4,5,6,7,8,9)*10**i)
}

# grafico per se, la gramatica de ggplot2 no la domino completamente
spec.2 %>%
  gather(Noisecolour,Power, White,Pink,Red,Blue,Violet) %>%
  ggplot(aes(x=Frecuency, y=Power, colour=Noisecolour,
             linetype=Noisecolour)) +
  geom_line() +
  theme_bw() +
  scale_linetype(guide=FALSE)+
  scale_y_continuous(breaks = c((-100/20):(100/20))*20,
                     labels = waiver())+
  scale_x_continuous(trans = log10_trans(),breaks = c(10,10**2,10**3,10**4,10**5),
                     labels = trans_format("log10", math_format(10^.x)),
                     expand=c(0,0))+
  scale_color_manual(values=c('blue','pink','red','violet','black'))+
  annotation_logticks(sides = 'b')  +
  xlab('Frequency [Hz]\n (Log-scale)') +
  ylab('Power Spectrum Density [dB]\n (Logarithm of spectrum)') +
  ggtitle('The Colors of Noise') +
  labs(colour='Type of Noise')+
  theme(legend.position='bottom')

if(guardar.auto){
  ggsave(filename = 'color_noise_ggplot.png',dpi=400,
         device='png',units='cm',width=8,height=7,scale=1.9)
}

