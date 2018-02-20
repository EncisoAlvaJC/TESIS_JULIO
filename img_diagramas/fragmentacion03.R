#################################################
# directorio de trabajo
dir_actual = '~/TESIS/TESIS_JULIO/img_diagramas'

#################################################
# librerias
require(ggplot2)
require(hms)
require(scales)
require(ggpubr)

#################################################
# parametros
N.base = 175

#################################################
# colores
azul1 = rgb( 60, 49,118,maxColorValue=255)
mora1 = rgb( 95, 40,113,maxColorValue=255)
azul2 = rgb( 40, 83,108,maxColorValue=255)

#################################################
# X es un AR que cambia de orden
setwd(dir_actual)
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

X = c(X,0,0)

#################################################
# parametros graficos, MUY modificados
SS = as.data.frame(X)
colnames(SS) = c('xx')
SS$tt = (seq(1,length(SS$xx))-1)/17.5 + 7*60
SS$tt = as.POSIXct(SS$tt,origin='1960-01-01',tz='UTC')

ggplot(SS,aes(x=tt,y=xx))+
  xlab('Tiempo [mm:ss]') + ylab('Amplitud [mV]') +
  labs(title='Participante: ---  |  Canal: ---')+
  scale_x_datetime(expand=c(0,0),labels=date_format("%M:%S"),
                   breaks = date_breaks("15 sec"))+
  theme_bw()+
  rotate_x_text(angle = 45) +
  geom_line(color=azul1)

ggsave(file='espectrosA_v3.pdf',device='pdf',
       width=5,height=2,unit='in',scale=1.35)

# termina imagen 1
#################################################

#################################################
# inicia imagen 2
en = floor(N/4)
SS$tipo1 = c(rep(c(rep(0,en),rep(1,en)),2),1,1)
#SS$tipo1 = factor(SS$tipo1,labels=c('0','1'))

un = en/17.5

i2 = ggplot(SS,aes(x=tt,y=xx,color=tipo1))+
  xlab('Tiempo [mm:ss]') + ylab('Amplitud [mV]') +
  labs(title='Participante: ---  |  Canal: ---  |  Época = 30 s')+
  scale_color_gradient(low=mora1,high=azul2)+
  scale_x_datetime(expand=c(0,0),labels=date_format("%M:%S"),
                   breaks = date_breaks("15 sec"))+
  theme_bw()+
  rotate_x_text(angle = 45) +
  theme(legend.position='none') +
  annotate('rect',alpha=0.35, fill='gray',
           ymin=-Inf,ymax=Inf,
           xmin=as.POSIXct(0*un+7*60,origin='1960-01-01',tz='UTC'), 
           xmax=as.POSIXct(1*un+7*60,origin='1960-01-01',tz='UTC'))+
  annotate('rect',alpha=0.35, fill='gray',
           ymin=-Inf,ymax=Inf,
           xmin=as.POSIXct(2*un+7*60,origin='1960-01-01',tz='UTC'), 
           xmax=as.POSIXct(3*un+7*60,origin='1960-01-01',tz='UTC'))+
  geom_line()

ggsave(file='espectrosB_v3.pdf',device='pdf',
       width=3.25,height=2,unit='in',scale=1.35)

# fin de la imagen 2
#################################################

#################################################
# inicia la imagen 3
en = floor(N/12)
SS$tipo2 = c(rep(c(rep(0,en),rep(1,en)),6),1,1)
#SS$tipo1 = factor(SS$tipo1,labels=c('0','1'))

un = en/17.5

i3 = ggplot(SS,aes(x=tt,y=xx,color=tipo2))+
  xlab('Tiempo [mm:ss]') + ylab('Amplitud [mV]') +
  labs(title='Participante: ---  |  Canal: ---  |  Época = 10 s')+
  scale_color_gradient(low=mora1,high=azul2)+
  scale_x_datetime(expand=c(0,0),labels=date_format("%M:%S"),
                   breaks = date_breaks("15 sec"))+
  theme_bw()+
  rotate_x_text(angle = 45) +
  theme(legend.position='none') +
  annotate('rect',alpha=0.35, fill='gray',
           ymin=-Inf,ymax=Inf,
           xmin=as.POSIXct(0*un+7*60,origin='1960-01-01',tz='UTC'), 
           xmax=as.POSIXct(1*un+7*60,origin='1960-01-01',tz='UTC'))+
  annotate('rect',alpha=0.35, fill='gray',
           ymin=-Inf,ymax=Inf,
           xmin=as.POSIXct(2*un+7*60,origin='1960-01-01',tz='UTC'), 
           xmax=as.POSIXct(3*un+7*60,origin='1960-01-01',tz='UTC'))+
  annotate('rect',alpha=0.35, fill='gray',
           ymin=-Inf,ymax=Inf,
           xmin=as.POSIXct(4*un+7*60,origin='1960-01-01',tz='UTC'), 
           xmax=as.POSIXct(5*un+7*60,origin='1960-01-01',tz='UTC'))+
  annotate('rect',alpha=0.35, fill='gray',
           ymin=-Inf,ymax=Inf,
           xmin=as.POSIXct(6*un+7*60,origin='1960-01-01',tz='UTC'), 
           xmax=as.POSIXct(7*un+7*60,origin='1960-01-01',tz='UTC'))+
  annotate('rect',alpha=0.35, fill='gray',
           ymin=-Inf,ymax=Inf,
           xmin=as.POSIXct(8*un+7*60,origin='1960-01-01',tz='UTC'), 
           xmax=as.POSIXct(9*un+7*60,origin='1960-01-01',tz='UTC'))+
  annotate('rect',alpha=0.35, fill='gray',
           ymin=-Inf,ymax=Inf,
           xmin=as.POSIXct(10*un+7*60,origin='1960-01-01',tz='UTC'), 
           xmax=as.POSIXct(11*un+7*60,origin='1960-01-01',tz='UTC'))+
  geom_line()

ggsave(file='espectrosC_v3.pdf',device='pdf',
       width=3.25,height=2,unit='in',scale=1.35)

# fin de la imagen 3
#################################################

#################################################
# inicia la imagen 4

# matriz ejemplo
M = c(1,1,1,1)

MM = as.data.frame(M)
colnames(MM) = c('Val')

MM$tt = (1:length(MM$Val)-1)*30 + 7*60 +15
MM$tt = as.POSIXct(MM$tt,origin='1960-01-01',tz='UTC')
MM$Relleno = MM$Val*0
MM$Relleno = factor(MM$Relleno,labels='Canal')
MM$Val     = factor(MM$Val,labels = c('No-Estacionario'))

i4 = ggplot(MM,aes(x=tt,y=Relleno,fill=Val,colour='black',
              linetype='dotted'))+
  xlab('Tiempo [mm:ss]') + ylab('Canal: ---') +
  labs(title='Participante: ---  |  Época = 30 s')+
  theme_bw() +
  scale_x_datetime(expand=c(0,0),labels=date_format("%M:%S"),
                   breaks = date_breaks("15 sec"))+
  scale_y_discrete(expand=c(0,0),breaks=NULL) +
  scale_fill_manual(values=c('white','black'))+
  #labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
  theme(legend.position='none') +
  rotate_x_text(angle = 45)+
  geom_tile()

ggarrange(i2,i4,ncol=1,nrow=2,align='v',heights = c(0,1))

ggsave(file='espectrosD_v3.pdf',device='pdf',
       width=3.25,height=1.5,unit='in',scale=1.35)

# fin de la imagen 4
#################################################

#################################################
# inicia la imagen 5

# matriz ejemplo
M = c(1,0,1,0,1,0,1,0,1,0,1,0)

MM = as.data.frame(M)
colnames(MM) = c('Val')

MM$tt = (1:length(MM$Val)-1)*10 + 7*60 +5
MM$tt = as.POSIXct(MM$tt,origin='1960-01-01',tz='UTC')
MM$Relleno = MM$Val*0
MM$Relleno = factor(MM$Relleno,labels='Canal')
MM$Val     = factor(MM$Val,labels = c('Estacionario',
                                      'No-Estacionario'))

i5 = ggplot(MM,aes(x=tt,y=Relleno,fill=Val,colour='black',
              linetype='dotted'))+
  xlab('Tiempo [mm:ss]') + ylab('Canal: ---') +
  labs(title='Participante: ---  |  Época = 10 s')+
  theme_bw() +
  scale_x_datetime(expand=c(0,0),labels=date_format("%M:%S"),
                   breaks = date_breaks("15 sec"))+
  scale_y_discrete(expand=c(0,0),breaks=NULL) +
  scale_fill_manual(values=c('white','black'))+
  #labs(title=paste('Participante:',etiqueta,'| Grupo:',grupo)) +
  theme(legend.position='none') +
  rotate_x_text(angle = 45)+
  geom_tile()

ggarrange(i3,i5,ncol=1,nrow=2,align='v',heights = c(0,1))

ggsave(file='espectrosE_v3.pdf',device='pdf',
       width=3.25,height=1.5,unit='in',scale=1.35)

# termina la imagen 5
#################################################