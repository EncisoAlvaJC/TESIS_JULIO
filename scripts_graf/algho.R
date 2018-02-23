#########################################################
# directorios
dir_datos = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/ejemplos_viejos'
dir_graf  = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/Tesis/scripts_graf_res'

#########################################################
# librerias
require('ggpubr')
require('locits')
require('fractal')
require('hms')
require('scales')

#########################################################
# cargar datos
setwd(dir_datos)
X = scan('nF8.csv')

Y = c(X,rev(X))
X = Y[1:(2**14)]
#X = ts(X,start=c(0,0),frequency = 512)
plot(X,type='l')

#plot(hwtos2(X))
#mm     = matrix(0,ncol=2,nrow=(2**(14-9))*(14-9))
mm = c()
ee = c()
nn = rep(1:(2**(14-9)),14-9+1)

for(expon in 9:14){
  f.extra = 2**(14-expon)
  dep = 2**expon
  cmp = dep/(2**9)
  
  res = rep(0,f.extra)
  ind = 1:f.extra
  for(i in 1:f.extra){
    tmp = X[(1:dep)+(i-1)*dep]
    z   = stationarity(tmp)
    pv  = 1*(as.numeric( attr(z,'pvals')[1])<.05 &&
               as.numeric( attr(z,'pvals')[3])<.05)
    res[i] = pv
  }
  clt = c()
  for(i in 1:f.extra){
    clt = c(clt,rep(res[i],cmp))
  }
  #mm[,expon- 9+1] = clt
  mm = c(mm,clt)
  ee = c(ee,rep(expon,length(clt)))
}

rr = as.data.frame(cbind(nn,mm,nn*0,ee))
colnames(rr) = c('Tiempo','Est','Relleno','Expon')
rr$Est = factor(rr$Est,labels=c('Estacionario','No Esatcionario'))

rr$Tiempo = rr$Tiempo-1
rr$Tiempo = as.POSIXct(as.hms(rr$Tiempo))

rr$Expon = 2**rr$Expon/512

A = ggplot(rr,aes(x=Tiempo,y=Expon,fill=Est))+
  theme_bw()+
  xlab('Tiempo [s]') + ylab('Tamaño de época [s]') +
  scale_x_datetime(expand=c(0,0),labels=date_format("%S"),
                   breaks = date_breaks("2 sec"))+
  scale_y_continuous(expand=c(0,0),trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x))) +
  theme(legend.position='bottom') +
  theme(legend.title=element_blank()) +
  scale_fill_manual(values=c('black','White'))+
  rotate_x_text(angle = 45)+
  theme(legend.key = element_rect(color = 'black')) +
  geom_raster(hjust=1)

ex = as.data.frame(X)
colnames(ex) = c('xx')
ex$tt = 1:length(X)

B = ggplot(ex,aes(x=tt,y=xx))+
  scale_x_continuous(expand=c(0,0),breaks=NULL)+
  xlab(NULL) + ylab('Amplitud [mV]')+
  theme_bw()+
  geom_line()

ggarrange(B,A,ncol=1,nrow=2,align='v')
