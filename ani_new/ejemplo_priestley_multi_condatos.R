library(fractal)

n7 = scan('nF7.csv')
n8 = scan('nF8.csv')
d7 = scan('dF7.csv')
d8 = scan('dF8.csv')


n7.t = ts(n7,frequency=512)
n8.t = ts(n8,frequency=512)
d7.t = ts(d7,frequency=512)
d8.t = ts(d8,frequency=512)

s.n7 = stl(n7.t,s.window='periodic',robust=T)
s.n8 = stl(n8.t,s.window='periodic',robust=T)
s.d7 = stl(d7.t,s.window='periodic',robust=T)
s.d8 = stl(d8.t,s.window='periodic',robust=T)

n7.r.t = s.n7$time.series[,'remainder']
n8.r.t = s.n8$time.series[,'remainder']
d7.r.t = s.d7$time.series[,'remainder']
d8.r.t = s.d8$time.series[,'remainder']

n7.r = unclass(n7.r.t)
n8.r = unclass(n8.r.t)
d7.r = unclass(d7.r.t)
d8.r = unclass(d8.r.t)

n7.r = n7.r[1:length(n7.r)]
n8.r = n8.r[1:length(n8.r)]
d7.r = d7.r[1:length(d7.r)]
d8.r = d8.r[1:length(d8.r)]



zn7o = stationarity(n7)
zn8o = stationarity(n8)
zd7o = stationarity(d7)
zd8o = stationarity(d8)

zn7f = stationarity(n7.r)
zn8f = stationarity(n8.r)
zd7f = stationarity(d7.r)
zd8f = stationarity(d8.r)



serie_full = n7.r
serie = serie_full[1:(1181*12)]
datos = serie
datos_full = serie_full[1:(1181*13)]

tt = 1:(1181*12)
tt = tt/512

###################################################################################
###################################################################################

#le = 1181
#jump = 2500 #0:10:(512*6-279)
#jum_step = 10
#max_jum = (512*6-le)/jum_step

###################################################################################

#dev.off()
#par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
#plot(tt,datos,xlab='',ylab='',col='white')
#lines(tt[jump:(jump+le)],datos[jump:(jump+le)],type='l',col=rainbow(max_jum)[jump/jum_step+1])
#lines(tt[0:jump],datos[0:jump],type='l')
#lines(tt[(jump+le):length(tt)],datos[(jump+le):length(tt)],type='l')
#
#
#abline(v=(jump+le/2)/512)
#
#s = stationarity(datos_full[jump:(jump+le)],n.block = 2)
#espec = s$anova
#frecu = s$freq
#
#plot(frecu,espec[1,1:length(frecu)],type='l',xlab='',ylab='',col=rainbow(max_jum)[jump/jum_step+1])

###################################################################################
###################################################################################

maxi = -100
mini = 100

library(animation)

saveGIF({
  ani.options(interval = 0.2, nmax = 50)
  
  le = 1181/2
  jum_step = 100
  max_jum = (1181*12-le)/jum_step
  
  for(j in 0:(max_jum-1)){
    
    jump = j*jum_step
    
    par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
    plot(tt,datos,xlab='',ylab='',col='white')
    lines(tt[jump:(jump+le)],datos[jump:(jump+le)],type='l',col=rainbow(max_jum)[jump/jum_step+1])
    lines(tt[0:jump],datos[0:jump],type='l')
    lines(tt[(jump+le):length(tt)],datos[(jump+le):length(tt)],type='l')
    
    
    abline(v=(jump+le/2)/512)
    
    s = stationarity(datos_full[jump:(jump+le)],n.block = 2)
    espec = s$anova
    frecu = s$freq
    
    plot(frecu,(espec[1,1:length(frecu)]),type='l',xlab='',ylab='',col=rainbow(max_jum)[jump/jum_step+1],ylim=c(-8.5,3.5),lwd=5,pch=1)
    
    #maxi = max(maxi,(max(espec[1,1:length(frecu)])))
    #mini = min(mini,(min(espec[1,1:length(frecu)])))
    
    ani.pause()
  }
}, movie.name="priestley_spectra_full.gif", ani.width=623, ani.height=300)
#}, movie.name="priestley_spectra.gif", ani.width=623, ani.height=400)


###################################################################################
###################################################################################


saveGIF({
  ani.options(interval = 0.5, nmax = 50)
  
  set.seed(2016)
  serie_full = noise(kind = c("power"),samp.rate=512,duration=512*7,alpha=1)
  serie = serie[1:(512*6)]
  datos = serie@left
  datos_full = serie_full@left
  
  tt = 1:(512*6)
  tt = tt/512
  
  le = 279
  jum_step = 279
  max_jum = (512*6)/jum_step
  
  for(j in 0:(max_jum-1)){
    
    jump = j*jum_step
    
    par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
    plot(tt,datos,xlab='',ylab='',col='white')
    lines(tt[jump:(jump+le)],datos[jump:(jump+le)],type='l',col=rainbow(max_jum)[jump/jum_step+1])
    lines(tt[0:jump],datos[0:jump],type='l')
    lines(tt[(jump+le):length(tt)],datos[(jump+le):length(tt)],type='l')
    
    
    abline(v=(jump+le/2)/512)
    
    s = stationarity(datos_full[jump:(jump+le)],n.block = 2)
    espec = s$anova
    frecu = s$freq
    
    plot(frecu,exp(espec[1,1:length(frecu)]),type='l',xlab='',ylab='',col=rainbow(max_jum)[jump/jum_step+1],ylim=c(-8.5,3.5),lwd=5,pch=1)
    
    ani.pause()
  }
}, movie.name="priestley_spectra_small.gif", ani.width=623, ani.height=300)

###################################################################################
###################################################################################

z = stationarity(datos)

y = z$anova
f = z$freq

write.csv(y,'ej_priestley_y.csv')
write.csv(f,'ej_priestley_f.csv')
