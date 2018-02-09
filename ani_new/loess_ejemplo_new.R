set.seed(2016)

x = seq(-1,1,0.01)
r = rnorm(length(x),0,0.5)
y = sin(x*2*pi)+ r

y.loess = loess(y~x,span = 1, degree = 1,method=c('loess'),data.frame(x=x,y=y))

plot(x,y,type='l')
lines(x,y.loess$fitted,type='l',col='red')


y.predict <- predict(y.loess, data.frame(x=x))
plot(x,y,type='l')
lines(x,y.predict,type='l',col='red')


####################################################################################
####################################################################################

S = 0.6



y.loess = loess(y~x,span = S, degree = 1,method=c('loess'),data.frame(x=x,y=y))
y.predict <- predict(y.loess, data.frame(x=x))
plot(x,y,type='l')
lines(x,y.predict,type='l',col='red')



####################################################################################
####################################################################################

set.seed(2016)

x = seq(-1,1,0.01)
r = rnorm(length(x),0,0.5)
y = sin(x*2*pi)+ r

i = 101

R = x*0+1
W = exp(-20*(x-x[i])**2)
mod = lm( y~x, weights=W )

dev.off()
par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)

plot(x,y,xlab='',ylab='',type='p',col=gray(1-W))
abline(mod,col='red')
points(x[i],mod$fitted.values[i],col='blue',pch=19)
#lines(x,mod$fitted.values,type='l')

plot(x,W,type='l',xlab='',ylab='')
lines(x,R,type='l',col='green')
lines(x,W,type='l',xlab='',ylab='')

L = 1:length(x)
for(i in 1:length(x)){
  W = exp(-20*(x-x[i])**2)
  mod = lm( y~x, weights=W )
  
  L[i] = mod$fitted.values[i]
}

####################################################################################
####################################################################################
library(animation)

saveGIF({
  ani.options(interval = 0.2, nmax = 50)

  set.seed(2016)
    
  x = seq(-1,1,0.01)
  r = rnorm(length(x),0,0.5)
  y = sin(x*2*pi)+ r
  
  t = x
  L = x
  
  for(i in 1:length(x)){
    
    R = x*0+1
    W = exp(-20*(x-x[i])**2)
    mod = lm( y~x, weights=W )
  
    #dev.off()
    par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
    
    L[i] = mod$fitted.values[i]
    
    #plot(x,y,xlab='',ylab='')
    plot(x,y,xlab='',ylab='',type='p',col=gray(1-W),
         bty='n',xaxt='n')
    axis(1,labels=F)
    lines(x[1:i],L[1:i],type='l',col='blue')
    abline(mod,col='red')
    points(x[i],L[i],col='blue',pch=19)
    
    plot(x,W,type='l',xlab='',ylab='',bty='n')
    lines(x,R,type='l',col='green')
    
    ani.pause()
  }
}, movie.name="stl_1.gif", ani.width=623, ani.height=400)

####################################################################################
####################################################################################

bisquare = function(u){
  if(u>1){
    reg = 0
  }
  else{
    reg = (1-u**2)**2
  }
  return(reg)
}

h = 6*median(abs(y-L))
R2 = R

for(i in 1:length(W)){
  R2[i] = bisquare(abs(y[i]-L[i])/h)
}

dev.off()
par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)

plot(x,y,xlab='',ylab='',type='p')
lines(x,L,type='l',col='blue')

plot(x,R2,type = 'l',col='green')

#plot(x,y-L,xlab='',ylab='')
#abline(0,0,col='blue')

dev.off()
par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)

plot(x,y,xlab='',ylab='',type='p',col=grey(1-W2))
lines(x,L,type='l',col='blue')

plot(x,W2,type = 'l',col='green')

####################################################################################
####################################################################################

library(animation)

saveGIF({
  ani.options(interval = 0.2, nmax = 50)
  
  set.seed(2016)
  
  x = seq(-1,1,0.01)
  r = rnorm(length(x),0,0.5)
  y = sin(x*2*pi)+ r
  
  t = x
  L = x
  
  for(i in 1:length(x)){
    
    #R = x*0+1
    W = exp(-20*(x-x[i])**2)
    mod = lm( y~x, weights=W*R2 )
    
    #dev.off()
    par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
    
    L[i] = mod$fitted.values[i]
    
    #plot(x,y,xlab='',ylab='')
    plot(x,y,xlab='',ylab='',type='p',col=gray(1-W*R2),
         bty='n',xaxt='n')
    axis(1,labels=F)
    lines(x[1:i],L[1:i],type='l',col='blue')
    abline(mod,col='red')
    points(x[i],L[i],col='blue',pch=19)
    
    plot(x,W*R2,type='l',xlab='',ylab='',bty='n',ylim=c(0,1))
    lines(x,R2,type='l',col='green')
    
    ani.pause()
  }
}, movie.name="stl_2.gif", ani.width=623, ani.height=400)

####################################################################################
####################################################################################

# library(animation)
# 
# saveLatex({
#   ani.options(interval = 0.2, nmax = 50)
#   
#   x = seq(-1,1,0.01)
#   r = rnorm(length(x),0,0.5)
#   y = sin(x*2*pi)+ r
#   
#   t = x
#   L = x
#   
#   for(i in 1:length(x)){
#     
#     R = x*0+1
#     W = exp(-20*(x-x[i])**2)
#     mod = lm( y~x, weights=W )
#     
#     #dev.off()
#     par(mfrow=c(2,1),mar=c(1,2,1,1)+0.1)
#     
#     L[i] = mod$fitted.values[i]
#     
#     plot(x,y,xlab='',ylab='')
#     lines(x[1:i],L[1:i],type='l',col='blue')
#     abline(mod,col='red')
#     points(x[i],L[i],col='blue',pch=19)
#     
#     plot(x,W,type='l',xlab='',ylab='')
#     lines(x,R,type='l',col='green')
#     
#     ani.pause()
#   }
# }, ani.basename='STLdemo',ani.opts='controls,loop,width=\\linewidth',
#    documentclass=NULL,latex.filename='stl_demo_latex0.tex')

####################################################################################
####################################################################################

# library(animation)
# 
# saveGIF({
#   
#   ani.options(interval = 0.2, nmax = 50)
#   
#   t = seq(0,pi,.01)
#   
#   x = cos(2*t)
#   
#   y = sin(2*t)
#   
#   idx = seq(1,length(x),10)
#   
#   for (i in seq_along(idx)) {
#     
#     plot(x,y,type='n')
#     
#     points(x[seq(idx[i])],
#            
#            y[seq(idx[i])], pch=15, col='dark green')
#     
#     ani.pause() }
#   
# }, movie.name = "circle.gif",
# 
# ani.width = 600, ani.height = 600) 
# 
