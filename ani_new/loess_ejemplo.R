####################################################################################
####################################################################################
library(animation)

saveGIF({
  ani.options(interval = 0.2, nmax = 50)

  set.seed(2017)
    
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
    
    plot(x,W,type='l',xlab='',ylab='',ylim=c(0,1),bty='n')
    lines(x,R,type='l',col='green')
    
    ani.pause()
  }
}, movie.name="stl_demo_hoy.gif", ani.width=623, ani.height=400)

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

####################################################################################
####################################################################################

 library(animation)
 
 saveGIF({
   ani.options(interval = 0.2, nmax = 50)
   
   set.seed(2017)
   
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
     plot(x,y,xlab='',ylab='',type='p',col=gray(1-W),
          bty='n',xaxt='n')
     plot(x,y,xlab='',ylab='',type='p',col=gray(1-W*R2))
     lines(x[1:i],L[1:i],type='l',col='blue')
     abline(mod,col='red')
     points(x[i],L[i],col='blue',pch=19)
#     
     plot(x,W*R2,type='l',xlab='',ylab='')
     lines(x,R2,type='l',col='green')
     
     ani.pause()
   }
 }, movie.name="stl_demo2_gif.gif", ani.width=623, ani.height=400)

####################################################################################
####################################################################################