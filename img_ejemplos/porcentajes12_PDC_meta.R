# para grabar en formato de excel
#library(xlsx)

#len = 2
p.val = 0.05

grabar = T

# constantes genericas
channel   = c('C3','C4','CZ',
              'F3','F4','F7','F8',
              'FP1','FP2','FZ',
              'O1','O2','P3','P4','PZ',
              'T3','T4','T5','T6',
              'LOG','ROG',
              'EMG'
)

nomb_dir  = c('VCNNS',
              'MJNNVIGILOScCanal',
              'JANASUE_revisado',
              'GH',
              'GURM_revisado',
              'CLMN10SUE',
              'RLMN',
              'RRMNS_2',
              'JGMN6SUE',
              'FGH_EEGdescompuesto',
              'MGNA',
              'EMNN')
nomb_arch = c('VCNNS1',
              'MJNNVIGILOS',
              'JANASUE',
              'GH24031950SUEÑO',
              'GURM251148SUE',
              'CLMN10SUE',
              'RLMN10SUE',
              'RRMNS',
              'JGMN6SUE',
              'FGHSUE',
              'MGNA5SUE',
              'EMNNS')
nomb_facil = c('VCR',
               'MJH',
               'JAE',
               'GHA',
               'MFGR',
               'CLO',
               'RLO',
               'RRU',
               'JGZ', 
               'FGH',
               'MGG',
               'EMT')

grupo_de = c(0,0,0,0,0,1,1,1,1,-1,-1,-1)
#grupo_de = c(-1,0,0,-1,-1,1,1,-1,-1,-1,-1,-1)
#grupo_de =  c(0,-1,-1,0,0,-1,-1,1,1,-1,-1,-1)
#grupo_de = c(1,1,1,1,1,1,1,1,1,-1,0,0)

dif_significativas = matrix(nrow=22,ncol=12)
colnames(dif_significativas) = nomb_facil
row.names(dif_significativas) = channel

matriz_mor  = matrix(nrow=12,ncol=22)
matriz_nmor = matrix(nrow=12,ncol=22)
matriz_tot  = matrix(nrow=12,ncol=22)

ast = c(' ','*','**','***')

colnames(matriz_mor)=channel
row.names(matriz_mor) = nomb_facil

central_dir = '/home/julio/Tesis/trabajo/scripts170620'
save_dir    = '/home/julio/Tesis/trabajo/scripts170620/poster_170718'

setwd(central_dir)

for(sujeto in 1:12){
  setwd(central_dir)
  if(grupo_de[sujeto]!=-1){
    graficar = T
  }else{
    graficar=F
  }
  grabar=F
  source('porcentajes12_PDC.R')
}
dev.off()

# if(grabar){
#   setwd(save_dir)
#   write.xlsx(dif_significativas,file='asteriscos.xlsx')
# }

m_mor_NN = matriz_mor[(grupo_de==0),]
m_mor_MN = matriz_mor[(grupo_de==1),]

m_nmor_NN = matriz_nmor[(grupo_de==0),]
m_nmor_MN = matriz_nmor[(grupo_de==1),]

m_tot_NN = matriz_tot[(grupo_de==0),]
m_tot_MN = matriz_tot[(grupo_de==1),]

codigo_NN = nomb_facil[(grupo_de==0)]
codigo_MN = nomb_facil[(grupo_de==1)]

n_NN = sum((grupo_de==0)*1)
n_MN = sum((grupo_de==1)*1)

################################################################

if(grabar){
  setwd(save_dir)
  #png(paste0('Comparacion_gpos_',
  pdf(paste0('Comparacion_gpos_',
             'MOR',
             '.pdf'),width=14,height=6)
  #'.png'),units='in',res=150,width=14,height=6)
}

################################################################

promedios = matrix(nrow=2,ncol=22)
varianzas = matrix(nrow=2,ncol=22)

for(ch in 1:22){
  promedios[1,ch] = mean(m_mor_NN[,ch])
  promedios[2,ch] = mean(m_mor_MN[,ch])
  varianzas[1,ch] = sd(m_mor_NN[,ch])**(3/2)
  varianzas[2,ch] = sd(m_mor_MN[,ch])**(3/2)
}

MAXI = max(promedios+varianzas)
MINI = min(promedios-varianzas)

plot(promedios[1,],type='l',col='white',ylim=c(MINI,MAXI),
     xaxt='n',ylab='% epocas PE',xlab='',
     main=paste0('Estacionariedad (MOR) , *=',p.val))

axis(1,at=1:22,labels=(channel),las=2,
     tick=T)

lines(promedios[1,],type='l',col='blue',lwd=2)
lines(promedios[1,],type='o',col='blue',pch=19)

lines(promedios[2,],type='l',col='red',lwd=2)
lines(promedios[2,],type='o',col='red',pch=19)

arrows(1:22,promedios[1,]-varianzas[1,],
       1:22,promedios[1,]+varianzas[1,],
       code=3,length=0.1,angle=90,col='blue')
arrows(1:22,promedios[2,]-varianzas[2,],
       1:22,promedios[2,]+varianzas[2,],
       code=3,length=0.1,angle=90,col='red')

legend('topright',
       legend=c('Control','PDC'),
       col=c('blue','red'),
       lty=1,lwd=2,cex=0.5)

########
########

signi = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_mor_MN[,ch],equal.variances=F)
  tt = wilcox.test(m_mor_NN[,ch],m_mor_MN[,ch],
                   paired=F,exact=F)
  signi[ch] = as.numeric(tt['p.value'])
}

strst=(MAXI-MINI)/40

for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<.1)
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.05)
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.01)
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

################################################################

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('Comparacion_gpos_',
  pdf(paste0('Comparacion_gpos_',
             'NMOR',
             '.pdf'),width=14,height=6)
  #'.png'),units='in',res=150,width=14,height=6)
}

################################################################
################################################################

promedios = matrix(nrow=2,ncol=22)
varianzas = matrix(nrow=2,ncol=22)

for(ch in 1:22){
  promedios[1,ch] = mean(m_nmor_NN[,ch])
  promedios[2,ch] = mean(m_nmor_MN[,ch])
  varianzas[1,ch] = sd(m_nmor_NN[,ch])**(3/2)
  varianzas[2,ch] = sd(m_nmor_MN[,ch])**(3/2)
}

MAXI = max(promedios+varianzas)
MINI = min(promedios-varianzas)

plot(promedios[1,],type='l',col='white',ylim=c(MINI,MAXI),
     xaxt='n',ylab='% epocas PE',xlab='',
     main=paste0('Estacionariedad (NMOR) , *=',p.val))

axis(1,at=1:22,labels=(channel),las=2,
     tick=T)

lines(promedios[1,],type='l',col='blue',lwd=2)
lines(promedios[1,],type='o',col='blue',pch=19)

lines(promedios[2,],type='l',col='red',lwd=2)
lines(promedios[2,],type='o',col='red',pch=19)

arrows(1:22,promedios[1,]-varianzas[1,],
       1:22,promedios[1,]+varianzas[1,],
       code=3,length=0.1,angle=90,col='blue')
arrows(1:22,promedios[2,]-varianzas[2,],
       1:22,promedios[2,]+varianzas[2,],
       code=3,length=0.1,angle=90,col='red')

legend('topleft',
       legend=c('Control','PDC'),
       col=c('blue','red'),
       lty=1,lwd=2,cex=0.5)

########
########

signi = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_nmor_NN[,ch],m_nmor_MN[,ch],equal.variances=F)
  tt = wilcox.test(m_nmor_NN[,ch],m_nmor_MN[,ch],
                   paired=F,exact=F)
  signi[ch] = as.numeric(tt['p.value'])
}

strst=(MAXI-MINI)/40

for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<.1)
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.05)
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.01)
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

################################################################

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('Comparacion_gpos_',
  pdf(paste0('Comparacion_gpos_',
             'TOT',
             '.pdf'),width=14,height=6)
  #'.png'),units='in',res=150,width=14,height=6)
}

################################################################

promedios = matrix(nrow=2,ncol=22)
varianzas = matrix(nrow=2,ncol=22)

for(ch in 1:22){
  promedios[1,ch] = mean(m_tot_NN[,ch])
  promedios[2,ch] = mean(m_tot_MN[,ch])
  varianzas[1,ch] = sd(m_tot_NN[,ch])**(3/2)
  varianzas[2,ch] = sd(m_tot_MN[,ch])**(3/2)
}

MAXI = max(promedios+varianzas)
MINI = min(promedios-varianzas)

plot(promedios[1,],type='l',col='white',ylim=c(MINI,MAXI),
     xaxt='n',ylab='% epocas PE',xlab='',
     main=paste0('Estacionariedad (TOTAL) , *=',p.val))

axis(1,at=1:22,labels=(channel),las=2,
     tick=T)

lines(promedios[1,],type='l',col='blue',lwd=2)
lines(promedios[1,],type='o',col='blue',pch=19)

lines(promedios[2,],type='l',col='red',lwd=2)
lines(promedios[2,],type='o',col='red',pch=19)

arrows(1:22,promedios[1,]-varianzas[1,],
       1:22,promedios[1,]+varianzas[1,],
       code=3,length=0.1,angle=90,col='blue')
arrows(1:22,promedios[2,]-varianzas[2,],
       1:22,promedios[2,]+varianzas[2,],
       code=3,length=0.1,angle=90,col='red')

legend('topleft',
       legend=c('Control','PDC'),
       col=c('blue','red'),
       lty=1,lwd=2,cex=0.5)

########
########

signi = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_tot_NN[,ch],m_tot_MN[,ch],equal.variances=F)
  tt = wilcox.test(m_tot_NN[,ch],m_tot_MN[,ch],
                   paired=F,exact=F)
  signi[ch] = as.numeric(tt['p.value'])
}

strst=(MAXI-MINI)/40

for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<.1)
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.05)
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.01)
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

################################################################

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('Comparacion_pvals_gpos_',
  pdf(paste0('Comparacion_pvals_gpos_',
             'MOR_vs_TOTAL',
             '.pdf'),width=14,height=6)
  #'.png'),units='in',res=150,width=14,height=6)
}

dif_par_NN = rep(0,22)
dif_par_MN = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_tot_NN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_mor_NN[,ch],m_tot_NN[,ch],paired=T)
  dif_par_NN[ch] = as.numeric(tt['p.value'])
  
  #tt = t.test(m_mor_MN[,ch],m_tot_MN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_mor_MN[,ch],m_tot_MN[,ch],paired=T)
  dif_par_MN[ch] = as.numeric(tt['p.value'])
}

plot(dif_par_NN,type='l',col='blue',lwd=2,
     ylim=c(0,1),
     main='p-val para diferencias % est MOR vs TOTAL',
     xlab = '',xaxt='n',
     ylab='')
axis(1,at=1:22,label=channel)
lines(dif_par_MN,type='l',col='red',lwd=2)
abline(h=0.1)
abline(h=0.05)
abline(h=0.01)
legend('topright',
       legend=c('Normal','PDC'),
       col=c('blue','red'),
       lty=1,lwd=2,cex=0.5)



#########################################33
#########################################33

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('Comparacion_pvals_gpos_',
  pdf(paste0('Comparacion_pvals_gpos_',
             'MOR_vs_NMOR',
             '.pdf'),width=14,height=6)
             #'.png'),units='in',res=150,width=14,height=6)
}

dif_par_NN = rep(0,22)
dif_par_MN = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_nmor_NN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_mor_NN[,ch],m_nmor_NN[,ch],paired=T)
  dif_par_NN[ch] = as.numeric(tt['p.value'])
  
  #tt = t.test(m_mor_MN[,ch],m_nmor_MN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_mor_MN[,ch],m_nmor_MN[,ch],paired=T)
  dif_par_MN[ch] = as.numeric(tt['p.value'])
}

plot(dif_par_NN,type='l',col='blue',lwd=2,
     ylim=c(0,1),
     main='p-val para diferencias % est MOR vs NMOR',
     xlab = '',xaxt='n',
     ylab='')
axis(1,at=1:22,label=channel)
lines(dif_par_MN,type='l',col='red',lwd=2)
abline(h=0.1)
abline(h=0.05)
abline(h=0.01)
legend('topright',
       legend=c('Normal','PDC'),
       col=c('blue','red'),
       lty=1,lwd=2,cex=0.5)

#########################################33
#########################################33

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('Comparacion_pvals_gpos_',
  pdf(paste0('Comparacion_pvals_gpos_',
             'NMOR_vs_TOTAL',
             '.pdf'),width=14,height=6)
             #'.png'),units='in',res=150,width=14,height=6)
}

dif_par_NN = rep(0,22)
dif_par_MN = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_tot_NN[,ch],m_nmor_NN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_tot_NN[,ch],m_nmor_NN[,ch],paired=T)
  dif_par_NN[ch] = as.numeric(tt['p.value'])
  
  #tt = t.test(m_tot_MN[,ch],m_nmor_MN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_tot_MN[,ch],m_nmor_MN[,ch],paired=T)
  dif_par_MN[ch] = as.numeric(tt['p.value'])
}

plot(dif_par_NN,type='l',col='blue',lwd=2,
     ylim=c(0,1),
     main='p-val para diferencias % est TOTAL vs NMOR',
     xlab = '',xaxt='n',
     ylab='')
axis(1,at=1:22,label=channel)
lines(dif_par_MN,type='l',col='red',lwd=2)
abline(h=0.1)
abline(h=0.05)
abline(h=0.01)
legend('topright',
       legend=c('Normal','PDC'),
       col=c('blue','red'),
       lty=1,lwd=2,cex=0.5)

##################################################
##################################################
##################################################
##################################################

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('comp_etapas_gpos_NORMAL',
  pdf(paste0('comp_etapas_gpos_NORMAL',
             'MOR_vs_TOTAL',
             '.pdf'),width=25/2,height=8.5/2)
             #'.png'),units='in',res=150,width=14,height=6)
}

################################################################

promedios = matrix(nrow=2,ncol=22)
varianzas = matrix(nrow=2,ncol=22)

for(ch in 1:22){
  promedios[1,ch] = mean(m_nmor_NN[,ch])
  promedios[2,ch] = mean(m_mor_NN[,ch])
  varianzas[1,ch] = sd(m_nmor_NN[,ch])**(3/2)
  varianzas[2,ch] = sd(m_mor_NN[,ch])**(3/2)
}

MAXI = max(promedios+varianzas)
MINI = min(promedios-varianzas)

plot(100*promedios[1,],type='l',col='white',ylim=100*c(MINI,MAXI),
     xaxt='n',ylab='% épocas estacionarias',xlab='',
     #main=paste0('Gpo. normal , ','MOR vs TOTAL')
     las=2
)

lines(100*promedios[1,],type='l',col='black',lwd=2)
lines(100*promedios[1,],type='o',col='black',pch=19)

lines(100*promedios[2,],type='l',col='green4',lwd=2)
lines(100*promedios[2,],type='o',col='green4',pch=19)

arrows(1:22,100*promedios[1,]-100*varianzas[1,],
       1:22,100*promedios[1,]+100*varianzas[1,],
       code=3,length=0.1,angle=90,col='black')
arrows(1:22,100*promedios[2,]-100*varianzas[2,],
       1:22,100*promedios[2,]+100*varianzas[2,],
       code=3,length=0.1,angle=90,col='green4')

axis(1,at=1:22,labels=(channel),las=2,
     tick=T)

legend('topleft',
       legend=c('NMOR','MOR'),
       col=c('black','green4'),
       lty=1,lwd=2,cex=0.7)

########
########

strst=(MAXI-MINI)/40

signi = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_nmor_NN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_mor_NN[,ch],m_tot_NN[,ch],paired=T,
                   correct=T)
  signi[ch] = as.numeric(tt['p.value'])
}

for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<.1)
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.05)
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.01)
lines(ch_lin[equis],100*rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

################################################################

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('comp_etapas_gpos_PDC',
  pdf(paste0('comp_etapas_gpos_PDC',
             'MOR_vs_TOTAL',
             '.pdf'),width=14,height=6)
             #'.png'),units='in',res=150,width=14,height=6)
}

################################################################

promedios = matrix(nrow=2,ncol=22)
varianzas = matrix(nrow=2,ncol=22)

for(ch in 1:22){
  promedios[1,ch] = mean(m_tot_MN[,ch])
  promedios[2,ch] = mean(m_mor_MN[,ch])
  varianzas[1,ch] = sd(m_tot_MN[,ch])**(3/2)
  varianzas[2,ch] = sd(m_mor_MN[,ch])**(3/2)
}

MAXI = max(promedios+varianzas)
MINI = min(promedios-varianzas)

plot(promedios[1,],type='l',col='white',ylim=c(MINI,MAXI),
     xaxt='n',ylab='% epocas PE',xlab='',
     main=paste0('Gpo. PDC , ','MOR vs TOTAL')
)

lines(promedios[1,],type='l',col='black',lwd=2)
lines(promedios[1,],type='o',col='black',pch=19)

lines(promedios[2,],type='l',col='green4',lwd=2)
lines(promedios[2,],type='o',col='green4',pch=19)

arrows(1:22,promedios[1,]-varianzas[1,],
       1:22,promedios[1,]+varianzas[1,],
       code=3,length=0.1,angle=90,col='black')
arrows(1:22,promedios[2,]-varianzas[2,],
       1:22,promedios[2,]+varianzas[2,],
       code=3,length=0.1,angle=90,col='green4')

axis(1,at=1:22,labels=(channel),las=2,
     tick=T)

legend('topleft',
       legend=c('Total',
                'MOR'),
       col=c('black','green4'),
       lty=1,lwd=2,cex=0.7)

########
########

strst=(MAXI-MINI)/40

signi = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_nmor_NN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_mor_MN[,ch],m_tot_MN[,ch],paired=T,
                   correct=T)
  signi[ch] = as.numeric(tt['p.value'])
}

for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<.1)
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.05)
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.01)
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

################################################################

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('comp_etapas_gpos_NORMAL',
  pdf(paste0('comp_etapas_gpos_NORMAL',
             'MOR_vs_NMOR',
             '.pdf'),width=14,height=6)
             #'.png'),units='in',res=150,width=14,height=6)
}

################################################################

promedios = matrix(nrow=2,ncol=22)
varianzas = matrix(nrow=2,ncol=22)

for(ch in 1:22){
  promedios[1,ch] = mean(m_nmor_NN[,ch])
  promedios[2,ch] = mean(m_mor_NN[,ch])
  varianzas[1,ch] = sd(m_nmor_NN[,ch])**(3/2)
  varianzas[2,ch] = sd(m_mor_NN[,ch])**(3/2)
}

MAXI = max(promedios+varianzas)
MINI = min(promedios-varianzas)

plot(promedios[1,],type='l',col='white',ylim=c(MINI,MAXI),
     xaxt='n',ylab='% epocas PE',xlab='',
     main=paste0('Gpo. normal , ','MOR vs NMOR')
)

lines(promedios[1,],type='l',col='black',lwd=2)
lines(promedios[1,],type='o',col='black',pch=19)

lines(promedios[2,],type='l',col='green4',lwd=2)
lines(promedios[2,],type='o',col='green4',pch=19)

arrows(1:22,promedios[1,]-varianzas[1,],
       1:22,promedios[1,]+varianzas[1,],
       code=3,length=0.1,angle=90,col='black')
arrows(1:22,promedios[2,]-varianzas[2,],
       1:22,promedios[2,]+varianzas[2,],
       code=3,length=0.1,angle=90,col='green4')

axis(1,at=1:22,labels=(channel),las=2,
     tick=T)

legend('topleft',
       legend=c('NMOR',
                'MOR'),
       col=c('black','green4'),
       lty=1,lwd=2,cex=0.7)

########
########

strst=(MAXI-MINI)/40

signi = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_nmor_NN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_mor_NN[,ch],m_nmor_NN[,ch],paired=T,
                   correct=T)
  signi[ch] = as.numeric(tt['p.value'])
}

for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<.1)
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.05)
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.01)
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

################################################################

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('comp_etapas_gpos_PDC',
  pdf(paste0('comp_etapas_gpos_PDC',
             'MOR_vs_NMOR',
             '.pdf'),width=14,height=6)
             #'.png'),units='in',res=150,width=14,height=6)
}

################################################################

promedios = matrix(nrow=2,ncol=22)
varianzas = matrix(nrow=2,ncol=22)

for(ch in 1:22){
  promedios[1,ch] = mean(m_nmor_MN[,ch])
  promedios[2,ch] = mean(m_mor_MN[,ch])
  varianzas[1,ch] = sd(m_nmor_MN[,ch])**(3/2)
  varianzas[2,ch] = sd(m_mor_MN[,ch])**(3/2)
}

MAXI = max(promedios+varianzas)
MINI = min(promedios-varianzas)

plot(promedios[1,],type='l',col='white',ylim=c(MINI,MAXI),
     xaxt='n',ylab='% epocas PE',xlab='',
     main=paste0('Gpo. PDC , ','MOR vs NMOR')
)

lines(promedios[1,],type='l',col='black',lwd=2)
lines(promedios[1,],type='o',col='black',pch=19)

lines(promedios[2,],type='l',col='green4',lwd=2)
lines(promedios[2,],type='o',col='green4',pch=19)

arrows(1:22,promedios[1,]-varianzas[1,],
       1:22,promedios[1,]+varianzas[1,],
       code=3,length=0.1,angle=90,col='black')
arrows(1:22,promedios[2,]-varianzas[2,],
       1:22,promedios[2,]+varianzas[2,],
       code=3,length=0.1,angle=90,col='green4')

axis(1,at=1:22,labels=(channel),las=2,
     tick=T)

legend('topleft',
       legend=c('NMOR',
                'MOR'),
       col=c('black','green4'),
       lty=1,lwd=2,cex=0.7)

########
########

strst=(MAXI-MINI)/40

signi = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_nmor_NN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_mor_MN[,ch],m_nmor_MN[,ch],paired=T,
                   correct=T)
  signi[ch] = as.numeric(tt['p.value'])
}

for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<.1)
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.05)
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.01)
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

################################################################

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('comp_etapas_gpos_NORMAL',
  pdf(paste0('comp_etapas_gpos_NORMAL',
             'TOTAL_vs_NMOR',
             '.pdf'),width=14,height=6)
             #'.png'),units='in',res=150,width=14,height=6)
}

################################################################

promedios = matrix(nrow=2,ncol=22)
varianzas = matrix(nrow=2,ncol=22)

for(ch in 1:22){
  promedios[1,ch] = mean(m_tot_NN[,ch])
  promedios[2,ch] = mean(m_nmor_NN[,ch])
  varianzas[1,ch] = sd(m_tot_NN[,ch])**(3/2)
  varianzas[2,ch] = sd(m_nmor_NN[,ch])**(3/2)
}

MAXI = max(promedios+varianzas)
MINI = min(promedios-varianzas)

plot(promedios[1,],type='l',col='white',ylim=c(MINI,MAXI),
     xaxt='n',ylab='% epocas PE',xlab='',
     main=paste0('Gpo. normal , ','NMOR vs TOTAL')
)

lines(promedios[1,],type='l',col='black',lwd=2)
lines(promedios[1,],type='o',col='black',pch=19)

lines(promedios[2,],type='l',col='purple',lwd=2)
lines(promedios[2,],type='o',col='purple',pch=19)

arrows(1:22,promedios[1,]-varianzas[1,],
       1:22,promedios[1,]+varianzas[1,],
       code=3,length=0.1,angle=90,col='black')
arrows(1:22,promedios[2,]-varianzas[2,],
       1:22,promedios[2,]+varianzas[2,],
       code=3,length=0.1,angle=90,col='purple')

axis(1,at=1:22,labels=(channel),las=2,
     tick=T)

legend('topleft',
       legend=c('Total',
                'NMOR'),
       col=c('black','purple'),
       lty=1,lwd=2,cex=0.7)

########
########

strst=(MAXI-MINI)/40

signi = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_nmor_NN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_tot_NN[,ch],m_nmor_NN[,ch],paired=T,
                   correct=T)
  signi[ch] = as.numeric(tt['p.value'])
}

for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<.1)
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.05)
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.01)
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

################################################################

if(grabar){
  dev.off()
  
  setwd(save_dir)
  #png(paste0('comp_etapas_gpos_PDC',
  pdf(paste0('comp_etapas_gpos_PDC',
             'TOTAL_vs_NMOR',
             '.pdf'),width=14,height=6)
             #'.png'),units='in',res=150,width=14,height=6)
}

################################################################

promedios = matrix(nrow=2,ncol=22)
varianzas = matrix(nrow=2,ncol=22)

for(ch in 1:22){
  promedios[1,ch] = mean(m_tot_MN[,ch])
  promedios[2,ch] = mean(m_nmor_MN[,ch])
  varianzas[1,ch] = sd(m_tot_MN[,ch])**(3/2)
  varianzas[2,ch] = sd(m_nmor_MN[,ch])**(3/2)
}

MAXI = max(promedios+varianzas)
MINI = min(promedios-varianzas)

plot(promedios[1,],type='l',col='white',ylim=c(MINI,MAXI),
     xaxt='n',ylab='% epocas PE',xlab='',
     main=paste0('Gpo. PDC , ','NMOR vs TOTAL')
)

lines(promedios[1,],type='l',col='black',lwd=2)
lines(promedios[1,],type='o',col='black',pch=19)

lines(promedios[2,],type='l',col='purple',lwd=2)
lines(promedios[2,],type='o',col='purple',pch=19)

arrows(1:22,promedios[1,]-varianzas[1,],
       1:22,promedios[1,]+varianzas[1,],
       code=3,length=0.1,angle=90,col='black')
arrows(1:22,promedios[2,]-varianzas[2,],
       1:22,promedios[2,]+varianzas[2,],
       code=3,length=0.1,angle=90,col='purple')

axis(1,at=1:22,labels=(channel),las=2,
     tick=T)

legend('topleft',
       legend=c('Total',
                'NMOR'),
       col=c('black','purple'),
       lty=1,lwd=2,cex=0.7)

########
########

strst=(MAXI-MINI)/40

signi = rep(0,22)

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_nmor_NN[,ch],equal.variances=F,paired=T)
  tt = wilcox.test(m_tot_MN[,ch],m_nmor_MN[,ch],paired=T,
                   correct=T)
  signi[ch] = as.numeric(tt['p.value'])
}

for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<.1)
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.05)
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<.01)
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

################################################################

if(grabar){
  dev.off()
}

################################################################