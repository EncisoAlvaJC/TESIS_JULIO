#################################################
# directorios de trabajo
data_dir    = '~/TESIS/graf_datos/estacionariedad_sinfiltro/'
central_dir = '~/TESIS/TESIS/img_ejemplos'
e_dir       = '~/TESIS/graf_datos/epocas3/'
r_dir       = central_dir

#################################################
# parametros del script
p.val  = 0.05
p.ast  = c(.05,.01,.005)

grabar.gral = T

graf.indv   = T
grabar.indv = F

grabar.ast  = T

#################################################
# constantes generales
channel   = c('C3','C4','CZ',
              'F3','F4','F7','F8',
              'FP1','FP2','FZ',
              'O1','O2','P3','P4','PZ',
              'T3','T4','T5','T6',
              'LOG','ROG',
              'EMG')
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
frecuenciasss = c(200,
                  512,512,
                  200,200,
                  512,512,
                  200,
                  512,512,
                  512,512)
grupo_de = c(0,0,0,0,0,1,1,1,1,-1,-1,-1)
#grupo_de = c(-1,0,0,-1,-1,1,1,-1,-1,-1,-1,-1)
#grupo_de =  c(0,-1,-1,0,0,-1,-1,1,1,-1,-1,-1)
#grupo_de = c(1,1,1,1,1,1,1,1,1,-1,0,0)

#################################################
# para grabar datos en excel
require(xlsx)

#################################################
# parametros de apoyo
n.canales = length(channel)
ast       = c(' ','*','**','***')

#################################################
# contenedores de datos
dif_significativas            = matrix(nrow=22,ncol=12)
colnames(dif_significativas)  = nomb_facil
row.names(dif_significativas) = channel

matriz_mor  = matrix(nrow=12,ncol=22)
matriz_nmor = matrix(nrow=12,ncol=22)
matriz_tot  = matrix(nrow=12,ncol=22)

colnames(matriz_mor)  = channel
row.names(matriz_mor) = nomb_facil

#################################################
# cargar los datos
for(sujeto in 1:12){
  setwd(central_dir)
  source('~/TESIS/TESIS/img_ejemplos/porcentajes13_PDC.R')
}

#################################################
# diferencias significativas MOR VS NMOR
if(grabar.ast){
  setwd(r_dir)
  write.xlsx(dif_significativas,file='asteriscos.xlsx')
}

#################################################
# separacion de grupos para comparar
m_mor_NN  =  matriz_mor[(grupo_de==0),]
m_mor_MN  =  matriz_mor[(grupo_de==1),]
m_nmor_NN = matriz_nmor[(grupo_de==0),]
m_nmor_MN = matriz_nmor[(grupo_de==1),]
m_tot_NN  =  matriz_tot[(grupo_de==0),]
m_tot_MN  =  matriz_tot[(grupo_de==1),]

codigo_NN = nomb_facil[(grupo_de==0)]
codigo_MN = nomb_facil[(grupo_de==1)]

n_NN = sum((grupo_de==0)*1)
n_MN = sum((grupo_de==1)*1)

#################################################
# parametros graficos
rojito    = rgb(255, 64, 64,maxColorValue=255)
verdecito = rgb( 64,255, 64,maxColorValue=255)
azulito   = rgb( 64, 64,255,maxColorValue=255)
gricesito = rgb(128,128,128,maxColorValue=255)

################################################################
# inicio 
# Grupos MOR
if(grabar.gral){
  setwd(r_dir)
  #png(
  pdf(
    paste0('Comparacion_gpos_',
           'MOR',
           '_v2.pdf'),width=6,height=3.5)
  #'.png'),units='in',res=150,width=14,height=6)
}

MAXI = 60

par(mar=c(2.5,3,2,0),
    mgp=c(1.7,.6,0))
#par(new=T)
boxplot(100*m_mor_MN,type='l',#col='red',
        col=rojito,
        ylim=c(0,MAXI),
        xaxt='n',ylab='Épocas estacionarias [%]',xlab='',
        main='Estacionariedad | Sueño MOR',
        bty='n',las=2,frame=F,
        at=(1:22)-.15,boxwex=.3)
boxplot(100*m_mor_NN,type='l',#col='blue',
        col=azulito,
        ylim=c(0,MAXI),
        bty='n',las=2,frame=F,add=T,
        at=(1:22)+.15,boxwex=.3,
        xaxt='n',yaxt='n')

axis(1,at=1:22,labels=(channel),las=2,tick=T)
legend('topright',
       legend=c('Grupo Nn','Grupo Mn'),
       col=c('blue','red'),
       lty=1,lwd=4,cex=1,bty='n')

# asteriscos de diferencias significativas
signi = rep(0,22)
strst=5

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_mor_MN[,ch],equal.variances=F)
  tt = wilcox.test(m_mor_NN[,ch],m_mor_MN[,ch],
                   paired=F,exact=F)
  signi[ch] = as.numeric(tt['p.value'])
}
for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<p.ast[1])
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='black')
equis = (signi<p.ast[2])
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='black')
equis = (signi<p.ast[3])
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='black')

if(grabar.gral){
  dev.off()
}
# fin
# Grupos MOR
################################################################

################################################################
# inicio 
# Grupos NMOR
if(grabar.gral){
  setwd(r_dir)
  #png(
  pdf(
    paste0('Comparacion_gpos_',
           'NMOR',
           '_v2.pdf'),width=6,height=3.5)
  #'.png'),units='in',res=150,width=14,height=6)
}

MAXI = 50

par(mar=c(2.5,3,2,0),
    mgp=c(1.7,.6,0))
#par(new=T)
boxplot(100*m_nmor_MN,type='l',col=rojito,
        ylim=c(0,MAXI),
        xaxt='n',ylab='Épocas estacionarias [%]',xlab='',
        main='Estacionariedad | Sueño NMOR',
        bty='n',las=2,frame=F,
        at=(1:22)-.15,boxwex=.3)
boxplot(100*m_nmor_NN,type='l',col=azulito,
        ylim=c(0,MAXI),
        bty='n',las=2,frame=F,add=T,
        at=(1:22)+.15,boxwex=.3,
        xaxt='n',yaxt='n')

axis(1,at=1:22,labels=(channel),las=2,tick=T)
legend('topleft',
       legend=c('Grupo Nn','Grupo Mn'),
       col=c('blue','red'),
       lty=1,lwd=4,cex=1,bty='n')

# asteriscos de diferencias significativas
signi = rep(0,22)
strst=5

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_mor_MN[,ch],equal.variances=F)
  tt = wilcox.test(m_nmor_NN[,ch],m_nmor_MN[,ch],
                   paired=F,exact=F)
  signi[ch] = as.numeric(tt['p.value'])
}
for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<p.ast[1])
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='black')
equis = (signi<p.ast[2])
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='black')
equis = (signi<p.ast[3])
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='black')

if(grabar.gral){
  dev.off()
}
# fin
# Grupos NMOR
################################################################

################################################################
# inicio 
# Grupos NMOR
if(grabar.gral){
  setwd(r_dir)
  #png(
  pdf(
    paste0('Comparacion_etapas_',
           'normal_MOR_vs_NMOR',
           '_v2.pdf'),width=6,height=3.5)
  #'.png'),units='in',res=150,width=14,height=6)
}

MAXI = 50

par(mar=c(2.5,3,2,0),
    mgp=c(1.7,.6,0))
#par(new=T)
boxplot(100*m_nmor_NN,type='l',col=gricesito,
        ylim=c(0,MAXI),
        xaxt='n',ylab='Épocas estacionarias [%]',xlab='',
        main='Estacionariedad | Grupo Nn',
        bty='n',las=2,frame=F,
        at=(1:22)-.15,boxwex=.3)
boxplot(100*m_mor_NN,type='l',col=verdecito,
        ylim=c(0,MAXI),
        bty='n',las=2,frame=F,add=T,
        at=(1:22)+.15,boxwex=.3,
        xaxt='n',yaxt='n')

axis(1,at=1:22,labels=(channel),las=2,tick=T)
legend('topleft',
       legend=c('NMOR','MOR'),
       col=c(gray(.25),'green4'),
       lty=1,lwd=4,cex=1,bty='n')

# asteriscos de diferencias significativas
signi = rep(0,22)
strst=5

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_mor_MN[,ch],equal.variances=F)
  tt = wilcox.test(m_mor_NN[,ch],m_nmor_NN[,ch],
                   paired=F,exact=F)
  signi[ch] = as.numeric(tt['p.value'])
}
for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<p.ast[1])
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<p.ast[2])
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<p.ast[3])
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

if(grabar.gral){
  dev.off()
}
# fin
# Grupos NMOR
################################################################

################################################################
# inicio 
# Grupos NMOR
if(grabar.gral){
  setwd(r_dir)
  #png(
  pdf(
    paste0('Comparacion_etapas_',
           'pdc_MOR_vs_NMOR',
           '_v2.pdf'),width=6,height=3.5)
  #'.png'),units='in',res=150,width=14,height=6)
}

MAXI = 60

par(mar=c(2.5,3,2,0),
    mgp=c(1.7,.6,0))
#par(new=T)
boxplot(100*m_nmor_MN,type='l',col=gricesito,
        ylim=c(0,MAXI),
        xaxt='n',ylab='Épocas estacionarias [%]',xlab='',
        main='Estacionariedad | Grupo Mn',
        bty='n',las=2,frame=F,
        at=(1:22)-.15,boxwex=.3)
boxplot(100*m_mor_MN,type='l',col=verdecito,
        ylim=c(0,MAXI),
        bty='n',las=2,frame=F,add=T,
        at=(1:22)+.15,boxwex=.3,
        xaxt='n',yaxt='n')

axis(1,at=1:22,labels=(channel),las=2,tick=T)
legend('topright',
       legend=c('NMOR','MOR'),
       col=c(gray(.25),'green4'),
       lty=1,lwd=4,cex=1,bty='n')

# asteriscos de diferencias significativas
signi = rep(0,22)
strst=5

for(ch in 1:22){
  #tt = t.test(m_mor_NN[,ch],m_mor_MN[,ch],equal.variances=F)
  tt = wilcox.test(m_mor_MN[,ch],m_nmor_MN[,ch],
                   paired=F,exact=F)
  signi[ch] = as.numeric(tt['p.value'])
}
for(i in 1:22){
  if(is.nan(signi[i])){
    signi[i] = 1
  }
}

ch_lin = 1:22
equis = (signi<p.ast[1])
lines(ch_lin[equis],rep(MAXI        ,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<p.ast[2])
lines(ch_lin[equis],rep(MAXI-  strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')
equis = (signi<p.ast[3])
lines(ch_lin[equis],rep(MAXI-2*strst,sum(equis*1)),
      type='p',pch='*',lwd=0,cex=2,col='red')

if(grabar.gral){
  dev.off()
}
# fin
# Grupos NMOR
################################################################