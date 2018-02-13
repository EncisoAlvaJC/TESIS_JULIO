
sujeto = 6

epo_0 = 166-10
epo_f = 176

dur_epoca = 30

######################################33

factor.extra = 1
if(fr_muestreo==200){
  factor.extra = 3
}

zoom           = F
unidad_par_t   = 'tiempo'
ajuste_ini_hms = c(0,0,0)

min_hms = t2hms(dur_epoca*(epo_0-1)/factor.extra)
max_hms = t2hms(dur_epoca* epo_f   /factor.extra)

etiquetar.corto = F

source('C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts/colorcitos_usable04_todo_zoom_win.R')

G0 = G1

zoom           = T

source('C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts/colorcitos_usable04_todo_zoom_win.R')

stop('Debug')

Q = ggarrange(G0,G1,ncol=1,nrow=2,heights = c(.5,.5),align = 'v')

plot(Q)
