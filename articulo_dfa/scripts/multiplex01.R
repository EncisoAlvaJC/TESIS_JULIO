dir_scripts = 'C:/Users/EQUIPO 1/Desktop/julio/TESIS/articulo_dfa/scripts/'

sujeto = 4

epo_0   = 1153
epo_f   = 1244

etiquetar.corto = F
dur_epoca = 30
source(paste0(dir_scripts,'colorcitos_usable04_todo_win.R'))

dur_epoca = 1
source(paste0(dir_scripts,'graf_espectro_integrado05_todo_win.R'))

Q = ggarrange(G1,G2,ncol=1,nrow=2,heights = c(1/5,4/5),align = 'v')

plot(Q)

#ggsave(filename = paste0('mezcla_',nombre,'png'),device='png',
#       dpi=400,width=6,heigth=10,scale=2,path=dir_graf)
