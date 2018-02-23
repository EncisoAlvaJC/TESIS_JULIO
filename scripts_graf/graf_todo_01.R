dir_todo = 'C:/Users/EQUIPO 1/Desktop/julio/Tesis_en_Git/Tesis/scripts_graf/'
setwd(dir_todo)

for(sujeto in 1:13){
  setwd(dir_todo)
  source('graf_espectro_integrado06_todo_bis.R')
  setwd(dir_todo)
  source('colorcitos_usable05_todo_bis.R')
}

#theme(plot.margin = unit(c(3,3,3,3), "lines"))