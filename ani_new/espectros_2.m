#fr = csvread('fn7o.csv');
#yy = csvread('yn7o.csv');

#fr = csvread('fn8o.csv');
#yy = csvread('yn8o.csv');

#fr = csvread('fd7o.csv');
#yy = csvread('yd7o.csv');

#fr = csvread('fd8o.csv');
#yy = csvread('yd8o.csv');

#fr = csvread('fn7f.csv');
#yy = csvread('yn7f.csv');

#fr = csvread('fn8f.csv');
#yy = csvread('yn8f.csv');

#fr = csvread('fd7f.csv');
#yy = csvread('yd7f.csv');

fr = csvread('fd8f.csv');
yy = csvread('yd8f.csv');



fr = fr(:,2);

L = length(fr)-1
fr = fr(1:L);

K = length(yy(:,1))-1
yy = yy(2:K,1:L);

ind = 1:(K-1)

###################################3

#
#surf(ind,fr,yy(ind,:)');
#ylabel('Frecuencia normalizada')
#

# usando la longitud de la serie
longitud = 512*6;
t_max = 6;
surf(ind*(t_max/(K-1)),fr*longitud,yy(ind,:)');

ylabel('1 / Frecuencia')
xlabel('Tiempo')
#title('Espectro de Priestley, EEG')

view(135,30)

axis( [ (t_max/(K-1)) t_max longitud*fr(1) longitud*fr(L)])


#title('F7 , normal , sin filtro')
#print('-S640,480', 'ejemplo_espectro.pdf')
