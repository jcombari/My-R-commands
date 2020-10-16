clear
K = [  70 0 0
       45 45 0
       30 30 30
      0 80 20
        35 5 10 ];

[I J] = size(K);      
n = sum(sum(K)); % total
%F=(1/n)*K;
 F=K/n;
 nj = sum(K); % Columnas
 ni = sum(K');  % filas
 DI = diag(ni)/n;
 DJ = diag(nj)/n;
 RIxJ=inv(DI)*F;
 CIxJ=F*inv(DJ);
 S=F'*inv(DI)*F*inv(DJ);
 
 [vect_S val_S] = eigs(S);

 %Variabilidad explicada para las filas
 val_S2=diag(val_S);
 var_S=val_S2(2:length(val_S2));
 var_S=var_S/sum(var_S)*100;
 explicacion_filas=[val_S2(2:length(val_S2)) var_S cumsum(var_S)]

Coord_ind=F*inv(DJ)*vect_S;
chi_cuadrado=(trace(S)-1)*n;
%Contribuciones del individuo al factor
CIFa=DI*Coord_ind.^2*inv(val_S); 



 T=F*inv(DJ)*F'*inv(DI);
 [vect_T val_T] = eigs(T);
 Coord_var=inv(sqrt(val_S))*CIxJ'*Coord_ind
  %Variabilidad explicada para las columnas 
 val_T2=diag(val_T);
 var_T=val_T2(2:length(val_T2));
 var_T=var_T/sum(var_T)*100;
 explicacion_columnaT=[val_T2(2:length(val_T2)) var_T cumsum(var_T)];
 Coord_var=CIxJ'*inv(DI)*vect_T;
 chi_cuadrado2=(trace(T)-1)*n;
 
 %Contribuciones de la variable al factor
CVF=DJ*Coord_var.^2*inv(val_T);




%Verificación de los ejercicios de la pag 11 del capitulo de
%correspondencias binarias
%***********************************
%       Relaciones Básicas
%***********************************
jI=ones(1,I)';
jJ=ones(1,J)';
g=F'*jI
h=F*jJ
%***********************************
%       Demostraciones
%***********************************
%1.- jJ'*g=1
jJ'*g
%2.- jJ'*DJ=g'   and jI'*DI=h'
jJ'*DJ
jI'*DI
%3.-inv(DJ)*g=jJ and inv(DJ)*g=jI
inv(DJ)*g
inv(DJ)*g
%4.-RIxJ*jJ=jI and CIxJ'*jI=jJ
RIxJ*jJ
CIxJ'*jI
%5.-h'*RIxJ=g' and CIxJ*g=h
h'*RIxJ
CIxJ*g
%6.-RIxJ'*DI*RIxJ=F'*inv(DI)*F and CIxJ*DJ*CIxJ'=F*inv(DJ)*F'
RIxJ'*DI*RIxJ
F'*inv(DI)*F
CIxJ*DJ*CIxJ'
F*inv(DJ)*F'
%7.-Coord_ind'*DI*Coord_ind
Coord_ind'*DI*Coord_ind
DJ*DJ
Coord_var'*DJ*Coord_var
DI*DI
%8.- S*g=g and T*h=h
S*g
T*h

