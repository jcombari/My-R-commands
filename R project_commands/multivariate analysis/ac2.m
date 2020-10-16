clear;
clc;
N = [  70 0 0
       45 45 0
       30 30 30
      0 80 20
        35 5 10 ]
% N = [11 14 107 75 ;
%  1 10 60 30 ;
%  23 2 166 40]

 [I J] = size(N)  
 
%  [L K] = size(N) 
 n = sum(sum(N)) % total
 % ------------ tableau des frequences
 freq = N./n
 %  -----------  distribution marginale
 nj = sum(N) % les  colonnes
 ni = sum(N')  % les lignes
 % ------------
 DI = diag(ni)
 DJ = diag(nj)
 % profil lignes et profil colonnes
 XI = DI^-1 * N
 XJ = (DJ^-1)' * N'
 
 RIJ = DI^(-1) * N
 CIJ = N*DJ^(-1)
 % -------- metrique de khi2
 MI = n * DJ^-1
 MJ = n * DI^-1
 % matrice variance covariance
 VI = XI' * (1/n * DI) * XI
 VJ = XJ' * (1/n * DJ) * XJ
 %
 MIVI = MI * VI
 MJVJ = MJ * VJ
 %
 [vect_I val_I] = eig(MIVI)
 [vect_J val_J] = eig(MJVJ)
 %
 %Variabilidad explicada para las filas
 val_I2=diag(val_I);
 var_I=val_I2(2:length(val_I2));
 var_I=var_I/sum(var_I)*100;
explicacion_filas=[val_I2(2:length(val_I2)) var_I cumsum(var_I)]
 %Variabilidad explicada para las columnas
 val_J2=diag(val_J);
 var_J=val_J2(2:length(val_J2));
 var_J=var_J/sum(var_J)*100;
 explicacion_columnas=[val_J2(2:length(val_J2)) var_J cumsum(var_J)]
% 
 c = XI * vect_I
 d = XJ * vect_J
 %
 figure
 plot(c(:,2),c(:,3),'*')%coordone ligne
 hold on
 plot(d(:,2),d(:,3),'o')%coordonne colonne