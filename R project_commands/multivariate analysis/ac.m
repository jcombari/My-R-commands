clear;
clc;
N = [  1 0 0
        0 1 0
        1 1 1
        1 0 1
        0 1 1  ]
% N = [11 14 107 75 ;
%  1 10 60 30 ;
%  23 2 166 40]

 [L K] = size(N) 
 n = sum(sum(N)) % total
 % ------------ tableau des frequences
 freq = N./n
 %  -----------  distribution marginale
 nj = sum(N) % les  colonnes
 ni = sum(N')  % les lignes
 % ------------
 DL = diag(ni)
 DK = diag(nj)
 % profil lignes et profil colonnes
 XL = DL^-1 * N
 XK = (DK^-1)' * N'
 % -------- metrique de khi2
 ML = n * DK^-1
 MK = n * DL^-1
 % matrice variance covariance
 VL = XL' * (1/n * DL) * XL
 VK = XK' * (1/n * DK) * XK
 %
 MLVL = ML * VL
 MKVK = MK * VK
 %
 [vect_L val_L] = eig(MLVL)
 [vect_K val_K] = eig(MKVK)
 %
 c = XL * vect_L
 d = XK * vect_K
 %
 figure
 plot(c(:,1),c(:,2),'*')%coordone ligne
 hold on
 plot(d(:,1),d(:,2),'o')%coordonne colonne