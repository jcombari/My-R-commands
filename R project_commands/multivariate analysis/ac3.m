clear;
clc;
N = [  49	50	42	18	25	23	25	59	291
83	83	76	60	69	68	69	74	582
61	61	51	32	38	39	39	72	393
60	88	42	41	75	70	61	19	456
78	22	18	19	17	19	14	80	267
26	11	13	17	13	11	13	29	133
64	64	56	34	45	42	46	68	419
88	79	85	64	45	46	37	41	485
24	21	12	10	13	12	13	85	190
7	61	12	11	53	50	48	54	296
83	87	85	79	83	82	80	90	669
45	77	36	16	65	69	76	89	473
88	92	87	60	70	67	67	81	612
12	4	13	38	5	6	8	7	93
50	62	69	43	49	51	61	60	445
38	41	27	11	16	18	17	49	217
36	30	24	16	19	19	17	40	201
3	35	9	8	28	25	21	4	133
43	87	29	32	82	80	43	40	436
12	91	27	16	84	81	72	67	450
950	1146	813	625	894	878	827	1108	7241
];

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