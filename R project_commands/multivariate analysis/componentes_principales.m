X =[ 14157.1036	3101.772	4632.2382	3954.0378	8268.7214	6694.9126	10220.8302	5056.3858
3101.772	22797.224	3690.546	3583.746	3344.782	2845.386	3768.686	1663.726
4632.2382	3690.546	7530.6409	2848.0521	4609.1823	5474.1817	5765.1159	2709.1991
3954.0378	3583.746	2848.0521	6079.3129	4249.0927	5756.8633	4403.9451	1798.1159
8268.7214	3344.782	4609.1823	4249.0927	10107.7721	8940.6159	11952.4993	2976.0977
6694.9126	2845.386	5474.1817	5756.8633	8940.6159	13698.9401	11663.3267	2285.0763
10220.8302	3768.686	5765.1159	4403.9451	11952.4993	11663.3267	18751.0529	5555.5581
5056.3858	1663.726	2709.1991	1798.1159	2976.0977	2285.0763	5555.5581	10407.2849
];
X=sqrt(X);
 
% 1.- Determinamos la matriz de correlación R.
R=corrcoef(X)
 
[u,lambda,v]=svd(R); %Estan normalizados

% y los componenetes principales quedan definidos por los autovectores v.
% Es decir
   
   % Siendo estos vectores ortonormales. Por ejemplo, el producto de los dos
% primeros autovectores debería ser cero.
%v(1:1,:)*v(2:2,:)'

%Estos autovectores definen el nuevo espacio dimensional ortonormal donde
% las variables originales podrían quedar definidas mediante una rotación 
% a estos nuevos ejes.
%
% Se puede observar que la traza tanto de la matriz de correlación R como  
% de los autovalores es la misma.
%
%trace(R)

% Además los autovalores están ordenados en orden decreciente y ellos
% representan en su sumatoria, la varianza total. Veamos:

lambda=lambda*[1 1 1 1 1 1 1 1 ]'
    
% De acuerdo a esta función comando de Matlab y utilizando la matriz de
% correlación R como matriz de entrada, tenemos
%
 [cp,lambda,explicacion]=pcacov(R);
%donde:
% cp: matriz de componentes principales,
% lambda: matriz diagonal con los autovalores de R,
% explicacion: porcentaje explicado de la varianza por cada uno de los 
% componentes.
% Los componentes principales (autovectores) son:


    
[pc, zscores, pcvars] = princomp(X);
scatter(zscores(:,1),zscores(:,2)); 
xlabel('First Principal Component'); 
ylabel('Second Principal Component'); 
title('Principal Component Scatter Plot'); 
cumsum(pcvars./sum(pcvars) * 100)
[pc,z,varianza] = princomp(X);
[m,b]=size(X); 
vex = varianza/sum(varianza); 
vexpor=vex*100; 
vexacumulada=vexpor;  
for i=2:b 
    vexacumulada(i,1)=vexacumulada(i-1,1)+vexpor(i,1); 
end 
  
figure(1)  
pareto(vex) 
xlabel('Componente Principal') 
ylabel('% Varianza Explicada') 
title('Proporción de variabilidad explicada por cada componente principal') 
figure(2) 
plot(varianza) 
title('Gráfico para seleccionar el número de componentes') 
xlabel('Número de componente'); 
ylabel('Valor propio'); 
size(zscores)
plot(zscores(:,1),zscores(:,2))

    

   