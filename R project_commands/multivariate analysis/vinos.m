clear all
vinos=[1	1	6	7	2	5	7	6	3	6	7
2	5	3	2	4	4	4	2	4	4	3
3	6	1	1	5	2	1	1	7	1	1
4	7	1	2	7	2	1	2	2	2	2
5	2	5	4	3	5	6	5	2	6	6
6	3	4	4	3	5	4	5	1	7	5
];

p = [3 4 3]; % 10 variables en total
P =cumsum(p);
X=vinos(:,2:P(length(P))+1);
n = size(X,1);             % 6 tipos de vino
t = 3;                    % 3 expertos catadores 
save datosafmabdi

clear;
load datosafmabdi
j=ones(n,1);
m=mean(X)';
Xc=X-j*m';
S=cov(X);
Ds=sqrt(diag(diag(S)));
Xe=Xc*inv(Ds);

%Matrices al detalle
X1=X(:,1:P(1));
X2=X(:,P(1)+1:P(2));
X3=X(:,P(2)+1:P(3));

%Matriz de centraje
j=ones(n,1); I=eye(n);
P=(I-(1/n)*j*j');

%Centramiento de datos
X1c=P*X1;
X2c=P*X2;
X3c=P*X3;

%Normalización de cada matriz
X1n= X1c./(ones(n,p(1))*diag(sqrt(sum(X1c.^2))));
X2n= X2c./(ones(n,p(2))*diag(sqrt(sum(X2c.^2))));
X3n= X3c./(ones(n,p(3))*diag(sqrt(sum(X3c.^2))));

%Matriz de pesos M
M=1/n*eye(n);

%Espacio Global
%Calcularemos un ACP para cada estudio
[V1,D1]=svd(X1n);
landa1=diag(D1);
sumV1=sum(V1);
Z1=(1/landa1(1))*X1n;

[V2,D2]=svd(X2n);
landa2=diag(D2);
sumV2=sum(V2);
Z2=(1/landa2(1))*X2n;

[V3,D3]=svd(X3n);
landa3=diag(D3);
sumV3=sum(V3);
Z3=(1/landa3(1))*X3n;

%construcción de Matriz global
Z=[Z1 Z2 Z3];

%Analisis de componentes principales Global
[U,D,V]=svd(Z);

%Puntuaciones globales para los vinos son
%En la matriz F cada fila representa un vino (una observación)
%y cada columna una componente.
F=M^(-0.5)*U*D
var_acumF=diag(D)/sum(diag(D))*100;
%OJO PROFESOR PORQUE CALCULAN LA VARIABILIDAD ACUMULADA COMO  var_acumF=diag(D.^2)/sum(diag(D.^2))*100;

