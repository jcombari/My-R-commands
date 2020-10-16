X1=[51.88 32.55 15.57
    7.76 70.93 21.31
    37.14 32.45 30.31 
    37.87 43.19 18.94
    41.24 35.12 23.64];
X2=[44.94 34.59 20.47
    6.22 65.96 27.82
    16.09 31.22 52.69
    34.20 43.32 22.48 
    21.08 42.14 36.78];
X3=[25.95 39.15 34.90
    6.44 57.06 36.50
    6.54 24.68 68.78
    16.13 42.18 41.69
    15.03 35.96 49.01];
X=[X1 X2 X3];
 K=3;
[I,z]=size(X);
[I,j1]=size(X1);
[I,j2]=size(X2);
[I,j3]=size(X3);
%Matriz de Centraje
%Centraje  de los datos
n=size(X1);
j=ones(n(1),1); I=eye(n(1));
P=(I-(1/n(1))*j*j');
X1c=P*X1;
X2c=P*X2;
X3c=P*X3;

X1c=X1-(diag(mean(X1))*ones(n(2),n(1)))';
X2c=X2-(diag(mean(X2))*ones(n(2),n(1)))';
X3c=X3-(diag(mean(X3))*ones(n(2),n(1)))';

%Metrica para el calculo de las distancia entre los individuos
M=eye(n(2));
%Objetos
W1=X1c*M*X1c';
W2=X2c*M*X2c';
W3=X3c*M*X3c';
%producto escalar entre objetos
%<Wk1,Wk2>=traza(Wk1*S*Wk2*S)

%Norma Pag. 109 Lavit
S=1/n(1)*eye(n(1));

NW1=trace(W1*S*W1*S);
NW2=trace(W2*S*W2*S);
NW3=trace(W3*S*W3*S);

W(1,1)=trace(W1*S*W1*S);
W(1,2)=trace(W1*S*W2*S);
W(1,3)=trace(W1*S*W3*S);
W(2,1)=trace(W2*S*W1*S);
W(2,2)=trace(W2*S*W2*S);
W(2,3)=trace(W2*S*W3*S);
W(3,1)=trace(W3*S*W1*S);
W(3,2)=trace(W3*S*W2*S);
W(3,3)=trace(W3*S*W3*S);

Norma=sqrt([NW1 NW2 NW3 ]);
fprintf('La norma es    %6.0f %6.0f %6.0f \n',Norma');

