clear; 
load datosvalentin;
j=ones(n,1);
m=mean(X)';
Xc=X-j*m';
S=cov(X);
Ds=sqrt(diag(diag(S)));
%Xe=Xc*inv(Ds);
Xe=Xc;
%calculo de los cuadrados dentro de cada submatriz;

%1-Calculo de  los cuadrados de cada submatriz

for k=1
    s2(k)=trace(Xe(:,1:P(k))'*Xe(:,1:P(k)));
end
for k=2:t
          s2(k)=trace(Xe(:,P(k-1)+1:P(k))'*Xe(:,P(k-1)+1:P(k)));
end
%2- Asignacion a B de la matriz estandarizada
for k=1
    for i=1:n
        for j=1:P(1)
            B(i,j,k)=Xe(i,j);
        end
    end
end


for k=2:t
    for i=1:n
        for j=P(k-1)+1:P(k)
            B(i, j-P(k-1),k)=Xe(i,j);
        end;
    end;
end;

%opcional
for k=1:t
    B(:,1:p(k),k)=B(:,1:p(k),k)/sqrt(s2(k));
end;

%Objetos
for k=1:t
    W(:,:,k)=B(:,:,k)*B(:,:,k)';
end;

%Matriz de productos internos entre los objetos
for i=1:t
    for h=1:t
        c(i,h)=trace(W(:,:,i)*W(:,:,h));
    end
end

%Coeficientes de Escoufier
for i=1:t 
    for h=1:t
        RV(i,h)=c(i,h)/sqrt(c(i,i)*c(h,h));
    end
end

%Distancia entre objetos
for i=1:t
    for h=1:t
        dist(i,h)=RV(i,i)+RV(h,h)-2*RV(i,h);
    end
end

%OJO: Algunos autores diagonalizan c otros RV.
[V,D]=svd(RV);
landa=diag(D);
sumV=sum(V);
alfa=V*inv(diag(sumV));

G=V*D^(0.5);

 figure
axis on
 plot(G(:,1),G(:,2),'*r') %coord ind
 hold on
 %plot(Coord_var(:,1),Coord_var(:,2),'o')  %coord var
%legend('individuos','variables','Location','SouthOutside' )


%Calculo del compromiso
alfa_compro=alfa(:,1);
j=ones(n,1);
I=eye(n(1));
m=mean(X)';
Pcent=(I-(1/n(1))*j*j');
Xe=Pcent*X;


X1e=Xe(:,1:P(1));
X2e=Xe(:,P(1)+1:P(2));
X3e=Xe(:,P(2)+1:P(3));


S1=X1e*X1e';
S2=X2e*X2e';
S3=X3e*X3e'
Scompro=alfa_compro(1)*S1+alfa_compro(2)*S2+alfa_compro(3)*S3;

%Analisis del compromiso

[V2,D2]=svd(Scompro);
landa2=diag(D2);
sumV2=sum(V2);
alfa2=V2*inv(diag(sumV2));

F=V2*D2^(0.5); 
% En esta matriz F cada columna representa una observación, que en este
% caso en un vino. Cada columna de la matriz F es una componente
 plot(F(:,1),F(:,2),'o')  %coord var
 
 %Proyeccion de los estudios en el espacio compromiso
 
Coordenada_experto1=S1*V2*D2^(-0.5);
Coordenada_experto2=S2*V2*D2^(-0.5);
Coordenada_experto3=S3*V2*D2^(-0.5);

