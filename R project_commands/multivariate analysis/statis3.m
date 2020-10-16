clear;
load datosstatisabdi;
j=ones(n,1);
m=mean(X)';
Xc=X-j*m';
S=cov(X);
Ds=sqrt(diag(diag(S)));
Xe=Xc*inv(Ds);

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
[V,D]=svd(c);
landa=diag(D);
sumV=sum(V);
alfa=V*inv(diag(sumV));

 G=V*D^(0.5);

plot(G(:,1),G(:,2), '+')

