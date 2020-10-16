A=[ 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 ;
    1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 ;
    1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 ;
    1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 ];
n=size(A);
p=[4, 4 ,4, 4]; %tamaño de cada uno de los arreglos
P=cumsum(p);
%Matriz de 3 dimensiones
t=4;
for k=1
    for i=1:n
         for j=1:P(1)
             B(i,j,k)=A(i,j);
         end;
    end;
end;

for k=2:t
    for i=1:n
        for j=P(k-1)+1:P(k)
            B(i,j-P(k-1),k)=A(i,j);
        end;
    end;
end;

        