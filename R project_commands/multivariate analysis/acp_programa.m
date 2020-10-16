% X=[1 2
%3 1
%2 3];
%X=[    1.0000    0.1727    0.4486    0.4262    0.6912    0.4807    0.6273    0.4166
%    0.1727    1.0000    0.2817    0.3044    0.2203    0.1610    0.1823    0.1080
%    0.4486    0.2817    1.0000    0.4209    0.5283    0.5390    0.4852    0.3060
%    0.4262    0.3044    0.4209    1.0000    0.5421    0.6308    0.4125    0.2261
%    0.6912    0.2203    0.5283    0.5421    1.0000    0.7598    0.8682    0.2902
%    0.4807    0.1610    0.5390    0.6308    0.7598    1.0000    0.7277    0.1914
%    0.6273    0.1823    0.4852    0.4125    0.8682    0.7277    1.0000    0.3977
%    0.4166    0.1080    0.3060    0.2261    0.2902    0.1914    0.3977    1.0000];

    
function acp(X)
matriz=X'*X;
[vect val] = eigs(matriz);
%Variabilidad capturada por cada factor
variabilidad=diag(val)/sum(diag(val))*100;
Coord_ind=X*vect;
D=sqrt(val);
Coord_var=vect*D;
Variabilidad=trace(X'*X);
%Contribuciones del individuo al factor
CIF=Coord_ind.^2*inv(val);
%Contribuciones de la variable al factor
CVF=Coord_var.^2*inv(val);
%Suma de cuadrados de las coordenadas de individuos
TI=sum(Coord_ind.^2')';
%Suma de cuadrados de las coordenadas de variables
TV=sum(Coord_var.^2')';
%Contribuciones del individuo a la variable
CFI=(Coord_ind.^2'*inv(diag(TI)))';
%Contribuciones del factor a la variable
CFV=(Coord_var.^2'*inv(diag(TV)))';

if s==1 
 figure
 plot(Coord_ind(:,1),Coord_ind(:,2),'*r') %coord ind
 hold on
 plot(Coord_var(:,1),Coord_var(:,2),'o')  %coord var
legend('individuos','variables','Location','SouthOutside' )

end

