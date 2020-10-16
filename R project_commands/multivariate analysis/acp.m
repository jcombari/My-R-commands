    
function [variabilidad_acumulada,Coord_ind,CIF,CVF, CFI, CFV]=acp(X)
matriz=X'*X;
[vect val] = eigs(matriz);
%Variabilidad capturada por cada factor
variabilidad_acumulada=diag(val)/sum(diag(val))*100;
Coord_ind=X*vect;
D=sqrt(val);
Coord_var=vect*D;
%Varianilidad total
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

% figure
% plot(Coord_ind(:,1),Coord_ind(:,2),'*r') %coord ind
% hold on
% plot(Coord_var(:,1),Coord_var(:,2),'o')  %coord var
% legend('individuos','variables','Location','SouthOutside' )


