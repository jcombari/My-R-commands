clear
X  = [  1 6 7 2 5 7 6 3 6 7
        5 3 2 4 4 4 2 4 4 3
        6 1 1 5 2 1 1 7 1 1
        7 1 2 7 2 1 2 2 2 2
        2 5 4 3 5 6 5 2 6 6
        3 4 4 3 5 4 5 1 7 5
    ]; % 10 variables en total
p = [3 4 3]; % 10 variables en total
P =cumsum(p);
n = size(X,1);             % 12 tipos de vino
t = 3;                    % 10 expertos catadores 
save datosvalentin