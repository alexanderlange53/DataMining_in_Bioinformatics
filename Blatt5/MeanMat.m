function M = MeanMat(X, Labels)

Positiv = X;
M = [...
    mean(Positiv(:, Labels == 0.1),2), ...
    mean(Positiv(:, Labels == 0.2),2), ... 
    mean(Positiv(:, Labels == 0.3),2), ...
    mean(Positiv(:, Labels == 0.4),2), ... 
    mean(Positiv(:, Labels == 0),2), ... 
    mean(Positiv(:, Labels == 0.5),2), ... 
    mean(Positiv(:, Labels == 2),2), mean(Positiv(:, Labels == 5),2)];

end