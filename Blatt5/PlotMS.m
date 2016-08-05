function PW = PlotMS(X, Y, Z)

PW = sammon(X', 2, 200, [], [], Y);

gscatter(PW(:,1), PW(:,2),Z)
legend('1 Unwounded', '1 Harvested 0.5','1 Harvested 2', '1 Harvested 5',...
    '2 Unwounded', '2 Harvested 0.5','2 Harvested 2', '2 Harvested 5');
title('Hauptkomponenten');

end