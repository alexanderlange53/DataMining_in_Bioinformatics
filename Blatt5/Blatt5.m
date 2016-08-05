% Blatt 5
% Aufgabe 1
load('Hidden1.mat');
Xdist = squareform(pdist(Xdata'));
%%
P = sammon(Xdata', 2, [], [], [], Xdist);

scatter(P(:,1), P(:,2));

%% Optimale PCA

CovM = cov(Xdata');
[EigVek, EigVal] = eigs(CovM, 2);

z1 = EigVek(:,1)'*Xdata;
z2 = EigVek(:,2)'*Xdata;

scatter(z1, z2);

%% Aufgabe 2
WoundPos = csvread('wound_pos.csv');
WoundPos = WoundPos(1:837,6:77);
A = repmat([0.1,0.1,0.2, 0.2,0.3,0.3,0.4,0.4],4);
Labels = [A(1,:),TimeVec(:,33:end)];
%% Euklidische Abstaende
DistW = squareform(pdist(WoundPos'));
PlotMS(WoundPos, DistW, Labels)
%% Cityblock
DistW = squareform(pdist(WoundPos', 'cityblock'));
PlotMS(WoundPos, DistW, Labels)

%% Metabolit basiert

M = MeanMat(WoundPos, Labels);
DistW = squareform(pdist(M));
PW = sammon(M, 2, 100, [], [], DistW);
scatter(PW(:,1), PW(:,2))

%%
DistW = squareform(pdist(M, 'cityblock'));
PW = sammon(M, 2, 100, [], [], DistW);
scatter(PW(:,1), PW(:,2))
%%
PlotAll(WoundPos, Labels, 150);