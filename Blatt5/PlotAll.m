function [DistW, DistWC, DistWM, DistWMC] = PlotAll(X, Labels, It)

DistW = squareform(pdist(X'));
DistWC = squareform(pdist(X', 'cityblock'));

M = MeanMat(X, Labels);
DistWM = squareform(pdist(M));
DistWMC = squareform(pdist(M, 'cityblock'));

XEuk = [];
for n = 1:size(X,1)
        XEuk(n,:) = X(n,:)/norm(X(n,:));
end

MEuk = [];
for n = 1:size(M,1)
        MEuk(n,:) = M(n,:)/norm(M(n,:));
end

XC = [];
for n = 1:size(X,1)
        XC(n,:) = X(n,:)/norm(X(n,:),1);
end

MC = [];
for n = 1:size(M,1)
        MC(n,:) = M(n,:)/norm(M(n,:),1);
end

XST = [];
for n = 1:size(X, 1)
    XST(n,:) = X(n,:)/std(X(n,:));
end

MST =[];
for n = 1:size(X, 1)
    MST(n,:) = M(n,:)/std(M(n,:));
end

DistWE = squareform(pdist(XEuk'));
DistWCC = squareform(pdist(XC', 'cityblock'));
DistWME = squareform(pdist(MEuk));
DistWMCC = squareform(pdist(MC, 'cityblock'));
DistWST = squareform(pdist(XST));
DistWCST = squareform(pdist(XST, 'cityblock'));
DistWMST = squareform(pdist(MST));
DistWMCST = squareform(pdist(MST, 'cityblock'));

PW = sammon(X', 2, It, [], [], DistW);
PWC = sammon(X', 2, It, [], [], DistWC);
PWM = sammon(M, 2, It, [], [], DistWM);
PWMC = sammon(M, 2, It, [], [], DistWMC);

PWE = sammon(XEuk', 2, It, [], [], DistWE);
PWCC = sammon(XC', 2, It, [], [], DistWCC);
PWME = sammon(MEuk, 2, It, [], [], DistWME);
PWMCC = sammon(MC, 2, It, [], [], DistWMCC);

PWST = sammon(XST, 2, It, [], [], DistWST);
PWCST = sammon(XST, 2, It, [], [], DistWCST);
PWMST = sammon(MST, 2, It, [], [], DistWMST);
PWMCST = sammon(MST, 2, It, [], [], DistWMCST);

subplot(3,4,1);
scatter(PW(:,1), PW(:,2))
subplot(3,4,2);
scatter(PWC(:,1), PWC(:,2))
subplot(3,4,3);
scatter(PWM(:,1), PWM(:,2))
subplot(3,4,4);
scatter(PWMC(:,1), PWMC(:,2))

subplot(3,4,5);
scatter(PWE(:,1), PWE(:,2))
subplot(3,4,6);
scatter(PWCC(:,1), PWCC(:,2))
subplot(3,4,7);
scatter(PWME(:,1), PWME(:,2))
subplot(3,4,8);
scatter(PWMCC(:,1), PWMCC(:,2))

subplot(3,4,9);
scatter(PWST(:,1), PWST(:,2))
subplot(3,4,10);
scatter(PWCST(:,1), PWCST(:,2))
subplot(3,4,11);
scatter(PWMST(:,1), PWMST(:,2))
subplot(3,4,12);
scatter(PWMCST(:,1), PWMCST(:,2))


end