#reading the data
ziptrain=read.table('zip.train.csv',sep=',');
ziptest=read.table('zip.test.csv',sep=',');
ziptrain27 <- subset(ziptrain, ziptrain[,1]==2 | ziptrain[,1]==7);
ziptest27 <- subset(ziptest,ziptest[,1]==2 | ziptest[,1]==7);

#exploratory data analysis
rowindex = 15; 
ziptrain27[rowindex,1];
Xval = t(matrix(data.matrix(ziptrain27[,-1])[rowindex,],byrow=TRUE,16,16)[16:1,]);
image(Xval,col=gray(0:1),axes=FALSE)
#linear regression
mod1 <- lm( V1 ~ . , data= ziptrain27);
pred1.train <- predict.lm(mod1, ziptrain27[,-1]);
y1pred.train <- 2 + 5*(pred1.train >= 4.5);
mean( y1pred.train != ziptrain27[,1]);

pred1.test <- predict.lm(mod1, ziptest27[,-1]);
y1pred.test <- 2 + 5*(pred1.test >= 4.5);
mean( y1pred.test != ziptest27[,1])
#knn
library(class);
kk <- 13;
xnew <- ziptrain27[,-1];
ypred2.train <- knn(ziptrain27[,-1], xnew, ziptrain27[,1], k=kk);
mean( ypred2.train != ziptrain27[,1])
xnew2 <- ziptest27[,-1];
ypred2.test <- knn(ziptrain27[,-1], xnew2, ziptrain27[,1], k=kk);
mean( ypred2.test != ziptest27[,1])

#cross-validation
zip27full = rbind(ziptrain27, ziptest27)
n1 = 1376; 
n2= 345; 
n = dim(zip27full)[1]
set.seed(7406); 
B= 100; 
TEALL = NULL; 
for (b in 1:B){
  flag <- sort(sample(1:n, n1));
  zip27traintempset <- zip27full[flag,];
  zip27testtempset <- zip27full[-flag,];
  mod1 <- lm( V1 ~ . , data= zip27traintempset);
  pred1.test <- predict.lm(mod1, zip27testtempset[,-1]);
  y1pred.test <- 2 + 5*(pred1.test >= 4.5);
  te0=mean( y1pred.test != zip27testtempset[,1])
  kk <- 1;
  xnew2 <- zip27testtempset[,-1];
  ypred2.test <- knn(zip27traintempset[,-1], xnew2, zip27traintempset[,1], k=kk);
  te1=mean( ypred2.test != zip27testtempset[,1])
  kk <- 3;
  xnew2 <- zip27testtempset[,-1];
  ypred2.test <- knn(zip27traintempset[,-1], xnew2, zip27traintempset[,1], k=kk);
  te2=mean( ypred2.test != zip27testtempset[,1])
  kk <- 5;
  xnew2 <- zip27testtempset[,-1];
  ypred2.test <- knn(zip27traintempset[,-1], xnew2, zip27traintempset[,1], k=kk);
  te3=mean( ypred2.test != zip27testtempset[,1])
  kk <- 7;
  xnew2 <- zip27testtempset[,-1];
  ypred2.test <- knn(zip27traintempset[,-1], xnew2, zip27traintempset[,1], k=kk);
  te4=mean( ypred2.test != zip27testtempset[,1])
  kk <- 9;
  xnew2 <- zip27testtempset[,-1];
  ypred2.test <- knn(zip27traintempset[,-1], xnew2, zip27traintempset[,1], k=kk);
  te5=mean( ypred2.test != zip27testtempset[,1])
  kk <- 11;
  xnew2 <- zip27testtempset[,-1];
  ypred2.test <- knn(zip27traintempset[,-1], xnew2, zip27traintempset[,1], k=kk);
  te6=mean( ypred2.test != zip27testtempset[,1])
  kk <- 13;
  xnew2 <- zip27testtempset[,-1];
  ypred2.test <- knn(zip27traintempset[,-1], xnew2, zip27traintempset[,1], k=kk);
  te7=mean( ypred2.test != zip27testtempset[,1])
  kk <- 15;
  xnew2 <- zip27testtempset[,-1];
  ypred2.test <- knn(zip27traintempset[,-1], xnew2, zip27traintempset[,1], k=kk);
  te8=mean( ypred2.test != zip27testtempset[,1])
  TEALL = rbind( TEALL, cbind(te0, te1, te2, te3, te4, te5, te6, te7, te8) );
}
dim(TEALL); 
colnames(TEALL) <- c("linearRegression", "KNN1", "KNN3", "KNN5", "KNN7",
                     "KNN9", "KNN11", "KNN13", "KNN15");
apply(TEALL, 2, mean);
apply(TEALL, 2, var)