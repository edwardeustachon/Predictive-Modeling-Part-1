##Ch6 Q9
#A
library(ISLR)
data(College)
set.seed(1)
train = sample(1:nrow(College), nrow(College)/2)
test <- -train
College.train <- College[train,]
College.test <- College[test,]

#B
fit.lm <- lm(Apps ~ ., data = College.train)
pred.lm <- predict(fit.lm, College.test)
mean((pred.lm - College.test$Apps)^2)

#C
library(glmnet)
train.mat <- model.matrix(Apps ~ ., data = College.train)
test.mat <- model.matrix(Apps ~ ., data = College.test)
grid <- 10 ^ seq(4, -2, length = 100)
fit.ridge <- glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
cv.ridge <- cv.glmnet(train.mat, College.train$Apps, alpha = 0, lambda = grid, thresh = 1e-12)
bestlam.ridge <- cv.ridge$lambda.min
bestlam.ridge
pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test.mat)
mean((pred.ridge - College.test$Apps)^2)

#D
fit.lasso <- glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
cv.lasso <- cv.glmnet(train.mat, College.train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)
bestlam.lasso <- cv.lasso$lambda.min
bestlam.lasso
pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
mean((pred.lasso - College.test$Apps)^2)
predict(fit.lasso, s = bestlam.lasso, type = "coefficients")

#E
library(pls)
fit.pcr <- pcr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pcr, val.type = "MSEP")
pred.pcr <- predict(fit.pcr, College.test, ncomp = 10)
mean((pred.pcr - College.test$Apps)^2)
summary(fit.pcr)

#F
fit.pls <- plsr(Apps ~ ., data = College.train, scale = TRUE, validation = "CV")
validationplot(fit.pls, val.type = "MSEP")
pred.pls <- predict(fit.pls, College.test, ncomp = 10)
mean((pred.pls - College.test$Apps)^2)
summary(fit.pls)

#G
test.avg <- mean(College.test$Apps)
lm.r2 <- 1 - mean((pred.lm - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
ridge.r2 <- 1 - mean((pred.ridge - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
lasso.r2 <- 1 - mean((pred.lasso - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pcr.r2 <- 1 - mean((pred.pcr - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)
pls.r2 <- 1 - mean((pred.pls - College.test$Apps)^2) / mean((test.avg - College.test$Apps)^2)

library(MASS)
library(glmnet)
library(leaps)
data(Boston)
attach(Boston)
set.seed(1)
##Ch6 Q11
#Best Subset CV
predict.regsubsets = function(object ,newdata ,id ,...){
  form = as.formula(object$call [[2]])
  mat = model.matrix(form ,newdata )
  coefi = coef(object ,id=id)
  xvars = names(coefi )
  mat[,xvars]%*% coefi
}
k = 10
set.seed(1)
folds <- sample(1:k, nrow(Boston), replace = TRUE)
cv.errors <- matrix(NA, k, 13, dimnames = list(NULL, paste(1:13)))
for (j in 1:k) {
  best.fit <- regsubsets(crim~., data = Boston[folds != j, ], nvmax = 13)
  for (i in 1:13) {
    pred <- predict(best.fit, Boston[folds == j,], id = i)
    cv.errors[j, i] <- mean((Boston$crim[folds == j] - pred)^2)
  }
}
min(apply(cv.errors, 2, mean))
plot(mean.cv.errors, type = "b", xlab = "Number of variables", ylab = "CV error")
reg.best=regsubsets(crim~., data=Boston, nvmax=13)
coef(reg.best, 12)

# Resampling Setup
set.seed(1)
x=model.matrix(crim~.,Boston)[,-1]
y=Boston$crim
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
grid=10^seq(10, -2, length=100)

#Ridge Regression CV
ridge.mod=glmnet(x[train,], y[train], alpha=0, lambda=grid)
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)
out=glmnet(x,y,alpha=0)
predict(out, type='coefficients', s=bestlam)

#Lasso CV
lasso.mod = glmnet(x[train,], y[train], alpha=1, lambda=grid)
set.seed(1)
lasso.out= cv.glmnet(x[train,], y[train], alpha=1)
plot(lasso.out)
lam.lasso = lasso.out$lambda.min
lam.lasso
lasso.pred = predict(lasso.mod, s=lam.lasso, newx=x[test,])
mean((lasso.pred - y.test)^2)
bestlasso = glmnet(x, y, alpha=1, lambda=grid)
predict(bestlasso, type='coefficients', s=lam.lasso)

##Ch4 Q10
library(ISLR)
data(Weekly)
attach(Weekly)
set.seed(1)

#A
summary(Weekly)
plot(Weekly)
cor(Weekly[,-9])

#B
logit.full=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
            data=Weekly,family=binomial)
summary(logit.full)

#C
logit.probs=predict(logit.full,type="response") 
logit.pred=ifelse(logit.probs>0.5,"Up","Down")
table(logit.pred,Direction)
mean(logit.pred==Direction)

#D
train = Year<2009
logit.fit=glm(Direction~Lag2, data=Weekly, family=binomial, subset=train)
logit.prob=predict(logit.fit,newdata=Weekly[!train,],type="response") 
logit.preds=ifelse(logit.prob >0.5,"Up","Down")
table(logit.preds,Direction[!train])
mean(logit.preds==Direction[!train])

#G
set.seed(1)
library(class)
knn.pred = knn(as.matrix(Lag2[train]), as.matrix(Lag2[!train]), Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])

#I
logit.fit2 = glm(Direction ~ Lag1 + Lag2, data=Weekly, family=binomial, subset=train)
logit.prob2=predict(logit.fit2,newdata=Weekly[!train,],type="response") 
logit.preds2=ifelse(logit.prob2 >0.5,"Up","Down")
table(logit.preds2,Direction[!train])
mean(logit.preds2==Direction[!train])
logit.fit3 = glm(Direction ~ Lag1 + Lag2 + Lag1:Lag2, data=Weekly, family=binomial, subset=train)
logit.prob3=predict(logit.fit3,newdata=Weekly[!train,],type="response") 
logit.preds3=ifelse(logit.prob3 >0.5,"Up","Down")
table(logit.preds3,Direction[!train])
mean(logit.preds3==Direction[!train])
test_errors = rep(0, 100)
for (i in 1:100)
{
  knn.preds = knn(as.matrix(Lag2[train]), as.matrix(Lag2[!train]), Direction[train], k=i)
  test_errors[i]=mean(knn.preds==Direction[!train])
}
which.max(test_errors)
max(test_errors)

##Ch8 Q8
library(tree)
library(ISLR)
data(Carseats)
attach(Carseats)
#A
set.seed(1)
train=sample(1:nrow(Carseats), nrow(Carseats)/2)
test = -train
Carseats.train = Carseats[train,]
Carseats.test = Carseats[test,]

#B
Carseats.tree = tree(Sales~., data=Carseats.train)
plot(Carseats.tree)
text(Carseats.tree, pretty=0)
summary(Carseats.tree)
yhat = predict(Carseats.tree, newdata=Carseats.test)
mean((yhat-Carseats.test$Sales)^2)

#C
cv.Carseats = cv.tree(Carseats.tree)
plot(cv.Carseats$size, cv.Carseats$dev, type='b')
Carseats.prune = prune.tree(Carseats.tree, best=8)
yhat.prune = predict(Carseats.prune, newdata=Carseats.test)
mean((yhat.prune-Carseats.test$Sales)^2)

#D
library(randomForest)
Carseats.bag = randomForest(Sales~., data=Carseats.train, mtry=10, importance=TRUE)
yhat.bag = predict(Carseats.bag, newdata=Carseats.test)
mean((yhat.bag-Carseats.test$Sales)^2)
importance(Carseats.bag)

#E
Carseats.rf = randomForest(Sales~., data=Carseats.train, mtry=round(sqrt(10)), importance=TRUE)
yhat.rf = predict(Carseats.rf, newdata=Carseats.test)
mean((yhat.rf-Carseats.test$Sales)^2)
importance(Carseats.rf)
test_error = rep(0, 10)
for (i in 1:10)
{
  Carseats.rfloop = randomForest(Sales~., data=Carseats.train, mtry=i, importance=TRUE)
  yhat.rfloop = predict(Carseats.rfloop, newdata=Carseats.test)
  test_error[i]=mean((yhat.rfloop-Carseats.test$Sales)^2)
}
min(test_error)
max(test_error)
which.min(test_error)
which.max(test_error)

##Ch8 Q11
library(gbm)
data(Caravan)
attach(Caravan)
library(class)
#A
set.seed(1)
train = 1:1000
Caravan$Purchase = ifelse(Caravan$Purchase == 'Yes', 1, 0)
Caravan.train = Caravan[train,]
Caravan.test = Caravan[-train,]

#B
Caravan.boost2=gbm(Purchase~.,data=Caravan.train, distribution="bernoulli",
                   n.trees=1000,shrinkage=0.01)


#C
boost.probs2 = predict(Caravan.boost2, newdata=Caravan.test, n.trees=1000, type='response')
boost.preds2 = ifelse(boost.probs2 > .2, 1, 0)
table(Caravan.test$Purchase, boost.preds2)

Caravan.logit=glm(Purchase~., data=Caravan.train, family=binomial)
Caravan.logit.probs = predict(Caravan.logit, newdata=Caravan.test, type='response')
Caravan.logit.preds = ifelse(Caravan.logit.probs>0.2, 1, 0)
table(Caravan.test$Purchase, Caravan.logit.preds)

##Problem 1: Beauty Pays
#A
beauty = read.csv("BeautyData.csv")
#full linear model
beauty1=glm(CourseEvals~., data=beauty)
summary(beauty1)
beauty2=glm(CourseEvals~BeautyScore, data=beauty)
summary(beauty2)
beauty3=glm(CourseEvals~BeautyScore+female+lower+nonenglish+tenuretrack+BeautyScore:female+BeautyScore:lower
                        +BeautyScore:nonenglish+BeautyScore:tenuretrack, data=beauty)
summary(beauty3)

##Problem 2: Housing Price Structure
#A
housing=read.csv("MidCity.csv")
housing=housing[,-c(1)]
housing.fit1 = glm(Price~., data=housing)
summary(housing.fit1)
housing.fit2 = glm(Price~Nbhd+Offers+SqFt+Brick+Bedrooms+Bathrooms+Nbhd:Brick, data=housing)
summary(housing.fit2)
housing.fit3 = glm(Price~Nbhd+Offers+SqFt+Bedrooms+Bathrooms+Nbhd:Brick, data=housing)
summary(housing.fit3)

##Problem 4: BART
#A
ca = read.csv('CAhousing.csv', header=TRUE)
ca$AveBedrms <- ca$totalBedrooms/ca$households
ca$AveRooms <- ca$totalRooms/ca$households
ca$AveOccupancy <- ca$population/ca$households
logMedVal <- log(ca$medianHouseValue)
ca <- ca[,-c(4,5,9)] # lose lmedval and the room totals
ca$logMedVal = logMedVal
x = ca[,1:9] 
y = ca$logMedVal # median value
head(cbind(x,y))

library(BART) #BART package
set.seed(1) #MCMC, so set the seed
nd=200 # number of kept draws
burn=50 # number of burn in draws
bf = wbart(x,y,nskip=burn,ndpost=nd)

lmf = lm(y~.,data.frame(x,y))
fitmat = cbind(y,bf$yhat.train.mean,lmf$fitted.values)
colnames(fitmat)=c("y","BART","Linear")
cor(fitmat)

n=length(y) #total sample size
set.seed(1) #
ii = sample(1:n,floor(.75*n)) # indices for train data, 75% of data
xtrain=x[ii,]; ytrain=y[ii] # training data
xtest=x[-ii,]; ytest=y[-ii] # test data
cat("train sample size is ",length(ytrain)," and test sample size is ",length(ytest),"\n")

set.seed(1)
bf_train = wbart(xtrain,ytrain)
yhat = predict(bf_train,as.matrix(xtest))
yhat.mean = apply(yhat,2,mean)
RMSE = sqrt(sum((yhat.mean-ytest)^2)/length(ytest))
RMSE2 = sqrt(mean((yhat.mean-ytest)^2))
RMSE
RMSE2

set.seed(1)
bf_train2 = wbart(xtrain, ytrain, xtest)
RMSE3 = sqrt(mean((bf_train2$yhat.test.mean - ytest)^2))

plot(ytest,yhat.mean)
abline(0,1,col=2)A

##Problem 5: Neural Nets
#A
library(nnet)
library(MASS)
set.seed(1)
data(Boston)
attach(Boston)

#standardize the x's
minv = rep(0,13)
maxv = rep(0,13)
Bostonsc = Boston
for(i in 1:13) {
  minv[i] = min(Boston[[i]])
  maxv[i] = max(Boston[[i]])
  Bostonsc[[i]] = (Boston[[i]]-minv[i])/(maxv[i]-minv[i])
}
set.seed(1)
train = sample(1:nrow(Bostonsc), nrow(Bostonsc)/2)
Bostonsc.train = Bostonsc[train,]
Bostonsc.test = Bostonsc[-train,]

set.seed(1)
znn1 = nnet(medv~.,Bostonsc.train,size=3,decay=.5,linout=T)
znn2 = nnet(medv~.,Bostonsc.train,size=3,decay=.00001,linout=T)
znn3 = nnet(medv~.,Bostonsc.train,size=50,decay=.5,linout=T)
znn4 = nnet(medv~.,Bostonsc.train,size=50,decay=.00001,linout=T)
znnf1 = predict(znn1,Bostonsc.test)
znnf2 = predict(znn2,Bostonsc.test)
znnf3 = predict(znn3,Bostonsc.test)
znnf4 = predict(znn4,Bostonsc.test)
temp=data.frame(y=Bostonsc.test$medv, znn1=znnf1, znn2=znnf2, znn3=znnf3, znn4=znnf4)
print(cor(temp$y,temp$znn1))
print(cor(temp$y,temp$znn2))
print(cor(temp$y,temp$znn3))
print(cor(temp$y,temp$znn4))
