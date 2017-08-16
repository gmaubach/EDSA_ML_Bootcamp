
# Excercise 2
# Question 1

## Config
set.seed(0)

# Libs
library(nnet)

## Data
getPoly4 = function(n=100) {
  set.seed(0)
  X=runif(n,0,10)
  Y=X+X^2-.25*X^3+.015*X^4+rnorm(n,0,2)
  data.frame(X=X,Y=Y)
}

ds_poly4 <- getPoly4()
head(ds_poly4)

## Linear Regression Model
model_1 <- lm(Y ~ ., ds_poly4)

## Polynomial Regression
model_2_1 <- lm(Y ~ poly(X, 2), ds_poly4)
model_2_2 <- lm(Y ~ poly(X, 3), ds_poly4)
model_2_3 <- lm(Y ~ poly(X, 4), ds_poly4)
model_2_4 <- lm(Y ~ poly(X, 5), ds_poly4)
model_2_5 <- lm(Y ~ poly(X, 6), ds_poly4)

## Neural Networks
set.seed(0)
model_3 <- nnet(Y ~ X, ds_poly4, size = 4, linout = TRUE)

set.seed(1)
model_4 <- nnet(Y ~ X, ds_poly4, size = 4, linout = TRUE)

## Plot models

# We create a function which will allow us to plot the models
# The parameters are:
# model - The model to plot
# features - The input features *as a data frame*.
# target - The target variable *as a vector*
# from, to - The range to plot (we could look at the data to decide this automatically)
# featureNames - The names of the feature
# n - The number of points to use to create the regression curve.
# sigma - The number of residuals standard deviations our confidence intervals will be.
# The ... allows additional arguments to be given which are then passed to the plot function
plotModel=function(model,features,target,from,to,n=100,sigma=2,...) {
  plot(cbind(features,target),...)
  xseq=seq(from,to,(to-from)/n)
  newData=data.frame(X=xseq)
  colnames(newData)=colnames(features)
  p=predict(model,newData)
  points(xseq,p,type="l",col="blue")
  est=predict(model,features)
  err=target-est
  s=sqrt(sum(err^2)/(length(err)-1))
  points(xseq,p-2*s,type="l",col="green")
  points(xseq,p+2*s,type="l",col="green")
  segments(features[,1],target,features[,1],est,col="red")
}

plotModel(model_1, ds_poly4[1], ds_poly4$Y, 0, 10)
plotModel(model_2_1, ds_poly4[1], ds_poly4$Y, 0, 10)
plotModel(model_2_2, ds_poly4[1], ds_poly4$Y, 0, 10)
plotModel(model_2_3, ds_poly4[1], ds_poly4$Y, 0, 10)
plotModel(model_2_4, ds_poly4[1], ds_poly4$Y, 0, 10)
plotModel(model_2_5, ds_poly4[1], ds_poly4$Y, 0, 10)

# Mean Squared Error

mseFunction <- function(myModel) {
  mean((predict(myModel, ds_poly4) - ds_poly4$Y)^2)
}

mseFunction(model_2_1)
mseFunction(model_2_2)
mseFunction(model_2_3)
mseFunction(model_2_4)
mseFunction(model_2_5)


# EOF .

