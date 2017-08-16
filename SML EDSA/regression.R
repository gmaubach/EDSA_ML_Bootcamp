##############
# Before you begin:
# 1. Make sure you have set up the datasets.R file
#   (i)  You have installed all required packages for the datasets.R file
#   (ii) You have sourced (selected all and run) datasets.R file
# 2. Make sure you have installed the nnet package
# 3. For the 3d extension, make sure you have installed the gdata and rgl packages
#    - If you are using a mac see note about rgl package in Intro to R.R
##############

library(nnet)

### General functions used
# set.seed(n)         Set the random number generator with a seed n
# seq(from,to,n)      Create a vector: from, from+n, from+2n, ... whil less than to
# rep(vector,times)   Repeat the vector the specified number of times
# rep(vector,each=n)  Repeat each item in the vector n times
# data.frame(A=v,B=w) Create a data frame with columns called A and B from vectors v and w
# colnames(df)        Get the column names of the data frame (or martix) df
# colnames(df)=v      Set the column names of the data frame (or martix) df to the values in vector v
# cbind(v,w)          Bind vectors v and w into a matrix column-wise
# sum(v)              Sum the items of vector v
# sqrt(v)             Take the square root of each item in vector v

### Graphic functions used
# plot
# points
# segments

### Modeling functions used
# lm
# glm (if doing Poisson regression)
# poly
# nnet
# predict

### Operators used
# [,]
# -
# /
# ^

### Keywords used
# function

# Get data
# I use the synthetic poly4 data.
# You can choose other data, but restrict yourself to using only one feature.
# This is so you can plot the resulting model in 2 dimensions.
poly4=getPoly4()

# Alternatives are:
# airfoil=getAirfoil()
# mussels=getMussels()
# careers=getCareers()
# ozone=getOzone()

# Create a linear model
lm_model=lm(Y~X,poly4)

# Cannot do a Poisson model on the poly4 data since it has negative Y values
# pr_model=glm(Y~X,"poisson",poly4) # WILL RETURN ERROR

# Create 5 polynomial regression models
pr2_model=lm(Y~poly(X,2),poly4)
pr3_model=lm(Y~poly(X,3),poly4)
pr4_model=lm(Y~poly(X,4),poly4)
pr5_model=lm(Y~poly(X,5),poly4)
pr6_model=lm(Y~poly(X,6),poly4)

# Create two neural network model
# There are important tuning parameters we do not look at yet
set.seed(0)
nn_model1=nnet(
  Y~X,          # Formula
  poly4,        # Data
  size=4,       # Nodes in hidden layer
  linout=TRUE   # We want a linear output since we are doing regression
)
set.seed(1)
nn_model2=nnet(
  Y~X,          # Formula
  poly4,        # Data
  size=4,       # Nodes in hidden layer
  linout=TRUE   # We want a linear output since we are doing regression
)

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
plot(plot4)
# Note that the features must be a data frame and the target a vector
plotModel(lm_model,poly4[1],poly4$Y,0,10)
plotModel(pr2_model,poly4[1],poly4$Y,0,10)
plotModel(pr3_model,poly4[1],poly4$Y,0,10)
plotModel(pr4_model,poly4[1],poly4$Y,0,10)
plotModel(pr5_model,poly4[1],poly4$Y,0,10)
plotModel(pr6_model,poly4[1],poly4$Y,0,10)
plotModel(nn_model1,poly4[1],poly4$Y,0,10)
plotModel(nn_model2,poly4[1],poly4$Y,0,10)

# EXTENSION - 3D plotting
# Here is what the 3d version of plotModel would look like.
# Note that we need to know the feature names to make this work.
plotModel3d=function(model,features,target,from_X,to_X,from_Y,to_Y,n=100,sigma=2,...) {
  require(gdata)
  require(rgl)
  plot3d(cbind(features,target),...)
  xseq=seq(from_X,to_X,(to_X-from_X)/n)
  yseq=seq(from_Y,to_Y,(to_Y-from_Y)/n)
  xseq_=rep(xseq,each=n+1)
  yseq_=rep(yseq,n+1)
  newData=data.frame(xseq_,yseq_)
  colnames(newData)=colnames(features)
  p=predict(model,newData)
  surface3d(xseq,yseq,p,type="l",col="blue")
  est=predict(model,features)
  err=target-est
  s=sqrt(sum(err^2)/(length(err)-1))
  surface3d(xseq,yseq,p-2*s,type="l",col="green")
  surface3d(xseq,yseq,p+2*s,type="l",col="green")
  segMatrix=gdata::interleave(cbind(features,y=target),cbind(features,y=est))
  segments3d(segMatrix,col="red")
}
set.seed(17744) # Some random initializations produce very poor models, this one does an ok model.
model3d=nnet(prestige~income+education,Duncan,size=2,linout=T)
plotModel3d(model3d,Duncan[2:3],Duncan$prestige,0,100,0,100)


