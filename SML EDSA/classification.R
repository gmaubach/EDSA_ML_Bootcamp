##############
# Before you begin:
# 1. Make sure you have set up the datasets.R file
#   (i)  You have installed all required packages for the datasets.R file
#   (ii) You have sourced (selected all and run) datasets.R file
# 2. Make sure you have installed the mlbench, MASS, e1071 and nnet packages
##############

# This code shows one way to complete the classification exercise.

# Load libraries
library(mlbench)
library(MASS)
library(e1071)
library(nnet)

### General functions used
# as.numeric     Forcing non-numeric variables (in our case factors) into numeric values. Be careful doing this until you understand well how R does it!
# cbind
# length
# matrix
# mean
# nrow
# sample
# set.seed
# which

### Graphing functions used
# barplot
# plot

### Modeling functions used
# lda
# qda
# glm
# svm
# tune.svm
# nnet
# randomForest (may be covered later in course depending on time)
# predict

### Operators used
# []
# [,]
# +
# -
# *
# /
# ==
# >
# &&

### Keywords used
# function

# Load data
Sonar=getSonar()

# Alternatives data:
# seeds=getSeeds()             Very few data points
# mussels=getMussels()         Try to predict the cond (condition) variable
# careers=getCareers(TRUE)     TRUE includes the discrete profession column which you can predict

###
#  Set up training / evaluation / test data
###
# Set the random seed in R
set.seed(0)
# Set the proportion of data to use as training data
trainprop=.65 # Proportion of whole data set
evalprop=.5 # Proportion of non-training data
# Create training, evaluation and test data
train.ind = sample(nrow(Sonar),nrow(Sonar)*trainprop)
train.Sonar=Sonar[train.ind,]
nonTraining=Sonar[-train.ind,]
eval.ind = sample(nrow(nonTraining),nrow(nonTraining)*evalprop)
eval.Sonar=nonTraining[eval.ind,]
test.Sonar=nonTraining[-eval.ind,]

# Create and evaluate models
lda_model=lda(Class~.,train.Sonar)
# Note 1
# When we use predict with an LDA or QDA model we a number of
# fields. We are interested in the predicted class so we look
# at the class field.
# Note 2
# The equality comparison will lead to a vector of logical values.
# When we pass them to the mean function they are cast to numbers
# such that TRUE is 1 and FALSE is 0.
lda_acc=mean(predict(lda_model,eval.Sonar)$class==eval.Sonar$Class)

qda_model=qda(Class~.,train.Sonar)
qda_acc=mean(predict(qda_model,eval.Sonar)$class==eval.Sonar$Class)

svm_model=svm(Class~.,train.Sonar,type="C")
svm_acc=mean(predict(svm_model,eval.Sonar)==eval.Sonar$Class)

svm_model2=svm(Class~.,train.Sonar,kernel="linear",type="C")
svm2_acc=mean(predict(svm_model2,eval.Sonar)==eval.Sonar$Class)

# Note 3
# The return value from tune.svm includes lots of information
# about the optimization of the tuning parameters. But the
# best model is returned in the field best.model.
svm_model3 <- tune.svm(
  Class~.,
  data=train.Sonar,
  type="C",
  gamma = (1/(ncol(train.Sonar)-1))*2^(-2:2),
  cost = 2^(-1:2)
  )
svm3_acc=mean(predict(svm_model3$best.model,eval.Sonar)==eval.Sonar$Class)

# Note 4
# The logistic regression model may give a warning when it is built.
# This is because the algorithm to find the optimal parameters for
# the model does not coverge. Don't worry.
lr_model=glm(Class~.,"binomial",train.Sonar)
# Note 5
# When we use predict with a binary logistic regression model, the
# return values are the probability of the case taking the
# positive class.
lr_est=predict(lr_model,eval.Sonar,type="response")>.5
# Note 6
# Here and in the similar lines in the neural network models
# I take advantage of knowing how R will turn factors and
# logical values into numbers if forced to.
lr_acc=mean((lr_est+1)==as.numeric(eval.Sonar$Class))

set.seed(0)
nn_model1=nnet(
  Class~.,      # Formula
  train.Sonar,        # Data
  size=10       # Nodes in hidden layer
)
nn1_est=predict(nn_model1,eval.Sonar)>.5
nn1_acc=mean((nn1_est+1)==as.numeric(eval.Sonar$Class))

set.seed(1)
nn_model2=nnet(
  Class~.,      # Formula
  train.Sonar,        # Data
  size=10       # Nodes in hidden layer
)
nn2_est=predict(nn_model2,eval.Sonar)>.5
nn2_acc=mean((nn2_est+1)==as.numeric(eval.Sonar$Class))

# This plots the performance of the difference models
barplot(
  c(lda_acc,qda_acc,svm_acc,svm2_acc,svm3_acc,lr_acc,nn1_acc,nn2_acc),
  names.arg = c("LDA","QDA","SVM1","SVM2","SVM3","LR","NN1","NN2")
  )

# Turns out the best performing model was the first neural network.
# Now we see how it performs on the test data.
# We find the values it estimates
nn1_est_test=predict(nn_model1,test.Sonar)>.5
# And check its accuracy
nn1_acc=mean((nn1_est+1)==as.numeric(eval.Sonar$Class))
# Now we view the confusion matrix.
# createCM wants predicted and actuals values as 0 (false) or 1 (true).
# The nn1_est_test has this already.
# For the actual values we can do:
actual=as.numeric(test.Sonar$Class)-1
cm=createCM(actual,nn1_est_test)
cm

# EXTENSION - Logistic regression with aggregated data
# The following is some code to show how to use logistic regression
# in the cases of non-aggregated and aggregated data.
seeds=getSeeds()
glm_model1=glm(germinated~age,"binomial",seeds)
plot(seeds,xlim=c(0,10))
plot(function(x)predict(glm_model1,data.frame(age=x),type="response"),from=0,to=10,add=T,col="blue")

menarche=getMenarche()
glm_model2=glm(cbind(Menarche,Total-Menarche)~Age,"binomial",menarche)
plot(menarche$Age,menarche$Menarche/menarche$Total,xlab="Age",ylab="Proportion")
plot(function(x)predict(glm_model2,data.frame(Age=x),type="response"),from=8,to=18,add=T,col="blue")


