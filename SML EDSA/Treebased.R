##############
# Before you begin:
# 1. Make sure you have set up the datasets.R file
#   (i)  You have installed all required packages for the datasets.R file
#   (ii) You have sourced (selected all and run) datasets.R file
# 2. Make sure you have installed the randomForest and ada packages.
# 3. Make sure you have selected and run the plotModel and plotModel3d functions
#    from the regression.R file
##############

# EXTENSION - Tree based methods
require(randomForest)

airfoil=getAirfoil()

# What do random forest 'look like'.
# Let's see in 2 and 3 dimensions
poly4=getPoly4()
careers=getCareers()

set.seed(1)
rf_model2d=randomForest(Y~X,poly4)
plotModel(rf_model2d,poly4[1],poly4$Y,0,10)

rf_model3d=randomForest(prestige~income+education,careers)
plotModel3d(rf_model3d,careers[2:3],careers$prestige,0,100,0,100)


### Random Forests and tuning the mtry parameter

# We want to tune the mtry parameter. We will use more complex data and
# tuneRF
# This will do a search on mtry parameters, using models with 50 trees.
# The different values are scored on OOB error.
# You can customize how this search proceeds and change the number of trees
# used in the models. But that is beyond this course.
# If we specify doBest=TRUE the best model is returned.
set.seed(1)
rf_model=tuneRF(airfoil[1:4],airfoil$Sound,ntreeTry=1000,doBest=TRUE)

# We can see how it would compare to other models from the selection code
# We recreate the data
train=.5
valid=.3
set.seed(0)
rndIndices=sample(nrow(poly4))
trainFrom=1
trainTo=length(rndIndices)*train
validFrom=trainTo+1
validTo=trainTo+length(rndIndices)*valid
testFrom=validTo+1
testTo=length(rndIndices)
trData=poly4[rndIndices[trainFrom:trainTo],]
evData=poly4[rndIndices[validFrom:validTo],]
teData=poly4[rndIndices[testFrom:testTo],]

set.seed(0)
# There is no need to tune it, since there is only one feature.
rf_model=randomForest(Y~X,trData)
pred_rf=predict(rf_model,evData)
mse_rf=mean((pred_rf-evData$Y)^2)
mse_rf
# Worse that polynomial regression of orders 4-6 and slightly worse
# than the first neural network model.
# But then... that data was simple for polynomial regression models!

# Random Forests can also be used for classification tasks

# Let's get the same data as in the classification exercises
Sonar=getSonar()
set.seed(0)
trainprop=.65 # Proportion of whole data set
evalprop=.5 # Proportion of non-training data
train.ind = sample(nrow(Sonar),nrow(Sonar)*trainprop)
train.Sonar=Sonar[train.ind,]
nonTraining=Sonar[-train.ind,]
eval.ind = sample(nrow(nonTraining),nrow(nonTraining)*evalprop)
eval.Sonar=nonTraining[eval.ind,]
test.Sonar=nonTraining[-eval.ind,]

# We can create the RF models
set.seed(1)

# Here is a basic one
set.seed(0)
rf_model_1=randomForest(Class~.,train.Sonar)
rf_1_acc=mean(predict(rf_model_1,eval.Sonar)==eval.Sonar$Class)
# Here is one with more trees and a tuned mtry parameter
rf_model_2=tuneRF(Sonar[1:60],Sonar$Class,ntreeTry=2000,doBest=TRUE)
rf_2_acc=mean(predict(rf_model_2,eval.Sonar)==eval.Sonar$Class)

# The second model got them all correct. Better than any of the
# other models we looked at. Was that luck?
# Let's see how it does on the test data
rf_2_test_acc=mean(predict(rf_model_2,test.Sonar)==test.Sonar$Class)
# Wow - all correct again. Looks like a good model... but probably not perfect
# Remember we only have small data sets...

### Adaboost
require(ada)
# Likewise we make one simple adaboost model without tuning
# By default there will be 50 trees.
set.seed(0)
ada_model=ada(Class~.,train.Sonar)
ada_model
# Out-Of-Bag Error:  0.044  iteration= 41            <- OOB suggests 41 trees
# Additional Estimates of number of iterations:
#  train.err1
#  49                <- Training misclassification error suggests 49 trees
#  train.kap1        <- Kappa statistic: See https://en.wikipedia.org/wiki/Cohen's_kappa
#  49                <- Training misclassification error suggests 49 trees

# Since these values are quite high, we might consider trying more trees:
set.seed(0)
# Since the seed in set the same, the first 50 trees will be identical.
# So we can overwrite the old model.
# Note that you would not normally do this.
ada_model=ada(Class~.,train.Sonar,iter=200)
ada_model
# Out-Of-Bag Error:  0  iteration= 103 <- OOB suggests 103 trees
# train.err1
# 49                                   <- 49 trees
# train.kap1
# 49                                   <- 49 trees

# We can evaluate these different suggestions on the test data
# Note that suggested numbers of trees used are for particular models.
ada_41_acc=mean(predict(ada_model,eval.Sonar,n.iter=41)==eval.Sonar$Class)
ada_49_acc=mean(predict(ada_model,eval.Sonar,n.iter=49)==eval.Sonar$Class)
ada_103_acc=mean(predict(ada_model,eval.Sonar,n.iter=103)==eval.Sonar$Class)
ada_41_acc
ada_49_acc
ada_103_acc
# Non are as good as the random forest model.
# But it looks like 49 trees is best, so if we were only looking at these 3
# models we would choose that one.
# How would it do on the test data?
ada_49_test_acc=mean(predict(ada_model_1,test.Sonar,n.iter=49)==test.Sonar$Class)
ada_49_test_acc

