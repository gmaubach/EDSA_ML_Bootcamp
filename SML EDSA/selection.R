##############
# Before you begin:
# 1. Make sure you have set up the datasets.R file
#   (i)  You have installed all required packages for the datasets.R file
#   (ii) You have sourced (selected all and run) datasets.R file
# 2. Make sure you have installed the nnet package.
##############

library(nnet)

# General functions used
# set.seed
# sample
# nrow
# length
# mean
# c

# Graphing functions used
# barplot

# Modeling functions used
# lm
# poly
# nnet
# predict

# Operators used
# []
# [,]
# :
# -
# ^

# Get data
# In this example I use the synthetic poly4 data.
# You can use any of the other data sets.
poly4=getPoly4()



# Split data into training, validation and test

# 1. Specify proportions of data to use.
train=.5
valid=.3
test=.2 # This is not used - it is just for comprehension

# 2. Randomize row numbers (set seed so it is repeatable)
set.seed(0)
rndIndices=sample(nrow(poly4))

# 3. Find to and from points to get mutually exclusive sets of random row
# numbers in proportions desired
trainFrom=1
trainTo=length(rndIndices)*train
validFrom=trainTo+1
validTo=trainTo+length(rndIndices)*valid
testFrom=validTo+1
testTo=length(rndIndices)

# 4. Split up data
trData=poly4[rndIndices[trainFrom:trainTo],]
evData=poly4[rndIndices[validFrom:validTo],]
teData=poly4[rndIndices[testFrom:testTo],]

# Create a linear model
lm_model=lm(Y~X,trData)

# Cannot do a Poisson model on the poly4 data because there are negative Y values
# pr_model=glm(Y~X,"poisson",trData) # WILL RETURN ERROR

# Create 5 polynomial regression models
# When using poly in the formula, you need to
#   1. Write out the whole formula - i.e. not use '.'
#   2. Specify which variables the poly function is to be applied to.
# When there are multiple features, you will not be able to try all combinations!
# Hint: For mussels, use poly on temp.
pr2_model=lm(Y~poly(X,2),trData)
pr3_model=lm(Y~poly(X,3),trData)
pr4_model=lm(Y~poly(X,4),trData)
pr5_model=lm(Y~poly(X,5),trData)
pr6_model=lm(Y~poly(X,6),trData)

# Create two neural network model
# There are important tuning parameters we do not look at yet
set.seed(0)
nn_model1=nnet(
  Y~X,          # Formula
  trData,        # Data
  size=4,       # Nodes in hidden layer
  linout=TRUE   # We want a linear output since we are doing regression
)
set.seed(1)
nn_model2=nnet(
  Y~X,          # Formula
  trData,        # Data
  size=4,       # Nodes in hidden layer
  linout=TRUE   # We want a linear output since we are doing regression
)

# Now we create predictions from our models for the 
# evaluation data and calculate the MSE for this data.
# Then we look at the results.
# Here we do it 'manually', afterwards we look at 
# a better approach (in terms of coding).
pred_lm=predict(lm_model,evData)
pred_pr2=predict(pr2_model,evData)
pred_pr3=predict(pr3_model,evData)
pred_pr4=predict(pr4_model,evData)
pred_pr5=predict(pr5_model,evData)
pred_pr6=predict(pr6_model,evData)
pred_nn1=predict(nn_model1,evData)
pred_nn2=predict(nn_model2,evData)
  
# Calculate the MSE
mse_lm=mean((pred_lm-evData$Y)^2)
mse_pr2=mean((pred_pr2-evData$Y)^2)
mse_pr3=mean((pred_pr3-evData$Y)^2)
mse_pr4=mean((pred_pr4-evData$Y)^2)
mse_pr5=mean((pred_pr5-evData$Y)^2)
mse_pr6=mean((pred_pr6-evData$Y)^2)
mse_nn1=mean((pred_nn1-evData$Y)^2)
mse_nn2=mean((pred_nn2-evData$Y)^2)

# Let's look at the results
results=c(mse_lm,mse_pr2,mse_pr3,mse_pr4,mse_pr5,mse_pr6,mse_nn1,mse_nn2)
barplot(results,names.arg=c("lm","pr2","pr3","pr4","pr5","pr6","nn1","nn2"))

# In the poly4 case, the best was the pr4_model (no surprise there!)
# Let's see how it does on the test data
pred_pr4_test=predict(pr4_model,teData)
mse_pr4_test=mean((pred_pr4_test-teData$Y)^2)
mse_pr4_test

# EXTENSION - Better R
# The above can be compressed.
# We can place all our models in a list:
models=list(lm=lm_model,pr2=pr2_model,pr3=pr3_model,
            pr4=pr4_model,pr5=pr5_model,pr6=pr6_model,
            nn1=nn_model1,nn2=nn_model2)
# Now get all predictions for evaluation data using lapply
predictions=lapply(models,predict,evData)
# And the MSEs...
MSEs=sapply(predictions,function(p)mean((p-evData$Y)^2))
# If we want we can produce the barplot:
barplot(MSEs,names.arg=names(models))
# Or we can automate finding the best model and 
# see how it performs on the test data
best=which.min(MSEs)
predBest=predict(models[[best]],teData)
mseBest_test=mean((predBest-teData$Y)^2)
