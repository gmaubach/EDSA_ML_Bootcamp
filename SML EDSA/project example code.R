library(randomForest)
library(MASS)
library(e1071)
library(nnet)

# Setup data
abalone=read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data",
                 header=F)
abalone[,9]=abalone[,9]>8 

# Split data
rndIndices=sample(nrow(abalone))
train=abalone[rndIndices[1:3000],]
eval=abalone[rndIndices[3001:3750],]
test=abalone[rndIndices[3751:nrow(abalone)],]

# CDD scores
cddScores=apply(train[2:8],2,function(col){
  s1=col[train[,9]]
  s2=col[!train[,9]]
  fpc::bhattacharyya.dist(mean(s1),mean(s2),var(s1),var(s2))
})
barplot(cddScores,main="CDD Scores")

# Create and evaluate raw data models
train[,9]=as.factor(train[,9]) # We run into problems otherwise
lda_raw=lda(V9~.,train)
qda_raw=qda(V9~.,train)
lr_raw=glm(V9~.,"binomial",train)
res=tuneRF(train[1:8],as.factor(train[,9])) # Looks like mtry = 2 is good
rf_raw=randomForest(V9~.,train,mtry=2,ntree=1000) # 1000 trees - takes a while
svm_tune_results=tune.svm(V9~.,data=train,type="C",cost=c(.5,1,2),gamma=c(.1,1,2)) # 9 models - takes a while!
svm_raw=svm_tune_results$best.model
nn_raw=nnet(V9~.,train,size=3,decay=.1,maxit=100000) # Should tune these, but oh well...

lda_raw_mse=mean(predict(lda_raw,eval)$class==eval$V9)
qda_raw_mse=mean(predict(qda_raw,eval)$class==eval$V9)
lr_raw_mse=mean((predict(lr_raw,eval)>.5)==eval$V9)
rf_raw_mse=mean(predict(rf_raw,eval)==eval$V9)
svm_raw_mse=mean(predict(svm_raw,eval)==eval$V9)
nn_raw_mse=mean(predict(nn_raw,eval)>.5==eval$V9)

barplot(c(lda_raw_mse,qda_raw_mse,lr_raw_mse,rf_raw_mse,svm_raw_mse,nn_raw_mse),
        names.arg=c("LDA","QDA","LR","RF","SVM","NN"),
        main="Evaluation MSE using raw features")

# Create and evaluate models using good numeric features: V2,V3,V5,V7,V8
lda_fs=lda(V9~V1+V2+V3+V5+V7+V8,train)
qda_fs=qda(V9~V1+V2+V3+V5+V7+V8,train)
lr_fs=glm(V9~V1+V2+V3+V5+V7+V8,"binomial",train)
res=tuneRF(train[c(1,2,3,5,7,8)],as.factor(train[,9])) # Looks like mtry = 1 is good
rf_fs=randomForest(V9~V1+V2+V3+V5+V7+V8,train,mtry=1,ntree=1000) # 1000 trees - takes a while
svm_tune_results=tune.svm(V9~V1+V2+V3+V5+V7+V8,data=train,type="C",cost=c(.5,1,2),gamma=c(.1,1,2)) # 9 models - takes a while!
svm_fs=svm_tune_results$best.model
nn_fs=nnet(V9~V1+V2+V3+V5+V7+V8,train,size=3,decay=.1,maxit=100000) # Should tune these, but oh well...

lda_fs_mse=mean(predict(lda_fs,eval)$class==eval$V9)
qda_fs_mse=mean(predict(qda_fs,eval)$class==eval$V9)
lr_fs_mse=mean((predict(lr_fs,eval)>.5)==eval$V9)
rf_fs_mse=mean(predict(rf_fs,eval)==eval$V9)
svm_fs_mse=mean(predict(svm_fs,eval)==eval$V9)
nn_fs_mse=mean((predict(nn_fs,eval)>.5)==eval$V9)

barplot(c(lda_fs_mse,qda_fs_mse,lr_fs_mse,rf_fs_mse,svm_fs_mse,nn_fs_mse),
        names.arg=c("LDA","QDA","LR","RF","SVM","NN"),
        main="Evaluation MSE using selected features")

# PCA
# Make the PCA data
pc=prcomp(train[2:8],scale=TRUE) # Good to scale data as part of PCA. This does that.
barplot(pc$sdev) # Looks like the first PC contains most of the variance.
pcTrain=data.frame(PC1=pc$x[,1],V1=train$V1,V9=train$V9)
pc_eval=predict(pc,eval)
pcEval=data.frame(PC1=pc_eval[,1],V1=eval$V1,V9=eval$V9)
pc_test=predict(pc,test)
pcTest=data.frame(PC1=pc_test[,1],V1=test$V1,V9=test$V9)

# Create and evaluate PC models
lda_pc=lda(V9~.,pcTrain)
qda_pc=qda(V9~.,pcTrain)
lr_pc=glm(V9~.,"binomial",pcTrain)
res=tuneRF(pcTrain[1:2],pcTrain[,3]) # Looks like mtry = 1 is good
rf_pc=randomForest(V9~.,pcTrain,mtry=1,ntree=1000) # 1000 trees - takes a while
svm_tune_results=tune.svm(V9~.,data=pcTrain,type="C",cost=c(.5,1,2),gamma=c(.1,1,2)) # 9 models - takes a while!
svm_pc=svm_tune_results$best.model
nn_pc=nnet(V9~.,pcTrain,size=3,decay=.1,maxit=100000) # Should tune these, but oh well...

lda_pc_mse=mean(predict(lda_pc,pcEval)$class==pcEval$V9)
qda_pc_mse=mean(predict(qda_pc,pcEval)$class==pcEval$V9)
lr_pc_mse=mean((predict(lr_pc,pcEval)>.5)==pcEval$V9)
rf_pc_mse=mean(predict(rf_pc,pcEval)==pcEval$V9)
svm_pc_mse=mean(predict(svm_pc,pcEval)==pcEval$V9)
nn_pc_mse=mean((predict(nn_pc,pcEval)>.5)==pcEval$V9)

barplot(c(lda_pc_mse,qda_pc_mse,lr_pc_mse,rf_pc_mse,svm_pc_mse,nn_pc_mse),
        names.arg=c("LDA","QDA","LR","RF","SVM","NN"),
        main="Evaluation MSE using first PC")

# Let's look at everything!
m=matrix(c(lda_raw_mse,qda_raw_mse,lr_raw_mse,rf_raw_mse,svm_raw_mse,nn_raw_mse,
         lda_fs_mse,qda_fs_mse,lr_fs_mse,rf_fs_mse,svm_fs_mse,nn_fs_mse,
         lda_pc_mse,qda_pc_mse,lr_pc_mse,rf_pc_mse,svm_pc_mse,nn_pc_mse),nrow=3,byrow=TRUE)
         
barplot(m,beside=TRUE,
        names.arg=c("LDA","QDA","LR","RF","SVM","NN"),
        main="Evaluation MSE")

# The best model overall (on my run) was the random forest with all raw
# features. So that is our chosen model.

# Evaluate chosen model on test data:
rf_raw_test_mse=mean(predict(rf_raw,test)==test$V9)

# We should now:
# 1. Create a confusion matrix for the chosen model on the test data.
# 2. Create a learning graph for the chosen model.
# 3. Prepare a report. OK - not really :)
