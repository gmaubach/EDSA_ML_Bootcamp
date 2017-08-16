##############
# Before you begin:
# 1. Make sure you have install the nnet package
##############

# Load the four functions below into your environment (select and run them)
# I will explain what they do in class
# Then you can play around with exampleReg and findBestNNDecay
rr=function(X,Y,lambda,order) {
  X=poly(X,order,raw=T,simple=T)
  X=cbind(1,X)
  b=solve(crossprod(X)+diag(lambda,ncol(X)))%*%crossprod(X,Y)
  out=list(b=b,order=order)
  class(out)="rr"
  return(out)
}
predict.rr=function(model,X) {
  if (is.data.frame(X)) X=X[,1]
  X=poly(X,model$order,raw=T,simple=T)
  X=cbind(1,X)
  X%*%model$b
}
exampleReg=function(order,lambdas=c(0,.1,1,10,100,1000000000)) {
  poly4=getPoly4()
  for (l in lambdas) {
    model=rr(poly4$X,poly4$Y,l,order)
    plotModel(model,poly4[1],poly4$Y,0,10,main=paste("Lambda =",l))
    readline("Press Enter to Continue ...")
  }
}

# Work through together
# Required understanding:
# - sapply
# - rownames, colnames
# - which (arr.ind=TRUE)
# Note that there is also implicit regularization via
# early stopping when n is large if maxit is low
# Examine n=100,1000,10000
findBestNNDecay=function(n,nodes=2:6,lambdas=c(0,.001,.01,.1,.5,1,2,10),maxit=1000) {
  require(nnet)
  set.seed(0)
  # Get data
  poly4=getPoly4(n)

  # Split data into training, validation and test
  trainProp=.5
  validProp=.3
  testProp=.2
  rndIndices=sample(nrow(poly4))
  trainFrom=1
  trainTo=length(rndIndices)*trainProp
  validFrom=trainTo+1
  validTo=trainTo+length(rndIndices)*validProp
  testFrom=validTo+1
  testTo=length(rndIndices)
  train=poly4[rndIndices[trainFrom:trainTo],]
  evaluation=poly4[rndIndices[validFrom:validTo],]
  test=poly4[rndIndices[testFrom:testTo],]

  mses=sapply(nodes,function(n){
    sapply(lambdas,function(l){
      model=nnet(
        Y~X,          # Formula
        train,        # Data
        size=n,       # Nodes in hidden layer
        linout=TRUE,  # We want a linear output since we are doing regression
        decay=l,       # L2 reglarization
        maxit=maxit
      )
      est=predict(model,evaluation)
      mse=mean((est-evaluation$Y)^2) # We use the fact we know the target variable's name
    })
  })
  rownames(mses)=lambdas
  colnames(mses)=nodes
  minMSE=min(mses)
  best=which(mses==minMSE,arr.ind=TRUE)
  model=nnet(
    Y~X,          # Formula
    train,        # Data
    size=best[2],       # Nodes in hidden layer
    linout=TRUE,  # We want a linear output since we are doing regression
    decay=best[1],       # L2 reglarization
    maxit=maxit
  )
  est=predict(model,test)
  mse=mean((est-test$Y)^2) # We use the fact we know the target variable's name
  list(mses=mses,model=model,mse=mse,best=best)
}
