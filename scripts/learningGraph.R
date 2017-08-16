##############
# Before you begin:
# 1. Make sure you have set up the datasets.R file
#   (i)  You have installed all required packages for the datasets.R file
#   (ii) You have sourced (selected all and run) datasets.R file
##############

# Load the functions below into your environment (select and run it)
# I will explain what it does in class
# Then you can play around with it
# Try:
# learningGraph(200)
# learningGraph(2000)
# Smoothing helps:
# learningGraph(200,smooth=6)
# learningGraph(2000,smooth=6)
learningGraph=function(
  n,
  trainProp=seq(.05,.75,.05),
  order=4,
  smooth=1,
  ylim=NULL
) {
  set.seed(0)
  poly4=getPoly4(n)

  mses=sapply(trainProp,function(tp){
      # Split data into training and validation
      rndIndices=sample(nrow(poly4))
      trainFrom=1
      trainTo=length(rndIndices)*tp
      validFrom=trainTo+1
      validTo=length(rndIndices)
      train=poly4[rndIndices[trainFrom:trainTo],]
      evaluation=poly4[rndIndices[validFrom:validTo],]

      # Create model and perform experiments
      model=lm(Y~poly(X,order),train)
      est_train=predict(model,train)
      est_valid=predict(model,evaluation)
      mse_train=mean((est_train-train$Y)^2)
      mse_valid=mean((est_valid-evaluation$Y)^2)
      c(mse_train,mse_valid)
  })

  if (smooth>1) {
    mses=sapply(1:(ncol(mses)-smooth),function(i) {
      c(mean(mses[1,i:(i+smooth)]),mean(mses[2,i:(i+smooth)]))
    })
  }

  if (is.null(ylim)) ylim=c(min(mses),max(mses))
  plot(mses[1,],type="b",col="blue",  ylim=ylim,ylab="MSE")
  points(mses[2,],type="b",col="red")

  return(mses)
}
