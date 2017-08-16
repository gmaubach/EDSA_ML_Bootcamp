##############
# Before you begin:
# 1. Make sure you have set up the datasets.R file
#   (i)  You have installed all required packages for the datasets.R file
#   (ii) You have sourced (selected all and run) datasets.R file
##############

# This code shows one way to complete the PCA exercise

# Load data
careers=getCareers()

# We take the principle components of the numeric features.
pcs=prcomp(careers[1:2])

# And we make a new data frame with the principle components
# and the target prestige variable
pcCareers=data.frame(PC1=pcs$x[,1],PC2=pcs$x[,2],prestige=careers$prestige)

# Now we build a function that will calculate the
# all-but-one cross validate error for a linear regression
# model given a formula and data set.
cv=function(formula,data) {
  # We iterate over the row numbers
  cv_errs=sapply(1:nrow(data),function(i) {
    # Create the model from all rows except i
    model=lm(formula,data[-i,])
    # And we calculate the model's error on row i
    data$prestige[i]-predict(model,data[i,])
  })
  # Now we calculate the MSE
  mean(cv_errs^2)
}

# Lets see how the different alternatives perform
cv(prestige~income+education,careers)
cv(prestige~income,careers)
cv(prestige~education,careers)
cv(prestige~PC1+PC2,pcCareers)
cv(prestige~PC1,pcCareers)


cvPCA=function(formula,data) {
  # We iterate over the row numbers
  cv_errs=sapply(1:nrow(data),function(i) {
    # Perform PCA on all but i
    pc=prcomp(data[-i,1:2])
    # Create new data with all rows but i and with PCs
    newData=cbind(data[-i,],pc$x)
    # Create the model from all rows except i
    model=lm(formula,newData)
    # Project hold out row onto PCs
    pci=predict(pc,data[i,])
    prData=cbind(data[i,],pci)
    # And we calculate the model's error on row i
    data$prestige[i]-predict(model,prData)
  })
  # Now we calculate the MSE
  mean(cv_errs^2)
}
cvPCA(prestige~PC1+PC2,careers)
cvPCA(prestige~PC1,careers)



# Lets see how the different alternatives perform
raw12=cv(prestige~income+education,careers)
raw1=cv(prestige~income,careers)
raw2=cv(prestige~education,careers)
pca12=cv(prestige~PC1+PC2,pcCareers)
pca1=cv(prestige~PC1,pcCareers)
MSES=c(raw12,raw1,raw2,pca12,pca1)
barplot(MSES,
        names.arg=c("raw12","raw1","raw2","pca12","pca1"),
        main="CV ABO MSE"
        )
