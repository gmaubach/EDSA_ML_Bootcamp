##############
# Before you begin:
# 1. Make sure you have set up the datasets.R file
#   (i)  You have installed all required packages for the datasets.R file
#   (ii) You have sourced (selected all and run) datasets.R file
# 2. Make sure you have installed the entropy package
##############

# This code shows one way to complete the feature selection exercise
# You may find it a little more complex than other code you have seen
# Try to understand it and replicate it, but if you are having trouble
# ask me, and review it after class

# Load libraries
library(entropy)
library(fpc)

### General functions used
# apply
# cbind
# lapply
# length
# max
# mean
# sapply
# table         See the note on line 110
# unique(vec)   Returns a vector of the unique items in the passed vector vec.
# unlist        See the note on line 91
# which
#
### Statistical functions used
# cor.test
# mi.plugin
# bhattacharyya.dist
#
### Operators used
# []
# -
# /
# ^
# %/%           Integer division. Result rounded down to closest integer.
# ==
# $
# :
#
### Keywords used
# for
# function

# Load data
careers=getCareers(TRUE) # TRUE gets the discrete Profession column too

# Correlation tests for income and education versus prestige
# Note 1
# What we are doing here is getting the correlation between
# the first, second and third powers of the features and the target
# and using the maximum value as the score for the feature.
# Note 2
# The cor.test function returns lots of information. The correlation
# is given in the estimate field.
ct_income=max(sapply(1:3,function(order)cor.test(careers$income^order,careers$prestige)$estimate))
ct_education=max(sapply(1:3,function(order)cor.test(careers$education^order,careers$prestige)$estimate))
# We can look at the values
ct_income
ct_education

# Now we bin the income and education features
# I use the %/% operator, which is integer division.
# ***Note mod 20 leaves singleton class in income.***
careers$income=(careers$income-1)%/%25
careers$education=(careers$education-1)%/%25

#' Here we will calculate a conditional distribution divergence
#' score for each feature. 
#' 
#' We begin by dividing the row indices into sets
#' according to values taken by the discrete variable. 
cddScores=apply(careers[1:3],2,function(feature){
  # feature is the feature column
  # Let's find out what values it has using the unique function
  values=unique(feature)
  # Now we can split the careers prestige data based on these values
  splits=lapply(values,function(v) careers$prestige[which(feature==v)])
  # We can find the means of these subsets
  means=sapply(splits,mean)
  # And the variance
  vars=sapply(splits,var)
  #' Using these statistics, we calculate the Bhattacharyya 
  #' distance of each split vector from each of the others.
  #' We average these for the cdd score for the feature.
  #' To calculate the Bhattacharyya distance use the
  #' bhattacharyya.dist function. It expects the mean of 
  #' vector 1, the mean of vector 2, the variance of 
  #' vector 1 and the variance of vector 2. You can find 
  #' the bhattacharyya.dist function in fpc package.
  dists=lapply(1:(length(splits)-1),function(i){
    sapply(i:length(splits),function(j) {
      bhattacharyya.dist(means[i],means[j],vars[i],vars[j])
    })
  }) 
  mean(unlist(dists))
})
# We can look at the values
cddScores


# Now we bin the target prestige variable
# I use the %/% operator, which is integer division.
careers$prestige=(careers$prestige-1)%/%20

# Now we calculate the mutual information between the
# discrete features and discrete target variable
# NOTE: mi.plugin wants a 2d frequency matrix that
# gives the number of rows of each combination of feature
# and target values. We can get this with the table function
# passing the two columns.
mis=apply(careers[1:3],2,function(feature){
  mi.plugin(table(cbind(feature,careers[4])))
})

# We can look at the values
mis




#' We could use Mahalanobis distance instead of Bhattacharyya
#' distance for the cdd scores. This makes the additional
#' assumption that the conditional distributions have
#' the same variance. We would use it in this way:
mahScores=apply(careers[1:3],2,function(feature) {
  # feature is the feature column
  # Let's find out what values it has using the unique function
  values=unique(feature)
  # Now we can split the careers prestige data based on these values
  splits=lapply(values,function(v) careers$prestige[which(feature==v)])
  # We can find the means of these subsets
  means=sapply(splits,mean)
  
  # Now we find the shared variance
  # It might take you a while to see how this matches the formula
  # in the slides, but it does.
  for (i in 1:length(splits)) {
    splits[[i]]=(splits[[i]]-means[i])^2
  }
  sharedVar=sum(sapply(splits,sum))/(nrow(careers)-length(values))
  
  # Now we calculate the average Mahalanobis distance of the
  # different conditional distributions from each other
  # Note: The unlist function turns the list of vectors
  # (each of a different length) into a single vector
  # which we can take the mean of
  mean(unlist(lapply(1:(length(means)-1),function(i) {
    sapply((i+1):length(means),function(j) {
      mahalanobis(means[i],means[j],sharedVar)
    })
  })))
})

# We can look at the values
mahScores
