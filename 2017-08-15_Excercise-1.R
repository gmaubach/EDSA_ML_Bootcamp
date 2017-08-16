# Don't worry about missing data!

# Data

getOzone = function () {
  out=datasets::airquality[c(3,1) , ]
  out=out[-which(is.na(out[,1])) , ]
  return(out)
}
ozoneData=getOzone()

# Model
lm_model=lm(Ozone~Wind,ozoneData)
pr_model=glm(Ozone~Wind,"poisson",ozoneData)

# Mean Squared Error for Linear Regression Model
lm_est=predict(lm_model,ozoneData)  # on the original data
lm_mse=mean((lm_est-ozoneData$Ozone)^2)

# Mean Squared Error for Polynomial Regression
pr_est=predict(pr_model,ozoneData,type="response")
pr_mse=mean((pr_est-ozoneData$Ozone)^2)
lm_mse
pr_mse