# Don't worry about missing data!
ozoneData=getOzone()
lm_model=lm(Ozone~Wind,ozoneData)
pr_model=glm(Ozone~Wind,"poisson",ozoneData)
lm_est=predict(lm_model,ozoneData)
lm_mse=mean((lm_est-ozoneData$Ozone)^2)
pr_est=predict(pr_model,ozoneData,type="response")
pr_mse=mean((pr_est-ozoneData$Ozone)^2)
lm_mse
pr_mse