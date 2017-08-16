##############
# Before you begin:
# 1. Make sure you have installed the car, MASS, mlbench datasets
# 2. Note that getAirfoil require access to the internet
##############

getPoly4 = function(n=100) {
  set.seed(0)
  X=runif(n,0,10)
  Y=X+X^2-.25*X^3+.015*X^4+rnorm(n,0,2)
  data.frame(X=X,Y=Y)
}
getAirfoil = function () {
  airfoil=read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/00291/airfoil_self_noise.dat")
  colnames(airfoil)=c("Frequency","Angle","Chord","Velocity","Suction","Sound")
  return(airfoil)
}
getCareers = function (includeProfession=FALSE) {
  if (includeProfession)
    return (car::Duncan)
  else
    return (car::Duncan[-1])
}
getOzone = function () {
  out=datasets::airquality[c(3,1)]
  out=out[-which(is.na(out[,1])),]
  return(out)
}
getMussels = function () {
  set.seed(1)
  dist=runif(120,50,500)
  depth=runif(120,2,15)
  temp=rnorm(120,20.5,1.5)
  cond=sample(c("SHE","MED","RGH"),120,T,c(3,2,1))
  cond_n=sapply(cond,function(x) {
    if (x=="SHE") return(1.3)
    else if (x=="MED") return (.9)
    else return (.7)
  })
  yeild=(100-.1*dist-.1*depth+.2*(temp-20)^2)*cond_n+rnorm(120,0,20)
  cond=as.factor(cond)
  data.frame(dist,depth,temp,cond,yeild)
}

getSonar=function() {
  require(mlbench)
  data(Sonar)
  return(Sonar)
}
getMenarche=function() {
  return (MASS::menarche)
}
getSeeds=function(a=3,b=4) {
  set.seed(0)
  data.frame(age=c(sample(6,a,T),sample(4:8,b,T)),germinated=c(rep(0,a),rep(1,b)))
}

createCM=function(actual,predicted) {
  TP=length(which(predicted==1 & actual==1))
  FP=length(which(predicted==1 & actual==0))
  FN=length(which(predicted==0 & actual==1))
  TN=length(which(predicted==0 & actual==0))
  cm=matrix(c(TP,FP,FN,TN),nrow=2)
  colnames(cm)=c("Predicted True","Predicted False")
  rownames(cm)=c("Actual True","Actual False")
  return (cm)
}

