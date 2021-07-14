'''
The purpose of the project is to build a model to predict the median house value in California.
I utilized the K-Nearest neighbor machine learning algorithm to predict the house value.
I first used the the longitude and latitude value to predict the housing value.
Then I added on some more variable to check if the mean squared error has declined over the time. 
'''

#Reading in the library and data
library(kknn)
ca =read.csv('CAhousing.csv')
logMed = log(ca$medianHouseValue)

n=dim(ca)[1]
ind = sample(1:n, 1000)
CAdata = ca[ind,]
Y=logMed[ind]

#Use the longitude and latitude to predict the house value
train = data.frame(Y,CAdata$longitude, CAdata$latitude)
test = data.frame(Y,CAdata$longitude, CAdata$latitude)

near = kknn(Y~., train, test, k=10, kernel='rectangular')
res = Y-near$fitted


n = dim(CAdata)[1]
kcv = 10
n0 = round(n/kcv, 0)

out_MSE = matrix(0, kcv, 100)

used = NULL
set = 1:n

for (j in 1:kcv){
  if(n0 >length(set)){val=set}
  if(n0 <=length(set)){val=sample(set, n0)}
  
  for (i in 1:100) {
    train_i = train[-val,]
    test_i = test[val,]
    
    near = kknn(Y~., train_i, test_i, k=i, kernel='rectangular')
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
  }
  used = union(used, val)
  set = (1:n)[-used]
  
  cat(j, '\n')
}

par(mfrow=c(1,1))
mMSE = apply(out_MSE, 2, mean)
plot(log(1/(1:100)), sqrt(mMSE), type="l", xlab='complexity', ylab='out-of-sample RMSE', col=4, lwd=2)
best= which.min(mMSE)
text(log(1/best), sqrt(mMSE[best])+0.01,paste('k=',best))

#=======================================================================
## Putting in the income variable
Income = scale(CAdata$medianIncome)*sd(CAdata$longitude)

train = data.frame(Y, CAdata$longitude, CAdata$latitude, Income)
test = data.frame(Y, CAdata$longitude, CAdata$latitude, Income)

n = dim(CAdata)[1]
kcv = 10
n0 = round(n/kcv, 0)

out_MSE = matrix(0, kcv, 100)
used= NULL
set = 1:n

for (j in 1:kcv){
  if(n0>length(set)){val=set}
  if(n0<=length(set)){val=sample(set, n0)}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for (i in 1:100){
    near = kknn(Y~., train_i, test_i, k=i, kernel='rectangular')
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
    
  }
  used = union(used, val)
  set = (1:n)[-used]
  cat(j, '\n')
}

mMSE = apply(out_MSE, 2, mean)
plot(log(1/(1:100)), sqrt(mMSE), type="l", xlab='complexity', col=4, lwd=2)
best = which.min(mMSE)
text(log(1/best), sqrt(mMSE[best])+0.01, paste('k=', best))
#========================================================================

Room = scale(CAdata$totalBedrooms)*sd(CAdata$longitude)

train = data.frame(Y, CAdata$longitude, CAdata$latitude, Income, Room)
test = data.frame(Y, CAdata$longitude, CAdata$latitude, Income, Room)

n = dim(CAdata)[1]
kcv = 10
n0 = round(n/kcv, 0)

out_MSE = matrix(0, kcv, 100)
used= NULL
set = 1:n

for (j in 1:kcv){
  if(n0>length(set)){val=set}
  if(n0<=length(set)){val=sample(set, n0)}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for (i in 1:100){
    near = kknn(Y~., train_i, test_i, k=i, kernel='rectangular')
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
    
  }
  used = union(used, val)
  set = (1:n)[-used]
  cat(j, '\n')
}

mMSE = apply(out_MSE, 2, mean)
plot(log(1/(1:100)), sqrt(mMSE), type="l", xlab='complexity', col=4, lwd=2)
best = which.min(mMSE)
text(log(1/best), sqrt(mMSE[best])+0.01, paste('k=', best))

#===========================================
#Adding in the age variable
Age = scale(CAdata$housingMedianAge)*sd(CAdata$longitude)

train = data.frame(Y, CAdata$longitude, CAdata$latitude, Income, Room, Age)
test = data.frame(Y, CAdata$longitude, CAdata$latitude, Income, Room, Age)

n = dim(CAdata)[1]
kcv = 10
n0 = round(n/kcv, 0)

out_MSE = matrix(0, kcv, 100)
used= NULL
set = 1:n

for (j in 1:kcv){
  if(n0>length(set)){val=set}
  if(n0<=length(set)){val=sample(set, n0)}
  
  train_i = train[-val,]
  test_i = test[val,]
  
  for (i in 1:100){
    near = kknn(Y~., train_i, test_i, k=i, kernel='rectangular')
    aux = mean((test_i[,1]-near$fitted)^2)
    
    out_MSE[j,i] = aux
    
  }
  used = union(used, val)
  set = (1:n)[-used]
  cat(j, '\n')
}

mMSE = apply(out_MSE, 2, mean)
plot(log(1/(1:100)), sqrt(mMSE), type="l", xlab='complexity', col=4, lwd=2)
best = which.min(mMSE)
text(log(1/best), sqrt(mMSE[best])+0.01, paste('k=', best))
