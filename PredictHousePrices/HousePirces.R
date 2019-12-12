# Predict House Pirces for Kaggle Competiton

# Import train data and test data
train<-read.csv("C:\\Users\\Playdata\\Downloads\\ADP\\kaggleData\\HousePrice\\house-prices-advanced-regression-techniques\\train.csv", header=T, stringsAsFactors = F)
test<-read.csv("C:\\Users\\Playdata\\Downloads\\ADP\\kaggleData\\HousePrice\\house-prices-advanced-regression-techniques\\test.csv", header=T, stringsAsFactors = F)
real<-read.csv("C:\\Users\\Playdata\\Downloads\\ADP\\kaggleData\\HousePrice\\house-prices-advanced-regression-techniques\\sample_submission.csv", header=T, stringsAsFactors = F)

# Read data description and distinguish data into three groups
# Explanatory variables
factor<-c(2,3,6,7,8,9,10,11,12,13,14,15,16,17,18,
          22,23,24,25,26,28,29,30,31,32,33,34,36,40,
          41,42,43,54,56,58,59,61,64,65,66,73,74,75,79,80)
date<-c(20,21,60,77,78)
number<-c(4,5,19,27,35,37,38,39,44,45,46,47,48,49,50,51,
          52,53,55,57,62,63,67,68,69,70,71,72,76,77)

# Target variable
price<-train[,81]

# Call data from the source.
factorData<-train[,factor]
dateData<-train[,date]
numberData<-train[,number]

# Import libraries to deal with NA value.
library(DMwR)
library(Amelia)

# see missmap
# PoolQC, Alley, FirePlaceQu has too many valeus so that these will be thrown away.
missmap(factorData)
# Reasonably, the date when the building was sold does not affect price.
# so taht YrSold and MoSold will be discarded.
missmap(dateData)
# LotFrontage also will be discarded.
missmap(numberData)

# Change all rows into factor type for factor variables
for( i in 2:ncol(factorData)){
  factorData[,i]<-as.factor(factorData[,i])
}

# Rest of the NA value in data will be replaced with some methods.
numberData<-centralImputation(numberData)
factorData<-centralImputation(factorData)
dateData<-knnImputation(dateData)

# do anova test which factor does not affect building prices
# check p-value!
# if p value is bigger than 0.05, the variable will be not used.
for( i in 1:ncol(factorData)){
  data<-data.frame(factor=factorData[,i],price)
  result<-aov(price~factor,data=data)
  print(paste("-----------",names(factorData)[i]))
  print(summary(result))
}

# Do correlation analysis which variables does not affect building prices
# If cor is between -0.5 and 0.5, the variable will be not used.
for( i in 1:ncol(numberData)){
  print(paste('-------------------',names(numberData)[i]))
  print(cor(price,numberData[,i]))
}

# Reduced explanatory variables
factor<-c(2,3,8,9,11,13,14,15,16,17,18,
          22,23,24,25,26,28,29,30,31,32,33,34,36,40,
          41,42,43,54,56,59,61,64,65,66,79,80)
date<-c(20,21,60)
number<-c(39,44,47,50,55,62,63)

# Target variable
price<-train[,81]

# Call data to use from the source.
factorData<-train[,factor]
dateData<-train[,date]
numberData<-train[,number]

# Change all rows into factor type for factor variables
for( i in 2:ncol(factorData)){
  factorData[,i]<-as.factor(factorData[,i])
}

# Subtract to get the period of buildings.
for(i in 1:ncol(dateData)){
    dateData[,i]<-2018-dateData[,i]
}

# Rest of the NA value in data will be replaced with some methods.
numberData<-centralImputation(numberData)
factorData<-centralImputation(factorData)
dateData<-knnImputation(dateData)

# Bind dateData to numberData because both they are numeric with target variable, price. 
allNumberData<-cbind(numberData,dateData,price)

# Linear Regression for prices with numeric variables
priceByNumber<-lm(price~.,data=allNumberData)

# Adjusted R squared and p-value look good.
summary(priceByNumber)

# compared the price data to predicted values
pred<-predict(priceByNumber,newdata=allNumberData)

# There are some outlier points
plot(pred,price)

# There are 8 outlier points so that after removing them do linear regression again.
library(outliers)
outwith<-boxplot(pred)
outwith$out
# This is the group of outlier points.
outpoints<-c(441,497,524,692,770,799,1170,1183,1299)

# This is refined numeric data without outlier points
allNumberData.out<-allNumberData[-outpoints,]

# do again with refined data
priceByNumber<-lm(price~.,data=allNumberData.out)
pred<-predict(priceByNumber,newdata=allNumberData.out)

# Save model
saveRDS(priceByNumber,"./priceByNumber.rds")

# Numeric variables were used effectively.
# rest of data will be able to be explained by factorData

# Get the error by subtracting predicted prices from real prices
# This can be errors
targetPrice<-price[-outpoints]-pred
sum(as.numeric(is.na(pred)))

# combine error to factors Data
allFactorData<-cbind(factorData[-outpoints,],targetPrice)

# Here I converted targetPrices into factors by the magnititude of values.
# Distinguished the sections like this.

# lowest<-mean(targetPrice[targetPrice<=-18576])
# lower<-mean(targetPrice[targetPrice>-18576&targetPrice< -2114])
# higher<-mean(targetPrice[targetPrice>= -2114 & targetPrice<16639])
# highest<-mean(targetPrice[targetPrice>=16639])


# by doing randomForest, decision tree and logistic regression,
# I decided to use logistic regression to predict error wtih two sections.
# looking into targetPrice
# The median value will be able to differ from what I got due to the replacement of the NA above.

lower<-mean(targetPrice[targetPrice< median(targetPrice)])
higher<-mean(targetPrice[targetPrice>= median(targetPrice)])

# Then, change numeric values into factor. 
# 0 means lower parts
# 1 means higher parts.
allFactorData$targetPrice<-ifelse(targetPrice< median(targetPrice),0,1)

# Do step glm regression.
glmodel2<-step(glm(targetPrice ~ . , data=allFactorData),direction="backward")

# Save model
saveRDS(glmodel2,"./glmodel2.rds")

# Make Table and check how accurate this model is
glresult2<-predict(glmodel2,newdata=allFactorData[,-38])
glresult2<-ifelse(glresult2<0.5,0,1)
gt<-table(glresult2,allFactorData$targetPrice)
sum(diag(gt))/sum(gt)

# This points might be different from what I used.
# This are selected variables by step backward regression.
glmPoint<-c(2,6,7,9,10,11,19,20,21,22,23,29,30,33,36,37)

# Extract meaningful data from above
extraData<-allFactorData[,-glmPoint]

# Draw plot and see.
par(mfrow=c(1,2))
plot(pred,price[-outpoints])

# Judging by its shape. it looks similar to quadratic form a bit, but not quadratic.
# Therefore I decided to minimum MSE from 0.
# Here is my MSE function to get MSE on each predicted Prices.
MSE<-function(pred,price){
  value<-c()
  cost<-c()
  for( i in seq(0,300000,by=1000)){
    a<-0
    data<-pred[pred<=i]
    compared<-price[pred<=i]
    for( j in 1:length(data)){
      a = a + (data[j]-compared[j])^2
    }
    value<-c(value,i)
    cost<-c(cost,a/length(data))
  }
  result<-data.frame(value=value,cost=cost)
  result
}

# Use function
result<-MSE(pred,price[-outpoints])

# Find when MSE has minimum value.
which<-result[result$cost==min(result$cost),]$value

# when value is 159000, MSE has minimum

# This is a segment with only linear regression.
segments(0,0,which,which, lwd=3, col="cyan")

# These are segments with linear regression + logistic regression.
segments(lower,0,which+lower,which, lwd=3, col="red")
segments(higher,0,which+higher,which, lwd=3, col="blue")

# After seperating data into two sections, smaller than value 159000, bigger than value 159000
# Do Linear regression again.
extraData<-allNumberData.out[pred>=which,]
head(extraData)
extraExpectedPrice<-lm(price ~ . , data=extraData)

# Adjusted R squared and p-value look good.
summary(extraExpectedPrice)

# Save linear regression model of rest data
saveRDS(extraExpectedPrice,"./extraExpectedPrice.rds")

# Get predicted value of price of extraData
extraPred<-predict(extraExpectedPrice,newdata=extraData)

newTargetPrice<-price[-outpoints][pred>=which]

# Draw a plot of rest data
plot(extraPred,newTargetPrice)
abline(0,1,col="cyan",lwd=3)

# This method is perfectly same as the used method for logistic regression above.
# Get the error by subtracting expected value from the real data.
newTargetdiff<-newTargetPrice-extraPred
med<-median(newTargetdiff)
# Get mean value of each section.
extraLower<-mean(newTargetdiff[newTargetdiff< med])
extraHigher<-mean(newTargetdiff[newTargetdiff>= med])
extraLower#-17151.3
extraHigher#26496.94

# By median, divide into two sections.
newTargetdiff<-ifelse(newTargetdiff< med,0,1)

# Draw logistic lines on plot 
abline(extraHigher,1,lwd=3,col="red")
abline(extraLower,1,lwd=3,col="blue")

# After choosing data bigger than 159000 without outliers points
extraFactorData<-factorData[-outpoints,][pred>=which,]

# Combine factorData with target variable, newTargetdiff.
newExtraData<-cbind(extraFactorData,newTargetdiff)

# Do step backward logistic regression with above dataframe 
newGl<-step(glm(newTargetdiff~., data=newExtraData),direction="backward")

# Save model
saveRDS(newGl,"./newGl.rds")

# Get expected value
newGlpred<-predict(newGl,newdata=newExtraData[,-38])
newGlpred<-ifelse(newGlpred>=0.5,1,0)

# Get the accuracy
ntg<-table(newGlpred,newTargetdiff)
sum(diag(ntg))/sum(ntg)


# Total Summary
priceByNumber<-readRDS("./priceByNumber.rds")
glmodel2<-readRDS("./glmodel2.rds")
extraExpectedPrice<-readRDS("./extraExpectedPrice.rds")
newGl<-readRDS("./newGl.rds")
id<-test[,1]
factor<-c(2,3,8,9,11,13,14,15,16,17,18,
          22,23,24,25,26,28,29,30,31,32,33,34,36,40,
          41,42,43,54,56,59,61,64,65,66,79,80)

date<-c(20,21,60)
number<-c(1,39,44,47,50,55,62,63)
factorData<-test[,factor]
dateData<-test[,date]
numberData<-test[,number]
for( i in 2:ncol(factorData)){
  factorData[,i]<-as.factor(factorData[,i])
}
for(i in 1:ncol(dateData)){
  dateData[,i]<-2018-dateData[,i]
}
factorData<-centralImputation(factorData)
dateData<-centralImputation(dateData)
numberData<-centralImputation(numberData)
totalNumberData<-cbind(dateData,numberData)
criteriaValue<-predict(priceByNumber, newdata=totalNumberData)
firstPrice<-ifelse(criteriaValue<=which,criteriaValue,0)
firstValues<-predict(glmodel2, newdata=factorData)
firstValues<-ifelse(firstValues>=0.5,higher,lower)
firstPrice<-ifelse(firstPrice!=0,firstPrice+firstValues,0)
secondPrice<-ifelse(criteriaValue>which,criteriaValue,0)
secondValues<-predict(glmodel2, newdata=factorData)
secondValues<-ifelse(secondValues>=0.5,extraHigher,extraLower)
secondPrice<-ifelse(secondPrice!=0,secondPrice+secondValues,0)
predictedPrice<-firstPrice+secondPrice
predictedPrice<-data.frame(id=c(1461:2919),predictedPrice=predictedPrice)
write.csv(predictedPrice,"./predictedPrice.csv",row.names=F)

# Square root of MSE
myMSE<-function(pred,price){
  a<-0
  for( j in 1:length(pred)){
    a = a + (pred[j]-price[j])^2
  }
  a/length(pred)
}

sqrt(myMSE(real$SalePrice,predictedPrice$predictedPrice))




