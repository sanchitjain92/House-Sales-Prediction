# House-Sales-Prediction
setwd("C:/Users/sanch/Downloads/Kaggle/House Price Kaggle")
housePrices.train = read.csv(file= "train.csv", stringsAsFactors = FALSE, header = TRUE)
housePrices.test = read.csv(file= "test.csv", stringsAsFactors = FALSE, header = TRUE)


housePrices.test$SalePrice = NA

housePrices.train$IsTrainSet = TRUE
housePrices.test$IsTrainSet = FALSE

housePrices.full = rbind(housePrices.train,housePrices.test)

#Clean missing values

mean(housePrices.full$LotFrontage, na.rm=TRUE)

garageArea.median = median(housePrices.full$GarageArea, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$GarageArea),"GarageArea"] = garageArea.median

garageCars.median = median(housePrices.full$GarageCars, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$GarageCars),"GarageCars"] = garageCars.medians

garageYrBlt.median = median(housePrices.full$GarageYrBlt, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$GarageYrBlt),"GarageYrBlt"] = garageYrBlt.median

BsmtFullBath.median = median(housePrices.full$BsmtFullBath, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$BsmtFullBath),"BsmtFullBath"] = BsmtFullBath.median

BsmtHalfBath.median = median(housePrices.full$BsmtHalfBath, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$BsmtHalfBath),"BsmtHalfBath"] = BsmtHalfBath.median

TotalBsmtSF.median = median(housePrices.full$TotalBsmtSF, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$TotalBsmtSF),"TotalBsmtSF"] = TotalBsmtSF.median

BsmtUnfSF.median = median(housePrices.full$BsmtUnfSF, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$BsmtUnfSF),"BsmtUnfSF"] = BsmtUnfSF.median

BsmtFinSF1.median = median(housePrices.full$BsmtFinSF1, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$BsmtFinSF1),"BsmtFinSF1"] = BsmtFinSF1.median


BsmtFinSF2.median = median(housePrices.full$BsmtFinSF2, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$BsmtFinSF2),"BsmtFinSF2"] = BsmtFinSF2.median


BsmtFinSF1.median = median(housePrices.full$BsmtFinSF1, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$BsmtFinSF1),"BsmtFinSF1"] = BsmtFinSF1.median


MasVnrArea.median = median(housePrices.full$MasVnrArea, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$MasVnrArea),"MasVnrArea"] = MasVnrArea.median


LotFrontage.median = median(housePrices.full$LotFrontage, na.rm = TRUE)
housePrices.full[is.na(housePrices.full$LotFrontage),"LotFrontage"] = LotFrontage.median


#Replacing NA values with None for categorical variables

colSums(is.na(housePrices.full))

for (x in c("Alley", "PoolQC", "MiscFeature", "Fence", "FireplaceQu", "GarageType", 
            "GarageFinish", "GarageQual", "GarageCond", "BsmtQual", "BsmtCond", 
            "BsmtExposure", "BsmtFinType1", "BsmtFinType2", "MasVnrType", "Electrical")) {
  housePrices.full[is.na(housePrices.full[, x]), x] = "None"
}


#Factoring
summary(housePrices.full)

housePrices.full$SaleCondition = as.factor(housePrices.full$SaleCondition)
housePrices.full$SaleCondition = as.factor(housePrices.full$SaleCondition)
housePrices.full$SaleType =  as.factor(housePrices.full$SaleType)
housePrices.full$MiscFeature =  as.factor(housePrices.full$MiscFeature)
housePrices.full$Fence = as.factor(housePrices.full$Fence)
housePrices.full$PoolQC =  as.factor(housePrices.full$PoolQC)
housePrices.full$PavedDrive = as.factor(housePrices.full$PavedDrive)
housePrices.full$GarageCond = as.factor(housePrices.full$GarageCond)
housePrices.full$GarageQual =  factor(housePrices.full$GarageQual,levels = c('Ex','Gd','TA','Fa','Po','None'), labels = c(1,2,3,4,5,6))
housePrices.full$GarageFinish =  as.factor(housePrices.full$GarageFinish)
housePrices.full$GarageType =  as.factor(housePrices.full$GarageType)
housePrices.full$FireplaceQu = as.factor(housePrices.full$FireplaceQu)
housePrices.full$Functional = as.factor(housePrices.full$Functional)
housePrices.full$KitchenQual = factor(housePrices.full$KitchenQual,levels = c('Ex','Gd','TA','Fa'), labels = c(1,2,3,4))
housePrices.full$Electrical = as.factor(housePrices.full$Electrical)
housePrices.full$CentralAir = as.factor(housePrices.full$CentralAir)
housePrices.full$HeatingQC =  as.factor(housePrices.full$HeatingQC)
housePrices.full$Heating =  as.factor(housePrices.full$Heating)
housePrices.full$BsmtFinSF2 =  as.factor(housePrices.full$BsmtFinType2)
housePrices.full$BsmtFinSF1 =  as.factor(housePrices.full$BsmtFinType2)
housePrices.full$BsmtExposure =  as.factor(housePrices.full$BsmtExposure)
housePrices.full$BsmtCond =  as.factor(housePrices.full$BsmtCond)
housePrices.full$BsmtQual =  factor(housePrices.full$BsmtQual, levels = c('Ex','Gd','TA','Fa','None'),labels = c(1,2,3,4,5))
housePrices.full$Foundation =  as.factor(housePrices.full$Foundation)
housePrices.full$ExterCond =  as.factor(housePrices.full$ExterCond)
housePrices.full$ExterQual =  factor(housePrices.full$ExterQual,levels = c('Ex','Gd','TA','Fa'), labels = c(1,2,3,4))
housePrices.full$MasVnrType =  as.factor(housePrices.full$MasVnrType)
housePrices.full$Exterior2nd =  as.factor(housePrices.full$Exterior2nd)
housePrices.full$Exterior1st =  as.factor(housePrices.full$Exterior1st)
housePrices.full$RoofMatl =  as.factor(housePrices.full$RoofMatl)
housePrices.full$RoofStyle =  as.factor(housePrices.full$RoofStyle)
housePrices.full$HouseStyle =  as.factor(housePrices.full$HouseStyle)
housePrices.full$BldgType =  as.factor(housePrices.full$BldgType)
housePrices.full$Condition1 =  as.factor(housePrices.full$Condition1)
housePrices.full$Condition2 =  as.factor(housePrices.full$Condition2)
housePrices.full$Neighborhood =  as.factor(housePrices.full$Neighborhood)
housePrices.full$LandSlope =  as.factor(housePrices.full$LandSlope)
housePrices.full$LotConfig =  as.factor(housePrices.full$LotConfig)
housePrices.full$Utilities =  as.factor(housePrices.full$Utilities)
housePrices.full$LandContour =  as.factor(housePrices.full$LandContour)
housePrices.full$LotShape =  as.factor(housePrices.full$LotShape)
housePrices.full$Alley =  as.factor(housePrices.full$Alley)
housePrices.full$Street =  as.factor(housePrices.full$Street)
housePrices.full$MSZoning =  as.factor(housePrices.full$MSZoning)
housePrices.full$BsmtFinType1 =  as.factor(housePrices.full$BsmtFinType1)
housePrices.full$BsmtFinType2 =  as.factor(housePrices.full$BsmtFinType2)

#Identifying Outliers in our data
boxplot(housePrices.train$SalePrice)

boxplot.stats(housePrices.train$SalePrice)

upper.whisker = boxplot.stats(housePrices.train$SalePrice)$stats[5]

outlier.filter = housePrices.train$SalePrice < upper.whisker
housePrices.train[outlier.filter,]


#Examining relationship between 2 categorical variables
joint = CrossTable(housePrices.full$MiscFeature,housePrices.full$BldgType, prop.chisq = FALSE)
barplot(joint$t, beside = TRUE, col = rainbow(5))
legend('topright',c('Gar2','None','Othr','Shed',"TenC"), pch = 15, col = rainbow(5))

#In order to change this graph to stacked graph, change the beside value to FALSE

#Split data into train and test

library(caTools)

housePrices.train = housePrices.full[housePrices.full$IsTrainSet == TRUE,]
housePrices.test = housePrices.full[housePrices.full$IsTrainSet == FALSE,]

library(ggplot2)
library(glmnet)

x = model.matrix(housePrices.train$SalePrice ~ ., data = housePrices.train)
CV =cv.glmnet(x, y=(housePrices.train$SalePrice), alpha=1)
plot(CV)

fit = glmnet(x, y=(housePrices.train$SalePrice), alpha=1, lambda = CV$lambda.min)
fit$beta[,1]
as.data.frame(as.matrix(fit$beta)) -> a
View(a)

coef(fit)
a = which(coef(fit)!= 0)

attach(housePrices.train)

#K- Cross Fold Validation

library(caret)
folds = createFolds(housePrices.train$SalePrice, k=10)
cv = lapply(folds,function(x){
  training_fold = housePrices.train[-x,]
  test_fold = housePrices.train[x,]
  reg.cv = lm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+LotConfig+Neighborhood+Condition1+Condition2+BldgType+OverallCond+OverallQual+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrArea+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinType1+TotalBsmtSF+Heating+HeatingQC+LowQualFinSF+GrLivArea+BsmtFullBath+FullBath+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+Fireplaces+GarageType+GarageYrBlt+GarageFinish+GarageCars+GarageArea+GarageQual+WoodDeckSF+ScreenPorch+PoolArea+PoolQC+SaleType,
              data = training_fold)
  y_pred.cv = predict(reg.cv, newdata = test_fold)
  
})

install.packages("boot")
library(boot)
cv.error.k = rep(1,10)
for (i in 1:10) {
  glm.fit = glm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+LotConfig+Neighborhood+Condition1+Condition2+BldgType+OverallCond+OverallQual+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrArea+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinType1+TotalBsmtSF+Heating+HeatingQC+LowQualFinSF+GrLivArea+BsmtFullBath+FullBath+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+Fireplaces+GarageType+GarageYrBlt+GarageFinish+GarageCars+GarageArea+GarageQual+WoodDeckSF+ScreenPorch+PoolArea+PoolQC+SaleType,
                i,data = housePrices.train)
  cv.error.k[i] = cv.glm(housePrices.train, glm.fit,K=10)$delta[1]
}


# Variables selected from LASSO

reg1 = lm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+LotConfig+Neighborhood+Condition1+Condition2+BldgType+OverallCond+OverallQual+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrArea+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinType1+TotalBsmtSF+Heating+HeatingQC+LowQualFinSF+GrLivArea+BsmtFullBath+FullBath+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+Fireplaces+GarageType+GarageYrBlt+GarageFinish+GarageCars+GarageArea+GarageQual+WoodDeckSF+ScreenPorch+PoolArea+PoolQC+SaleType,
          data = housePrices.train)
summary(reg1)

reg.bw = step(lm(SalePrice ~ MSSubClass+MSZoning+LotArea+LotShape+LandContour+LotConfig+Neighborhood+Condition1+Condition2+BldgType+OverallCond+OverallQual+YearBuilt+YearRemodAdd+RoofStyle+RoofMatl+Exterior1st+Exterior2nd+MasVnrArea+ExterQual+Foundation+BsmtQual+BsmtExposure+BsmtFinType1+TotalBsmtSF+Heating+HeatingQC+LowQualFinSF+GrLivArea+BsmtFullBath+FullBath+KitchenAbvGr+KitchenQual+TotRmsAbvGrd+Functional+Fireplaces+GarageType+GarageYrBlt+GarageFinish+GarageCars+GarageArea+GarageQual+WoodDeckSF+ScreenPorch+PoolArea+PoolQC+SaleType,
                 data = housePrices.train, direction = "backward"))
summary(reg.bw)

y_pred1 = predict(reg1, newdata = housePrices.test[,-81])
summary(y_pred1)

y_pred.bw = predict(reg.bw, newdata = housePrices.test[,-81])
summary(y_pred.bw)


output.df = data.frame("Id" = housePrices.test$Id,"SalePrice" = housePrices.test$SalePrice)
housePrices.test$newSales = y_pred1

output.df.bw = data.frame("Id" = housePrices.test$Id,"SalePrice" = y_pred.bw)





boxplot(housePrices.test$newSales)
boxplot(housePrices.train$SalePrice)


write.csv(output.df, file = "kaggle_submission_housePrice.csv", row.names = FALSE)


#Decision Tree

library(rpart)
reg.decisionTree = rpart(formula = SalePrice ~ .,
                         data = housePrices.train,
                         control = rpart.control(minsplit = 10))
summary(reg.decisionTree)
y_pred.decisionTree = predict(reg.decisionTree, newdata = housePrices.test)
summary(y_pred.decisionTree)

housePrices.test$SalePrice = y_pred.decisionTree

output.df.decisionTree = data.frame("Id" = housePrices.test$Id,"SalePrice" = housePrices.test$SalePrice)
View(output.df.decisionTree)


housePrices.full[is.na(housePrices.full$GarageArea),"GarageArea"] = garageArea.median

output.df.bw[is.na(output.df.bw$SalePrice),"SalePrice"] = median(output.df.bw$SalePrice, na.rm = TRUE)
write.csv(output.df.bw, file = "kaggle_submission_housePrice_BWUpdated.csv", row.names = FALSE)


write.csv(output.df.decisionTree, file = "kaggle_submission_housePrice_DT.csv", row.names = FALSE)
