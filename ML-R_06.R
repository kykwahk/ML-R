
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

####################
## 제6장 성능평가 ##
####################

## 성능평가 지표

library(mlbench)
data(BreastCancer)
bc <- BreastCancer[-1]
bc <- cbind(lapply(bc[-10], function(x) as.numeric(as.character(x))), bc[10])
set.seed(123)
train <- sample(nrow(bc), 0.7*nrow(bc))
bc.train <- bc[train,]
bc.test <- bc[-train,]
library(C50)
bc.C50 <- C5.0(formula=Class ~ ., data=bc.train)
bc.C50.pred <- predict(bc.C50, newdata=bc.test, type="class")
bc.C50.cmatrix <- table(bc.test$Class, bc.C50.pred, dnn=c("Actual", "Predicted"))
bc.C50.cmatrix
sum(diag(bc.C50.cmatrix))/sum(bc.C50.cmatrix)
mean(bc.test$Class==bc.C50.pred)

library(gmodels)
CrossTable(bc.test$Class, bc.C50.pred, 
           prop.chisq=FALSE, dnn=c("Actual", "Predicted"))

(59+138)/(59+138+12+1)
59/(59+1)
138/(138+12)
59/(59+12)
138/(138+1)

library(caret)
confusionMatrix(bc.C50.pred, bc.test$Class, positive="malignant")

## 민감도와 특이도

sens <- 59/(59+1)
sens
spec <- 138/(138+12)
spec

library(caret)
sensitivity(bc.C50.pred, bc.test$Class, positive="malignant")
specificity(bc.C50.pred, bc.test$Class, negative="benign")

## ROC곡선 

bc.C50 <- C5.0(formula=Class ~ ., data=bc.train)
bc.C50.pred <- predict(bc.C50, newdata=bc.test, type="prob")
head(bc.C50.pred)

# [그림 6-6]
windows(width=7.0, height=5.5)
library(Epi)
ROC(test=bc.C50.pred[,2], stat=bc.test$Class, MI=FALSE, 
    main="ROC Curve of C5.0 Classification from Breast Cancer Dataset")
