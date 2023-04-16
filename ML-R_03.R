
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

########################
## 제3장 의사결정나무 ##
########################

## 사례: 유방암

library(mlbench)
data(BreastCancer)
str(BreastCancer)
table(BreastCancer$Class)
mean(BreastCancer$Class=="benign")
mean(BreastCancer$Class=="malignant")
sum(!complete.cases(BreastCancer))

bc <- BreastCancer[-1]
bc <- cbind(lapply(bc[-10], function(x) as.numeric(x)), bc[10])
str(bc)
set.seed(567)
train <- sample(nrow(bc), 0.7*nrow(bc))
bc.train <- bc[train,]
bc.test <- bc[-train,]
table(bc.train$Class)
table(bc.test$Class)

library(rpart)
bc.dtree <- rpart(formula=Class ~ ., data=bc.train, method="class", 
                  parms=list(split="information"))

bc.dtree

# [그림 3-6]
windows(width=7.0, height=5.5)
library(rpart.plot)
prp(bc.dtree, type=2, extra=104, fallen.leaves=TRUE, roundint=FALSE,
    main="Decision Tree from Wisconsin Breast Cancer Dataset")

bc.dtree.pred <- predict(bc.dtree, newdata=bc.test, type="prob")
head(bc.dtree.pred)
bc.dtree.pred <- predict(bc.dtree, newdata=bc.test, type="class")
head(bc.dtree.pred)

table(bc.test$Class, bc.dtree.pred, dnn=c("Actual", "Predicted"))
mean(bc.test$Class==bc.dtree.pred)

bc.dtree$cptable
printcp(bc.dtree)

# [그림 3-7]
windows(width=7.0, height=5.5)
plotcp(bc.dtree)

bc.dtree.pruned <- rpart(Class ~ ., data=bc.train, method="class", cp=0.020115,
                         parms=list(split="information"))
bc.dtree.pruned <- prune(bc.dtree, cp=0.020115)
bc.dtree.pruned
bc.dtree.pruned$cptable

# [그림 3-8]
windows(width=7.0, height=5.5)
cols <- ifelse(bc.dtree.pruned$frame$yval==1, "green4", "darkred")
prp(bc.dtree.pruned, type=2, extra=104, fallen.leaves=TRUE, roundint=FALSE,
    branch.lty=3, col=cols, border.col=cols, shadow.col="gray",
    split.cex=1.2, split.suffix="?",
    split.box.col="lightgray", split.border.col="darkgray", split.round=0.5,
    main="Pruned Decision Tree from Wisconsin Breast Cancer Dataset")

# [그림 3-9]
windows(width=7.0, height=5.5)
library(rattle)
fancyRpartPlot(bc.dtree.pruned, sub=NULL,
               main="Pruned Decision Tree from Wisconsin Breast Cancer Dataset")

bc.dtree.pred <- predict(bc.dtree.pruned, newdata=bc.test, type="class")
table(bc.test$Class, bc.dtree.pred, dnn=c("Actual", "Predicted"))
mean(bc.test$Class==bc.dtree.pred)
