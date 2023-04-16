
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

########################
## 제4장 랜덤포레스트 ##
########################

## 사례: 유방암

library(mlbench)
data(BreastCancer)
bc <- BreastCancer[-1]
bc <- cbind(lapply(bc[-10], function(x) as.numeric(as.character(x))), bc[10])
set.seed(567)
train <- sample(nrow(bc), 0.7*nrow(bc))
bc.train <- bc[train,]
bc.test <- bc[-train,]
table(bc.train$Class)
table(bc.test$Class)

library(randomForest)
set.seed(123)
bc.forest <- randomForest(Class ~ ., data=bc.train,
                          na.action=na.roughfix, importance=TRUE)
bc.forest

bc.forest.pred <- predict(bc.forest, newdata=bc.test, type="prob")
head(bc.forest.pred)
bc.forest.pred <- predict(bc.forest, newdata=bc.test, type="response")
head(bc.forest.pred)
table(bc.test$Class, bc.forest.pred, dnn=c("Actual", "Predicted"))
mean(bc.test$Class==bc.forest.pred, na.rm=TRUE)

# [그림 4-1]
windows(width=7.0, height=5.5)
library(cluster)
clusplot(x=na.omit(bc.test[, -10]), clus=na.omit(bc.forest.pred), 
         color=TRUE, shade=TRUE, labels=4, lines=0, 
         main="Random Forest Classification from Breast Cancer Dataset")

bc.forest.predAll <- predict(bc.forest, newdata=bc.test, predict.all=TRUE)
str(bc.forest.predAll)

bc.forest.predAll$individual[6,]

table(bc.forest.predAll$individual[6, ])
na.omit(bc.forest.predAll$aggregate)[6]

apply(bc.forest.predAll$individual[21:25, ], 1, table)
na.omit(bc.forest.predAll$aggregate)[21:25]

# [그림 4-2]
windows(width=7.0, height=5.5)
varImpPlot(bc.forest, pch=21, color="black", bg="red", pt.cex=1.2, 
           main="Variable Importance for Breast Cancer Classification")
importance(bc.forest)

importance(bc.forest, type=1)
importance(bc.forest, type=2)

# [그림 4-3]
windows(width=7.0, height=5.5)
library(gplots)
library(RColorBrewer)
heatmap.2(t(importance(bc.forest)[, 1:2]),
          col=brewer.pal(9, "Blues"),
          dend="none", trace="none", key=FALSE,
          margins=c(10, 7), cexRow=1.5, cexCol=1.2, colRow=c("green4", "maroon"),
          main="Variable Importance\nfor Breast Cancer Classification")
