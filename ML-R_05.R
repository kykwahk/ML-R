
########################################
## R을 이용한 머신러닝과 텍스트마이닝 ##
##       (곽기영, 도서출판 청람)      ## 
########################################

##########################
## 제5장 서포트벡터머신 ##
##########################

## 사례: 꽃 종류

str(iris)
iris.sub <- subset(iris, select=c("Sepal.Length", "Sepal.Width", "Species"),
                   subset=Species %in% c("setosa", "virginica"))
iris.sub$Species <- factor(iris.sub$Species)
head(iris.sub)

# [그림 5-6]
windows(width=7.0, height=5.5)
library(ggplot2)
ggplot(iris.sub, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species), size=2)

library(e1071)
set.seed(123)
iris.svm <- svm(Species ~ ., data=iris.sub, kernel="linear", cost=1, scale=FALSE)

summary(iris.svm)

iris.svm$index
iris.sub[iris.svm$index, ]

iris.svm$SV

# [그림 5-7]
windows(width=7.0, height=5.5)
ggplot(iris.sub, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species), size=2) + 
  geom_point(data=iris.sub[iris.svm$index, c(1, 2)], 
             color="darkblue", shape=21, stroke=1.0, size=5)

plot(x=iris.svm, data=iris.sub)

w <- t(iris.svm$coefs) %*% iris.svm$SV
w
b <- -iris.svm$rho
b

coef(iris.svm)

# [그림 5-8]
windows(width=7.0, height=5.5)
ggplot(iris.sub, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species), size=2) + 
  geom_point(data=iris.sub[iris.svm$index, c(1, 2)], 
             color="darkblue", shape=21, stroke=1.0, size=5) + 
  geom_abline(intercept=-b/w[1, 2], slope=-(w[1, 1]/w[1, 2]), 
              color="dimgray", lty="dashed", lwd=1)

iris.svm.pred <- predict(iris.svm, newdata=iris.sub)
head(iris.svm.pred)
table(iris.sub$Species, iris.svm.pred, dnn=c("Actual", "Predicted"))
mean(iris.sub$Species==iris.svm.pred)

set.seed(123)
iris.svm2 <- svm(Species ~ ., data=iris.sub, kernel="linear", cost=100, scale=FALSE)
summary(iris.svm2)

# [그림 5-9]
windows(width=7.0, height=5.5)
w <- t(iris.svm2$coefs) %*% iris.svm2$SV
b <- -iris.svm2$rho
ggplot(iris.sub, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(aes(color=Species, shape=Species), size=2) +
  geom_point(data=iris.sub[iris.svm2$index, c(1, 2)], 
             color="darkblue", shape=21, stroke=1.0, size=5) +
  geom_abline(intercept=-b/w[1, 2], slope=-(w[1, 1]/w[1, 2]), 
              color="dimgray", lty="dashed", lwd=1)
iris.svm2.pred <- predict(iris.svm2, newdata=iris.sub)
table(iris.sub$Species, iris.svm2.pred, dnn=c("Actual", "Predicted"))
mean(iris.sub$Species==iris.svm2.pred)

## 사례: 배우자 외도

library(AER)
data(Affairs)
str(Affairs)

aff <- Affairs
aff$affairs <- factor(ifelse(aff$affairs > 0, 1, 0), 
                      levels=c(0, 1), labels=c("No", "Yes"))
str(aff)
table(aff$affairs)
prop.table(table(aff$affairs))

set.seed(123)
train <- sample(nrow(aff), 0.7*nrow(aff))
aff.train <- aff[train,]
aff.test <- aff[-train,]
table(aff.train$affairs)
table(aff.test$affairs)

library(e1071)
set.seed(123)
aff.svm <- svm(affairs ~ ., data=aff.train)
summary(aff.svm)

# [그림 5-10]
windows(width=7.0, height=5.5)
library(cluster)
library(ggplot2)
aff.mds <- data.frame(cmdscale(daisy(aff.train[,-1])))
ggplot(aff.mds, aes(x=X1, y=X2)) +
  geom_point(aes(color=aff.train[,1], shape=aff.train[,1]), size=2) +
  geom_point(data=aff.mds[aff.svm$index, ], 
             color="dimgray", shape=21, stroke=1.0, size=5) +
  labs(color="Extramarital\nAffairs", shape="Extramarital\nAffairs", 
       x="Dimension 1", y="Dimension 2",
       title="SVM Classification from Affairs Dataset") +
  theme(plot.title=element_text(face="bold"))

aff.svm.pred <- predict(aff.svm, newdata=aff.test)
head(aff.svm.pred)
table(aff.test$affairs, aff.svm.pred, dnn=c("Actual", "Predicted"))
mean(aff.test$affairs==aff.svm.pred)

set.seed(123)
aff.svm2 <- svm(affairs ~ ., data=aff.train, probability=TRUE)

aff.svm2.pred <- predict(aff.svm2, newdata=aff.test, probability=TRUE)
str(aff.svm2.pred)
attr(aff.svm2.pred, "probabilities")[1:6,]

set.seed(123)
aff.svm.tuned <- tune.svm(affairs ~ ., data=aff.train, gamma=10^(-3:3), cost=2^(-5:5))
summary(aff.svm.tuned)
aff.svm.tuned$best.parameters

set.seed(123)
aff.svm <- svm(affairs ~ ., data=aff.train, gamma=0.001, cost=0.03125)
aff.svm.pred <- predict(aff.svm, aff.test)
table(aff.test$affairs, aff.svm.pred, dnn=c("Actual", "Predicted"))
mean(aff.test$affairs==aff.svm.pred)
