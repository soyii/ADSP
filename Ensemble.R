#Decision tree

colnames(iris) <- tolower(colnames(iris))
install.packages(rpart)
library(rpart)
k <- rpart(species ~., data = iris)
k

plot(k, compress = T, margin = .3)
text(k, cex = 1.0)

install.packages("rpart.plot")
library(rpart.plot)
prp(k, type = 4, extra = 2, digits = 3)

head(predict(k, newdata = iris, type = "class"))
printcp(k)
plotcp(k)

install.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)

rpartpred <- predict(k, iris, type = "class")
confusionMatrix(rpartpred, iris$species)


#Ensemble
#Bagging

install.packages("adabag")
library(adabag)
data(iris)
set.seed(1)

train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.bagging <- bagging(Species ~., data = iris[train, ], mfinal = 10, control = rpart.control(maxdepth = 1))

iris.bagging           
iris.bagging$importance
barplot(iris.bagging$importance[order(iris.bagging$importance, decreasing = TRUE)], ylim = c(0, 100), main = "Variables Relative Importance", col = "red")

table(iris.bagging$class, iris$Species[train], dnn = c("predicted Class", "observed Class"))
1-sum(iris.bagging$class == iris$Species[train]) /length(iris$Species[train])
iris.predbagging <- predict.bagging(iris.bagging, newdata = iris[train, ])
iris.predbagging


#Boosting

set.seed(1)
train <- c(sample(1:50, 25), sample(51:100, 25), sample(101:150, 25))
iris.adaboost <- boosting(Species ~., data = iris[train,], mfinal = 10, control = rpart.control(maxdepth = 1))
iris.adaboost

barplot(iris.adaboost$importance[order(iris.adaboost$importance, decreasing = TRUE)], ylim = c(0, 100), main = "Variables Relative Importance", col = "red")
table(iris.adaboost$class, iris$Species[train], dnn = c("predicted Class", "observed Class"))


#Random Forest

install.packages("randomForest")
library(randomForest)
data(stagec)

stagec3 <- stagec[complete.cases(stagec),]
set.seed(123)
ind <- sample(2, nrow(stagec3), replace = TRUE, prob = c(0.7, 0.3))

train <- stagec3[ind==1,]
test <- stagec3[ind==2,]
rf <- randomForest(ploidy ~., data=train, ntree=100, proximitiy=T, importance=T)
table(predict(rf), train$ploidy)
print(rf)

varImpPlot(rf)


