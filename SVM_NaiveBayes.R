#Support Vector Machine

install.packages("e1071")
library(e1071)

data(iris)
colnames(iris) <- tolower(colnames(iris))
s <- sample(150,100)
col <- c("petal.length", "petal.width", "species")
iris_train <- iris[s,col]
iris_test <- iris[-s,col]

iris_svm <- svm(species ~., data = iris_train, cost = 1, kernel = "linear")
plot(iris_svm, iris_train[,col])

p <- predict(iris_svm, iris_test[,col], type="class")
plot(p)
table(p, iris_test[,3])


#Naive Bayes Classification

data(iris)
colnames(iris) <- tolower(colnames(iris))
m <- naiveBayes(species ~., data = iris)
m
table(predict(m, iris[,-5]), iris[,5], dnn = list('predicted','actual'))
