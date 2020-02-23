#Artificial Neuron Network

colnames(iris) <- tolower(colnames(iris))
install.packages("nnet")
library(nnet)
data <- iris
Scale <- data.frame(lapply(data[,1:4], function(x) scale(x)))

Scale$species <- data$species
index <- sample(1:nrow(Scale), round(0.75 * nrow(Scale)), replace = FALSE)

train <- Scale[index,]
test <- Scale[-index,]
model <- nnet(species ~., size = 2, decay = 5e-04, data = train)
summary(model)

predict.model <- predict(model, test[,1:4], type = "class")
predict.model

actual <- test$species
confusion.matrix <- table(actual, predict.model)
confusion.matrix
sum(predict.model==actual)/NROW(predict.model)
