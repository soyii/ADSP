set.seed(1)
d <- data.frame (year=rep(2012:2014, each=6), count=round(runif(9,0,20)))
d

#ply
install.packages("ply")
library(plyr)
ddply(d, "year", function(x) {
  mean.count = mean(x$count)
  sd.count = sd(x$count)
  cv = sd.count/mean.count
  data.frame(cv.count = cv)
})

ddply(d,.(year), xummarise, mean.count=mean(count))
ddply(d,.(year), transform, total.count=sum(count))

#Data table
install.packages("data.table")
library(data.table)
DT <- data.table(x=c("b", "b", "b", "a", "a"), v=rnorm(5))
DT

str(cars)
CARS <- as.data.table(cars)
CARS
tables()

#difference between data table and data frame
sapply(CARS, class)
setkey(DT, x)
DT
tables()
DT["b"]
DT['b', mult="first"]
DT['a', mult="last"]

data(iris)
head(iris)
str(iris)
summary(iris)
cov(iris[,1:4])
cor(iris[,1:4])

y <- c(1,2,3,NA) 
is.na(y)

data(iris)
iris[iris$Petal.Width==0.2, "Petal.Width"] <- NA
is.na(iris$Sepal.Width)

x <- c(1,2,NA,3)
mean(x)
mean(x,na.rm=T)

install.packages("reshape")
library(reshape)
data(french_fries)
french_fries[!complete.cases(french_fries),]

install.packages("Amelia")
library(Amelia)
data(freetrade)
head(freetrade)

a.out <- amelia(freetrade, m=5, ts="year", cs="country")
hist(a.out$imputations[[3]]$tariff, col="pink", border="orange")
missmap(a.out)  #before remove NA

freetrade$tariff <- a.out$imputations[[5]]$tariff
missmap(freetrade)

install.packages("outliers")
library(outliers)
set.seed(1234)

y = rnorm(100)
outlier(y)
outlier(y, opposite=T)

dim(y) <- c(20,5)
outlier(y)
outlier(y, opposite=T)
boxplot(y)

data(iris)

which(iris$Sepal.Length==5.1)   #find locations
iris$Sepal.Length[which(iris$Sepal.Length==5.1)]   #find values

names(iris) <- tolower(names(iris))
attach(iris)
aggregate(sepal.length~species,iris,mean)

sort(iris$sepal.length)
iris$sepal.length

x <- c(1,5,3,9,7) 
order(x)
order(-x)

library(reshape2)
data('airquality')
names(airquality) <- tolower(names(airquality))
head(airquality)
aql <- melt(airquality, id.vars = c("month", "day"))
head(aql)

dcast(aql, month ~ variable)
dcast(aql, month ~ variable, mean, na.rm = T)

iris_na <- iris
iris_na[c(10,20,30), 3] <- NA
iris_na[!complete.cases(iris_na),]
iris_na[complete.cases(iris_na),]
na.omit(iris_na)
