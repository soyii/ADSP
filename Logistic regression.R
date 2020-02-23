x <- c(27.2, 27.7, 28.3, 28.4, 29.9)
male <- c(2, 17, 26, 19, 27)
female <- c(25, 7, 4, 8, 1)
total = male + female
pmale <- male / total

#Linear regression

z <- lm(pmale ~ x)
summary(z)
p <- coefficients(z)[1] + coefficients(z)[2] * x
p

#Logit transformation

logit <- log(pmale/(1-pmale))
z1 <- lm(logit ~ x)
summary(z1)

logit2 <- coefficients(z1)[1] + coefficients(z1)[2] * x
rmalehat <- exp(logit2) / (1+exp(logit2))
rmalehat

#Maximum likelihood estimation

logit <- glm(pmale ~ x, family = "binomial", weights = total)
summary(logit)

exp(-61.3183) * exp(2.2111 * 27) #0.2
exp(-61.3183) * exp(2.2111 * 28) #1.8
exp(2.211) #9.125
#0.2 * 9.125 = 1.825

#Logistic regression

data(iris)
colnames(iris) <- tolower(colnames(iris))
a <- subset(iris, species == "setosa"|species == "versicolor")
a$species <- factor(a$species)

b <- glm(species ~ sepal.length, data = a, family = binomial)
summary(b)

coef(b)
exp(coef(b))["sepal.length"] #170

fitted(b)[c(1:3, 98:100)]
predict(b, newdata = a[c(1, 50, 51, 100),], type = "response")

cdplot(species ~ sepal.length, data = a)

#Multinominal logistic regression

str(mtcars)
glm.vs <- glm(vs ~ mpg+am, data = mtcars, family = "binomial")
summary(glm.vs)

#Anova

anova(glm.vs, test = "Chisq")

#Mcfadden R2
install.packages("pscl")
library(pscl)
pR2(glm.vs)