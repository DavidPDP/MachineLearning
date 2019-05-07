data <- read.csv(file.choose())
head(data)
class(data)

summary(data)
str(data)
data$Y <- as.numeric(data$Y)
data$X1 <- as.numeric(data$X1)
data$X2 <- as.numeric(data$X2)

model1 <- lm(Y~X1+X2, data)
summary(model1)

vif(model1)

XTX <-model.matrix(model1)
e <-eigen(t(XTX) %*% XTX)
e$val

lambda.1 <-max(e$val)
lambda.k <-min(e$val)
kappa <-sqrt(lambda.1/lambda.k)
kappa

model2 <- lm(Y~X1, data)
summary(model2)

model3 <- lm(Y~X2, data)
summary(model3)
