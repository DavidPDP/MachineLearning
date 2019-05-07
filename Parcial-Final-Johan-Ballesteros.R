#Dependencias
library(ggplot2)
library(leaps)
library(AER)
library(olsrr)
library(sjPlot)
library(lmtest)
library(car)
library(sandwich)
library(tseries)
library(lm.beta)
library(jtools)

#Procedemos a cargar el dataset inicial
dataSkateBoards <- read.csv(file.choose())

#Análisis Exploratorio
dim(dataSkateBoards)
class(dataSkateBoards)
sapply(dataSkateBoards, class)

head(dataSkateBoards)
tail(dataSkateBoards, n=10)

names(dataSkateBoards)
str(dataSkateBoards)
summary(dataSkateBoards)

#Estructuración De Los Datos I
dataSkateBoards$season <- as.factor(dataSkateBoards$season)
dataSkateBoards$yr <- as.factor(dataSkateBoards$yr)
dataSkateBoards$mnth <- as.factor(dataSkateBoards$mnth) 
dataSkateBoards$holiday <- as.factor(dataSkateBoards$holiday) 
dataSkateBoards$weekday <- as.factor(dataSkateBoards$weekday) 
dataSkateBoards$workingday <- as.factor(dataSkateBoards$workingday)
dataSkateBoards$weathersit <- as.factor(dataSkateBoards$weathersit)

dataSkateBoards <- dataSkateBoards[,-1:-2]

#Análisis Descriptivo
##Continuas
ggplot(data = dataSkateBoards) + geom_histogram(mapping = aes(x = temp), bins = 10)
ggplot(data = dataSkateBoards) + geom_histogram(mapping = aes(x = atemp), bins = 10)
ggplot(data = dataSkateBoards) + geom_histogram(mapping = aes(x = hum), bins = 10)
ggplot(data = dataSkateBoards) + geom_histogram(mapping = aes(x = windspeed), bins = 10)
ggplot(data = dataSkateBoards) + geom_histogram(mapping = aes(x = casual), bins = 10)
ggplot(data = dataSkateBoards) + geom_histogram(mapping = aes(x = registered), bins = 10)
ggplot(data = dataSkateBoards) + geom_histogram(mapping = aes(x = cnt), bins = 10)
##Factores
plot(dataSkateBoards$season)
plot(dataSkateBoards$yr)
plot(dataSkateBoards$mnth)
plot(dataSkateBoards$holiday)
plot(dataSkateBoards$weekday)
plot(dataSkateBoards$weathersit)

#ATemp y CNT
ggplot(data = dataSkateBoards) +
  geom_point(mapping = aes(x = atemp, y = cnt, color = season))

ggplot(data = dataSkateBoards) +
  geom_point(mapping = aes(x = atemp, y = cnt, color = season)) +
  geom_smooth(mapping = aes(x = atemp, y = cnt, color = season)) +
  facet_wrap(~ season) +
  theme(legend.position="none")

#windspeed y CNT
ggplot(data = dataSkateBoards) +
  geom_point(mapping = aes(x = windspeed, y = cnt, color = season))

ggplot(data = dataSkateBoards) +
  geom_point(mapping = aes(x = windspeed, y = cnt, color = season)) +
  geom_smooth(mapping = aes(x = windspeed, y = cnt, color = season)) +
  facet_wrap(~ season) +
  theme(legend.position="none")

#casual y CNT
ggplot(data = dataSkateBoards) +
  geom_point(mapping = aes(x = casual, y = cnt, color = season))

ggplot(data = dataSkateBoards) +
  geom_point(mapping = aes(x = casual, y = cnt, color = season)) +
  geom_smooth(mapping = aes(x = casual, y = cnt, color = season)) +
  facet_wrap(~ season) +
  theme(legend.position="none")

#registered y CNT
ggplot(data = dataSkateBoards) +
  geom_point(mapping = aes(x = registered, y = cnt, color = season))

ggplot(data = dataSkateBoards) +
  geom_point(mapping = aes(x = registered, y = cnt, color = season)) +
  geom_smooth(mapping = aes(x = registered, y = cnt, color = season)) +
  facet_wrap(~ season) +
  theme(legend.position="none")

#Estructuración De Los Datos II
## Creación de factores como variables dummies
dataSkateBoards$summer <- as.numeric(dataSkateBoards$season == "2")
dataSkateBoards$fall <- as.numeric(dataSkateBoards$season == "3")
dataSkateBoards$winter <- as.numeric(dataSkateBoards$season == "4")
dataSkateBoards$season <- NULL
dataSkateBoards$mnth <- NULL

dataSkateBoards$yr <- as.numeric(dataSkateBoards$yr == "0")

dataSkateBoards$workingday <- as.numeric(dataSkateBoards$workingday == "0")
dataSkateBoards$holiday <- as.numeric(dataSkateBoards$holiday == "0")
dataSkateBoards$weekday <- NULL

dataSkateBoards$weathersit2 <- as.numeric(dataSkateBoards$weathersit == "2")
dataSkateBoards$weathersit3 <- as.numeric(dataSkateBoards$weathersit == "3")
dataSkateBoards$weathersit <- NULL

dataSkateBoards$casual <- NULL
dataSkateBoards$registered <- NULL

#Formulación De Modelos
## Max Model
model.max <- lm(formula = cnt ~ temp + atemp + hum + windspeed + yr + holiday + workingday +
                  summer + fall + winter + weathersit2 + weathersit3, 
                  data = dataSkateBoards)
##12 variables (sin contar el intercepto)
length(model.max$coefficients)

##Forward
fwd.model <- regsubsets(cnt~temp + atemp + hum + windspeed + yr + holiday + workingday +
                          summer + fall + winter + weathersit2 + weathersit3,
                          data = dataSkateBoards, method = "forward", nvmax = 100000)
best.fwd.model <- summary(fwd.model)

###R^2 ajustado
plot(fwd.model, scale = "adjr2", main = "Rˆ2 ajustado")
best.fwd.model.by.adjr <- which.max(best.fwd.model$adjr2)
vars.fwd.model.by.adjr <- names(coef(fwd.model, best.fwd.model.by.adjr))
formula.best.fwd.by.adjr <- as.formula(paste("cnt ~ 1 +", paste (vars.fwd.model.by.adjr[2:length(vars.fwd.model.by.adjr)], collapse = " + "), sep = ""))
model2 <- lm(formula.best.fwd.by.adjr, data = dataSkateBoards)

###BIC
plot(fwd.model, scale = "bic", main = "BIC criterio")
best.fwd.model.by.bic <- which.min(best.fwd.model$bic)
vars.fwd.model.by.bic <- names(coef(fwd.model, best.fwd.model.by.bic))
formula.best.fwd.by.bic <- as.formula(paste("cnt ~ 1 +", paste (vars.fwd.model.by.bic[2:length(vars.fwd.model.by.bic)], collapse = " + "), sep = ""))
model3 <- lm(formula.best.fwd.by.bic, data = dataSkateBoards)

###AIC
fwd.model.5 <-ols_step_forward_aic(model.max)
fwd.model.5
vars.fwd.model5 <- fwd.model.5$predictors
formula.model5 <- as.formula(
  paste("cnt ~ ",paste(vars.fwd.model5, collapse=" + "),sep=""))
model5 <- lm(formula = formula.model5, data = dataSkateBoards)

##Backward
bwd.model <- regsubsets(cnt~temp + atemp + hum + windspeed + yr + holiday + workingday +
                          summer + fall + winter + weathersit2 + weathersit3, 
                          data = dataSkateBoards, method = "backward", nvmax = 100000)
best.bwd.model <- summary(bwd.model)

###R^2 ajustado
plot(bwd.model, scale = "adjr2", main = "Rˆ2 ajustado")
best.bwd.model.by.adjr <- which.max(best.bwd.model$adjr2)
vars.bwd.model.by.adjr <- names(coef(fwd.model, best.bwd.model.by.adjr))
formula.best.bwd.by.adjr <- as.formula(paste("cnt ~ 1 +", paste (vars.bwd.model.by.adjr[2:length(vars.bwd.model.by.adjr)], collapse = " + "), sep = ""))
model6 <- lm(formula.best.bwd.by.adjr, data = dataSkateBoards)

###BIC
plot(bwd.model, scale = "bic", main = "BIC criterio")
best.bwd.model.by.bic <- which.min(best.bwd.model$bic)
vars.bwd.model.by.bic <- names(coef(bwd.model, best.bwd.model.by.bic))
formula.best.bwd.by.bic <- as.formula(paste("cnt ~ 1 +", paste (vars.bwd.model.by.bic[2:length(vars.bwd.model.by.bic)], collapse = " + "), sep = ""))
model7 <- lm(formula.best.bwd.by.bic, data = dataSkateBoards)

###AIC
fwd.model.9 <-ols_step_backward_aic(model.max)
fwd.model.9
model9 <- lm(formula = cnt ~ temp + hum + windspeed + yr + holiday + workingday +
               summer + fall + winter + weathersit2 + weathersit3, data = dataSkateBoards)

##Backward & Forward
bothwd.model <- regsubsets(cnt~temp + atemp + hum + windspeed + yr + holiday + workingday +
                            summer + fall + winter + weathersit2 + weathersit3, 
                            data = dataSkateBoards, method = "seqrep", nvmax = 100000)
best.bothwd.model <- summary(bothwd.model)

###R^2 ajustado
plot(bothwd.model, scale = "adjr2", main = "Rˆ2 ajustado")
best.bothwd.model.by.adjr <- which.max(best.bothwd.model$adjr2)
vars.bothwd.model.by.adjr <- names(coef(bothwd.model, best.bothwd.model.by.adjr))
formula.best.bothwd.by.adjr <- as.formula(paste("cnt ~ 1 +", paste (vars.bothwd.model.by.adjr[2:length(vars.bothwd.model.by.adjr)], collapse = " + "), sep = ""))
model10 <- lm(formula.best.bothwd.by.adjr, data = dataSkateBoards)

###BIC
plot(bothwd.model, scale = "bic", main = "BIC criterio")
best.bothwd.model.by.bic <- which.min(best.bothwd.model$bic)
vars.bothwd.model.by.bic <- names(coef(bothwd.model, best.bothwd.model.by.bic))
formula.best.bothwd.by.bic <- as.formula(paste("cnt ~ 1 +", paste (vars.bothwd.model.by.bic[2:length(vars.bothwd.model.by.bic)], collapse = " + "), sep = ""))
model11 <- lm(formula.best.bothwd.by.bic, data = dataSkateBoards)

###AIC
bothwd.model.13 <-ols_step_both_aic(model.max)
bothwd.model.13
vars.bothwd.model13 <- bothwd.model.13$predictors
formula.model13 <- as.formula(
  paste("cnt ~ ",paste(vars.bothwd.model13, collapse=" + "),sep=""))
model13 <- lm(formula = formula.model13, data = dataSkateBoards)

##Limpieza De Variables No Significativas
###Model Max
summary(model.max)
linearHypothesis(model.max,test="Chisq",c("atemp = 0", "workingday = 0"))

###Model 2 (Mismo modelo que Model Max)
summary(model2)

##Model 3
summary(model3)
linearHypothesis(model3,test="Chisq",c("atemp = 0"))

#Model 5 (es el mismo que max model)
summary(model5)
linearHypothesis(model5,test="Chisq",c("workingday = 0", "atemp = 0"))

#Model 6 (Es el mismo model 3)
summary(model6)

#Model 7
summary(model7)

#Model 9
summary(model9)
linearHypothesis(model9,test="Chisq",c("workingday = 0"))

#Model 10 (es el mismo model 9)
summary(model10)

#Model 11 (es el mismo model 7)
summary(model11)
length(model11$coefficients)

#Model 13 (es el mismo model max model)
summary(model13)

summary(model9)
##Selección Del Mejor Modelo Basado En Criterios R^2 ajustado, AIC y BIC
AIC(model.max)
AIC(model3)
AIC(model7)
AIC(model9)

### El mejor con AIC es el modelo 9

BIC(model.max)
BIC(model3)
BIC(model7)
BIC(model9)

### El mejor con BIC es el modelo 7

AdjRModelMax <- summary(model.max)
AdjRModel3 <- summary(model3)
AdjRModel7 <- summary(model7)
AdjRModel9 <- summary(model9)

AdjRModelMax$adj.r.squared
AdjRModel3$adj.r.squared
AdjRModel7$adj.r.squared
AdjRModel9$adj.r.squared

#El mejor modelo con R^2 ajustado es el modelo 9
#Por los criterios de AIC, BIC y R^2 ajustado se escoge como mejor modelo el número 9

##Comprobación De Supuestos Para los tres mejores modelos según criterio AIC,BIC y R^2 ajustado
##Multicolinealidad
remueve.VIF.grande <-function(modelo, u){
  require(car)# extrae el dataframe
  data <-  modelo$model# Calcula todos los VIF
  all_vifs <- car::vif(modelo)# extraer el nombre de todas las variables X
  names_all <-names(all_vifs)# extraer el nombre de la variables y
  dep_var <-all.vars(formula(modelo))[1]# Remover lsa variables con  VIF > u
  # y reestimar el modelo con las otras variables
  while(any(all_vifs > u)){
    # elimina variable con max vif
    var_max_vif <-names(which(all_vifs ==max(all_vifs)))# remueve la variable
    names_all <- names_all[!(names_all) %in% var_max_vif]# nueva formula
    myForm <-as.formula(paste(paste(dep_var, " ~ "),
                              paste(names_all, collapse=" + "), sep=""))# re-build model with new formula
    modelo.prueba <-lm(myForm, data= data)
    all_vifs <- car::vif(modelo.prueba)
  }
  modelo.limpio <- modelo.prueba
  return(modelo.limpio)
}

vif(model9) #Problema

XTXModel9 <-model.matrix(model9)
eModel9 <-eigen(t(XTXModel9) %*% XTXModel9)
lambda.Model9 <-max(eModel9$val)
lambda.Model9.k <-min(eModel9$val)
kappaModel9 <-sqrt(lambda.Model9/lambda.Model9.k)
kappaModel9 #Problema

### Eliminar variables con VIF demasiado alto
####Model 9
modelFinal <- remueve.VIF.grande(model9, 4)
summary(modelFinal)
vif(modelFinal)

XTXModelFinal <-model.matrix(modelFinal)
eModelFinal <-eigen(t(XTXModelFinal) %*% XTXModelFinal)
lambda.ModelFinal <-max(eModelFinal$val)
lambda.ModelFinal.k <-min(eModelFinal$val)
kappaModelFinal <-sqrt(lambda.ModelFinal/lambda.ModelFinal.k)
kappaModelFinal #Problema

##Se concluye que por tener un Kappa muy cercano a 20 y unos VIFs muchos menores que 4
##entonces ya no hay multicolinealidad alta

#### Nos damos cuenta que las dummies multiplicando generar multicolinealidad debido
#### a que relaciona tanto la variable dummy como la variable con la que multiplica
#### por esto solo se dejan las variables dummies y las otras variables independientes
#### para evitar la multicolinealidad

summary(modelFinal)
linearHypothesis(modelFinal,test="Chisq",c("workingday = 0"))

###Heteroscedasticidad
ols_plot_resid_qq(modelFinal)
ks.test(resid(modelFinal), "pnorm")
bptest(modelFinal, studentize = TRUE)

attach(dataSkateBoards)
bptest(modelFinal, ~ temp + I(temp^2) + hum +I(hum^2) + 
         windspeed + I(windspeed^2) + yr+ holiday + workingday + summer + fall +
         winter + weathersit2 + weathersit3 + temp*hum + temp*windspeed + 
         temp*yr + temp*holiday + temp*workingday + temp*summer + temp*fall +
         temp*winter + temp*weathersit2 + temp*weathersit3 + hum*windspeed + 
         hum*yr + hum*holiday + hum*workingday + hum*summer + hum*fall + 
         hum*winter + hum*weathersit2 + hum*weathersit3 + windspeed*yr + 
         windspeed*holiday + windspeed*summer + windspeed*fall + windspeed*winter +
         windspeed*weathersit2 + windspeed*weathersit3)

###Concluimos que existe una problema de heteroscedasticidad
coeftest(modelFinal, vcov = (vcovHC(modelFinal)))
modelFinal2 <- lm(cnt ~ temp + hum + windspeed + yr + holiday+ summer + 
                     winter + weathersit2 + weathersit3, data = dataSkateBoards)
waldtest(modelFinal2, modelFinal, vcov =vcovHC(modelFinal))
##Se concluye que el modeloFinal2 es mejor que el modeloFinal por lo cual podemos 
##obviar la variable de workingday
summary(modelFinal2)
##Autocorrelación
eModeloFinal2 <-residuals(modelFinal2)
ts.plot(eModeloFinal2, main= "Errores estimados", xlab= "tiempo",ylab= "errores")

signo.error <-factor(eModeloFinal2>0)
runs.test(signo.error)

runs.test(signo.error, alternative="less")
runs.test(signo.error, alternative="greater")

dwtest(modelFinal2, alternative = "two.sided")
dwtest(modelFinal2, alternative = "greater")
dwtest(modelFinal2, alternative ="less")
##Se concluye que hay autocorrelación positiva

##Correción de autocorrelación
coeftest(modelFinal2, vcov =NeweyWest(modelFinal2))

#Selección De La Variable Que Más Impacta
modelFinal2.s <- lm.beta(modelFinal2)
coeftest(modelFinal2.s, vcov = (vcovHC(modelFinal2)))
modelFinal2.s$standardized.coefficients
summary(modelFinal2.s)
coef(modelFinal2.s)

coef(modelFinal2.s)[which.max(coef(modelFinal2.s))]
coef(modelFinal2.s)[which.max(abs(coef(modelFinal2.s)))]

summary(modelFinal2)

coef.est <- matrix(0,9,2)
var.names  <- c(names(modelFinal2.s$standardized.coefficients)[-1])
coef.est <- matrix(modelFinal2.s$standardized.coefficients[-1])
coef.estand <- as.data.frame(var.names)
coef.estand$coef.est <- coef.est 

p <- ggplot(data=coef.estand, aes(x=var.names, y=coef.est)) +
  geom_bar(stat="identity", fill="steelblue") + 
  ggtitle("Coeficientes estandarizados")
p + coord_flip()

plot_summs(modelFinal2)
plot_summs(modelFinal2, robust = "HC3")
plot_summs(modelFinal2, robust = "HC3", scale = TRUE, transform.response = TRUE)
plot_summs(modelFinal2, robust = "HC3", scale = TRUE, transform.response = TRUE, inner_ci_level = .9)
plot_summs(modelFinal2, robust = "HC3", inner_ci_level = .9, plot.distributions = TRUE, rescale.distributions = TRUE)
