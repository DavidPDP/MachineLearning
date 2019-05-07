library(olsrr)
library(ggplot2)
library(leaps)

data <- read.csv(file.choose(), sep = " ")
head(data)
class(data)

sapply(data, class)

data2 <- data[,c(1:10,26)]
head(data2)

model <- lm(y~.,data2)
summary(model)

models <- ols_step_all_possible(model)
str(models)

plot(models)

models$mindex[which.max(models$predrsq)]
models$n[which.max(models$predrsq)]
models$predictors[which.max(models$predrsq)]

models$mindex[which.min(models$aic)]
models$n[which.min(models$aic)]
models$predictors[which.min(models$aic)]

models$mindex[which.min(models$sbc)]
models$n[which.min(models$sbc)]
models$predictors[which.min(models$sbc)]

modelo1 <- lm(y~x1+x2+x3+x4+x5+x9,data=data2)
modelo2 <- lm(y~x2+x3+x4+x5,data=data2)

summary(modelo1)
summary(modelo2)

anova(modelo2,modelo1)

fwd.model <- regsubsets(x=data[,1:25], y=data[,26], nvmax = 1000, method="forward")
attributes(fwd.model)
plot(fwd.model, scale = "adjr2", main = "R^2 ajustado")

model3 <- lm(y~x1+x2+x3+x4+x5+x8+x9+x10+x17+x19+x20+x21,data=data)
summary(model3)  

max.model <- lm(y~.,data)
fwd.model2 <- ols_step_forward_p(max.model)

model4 <- lm(y~x1+x2+x3+x4+x5+x9+x10+x17+x20, data=data)
summary(model4)

fwd.model3 <- ols_step_forward_aic(max.model)
fwd.model3

formuala.modelo5 <- as.formula(paste("y ~", 
                              paste(fwd.model3$predictors, collapse = " + "),sep=""))
formuala.modelo5
model5 <- lm(formula = formuala.modelo5, data=data)
summary(model5)

back.model <-regsubsets(x = data[,1:25], y = data[,26],nvmax=1000, method = "backward")
plot(back.model, scale = "adjr2", main = "Rˆ2 ajustado")

back.model.2 <-ols_step_backward_p(max.model)
back.model.2

model6 <- lm(y~x1+x2+x3+x4+x5+x8+x9+x10+x17+x19+x20+x21, data = data)
summary(model6)

back.model.3 <-ols_step_backward_aic(max.model)

vars.modelo7 <- back.model.3$predictors
formula.modelo7 <-as.formula(paste("y ~",
                             paste(vars.modelo7, collapse=" + "), sep=""))
formula.modelo7modelo7 <-lm( formula.modelo7 , data = data)

both.model <-regsubsets(x = data[,1:25], y = data[,26], nvmax=1000, method = "seqrep")
plot(both.model, scale = "adjr2", main = "Rˆ2 ajustado")

model8 <- lm(y~x1+x2+x3+x4+x5+x8+x9+x10+x17+x20+x21, data=data)
summary(model8)

both.model.2 <-ols_step_both_p(max.model)
model9 <- lm(y~x2+x5+x4+x3+x20+x1, data=data)
summary(model9)

both.model.3 <-ols_step_both_aic(max.model)
model10 <- lm(y~x2+x5+x4+x3+x20+x1+x9+x10, data=data)

remueve.no.sinifica <-function(modelo, p){
  # extrae el dataframe
  data <-  modelo$model
  # extraer el nombre de todas las variables X
  all_vars <-all.vars(formula(modelo))[-1]
  # extraer el nombre de la variables y
  dep_var <-all.vars(formula(modelo))[1]
  # Extraer las variables no significativas# resumen del modelo
  summ <-summary(modelo)
  # extrae los valores p
  pvals <- summ[[4]][, 4]
  # creando objeto para guardar las vriables no significativas
  not_signif <-character()
  not_signif<-names(which(pvals > p))
  # Si hay alguna variable no-significativa
  while(length(not_signif) > 0){
    all_vars <- all_vars[!all_vars %in% not_signif[1]]
    # nueva formula
    myForm <-as.formula(paste(paste(dep_var, "~ ")
                              ,paste(all_vars, collapse=" + "), sep=""))
    # re-escribe la formula
    modelo <-lm(myForm, data= data)
    # Extrae variables no significativas.
    summ <-summary(modelo)
    pvals <- summ[[4]][, 4]
    not_signif <-character()
    not_signif <-names(which(pvals > p))
    not_signif <- not_signif[!not_signif %in% "(Intercept)"]
  }
  modelo.limpio <- modelo
  return(modelo.limpio)
}

modelo3.a <-remueve.no.sinifica(model3, 0.05)
summary(modelo3.a)

modeloA <- lm(y~x1+x2+x3+x4+x5+x20, data)
modeloB <- lm(y~x2+x3+x4+x5+x20, data)
modeloC <- lm(y~x17+x19+x21, data)  

anova(modeloB,modeloA)

J.resA.C <-jtest(modeloA, modeloC)
J.resA.C