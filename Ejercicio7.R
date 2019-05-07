library(car)

data <- read.csv(file.choose())
head(data)

data$sexo <-as.numeric(data$sexo=="hombre")
data$expS <- data$exp^2

model1 <- lm(Lnih~sexo+yedu+exp+expS+sexo*exp+sexo*expS, data) 
summary(model1)

vif(model1)

XTX <-model.matrix(model1)
e <-eigen(t(XTX) %*% XTX)
e$val

lambda.1 <-max(e$val)
lambda.k <-min(e$val)
kappa <-sqrt(lambda.1/lambda.k)
kappa

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