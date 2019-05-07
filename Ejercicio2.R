data <- read.csv(file.choose())
head(data)

colnames(data) <- c("Anio", "CE", "CD", "I", "LDies", "LEI", "V")
data <- data[,-1]

R1 <- lm(formula = I~., data=data)
summary(R1)

#Interpretación de los coeficientes
#CE: Por cada incremento de un millon de kilovatios/hora en el consumo de electricidad
#se incrementa en 9.334e+04 millones de dólares los ingresos del sector
#CD: Por cada incremento de un millon de galones en el consumo de diesel 
#se incrementa en 8.053e+01 millones de dólares los ingresos del sector
#LDies: Por cada incremento de una locomotora diesel en el país
#se disminuye en 4.723e+01 millones de dólares los ingresos del sector
#LEI: Por cada incremento de una locomotora electrica en el país
#se disminuye en 4.808e+01 millones de dólares los ingresos del sector
#V: Por cada incremento de un millar de pasajeros en el año
#se incrementa en 1.743e+02 millones de dólares los ingresos del sector