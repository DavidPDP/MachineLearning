load(file.choose())
library(xts)
library(AER)

head(retornos.diarios)
class(retornos.diarios)
sapply(retornos.diarios, class)

summary(retornos.diarios)
plot(retornos.diarios)

cor(retornos.diarios)

res1 <- lm(GRUPOSURA~.,retornos.diarios)
summary(res1)
anova(res1)

# prueba F
linearHypothesis( res1,c("CONCONCRET = 0", "OCCIDENTE = 0"))
# prueba Wald test="Chisq"
linearHypothesis( res1, test="Chisq",c("CONCONCRET = 0", "OCCIDENTE = 0"))

linearHypothesis( res1,c("ECOPETROL = 0.12", "EXITO = 0.12"))
