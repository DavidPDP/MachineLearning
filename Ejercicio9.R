library(lmtest)
library(olsrr)
library(sandwich)

data <- read.csv(file.choose())
head(data)

res1 <- lm(Lnih~yedu+exp+I(exp^2)+sexo+sexo*exp+sexo*I(exp^2), data)
summary(res1)

e <-resid(res1)

attach(data)
#par(mfrow=c(2,2))
plot(yedu, e)
plot(exp, e)
plot(exp^2, e)
plot(sexo, e)

bptest(res1, studentize = FALSE)

ols_test_breusch_pagan(res1, rhs = TRUE, multiple = TRUE,p.adj = 'bonferroni')
ols_test_normality(res1)

bptest(res1, studentize = TRUE)
bptest(res1, ~ yedu+exp+I(exp^2)+sexo+sexo*yedu+sexo*exp+sexo*I(exp^2)
       +I(yedu^2)+I(exp^4)+I(yedu*exp)+I(yedu*exp^2)+sexo*I(yedu*exp) +
         sexo*I(exp^3)+I(exp^3))

vcovHC(res1)
coeftest(res1, vcov = (vcovHC(res1)))
coeftest(res1, vcov = (vcovHC(res1, "HC0")))
coeftest(res1, vcov = (vcovHC(res1, "HC1")))
coeftest(res1, vcov = (vcovHC(res1, "HC2")))
coeftest(res1, vcov = (vcovHC(res1, "HC4")))

res2<-lm(Lnih~yedu+exp+I(exp^2) , data)
waldtest(res2, res1, vcov =vcovHC( res1))