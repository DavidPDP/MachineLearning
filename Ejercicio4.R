data <- read.csv(file.choose())
head(data)
class(data)

str(data)

data <- data[,-1]
head(data,3)

res1 <- lm(y~x1+x2+x3+x6+x7,data)
res2 <- lm(y~x1+x2+x3,data)
res3 <- lm(y~x4+x5+x8+x9+x10,data)

R1 <- summary(res1)
R2 <- summary(res2)
R3 <- summary(res3)

R1$adj.r.squared
R2$adj.r.squared
R3$adj.r.squared

AIC(res1)
AIC(res2)
AIC(res3)

BIC(res1)
BIC(res2)
BIC(res3)

anova(res2,res1)

Jres13 <- jtest(res1,res3)
Jres13

Jres23 <- jtest(res2,res3)
Jres23

Coxres13 <- coxtest(res1,res3)
Coxres13

Coxres23 <- coxtest(res2,res3)
Coxres23
