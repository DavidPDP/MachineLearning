install.packages("dummies")
library(dummies)

data <- read.csv(file.choose())
head(data)
data <- data[,-1]

plot(data$log_PIB_USA,data$log_PIB_Col)

data$D <- as.numeric(data$year>=1991)
head(data)
tail(data)
data[29:36,]

res1 <- lm(log_PIB_Col~D*log_PIB_USA+D+log_PIB_USA, data)
summary(res1)

res2 <- lm(log_PIB_Col~log_PIB_USA, data)

anova(res2,res1)

data$D2 <- as.numeric((data$year>=1991 & data$year<=2000))
head(data)
tail(data)
data[30:42,]

res3 <- lm(log_PIB_Col~D2*log_PIB_USA+D2+log_PIB_USA, data)
summary(res3)

anova(res2,res3)

weekday <- c("monday","tuesday","wednesday","thursday","friday")
dummy(weekday)