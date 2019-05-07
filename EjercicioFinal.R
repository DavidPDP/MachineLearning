install.packages("lm.beta")
install.packages("jtools")
install.packages("ggstance")
library(olsrr)
library(lmtest)
library(sandwich)
library(lm.beta)
library(jtools)
library(ggstance)

data<-read.csv(file.choose())

formula.modelo <- shares ~ 1 + timedelta + n_tokens_title + num_hrefs + average_token_length + data_channel_is_entertainment + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + global_subjectivity + max_negative_polarity

modelo <- lm(formula.modelo, data = data)
summary(modelo)

ols_plot_resid_qq(modelo)
ks.test(resid(modelo), "pnorm")

bptest(modelo, studentize = TRUE)

attach(data)
bptest(modelo, ~ timedelta + n_tokens_title + num_hrefs + average_token_length + 
         data_channel_is_entertainment + kw_min_avg + kw_max_avg + 
         kw_avg_avg + self_reference_min_shares + global_subjectivity + 
         max_negative_polarity+
         I(timedelta^2) + I(n_tokens_title^2) + I(num_hrefs^2) + I(average_token_length^2) + I( data_channel_is_entertainment^2) + I(kw_min_avg^2) + I(kw_max_avg^2) + I(kw_avg_avg^2) + I(self_reference_min_shares^2) + I(global_subjectivity^2) + I(    max_negative_polarity) +
         n_tokens_title* timedelta +num_hrefs* timedelta +average_token_length* timedelta + data_channel_is_entertainment* timedelta +kw_min_avg* timedelta +kw_max_avg* timedelta +kw_avg_avg* timedelta +self_reference_min_shares* timedelta +global_subjectivity* timedelta +    max_negative_polarity* timedelta+
         timedelta* n_tokens_title + num_hrefs* n_tokens_title +average_token_length* n_tokens_title + data_channel_is_entertainment* n_tokens_title +kw_min_avg* n_tokens_title +kw_max_avg* n_tokens_title +kw_avg_avg* n_tokens_title +self_reference_min_shares* n_tokens_title +global_subjectivity* n_tokens_title +    max_negative_polarity* n_tokens_title +
         timedelta* num_hrefs + n_tokens_title* num_hrefs+average_token_length* num_hrefs+ data_channel_is_entertainment* num_hrefs+kw_min_avg* num_hrefs+kw_max_avg* num_hrefs+kw_avg_avg* num_hrefs+self_reference_min_shares* num_hrefs+global_subjectivity* num_hrefs+    max_negative_polarity* n_tokens_title +
         
         timedelta* average_token_length+n_tokens_title* average_token_length+average_token_length* average_token_length+ data_channel_is_entertainment* average_token_length+kw_min_avg* average_token_length+kw_max_avg* average_token_length+kw_avg_avg* average_token_length+self_reference_min_shares* average_token_length+global_subjectivity* average_token_length+    max_negative_polarity* n_tokens_title +
         timedelta* data_channel_is_entertainment+n_tokens_title* data_channel_is_entertainment+kw_min_avg* data_channel_is_entertainment+kw_max_avg* data_channel_is_entertainment+kw_avg_avg* data_channel_is_entertainment+self_reference_min_shares* data_channel_is_entertainment+global_subjectivity* data_channel_is_entertainment+    max_negative_polarity* n_tokens_title +
         timedelta* self_reference_min_shares+n_tokens_title* self_reference_min_shares+kw_min_avg* self_reference_min_shares+kw_max_avg* self_reference_min_shares+kw_avg_avg* self_reference_min_shares+self_reference_min_shares* self_reference_min_shares+global_subjectivity* self_reference_min_shares+    max_negative_polarity* n_tokens_title
       
)

coeftest(modelo, vcov = (vcovHC(modelo)))

formula.modelo2 <- shares ~ 1 + timedelta + n_tokens_title + num_hrefs + average_token_length + data_channel_is_entertainment + kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares + global_subjectivity 

modelo2 <- lm(formula.modelo2, data = data)
coeftest(modelo2, vcov = (vcovHC(modelo2))) 

modelo2.s <- lm.beta(modelo2)
summary(modelo2.s)
coeftest(modelo2.s, vcov = (vcovHC(modelo2))) 

coef(modelo2.s)
coef(modelo2.s)[which.max(coef(modelo2.s))]
coef(modelo2.s)[which.max(abs(coef(modelo2.s)))]


coef.est <- matrix(0,10,2)

var.names  <- c(names(modelo2.s$standardized.coefficients)[-1])
coef.est <- matrix(modelo2.s$standardized.coefficients[-1])
coef.estand <- as.data.frame(var.names)
coef.estand$coef.est <- coef.est

library(ggplot2)

p <- ggplot(data=coef.estand, aes(x=var.names, y=coef.est)) +
  geom_bar(stat="identity", fill="steelblue") + 
  ggtitle("Coeficientes estandarizados")
p + coord_flip()

escenario = data.frame(mean(data$timedelta),  mean(data$n_tokens_title) ,  mean(data$num_hrefs), mean(data$average_token_length),  mean(data$data_channel_is_entertainment), mean(data$kw_min_avg),  mean(data$kw_max_avg), mean(data$kw_avg_avg), mean(data$self_reference_min_shares), mean(data$global_subjectivity))
names(escenario) <- c("timedelta", "n_tokens_title", "num_hrefs", "average_token_length", "data_channel_is_entertainment", "kw_min_avg", "kw_max_avg", "kw_avg_avg", "self_reference_min_shares", "global_subjectivity")

predict(modelo2, newdata = escenario,  level = 0.95, interval="confidence")
predict(modelo2, newdata = escenario,  level = 0.99, interval="confidence")

plot_summs(modelo2)

plot_summs(modelo2, robust = "HC3")

plot_summs(modelo2, robust = "HC3", scale = TRUE, transform.response = TRUE)

plot_summs(modelo2, robust = "HC3", scale = TRUE, transform.response = TRUE, inner_ci_level = .9)

plot_summs(modelo2, robust = "HC3", inner_ci_level = .9, plot.distributions = TRUE, rescale.distributions = TRUE)

plot_summs(modelo, modelo2, robust = "HC3", inner_ci_level = .9)

plot_summs(modelo, modelo2, scale = TRUE, transform.response = TRUE, robust = "HC3", inner_ci_level = .9)

plot_summs(modelo, modelo2, robust = "HC3", plot.distributions = TRUE, rescale.distributions = TRUE)

plot_summs(modelo2, modelo2, modelo2, scale = TRUE, robust = list(FALSE, "HC0", "HC3"),
           model.names = c("OLS", "HC0", "HC3"))

plot_summs(modelo2, robust = "HC3", plot.distributions = TRUE, rescale.distributions = TRUE, omit.coefs = "global_subjectivity")

plot_summs(modelo2, robust = "HC3", plot.distributions = TRUE, rescale.distributions = TRUE, omit.coefs = c("global_subjectivity", "(Intercept)"))

