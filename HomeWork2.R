library(ggplot2)
library(dplyr)
library(car)
library(lmtest)
library(tsoutliers)
data <- read.table("./data.txt",quote="\"", comment.char="", stringsAsFactors = T)

# Rename headers from data
names(data) <- c("Caratage", "ColourPurity", "Clarity", "InstCert", "Price_SGD")
data <- data %>% mutate(ColourPurity = relevel(ColourPurity, ref = "I"))
data <- data %>% mutate(Clarity = relevel(Clarity, ref = "VS2"))
data <- data %>% mutate(InstCert = relevel(InstCert, ref = "HRD"))

# Plot price vs caratage
data %>% ggplot(aes(x = Caratage, y = Price_SGD)) + geom_point()

# Plot log price vs caratage (As it is a transformation, we avoid heterocedasticity)
data %>% ggplot(aes(x = Caratage, y = log(Price_SGD))) + geom_point()
data %>% ggplot(aes(x = Caratage, y = log(Price_SGD))) + geom_point(aes(color=InstCert))
data %>% ggplot(aes(x = Caratage, y = log(Price_SGD))) + geom_point(aes(color=Clarity))

# We are going to use log price since the explanatory variable is better
data <- data %>% mutate("Log_Price_SGD" = log(Price_SGD))

lm1 = lm(Log_Price_SGD ~ . -Price_SGD, data = data)
# lm2 = lm(Log_Price_SGD ~ . - InstCert, data = data) # Model discarding The certificates

summary(lm1)
# summary(lm2)

# anova(lm1, lm2)
# They show a visible pattern in the distriubtion along indexes, showing that the model can be improved.
## TESTS

residualPlot(lm1)
plot(lm1$residuals) # As we can se, residuals have no mean 0 on every element and variance is not the same.
outlierTest(lm1) # No stdres with Bonferroni p<0.05 was found in the dataset

# For constant variance -> https://cran.r-project.org/web/packages/olsrr/vignettes/heteroskedasticity.html
bptest(lm1)
# we can see that the p value is very low, implying heterocedasticity:
# https://en.wikipedia.org/wiki/Breusch%E2%80%93Pagan_test ref here for p value interpret

# For independence
dwtest(lm1, alternative="two.sided")
# DW is >0 and <2 meaning positive autocorrelation
# And the test tells us exists autocorr since pvalue is low
Box.test(lm1$residuals)
# exists dependency in the residuals
#https://stat.ethz.ch/pipermail/r-help/2004-April/049548.html ref for p value interp

# For normality
JarqueBera.test(lm1$residuals)
# as we can see, pvalues are very high, all > 0.05, meaning normality must ve discarded
#https://stats.stackexchange.com/questions/130368/why-do-i-get-this-p-value-doing-the-jarque-bera-test-in-r ref for interpret

# Question 3
caratcut <-cut(data$Caratage, c(0,0.5,1,max(data$Caratage)))

data <- data %>% mutate("Size" = cut(data$Caratage, c(0,0.4999999,0.9999999,max(data$Caratage)),  labels=c("Small","Medium","Large")))

lm2 <-lm(Log_Price_SGD ~ Caratage*Size+ ColourPurity + Clarity + InstCert, data = data)
summary(lm2) #small is ref by default
lm3 <-lm(Log_Price_SGD ~Caratage + I(Caratage^2)+ ColourPurity + Clarity + InstCert,data=data)
summary(lm3) 

