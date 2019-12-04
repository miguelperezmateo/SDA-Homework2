library(ggplot2)
library(dplyr)
library(car)
library(descriptr)
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

lm1 = lm(Log_Price_SGD ~ ., data = data)
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
ols_test_barlett(lm1)
#ols_test_breusch_pagan()
#ols_test_score
#ols_test_f(model)

# For independence
dwtest(lm1, alternative="two.sided")

#or
Box.test(residuals(lm1))

# For normality
jarque.bera.test(residuals(lm1))

plot(lm_1)