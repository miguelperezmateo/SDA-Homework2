library(ggplot2)
library(dplyr)

data <- read.table("./data.txt",quote="\"", comment.char="", stringsAsFactors = T)

# Rename headers from data
names(data) <- c("Caratage", "ColourPurity", "Clarity", "InstCert", "Price_SGD")

# Plot price vs caratage
data %>% ggplot(aes(x = Caratage, y = Price_SGD)) + geom_point()

# Plot log price vs caratage (As it is a transformation, we avoid heterocedasticity)
data %>% ggplot(aes(x = Caratage, y = log(Price_SGD))) + geom_point()

# We are going to use log price since the explanatory variable is better
data %>% mutate("Log_Price_SGD" = log(Price_SGD))
