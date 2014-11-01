data()
data(package = .packages(all.available = TRUE))

library(ggplot2)
data <- diamonds
model <- lm(data=data, price ~ carat)
summary(model)
model1 <- glm(data=data, price ~ 0 + carat)

model2 <- glm(data=data, price ~ carat + depth)
summary(model2)
