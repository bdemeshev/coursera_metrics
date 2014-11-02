data()
data(package = .packages(all.available = TRUE))

library(ggplot2)
data <- diamonds
model <- lm(data=data, price ~ carat)
summary(model)
model1 <- glm(data=data, price ~ 0 + carat)

model2 <- glm(data=data, price ~ carat + depth)
summary(model2)

model3 <- lm(data=data, price ~ carat+x+y)
summary(model3)

10117.99-62.56*1.645
10117.99+62.56*1.645

126.00-25.76*1.645
126.00+25.76*1.645

-1148.61-36.31*1.645
-1148.61+36.31*1.645

150-105
200-175

(45-25)*(25-5)/(2*25)

190-125
200-175

(65-25)*(40-5)/(2*25)

170-130
200-180

(40-20)*(35-5)/(2*20)
