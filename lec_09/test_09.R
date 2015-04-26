
## тест 8 к неделе 9

# вопрос про деление выборки на две части
library(ggplot2)
library(caret)
h <- diamonds

glimpse(h)

set.seed(12345)
train_ind <- createDataPartition(h$price, p=0.8, list=FALSE)
h_train <- h[train_ind,]
h_test <- h[-train_ind,]

# три вариации
model <- lm(data=h_train, log(price)~log(carat)+log(depth)+log(table)+clarity)
model <- lm(data=h_train, log(price)~log(carat)+log(table)+clarity)
model <- lm(data=h_train, log(price)~log(carat)+log(depth)+clarity)

h_test$y_hat <- exp(predict(model, newdata = h_test, type = "response"))

RSS <- sum((h_test$price-h_test$y_hat)^2)
RSS

deviance(model)
# qplot(data=h_test, price, y_hat)
RSS/10^9


# 
