
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

library("AER")
data("CollegeDistance")
h <- CollegeDistance
help("CollegeDistance")

model <- ivreg(data=h, wage~gender+unemp+ethnicity+education|gender+unemp+ethnicity+distance)
model <- ivreg(data=h, wage~region+gender+unemp+ethnicity+education|region+gender+unemp+ethnicity+distance)
model <- ivreg(data=h, wage~region+gender+unemp+education|region+gender+unemp+distance)


summary(model)
res <- coeftest(model)
res["education","Estimate"]

#model_0 <- ivreg(data=h, wage~gender+unemp+ethnicity+education)
#summary(model_0)
#summary(h$region)

# h <- diamonds
# set.seed(12345) 
# train_ind <- createDataPartition(h$price, p=0.8, list=FALSE) 
# h_train <- h[train_ind,] 
# h_test <- h[-train_ind,] 
# mod1 <- lm(data=h_train, log(price)~log(carat)+log(depth)+clarity)
# pred_1 <- predict(mod1, newdata=h_test)
# h2 <- exp(pred_1)
# x <- (sum((h2-h_test$price)^2))/10^9
# x
# 
# h <- diamonds
# set.seed(12345)
# train_ind <- createDataPartition(h$price, p=0.8, list=FALSE)
# h_train <- h[train_ind,]
# h_test <- h[-train_ind,]
# model <- lm(data=h_train, log(price)~log(carat)+log(depth)+clarity)
# h_test$y_hat <- exp(predict(model, newdata = h_test, type = "response"))
# RSS <- sum((h_test$price-h_test$y_hat)^2)
# RSS
# deviance(model)
# RSS/10^9
