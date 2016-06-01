install.packages("knitr") # взаимодействие R-LaTeX и R-markdown
install.packages("xtable") # перевод таблиц в LaTeX
install.packages("texreg") # сравнение моделей в LaTeX
install.packages("pander") # перевод таблиц в markdown
install.packages("memisc") # перевод таблиц в markdown

install.packages("lmtest") # тесты в линейных моделях
install.packages("sandwich") # оценки ковариационной матрицы робастные к гетероскедастичности
install.packages("erer")
install.packages("AUC")
install.packages("mfx") # не используется в курсе, хорош для предельных эффектов в logit/probit

install.packages("ggplot2") # грамматика графиков
install.packages("GGally")
install.packages("lattice") 
install.packages("vcd") # мозаичный график
install.packages("hexbin") # график из шестиугольников
install.packages("sjPlot") # визуализация результатов МНК


install.packages("dplyr") # базовые манипуляции с данными
install.packages("reshape2") # длинные <-> широкие таблицы
install.packages("psych") # описательные статистики
install.packages("broom") # стандартизация вывода моделей
install.packages("tidyr") # причёсывание наборов данных


install.packages("glmnet") # LASSO
install.packages("HSAUR")
install.packages("sgof")
install.packages("car")


install.packages("spikeslab") # байесовская регрессия пик-плато
install.packages("quantreg") # квантильная регрессия
install.packages("MCMCpack") # набор моделей с байесовским подходом


install.packages("devtools") # разработка пакетов

install.packages("caret") # подбор параметров с помощью кросс-валидации
install.packages("AER")
install.packages("ivpack") # интсрументальные переменные

install.packages("lubridate") # работа с датами
install.packages("zoo") # нерегулярные временные ряды
install.packages("xts") # еще ряды
install.packages("forecast") # ARMA, экспоненциальное сглаживание
install.packages("rugarch") # не используется в курсе, хорош для GARCH

install.packages("quantmod") # загрузка с finance.google.com
install.packages("Quandl") # загрузка с Quandl

# non-CRAN packages:

devtools::install_github("bdemeshev/rlms") # read RLMS data
devtools::install_github("bdemeshev/sophisthse") # read data from sophist.hse.ru

# дополнение к quantmod для загрузки данных с finam.ru
install.packages("rusquant", repos = "http://r-forge.r-project.org", type = "source")
