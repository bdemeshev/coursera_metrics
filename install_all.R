install.packages("tidyverse") # коллекция пакетов от Hadley Wickham

install.packages("knitr") # взаимодействие R-LaTeX и R-markdown
install.packages("rmarkdown") # взаимодействие R-markdown
install.packages("xtable") # перевод таблиц в LaTeX
install.packages("texreg") # сравнение моделей в LaTeX
install.packages("pander") # перевод таблиц в markdown
install.packages("memisc") # перевод таблиц в markdown
install.packages('huxtable') # красивые таблички для latex/markdown/html

install.packages("lmtest") # тесты в линейных моделях
install.packages("sandwich") # оценки ковариационной матрицы робастные к гетероскедастичности
install.packages("erer") # подборка пакетов для эмпирических исследований
install.packages("AUC") # подсчёт показателя AUC
install.packages("mfx") # для предельных эффектов в logit/probit
install.packages("estimatr") # модели с робастными стандартными ошибками

install.packages("GGally") # матрица диаграмм рассеяния
install.packages("lattice") # конкурент ggplot2
install.packages("vcd") # мозаичный график
install.packages("hexbin") # график из шестиугольников
install.packages("sjPlot") # визуализация результатов МНК
install.packages("factoextra") # визуализация для метода главных компонент и не только

install.packages("reshape2") # длинные <-> широкие таблицы
install.packages("psych") # описательные статистики
install.packages("skimr") # описательные статистики

install.packages("glmnet") # LASSO
install.packages("HSAUR")
install.packages("sgof")
install.packages("car") # для тестирования линейных гипотез, подсчёта vif

install.packages("spikeslab") # байесовская регрессия пик-плато
install.packages("quantreg") # квантильная регрессия
install.packages("MCMCpack") # набор моделей с байесовским подходом

install.packages("devtools") # разработка пакетов

install.packages("caret") # подбор параметров с помощью кросс-валидации
install.packages("AER")
install.packages("ivpack") # интсрументальные переменные

install.packages("zoo") # нерегулярные временные ряды
install.packages("xts") # еще ряды
install.packages("forecast") # ARMA, экспоненциальное сглаживание
install.packages("rugarch") # не используется в курсе, хорош для GARCH

install.packages("quantmod") # загрузка с finance.google.com
install.packages("Quandl") # загрузка с Quandl

# non-CRAN packages:
devtools::install_github("bdemeshev/sophisthse") # read data from sophist.hse.ru
devtools::install_github("bdemeshev/rlms") # авточистка данных RLMS 

# дополнение к quantmod для загрузки данных с finam.ru
install.packages("rusquant", repos = "http://r-forge.r-project.org", type = "source")
