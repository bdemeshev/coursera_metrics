
# обойтись без bstats через bptest в жанре:
# bptest(cig_lm2, ~ income * price + I(income^2) + I(price^2), data = CigarettesB)



install.packages("knitr")
install.packages("xtable")
install.packages("texreg")
install.packages("pander")
install.packages("memisc")

install.packages("lmtest")
install.packages("sandwich")
install.packages("erer")
install.packages("AUC")

install.packages("ggplot2")
install.packages("GGally")
install.packages("lattice")
install.packages("vcd")
install.packages("hexbin")
install.packages("sjPlot")

install.packages("dplyr")
install.packages("reshape2")
install.packages("broom")
install.packages("tidyr")
install.packages("psych")

install.packages("glmnet")
install.packages("HSAUR")
install.packages("sgof")
install.packages("car")

install.packages("spikeslab")
install.packages("quantreg")
install.packages("MCMCpack")

install.packages("devtools")

install.packages("caret")
install.packages("AER")
install.packages("ivpack")

install.packages("lubridate") # работа с датами
install.packages("zoo") # временные ряды
install.packages("xts") # еще ряды
install.packages("forecast")

install.packages("quantmod") # загрузка с finance.google.com
install.packages("Quandl") # загрузка с Quandl

# non-CRAN packages:

devtools::install_github("bdemeshev/rlms") # read RLMS data
devtools::install_github("bdemeshev/sophisthse") # read data from sophist.hse.ru

install.packages("rusquant",repos="http://r-forge.r-project.org", type="source")