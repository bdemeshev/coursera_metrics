# Esli russkie bukvi prevratilitis v krakozyabry, to File - Reopen with
# encoding... - UTF-8 - Set as default - OK

# lab 07

# подключаем пакеты
library(mfx)  # расчет предельных эффектов
library(vcd)  # графики для качественных данных
library(reshape2)  # манипуляции с данными
library(skimr) # описательные статистики (вместо psych в видеолекциях)
library(AUC)  # для ROC кривой
library(rio) # импорт файлов разных форматов
library(tidyverse) # графики и манипуляции с данными, подключаются пакеты dplyr, ggplot2, etc


# при загрузке файлов R автоматом переделывает все строковые переменные в
# факторные эта шаманская команда просит R так не делать :)
options(stringsAsFactors = FALSE)

# читаем данные по пассажирам Титаника
t <- import("titanic3.csv")
# источник и описание:
# http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html
