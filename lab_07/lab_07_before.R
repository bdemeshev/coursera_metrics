# lab_07

library("dplyr")
library("erer")
library("vcd")
library("ggplot2")
library("reshape2")
library("AUC")

options(stringsAsFactors=FALSE)

t <- read.csv("titanic3.csv")
# http://lib.stat.cmu.edu/S/Harrell/data/descriptions/titanic.html