
library('fastDummies')
library("dplyr")
library(xlsx)
library(rio)
library(devtools)
library(broom)
library("glmx")  
library("zoo") 
library("np")
library("rgl")


setwd('/Users/tatana/Documents') # установка рабочей директории


data = import('data.xls')         # чтение данных 

# укажем, что переменная popul является категориальной 
data$popul <- ordered(data$popul)

#вектор ширины окн с помощью кросс-валидации в stata
bws <- c( 0.493341, 13.47497, 5.469876, 6.496928, 2849.686, 0.9999974, 5.65185)




# оценка регресси c шириной окна, полученной с помощью кросс-валидации в stata
bw <- np::npregbw(formula = lngrp ~ linnexp+ htech+lnrd+inn_gs+res_num+popul+urban, data = data,bandwidth.compute=FALSE, bws =bws)

reg <- npreg(bw,gradients = TRUE)


# Выведем функции предельных эффектов
par(mfrow = c(2, 4))
sm<-plot(reg, plot.par.mfrow = FALSE)


# Оценка ассимптотических интервалов с помощью бутсрапа 
B <- 200
par(mfrow = c(2, 4))
sd <-plot(reg, plot.errors.method = "bootstrap", common.scale = FALSE,
     plot.par.mfrow = FALSE, plot.errors.boot.num = B, random.seed = 42,
     col = 2)














