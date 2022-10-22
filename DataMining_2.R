remove(list = ls())
# Лабороторная работа 2 от 21.10.2022 по дисциплине «Data Mining»
# В R деревья решений реализуются с помощью двух основных пакетов: 
install.packages('party')
install.packages('rpart')
install.packages('randomForest') # «Случайный лес» реализуется пакетом randomForest.

library(party)
library(rpart)
library(randomForest)


### Считаем данные ###
getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/)

myData <- read.csv("C:/Users/GudievZK/Desktop/GitHub/DF/tourism.csv")

### Задание 1. ###
# Имеется фрагмент базы данных об анкетировании клиентов турфирмы. Разделите данные на обучающую и тестовую выборки по 70% и 30%, 
# либо используя другие проценты по своему усмотрению. 
# library(party)
set.seed(1234)

ind <- sample(2, nrow(myData), replace=TRUE, prob=c(0.7, 0.3))
trainData <- myData[ind == 1,]
testData<- myData[ind == 2,]

# Постройте дерево решений на основе пакета party, выбрав интересующую Вас зависимую переменную. Поясните Ваш выбор.
# В частности, какую неизвестную информацию об исходных данных позволит получить построение дерева решений для этой переменной?
# Обозначьте зависимую переменную как Y, и классификационные переменные как Х1, Х2, и т.д., либо другим удобным способом.
# library(party)
myFormula <- Y ~ X1 + X2 + X3 + ... + Xk
myData_ctree <- ctree(myFormula, data=trainData)

# Приведите матрицу классификации.
> table(predict(myData_ctree), 
        trainData$имя_выбранной_результативной_переменной)
Приведите результаты построения дерева решений в виде таблицы. 
> print(myData_ctree)
Опишите последовательность проведения классификации с точки зрения переменных. 
Приведите дерево решений на графике в общем виде.
> plot(myData_ctree)
