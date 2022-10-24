remove(list = ls())
# Лабороторная работа 2 от 21.10.2022 по дисциплине «Data Mining»
# В R деревья решений реализуются с помощью двух основных пакетов: 
install.packages('party')
install.packages('rpart')
install.packages('randomForest') # «Случайный лес» реализуется пакетом randomForest.

library(party)
library(rpart)
library(randomForest)

#### Считаем данные ####
getwd()
myData <- read.csv("C:/Users/GudievZK/Desktop/GitHub/DF/tourism.csv")


#### Задание 1. ####
# Имеется фрагмент базы данных об анкетировании клиентов турфирмы. Разделите данные на обучающую и тестовую выборки по 70% и 30%, 
# либо используя другие проценты по своему усмотрению. 
# library(party)

set.seed(1234)

ind <- sample(2, nrow(myData), replace = TRUE, prob = c(0.7, 0.3)) # 
trainData <- myData[ind == 1, ]
testData<- myData[ind == 2, ]

# Постройте дерево решений на основе пакета party, выбрав интересующую Вас зависимую переменную. Поясните Ваш выбор.
# В частности, какую неизвестную информацию об исходных данных позволит получить построение дерева решений для этой переменной?
# Обозначьте зависимую переменную как Y, и классификационные переменные как Х1, Х2, и т.д., либо другим удобным способом.
# library(party)

myData[]<-lapply(myData, factor)

myData1 <- data.frame(myData$AGE,myData$TYPE,myData$AIM,myData$COMPANIONS,myData$ORGANIZE)

set.seed(1234)

ind <- sample(2, nrow(myData1), replace=TRUE, prob=c(0.7, 0.3))
trainData <- myData1[ind == 1,]
testData<- myData1[ind == 2,]

Y  <- as.integer(myData1$myData.TYPE)
X1 <- as.integer(myData1$myData.AGE)
X2 <- as.integer(myData1$myData.AIM)
X3 <- as.integer(myData1$myData.COMPANIONS)
X4 <- as.integer(myData1$myData.ORGANIZE)

myData_ctree<-ctree(myFormula, data=trainData)
table(predict(myData_ctree), Y)































str(myData)

library(dplyr)

factorNames <- names(select_if(myData, is.character))
myData[, factorNames] <- lapply(myData[factorNames], factor)

# 
myFormula <- Y ~ X1 + X2 + X3 + ... + Xk

myFormula <- trainData$AGE ~ trainData$COMPANIONS + trainData$CHOICE + trainData$LENGTH + trainData$TIMES_YEAR + 
                                            + trainData$AIM + trainData$EXPENCES + trainData$ORGANIZE +
                                            + trainData$TYPE + trainData$HOTEL_QUAL

myFormula <- trainData$COMPANIONS ~ trainData$AGE + trainData$CHOICE + trainData$LENGTH + trainData$TIMES_YEAR + 
                           + trainData$AIM + trainData$EXPENCES + trainData$ORGANIZE +
                           + trainData$TYPE + trainData$HOTEL_QUAL

myFormula <- myData

myData_ctree <- ctree(myFormula, data = trainData)


# Приведите матрицу классификации.
> table(predict(myData_ctree), 
        trainData$имя_выбранной_результативной_переменной)
Приведите результаты построения дерева решений в виде таблицы. 
> print(myData_ctree)
Опишите последовательность проведения классификации с точки зрения переменных. 
Приведите дерево решений на графике в общем виде.
> plot(myData_ctree)
Приведите дерево решений на графике в упрощенном виде.
> plot(myData_ctree, type="simple")
Дайте интерпретацию дерева решений с точки зрения листьев (терминальных узлов). 
Опишите вероятности для каждого класса результативного признака, указав количество 
наблюдений в каждом классе, с точки зрения значений параметров классифицирующих 
признаков.
Сведите полученные выводы в небольшую аналитическую записку о результатах 
классификации. 
Примените полученное дерево решений для тестовых данных. Дайте общую характеристику 
ошибкам классификации. 
> testPred<-predict(myData_ctree, newdata=testData)
> table(testPred, testData$имя_выбранной_результативной_переменной)
