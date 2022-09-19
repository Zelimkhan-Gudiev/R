remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####
library(ggplot2)
library(psych)
library(dplyr)
library(readxl)
library(ROCR)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")#

yt <- read.csv2("yt.csv")
yt <- read_xlsx("plan.xlsx")
data(diamonds)


# step 1 lapply 
apply(array, margin, ...)

lapply(list, function)

my_list <- list(x = c(rnorm(30), NA), y = rnorm(10))
str(my_list)

lapply(my_list, mean)
lapply(my_list, mean, na.rm = T)
lapply(my_list, function(x) x * 2)

sapply(my_list, range, na.rm = T, simplify = F)

# step 2

cars <- c("Mazda", "Volga", "Merc")
car <- "Mazda RX4"  

sapply(cars, function(x) grepl(x, car))
lapply(cars, function(x) grepl(x, car))

# step 3 by tapply
tapply(mtcars$mpg, mtcars$am, function(x) mean(x))
aggregate(mpg ~ am, mtcars, function(x) mean(x))

by(iris[1:4], iris$Species, 
   function(x) sapply(x, 
                      function(col) shapiro.test(col)$p.value))

aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)


# step 4 vapply, 

vapply(list, function, FUN.VALUE = type, ...)
vapply(mtcars, mean, FUN.VALUE = numeric(1))
sapply(mtcars, mean)

mapply(rep, c(1, 2, 3, 4), c(1, 2, 3, 4))

rep(1, 3)
x <- c(20, 25, 13)
m <- c(0, 1, 2)
s <- c(3, 5, 6)
mapply(rnorm, x, m, s)

#### Step 3 of 16 ####
# Напишите функцию positive_sum, которая получает на вход dataframe с произвольным количеством числовых переменных. 
# Основная задача функции - найти сумму положительных значений в каждой переменной и сохранить их в список.
# Рассмотрим пример работы функции на небольшом примере:

test_data <- data.frame(matrix(rnorm(30), ncol = 6))
#
positive_sum <- function(test_data) {lapply(test_data, function(x) {sum(x[!is.na(x) & x > 0])})}
positive_sum(test_data)
#
positive_sum <- function(d) {lapply(d, function(x) sum(x[x > 0], na.rm = T))}
#
positive_sum <-  function(d){
  lapply(d, FUN = function(x) sum(subset(x, x > 0)))
}
#
positive_sum <- function(df){
  lapply(df, function(x) sum(x[x > 0 & !is.na(x)]))
}
#
positive_sum <-  function(test_data){
  test_data[test_data < 0] <- 0
  lapply(test_data, sum, na.rm = T)
}
#
positive_sum <-  function(x){
  pos_sum <- apply(x, 2, function(x) sum(x[!is.na(x) & x > 0]))
  return(as.list(pos_sum))
  
}



#### Step 5 of 16  ####
# Обратите внимание на следующее выражение, которое очень часто будет вам помогать при работе с данными:
# давайте напишем команду, которая отбирает только количественные колонки в данных:
iris_num <- iris[sapply(iris, is.numeric)]
# Готово! sapply(iris, is.numeric) возвращает вектор логических значений, который мы и используем для индексации.
# Этот пример также иллюстрирует идею, что lapply и sapply можно применять к dataframe. Так как dataframe - это в том числе и список.
# Например, результат команды:
sapply(iris[1:4], sd)
# эквивалентна результату:
apply(iris[1:4], 2, sd)
# так как каждая колонка dataframe - это и есть элемент списка, то функция lapply и sapply возвращает результат применения некоторой функции 
# к каждой колонке данных!
# Но тут есть одно но!
# Как вы помните, apply производит все опперации именно над матрицами, поэтому если вы отправите в apply dataframe с разными типами данных, 
# то R сначала приведет все колонки к одному типу, чтобы получилась матрица, т.к. в матрице могут храниться данные только одного типа! 
# Это в свою очередь может привести к неожиданному результату:
sapply(iris, is.numeric)

apply(iris, 2, is.numeric)

# По результатам команды apply можно подумать, что в данных нет количественных переменных! Дело в том, что перед тем как применить функцию 
# is.numeric, сначала данные iris были переведены в матрицу, а все переменные переведены в строки, как в наиболее общий тип данных. 
# В результате получаем для каждой колонки FALSE.


#### Step 9 of 16  ####
# Давайте рассмотрим один небольшой пример работы с функцией mapply. Я оговорился, что она довольно специфична, однако иногда она все-таки 
# оказывается довольно полезной.
# Допустим у нас есть матрица размером 100 на 200:
m <- matrix(rnorm(100 * 200), nrow = 100)
# И мы хотим присвоить имена строчкам и столбикам в этой матрице по принципу:
# row_1, row_2, row_3, ..., row_100 - для строк
# col_1, col_2, col_3, ..., col_200 - для колонок
# Тогда мы могли бы сгенерировать список данными именами следующим образом:
m_names <- mapply(paste, list("row", "col"), list(1:100, 1:200), sep = "_")
str(m_names)

#### Step 10 of 16  ####
# Хотелось бы рассмотреть еще один подводный камень применения функций семейства apply к dataframe.
# Предположим, мы решили написать простенькую функцию для расчета стандартного отклонения количественных переменных в данных.
get_sd <- function(x){
  num_var <- sapply(x, is.numeric)
  sapply(x[, num_var], sd)
}

# Казалось бы, все логично и работает на различных примерах:
get_sd(iris)
# Но в нашем коде скрыта серьезная уязвимость!) Предположим, у нас есть набор данных, в котором только одна количественная переменная:
my_df <- data.frame(x = 1:10, y = letters[1:10])
get_sd(my_df)


#### Step 11 of 16  ####
#### Step 12 of 16  ####
#### Step 13 of 16  ####
#### Step 14 of 16  ####
#### Step 15 of 16  ####
#### Step 16 of 16  ####

