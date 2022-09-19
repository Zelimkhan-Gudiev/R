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

#### step 2 apply(x, 1, mean, ...) ####
head(airquality)
?airquality

apply(airquality, 2, mean)
apply(airquality, 2, mean, na.rm = T) # все аргументы, указанные вместо ... (apply(X, MARGIN, FUN, ...)) будут отнесены к функции, которрую нужно применить 

colMeans(airquality, na.rm = T)
colSums(airquality, na.rm = T)
rowMeans(airquality, na.rm = T)
rowSums(airquality, na.rm = T)

# step 3 
set.seed(42)

d <- as.data.frame(matrix(rnorm(30), nrow = 5))

my_fun <- function(x) x * 2

d[1, 1] <- NA

my_list <- list()
for (i in seq_along(d)){
  temp_col <- d[, i]
  neg_numbers <- temp_col[temp_col < 0]
  my_list[[i]] <- neg_numbers
}
names(my_list) <- colnames(d)

my_list <- apply(d, 2, function(x) x[x < 0])


find_negative <- function(x){
  x[x < 0]
}

apply(d, 2, function(x) x[x < 0])

find_positive <- function(x){
  x[x > 0]
}

apply(d, 2, find_positive)

apply(d, 2, function(x) x[x > 0])
# 2
apply(d, 2, FUN = 
            function(x) {
            y <- x[x > 0]
            return(y)
            }
)

# step 4
apply(array, margin, ...)
head(iris)

aov_result <- apply(iris[, 1:4], 2, function(x) aov(x ~ iris$Species))

norm_test <- apply(iris[, 1:4], 2, 
                   function(x) shapiro.test(x))

norm_test_p <- apply(iris[, 1:4], 2, 
                     function(x) shapiro.test(x)$p.value)


str(aov_result)
aov_result$Petal.Length
norm_test$Petal.Width


#### Step 5 of 8  ####
# Давайте завершим и слегка модифицируем задачу из предыдущей лекции. Напишите функцию get_negative_values, 
# которая получает на вход dataframe произвольного размера. Функция должна для каждой переменной в данных проверять, есть ли в ней 
# отрицательные значения. Если в переменной отрицательных значений нет, то эта переменная нас не интересует, для всех переменных, 
# в которых есть отрицательные значения мы сохраним их в виде списка или матрицы, если число элементов будет одинаковым в каждой
# переменной (смотри пример работы функции).

# Если чувствуете, что задачка не дается, ничего страшного, тут вся сложность отобрать только нужные колонки! 
# Пройдите следующий урок, возможно вооружившись знаниями, как нам отбирать колонки данных по некому условию вы слегкостью решите эту задачу!

x <- as.data.frame(matrix(rnorm(30), ncol = 6))
get_negative_values <- apply(x, 2, FUN = function(x) {
                                                  x[is.na(x)] <- 0
                                                  x[x < 0]
                                        }
)
get_negative_values




#### Step 8 of 8  ####
# Напишите функцию na_rm которая заменяет все пропущенные значения в столбцах dataframe на соответствующее среднее значение. 
# То есть все NA в первом столбце заменяются на среднее значение первого столбца (рассчитанного без учета NA). 
# Все NA второго столбца заменяются на среднее значение второго столбца и т.д.  
# Функция na_rm на вход получает dataframe произвольной размерности, состоящий из количественных переменных. 
# Функция должна возвращать  dataframe с замененными NA. Ни порядок столбцов, ни порядок строк в dataframe изменять не нужно.
# Вы можете создавать вспомогательные функции для решения этой задачи. 
# Напоминаю, что для проверки является ли наблюдение NA нужно использовать функцию is.na()
x
x[c(2, 4), c(1,3)] <- NA

na_rm <- function(x) {
  for(i in 1:length(x)) {
      x[is.na(x[, i]), i] <- mean(x[, i], na.rm = T)
  }
  return(as.data.frame(x))
}

na_rm(x)

#
na_rm  <- function(x){    
  na_to_mean  <- function(v){    
    v[is.na(v)]  <- mean(v , na.rm = T)    
    return(v)}    
  result  <- as.data.frame(apply(x, 2, na_to_mean))
  }

#
na_rm  <- function(test_data){
  as.data.frame(apply(test_data, 2, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
}

#
na_rm  <- function(x){
  as.data.frame(apply(x, 2, function(x) replace(x,is.na(x), mean(x, na.rm = T))))
}

#
na_rm  <- function(x) {
  if (anyNA(x)) {
    f <- function(xx) {
      nas <- is.na(xx)
      xx[nas] <- sum(xx, na.rm = TRUE) / sum(!nas)
      xx
    }
    x <- as.data.frame(apply(x, 2, f))
  }
  x
}



