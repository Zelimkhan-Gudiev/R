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

yt <- as_data_frame()

df <- mtcars
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
d <- as_data_frame(matrix(rnorm(30), nrow = 5))

my_fun <- function(x) x * 2

d[1, 1] <- NA

# let's write code, which records all negative values of each column
my_list <- list() # first of all we create a variable, which is a list. Necessary to create a list is due to the fact that different columns can 
# have a different number of negative values. In this variable we will save all negative values of each column.

# Further we write a cycle. The cycle will records all negative values of each columns.

for (i in seq_along(d)){ # The seq_along function creates the sequence from 1 to the namber of columns in the data frame
  temp_col <- d[, i] # create temp_col variable to store the data frame column number
  neg_numbers <- temp_col[temp_col < 0] # create neg_numbers variable to store negative numbers the i columns of data frame
  my_list[[i]] <- neg_numbers # save the negative numbers of i columns in i elements of my_list variable
}
names(my_list) <- colnames(d) # We can assign list names to column names

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

test_data <- as_data_frame(list(V1 = c(-9.7, -10, -10.5, -7.8, -8.9), 
                                V2 = c(NA, -10.2, -10.1, -9.3, -12.2),
                                V3 = c(NA, NA, -9.3, -10.9, -9.8)
                                )
                           )

test_data <- as_data_frame(list(V1 = c(NA, 0.5, 0.7, 8),
                                V2 = c(-0.3, NA, 2, 1.2),
                                V3 = c(2, -1, -5, -1.2)
                                )
                           )

test_data <- as_data_frame(list(V1 = c(NA, -0.5, -0.7, -8),
                                V2 = c(-0.3, NA, -2, -1.2),
                                V3 = c(1, 2, 3, NA)
                                )
                           )

get_negative_values <- function (x) {apply(x, 2, FUN = 
                               function(x) {
                                 x[is.na(x)] <- 0
                                 x1 <- x[x < 0]
                                 x1[sapply(x1, length) > 0]
                                 })
}



get_negative_values(test_data)

colSums(test_data < 0, na.rm = T)

test_data$V3[is.na(test_data$V3)] <- 0
t1 <- test_data$V3[test_data$V3 < 0]
t1[sapply(t1, length) > 0]
t1[lapply(t1, length) > 0]

t1[lapply(t1,length)>0]

l <- list(1:3, "foo", character(0), integer(0))

l[lapply(l,length)>0]




#### Step 8 of 8  ####
# Напишите функцию na_rm которая заменяет все пропущенные значения в столбцах dataframe на соответствующее среднее значение. 
# То есть все NA в первом столбце заменяются на среднее значение первого столбца (рассчитанного без учета NA). 
# Все NA второго столбца заменяются на среднее значение второго столбца и т.д.  
# Функция na_rm на вход получает dataframe произвольной размерности, состоящий из количественных переменных. 
# Функция должна возвращать  dataframe с замененными NA. Ни порядок столбцов, ни порядок строк в dataframe изменять не нужно.
# Вы можете создавать вспомогательные функции для решения этой задачи. 
# Напоминаю, что для проверки является ли наблюдение NA нужно использовать функцию is.na()

x <- test_data

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



