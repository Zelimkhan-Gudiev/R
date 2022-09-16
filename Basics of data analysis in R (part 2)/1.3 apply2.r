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

apply(airquality, 2, mean, na.rm = T)

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
find_positive <- function(x){
  x[x > 0]
}

apply(d, 2, find_positive)


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
# ƒавайте завершим и слегка модифицируем задачу из предыдущей лекции. Ќапишите функцию get_negative_values, 
# котора€ получает на вход dataframe произвольного размера. ‘ункци€ должна дл€ каждой переменной в данных провер€ть, 
# есть ли в ней отрицательные значени€. ≈сли в переменной отрицательных значений нет, то эта переменна€ 
# нас не интересует, дл€ всех переменных, в которых есть отрицательные значени€ мы сохраним их в виде списка 
# или матрицы, если число элементов будет одинаковым в каждой переменной (смотри пример работы функции).

# ≈сли чувствуете, что задачка не даетс€, ничего страшного, тут вс€ сложность отобрать только нужные колонки!
# ѕройдите следующий урок, возможно вооружившись знани€ми, как нам отбирать колонки данных по некому условию 
# вы слегкостью решите эту задачу!


