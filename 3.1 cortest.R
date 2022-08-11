remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

library(ggplot2)
library(psych)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")

yt <- read.csv2("yt.csv")


# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»

#### Step 1,2: Contens  _______________________________________________________________________________________________________ ####
# В этом уроке мы научимся рассчитывать коэффициенты корреляции и познакомимся с простой линейной регрессией.
# Ссылка на скрипт урока:
# Корреляция https://stepic.org/media/attachments/lesson/11508/cortest.R
# Регрессия ???https://stepic.org/media/attachments/lesson/11508/simple_regr.R


df  <- mtcars

cor.test(x = df$mpg, y = df$hp)
fit  <- cor.test(x = df$mpg, y = df$hp)

cor.test(~ mpg + hp, df)

str(fit)

fit$p.value

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)+
  facet_grid(. ~ am)


###########################################

df  <- mtcars
df_numeric  <- df[, c(1,3:7)]


pairs(df_numeric)

cor(df_numeric)

fit  <- corr.test(df_numeric)
fit$r
fit$p
fit$adjust

#### yt  ####

pairs(yt[, c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
             'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')])
cor(yt[, c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
           'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')])

#v1
yt_n_names <- c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
                'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')

yt[, yt_n_names] <- lapply(yt[, yt_n_names], numeric)

#v2
yt$numb_ret_depir <- as.numeric(yt$numb_ret_depir)
yt$numb_ret_oiv <- as.numeric(yt$numb_ret_oiv)
yt$time_plan <- as.numeric(yt$time_plan)
yt$time_ac <- as.numeric(yt$time_ac)
yt$time_rev_oiv <- as.numeric(yt$time_rev_oiv)
yt$teamleader <- as.numeric(yt$teamleader)
yt$time_rev_depir <- as.numeric(yt$time_rev_depir)
yt$time_vn_sogl <- as.numeric(yt$time_vn_sogl)
yt$time_depir <- as.numeric(yt$time_depir)
yt$time_oiv <- as.numeric(yt$time_oiv)
yt$time_prep_rg <- as.numeric(yt$time_prep_rg)
yt$time_rg <- as.numeric(yt$time_rg)
yt$time_mrg <- as.numeric(yt$time_mrg)
yt$time_eaist <- as.numeric(yt$time_eaist)
yt$duration <- as.numeric(yt$duration)

####  Step 5 of 16 ####

# Напишите функцию corr.calc, которая на вход получает data.frame с двумя количественными переменными, 
# рассчитывает коэффициент корреляции Пирсона и возвращает вектор из двух значений: коэффициент корреляции и p - уровень значимости.
# Пример работы функции:
# > corr.calc( mtcars[, c(1,5)] )  # на вход подаем данные mtcars только с переменными mpg и drat
# [1] 0.6811719078 0.0000177624
# > corr.calc( iris[,1:2] ) # на вход подаем данные iris только с переменными Sepal.Length и Sepal.Width
# [1] -0.1175698 0.1518983
# При написании функции обратите внимание, что названия переменных входящего dataframe могут быть произвольными. 
# Пишите функцию с учетом, что она должна работать на любом  dataframe с двумя количественными переменными как в примере выше.
# Не забудьте подгрузить библиотеку library(psych), если хотите использовать ее при решении этой задачи.

# тестирование 
str(df)
typeof(df$disp)
df$disp <- as.numeric(df$disp)
df$hp <- as.numeric(df$hp)
crt <- cor.test(df[, 'disp'], df[, 'hp'])
crt <- cor.test(df[, 3], df[, 4])
str(crt)
crt$estimate

#### How to Fix: ‘x’ must be numeric in R (Error in cor.test.default(df[, c(3, 4)]) : 'x' must be a numeric vector) ####
# Example 1: Error in vector ‘x’ must be numeric
# vector creation
x <- c("61", "4", "21", "67", "89", "2")
# display vector
print(x)
# plotting hist
hist(x)
# To solve this error we will convert the vector elements into numeric data using as.numeric() methods.
x <- c("61", "4", "21", "67", "89", "2")
print(x)
res <- as.numeric(x)
hist(res)

# Example 2: Error in dataframe ‘x’ must be numeric
# Create data for chart
val <-data.frame("num"=c("77","55","80","60"),
                 "course"=c('DSA','C++','R','Python'))

print(val)
hist(val[,1])
# To solve this error we will convert the dataframe element into numeric data using as.numeric() methods
val <-data.frame("num"=c(77,55,80,60),
                 "course"=c('DSA','C++','R','Python'))
print(val)
hist(val[,1])


#1
corr.calc <- function(x){
  crt <- cor.test(x[, 1], x[, 2])
  return(c(crt$estimate, crt$p.value))
}

corr.calc(df[, c(3, 4)])

#2
corr.calc <- function(x){    
  fit  <- cor.test(x[[1]], x[[2]])
  r <- fit$estimate
  p <- fit$p.value
  return(c(r, p))}
corr.calc(df[, c(3, 4)])

#3
corr.calc <- function(df){
  # put your code here  
  fit <- cor.test(~., df)
  return(c(fit$estimate, fit$p.value))
}

#4
corr.calc <- function(x){
  cor.test(x = x[,1], y = x[,2])[c("estimate", "p.value")]
}

#5
corr.calc <- function(x){
  
  ct<- psych::corr.test(x)
  return(c(ct$r[1,2], ct$p[1,2]))
  
}

#6
library(psych)
corr.calc <- function(x){
  fit  <- corr.test(x)
  return(c(fit$r[2], fit$p[2]))
}

#8



####  Step 6 of 16 ####
# Напишите функцию filtered.cor которая на вход получает data.frame с  произвольным количеством переменных 
# (как количественными, так и любых других типов), рассчитывает коэффициенты корреляции Пирсона между всеми парами
# количественных переменных и возвращает наибольшее по модулю значение коэффициента корреляции. 
# (То есть функция может вернуть -0.9, если это наибольшая по модулю  корреляция).

# Гарантируется наличие в data.frame хотя бы двух количественных переменных.

# Обратите внимание: при проверке вашей функции на вход будут подаваться данные с различными именами колонок. 
# Ваша функция должна корректно работать независимо от имен переменных. Перед тем, как сдавать решение, убедитесь, 
# что ваша функция работает корректно на разных данных, с разными именами колонок. 
# Если вы хотите использовать функцию corr.test не забудьте загрузить библиотекy psych.
# Данные для тренировки:
# https://stepic.org/media/attachments/lesson/11504/step6.csv
# Remove Non-Numeric Columns https://datamining.togaware.com/survivor/Remove_Non_Numeric.html

colnames(df)
typeof(df)
lapply(df, class)
typeof(yt)
is.numeric(yt$name)
is.numeric(yt$duration)
lapply(df, class)
yt_numeric <- yt[, sapply(yt, is.numeric)]
pairs(yt_numeric)

df_numeric[[]]
is.numeric(df_numeric)

filtered.cor <- function(x){
  if (x[[]] == numeric) {
    pairs(x)
  }
  
}

####  Step 7 of 16 ####
# Напишите функцию smart_cor, которая получает на вход dataframe с двумя количественными переменными. 
# Проверьте с помощью теста Шапиро-Уилка, что данные в обеих переменных принадлежат нормальному распределению.
# Если хотя бы в одном векторе распределение переменной отличается от нормального (p - value меньше 0.05), 
# то функция должна возвращать коэффициент корреляции Спирмена. (Числовой вектор из одного элемента).
# Если в обоих векторах распределение переменных от нормального значимо не отличается, 
# то функция должна возвращать коэффициент корреляции Пирсона.
# > test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
# > smart_cor(test_data)
# [1] -0.1031003


smart_cor <- function(x) {
  if(x[, 1])
}