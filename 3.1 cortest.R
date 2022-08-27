remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

library(ggplot2)
library(psych)
library(dplyr)
library(psych)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")

yt <- read.csv2("yt.csv")
df  <- mtcars

# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»

#### Step 1,2: Contens  _______________________________________________________________________________________________________ ####
# В этом уроке мы научимся рассчитывать коэффициенты корреляции и познакомимся с простой линейной регрессией.
# Ссылка на скрипт урока:
# Корреляция https://stepic.org/media/attachments/lesson/11508/cortest.R
# Регрессия ???https://stepic.org/media/attachments/lesson/11508/simple_regr.R




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

pairs(yt[, c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 
             'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
             'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')])

cor(yt[, c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 
           'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
           'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')])

#v1
yt_n_names <- c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 
                'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
                'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')

yt[, yt_n_names] <- lapply(yt[, yt_n_names], as.numeric)

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

# мы подробнее поговорим о функциях семейства apply в следующем курсе - Advanced R, но вы можете изучить справку о apply и sapply. Для решения данной задачи, эти функции могут пригодиться.
# обратите внимание на функцию which.max()
# обратите внимание на конструкцию diag(matrix) <- n???
# если вы получаете ошибку, ???'x' must be a numeric vector, ???значит в данных остались не только количественные переменные.

colnames(df)
typeof(df)
lapply(df, class)
typeof(yt)
is.numeric(yt$name)
is.numeric(yt$duration)
lapply(df, class)
yt_numeric <- yt[, sapply(yt, is.numeric)]
pairs(yt_numeric)

#
library(dplyr)
xNum <- select_if(x, is.numeric)
pairs(xNum)


x <- df

filtered.cor <- function(x){
  library(dplyr)
  library(psych)
  x <- select_if(x, is.numeric)
  crt <- corr.test(x)$r
  diag(crt) <- 0
  crt[which.max(abs(crt))]
}

filtered.cor(test_data)

test_data <- as.data.frame(list(V5 = c("b", "b", "b", "b", "b", "b", "b", "b"), 
                                V4 = c("r", "r", "r", "r", "r", "r", "r", "r"), 
                                V2 = c(-0.3, -0.1, -0.5, 0.3, 0.4, 1.4, -0.7, 1.8),
                                V6 = c("e", "e", "e", "e", "e", "e", "e", "e"), 
                                V3 = c("k", "k", "k", "k", "k", "k", "k", "k"), 
                                V1 = c(0.6, 0.6, 0.3, -1.2, -0.4, -0.2, -0.3, 0.5)))


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
  if(shapiro.test())
}

shapiro.test(c(yt$duration, yt$time_depir))
hist(yt$duration)
rnorm(100)
hist(rnorm(100))
shapiro.test(rnorm(100))
####  Step 12 of 16 ####
# Скачайте набор данных - dataframe с двумя количественными переменными (вспомните при необходимости, как задавать разделитель и другие параметры функции read.table), постройте линейную регрессию, где - первая переменная - зависимая, вторая - независимая. В ответ укажите значения регрессионных коэффициентов сначала intercept затем  slope.
# Десятичный разделитель - точка. В поле для ответа введите два числа, не округляйте значения, например;
# 12.434 6.2557
# У вас есть неограниченное число попыток.
# Время одной попытки: 5 mins


####  Step 13 of 16 ####
# Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. Только для бриллиантов класса Ideal (переменная cut) c числом карат равным 0.46 (переменная carat) постройте линейную регрессию, где в качестве зависимой переменной выступает price, в качестве предиктора - переменная  depth. Сохраните коэффициенты регрессии в переменную fit_coef.
# Памятка:
# > fit <- lm(mpg ~ disp + wt, mtcars)
# > fit$coefficients # коэффициенты модели
# Это задание нужно решить, не используя цикл for().


####  Step 14 of 16 ####
# Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
# Если две переменные значимо коррелируют (p - уровень значимости для коэффициента корреляции Пирсона меньше 0.05), то функция строит регрессионную модель, где первая переменная - зависимая, вторая - независимая. Затем создает в dataframe новую переменную с назанием fit, где сохраняет предсказанные моделью значения зависимой переменной. В результате функция должна возвращать исходный dataframe с добавленной новой переменной fit.
# Если две переменные значимо не коррелируют, то функция возвращает строчку "There is no sense in prediction"
# Примеры работы функции:
# > my_df = iris[,1:2] # на вход подаем данные iris только с переменными Sepal.Length и Sepal.Width
# > regr.calc(iris[,1:2]) # переменные значимо не коррелируют 
# [1] "There is no sense in prediction"
# > my_df = iris[,c(1,4)] # на вход подаем данные iris только с переменными Sepal.Length и Petal.Width
# > regr.calc(my_df) # переменные значимо коррелируют 

        Sepal.Length        Petal.Width        fit


1         5.1                   0.2           4.955345
2         4.9                   0.2           4.955345
3         4.7                   0.2           4.955345
.           .                    .                .
.           .                    .                .

# Обратите внимание, при проверке вашей функции на вход будут подаваться данные с различными именами колонок. 
# Ваша функция должна корректно работать в независимости от имен переменных.
# Перед тем как сдавать решение убедитесь, что ваша функция работает корректно на разных данных, с разными именами колонок.


####  Step 15 of 16 ####
# Постройте scatterplot по данным iris, сохранив его в переменную my_plot : 
# Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы наблюдений по переменной Species.
# Если Вы все сделали правильно должен получиться следующий график:


yt$reason
yt[yt$reason == 'План по стандартизации']




length("Аэрофотосъёмка ландшафта уже выявила земли богачей и процветающих крестьян.")
getwd()
list.files()  
sd
var
mean
