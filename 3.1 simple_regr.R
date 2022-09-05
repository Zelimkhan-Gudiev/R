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

# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»

#### Step 1,2: Contens  _______________________________________________________________________________________________________ ####
# В этом уроке мы научимся рассчитывать коэффициенты корреляции и познакомимся с простой линейной регрессией.
# Ссылка на скрипт урока:
# Корреляция https://stepic.org/media/attachments/lesson/11508/cortest.R
# Регрессия ???https://stepic.org/media/attachments/lesson/11508/simple_regr.R


df  <- mtcars
df_numeric  <- df[,c(1,3:7)]

fit  <- lm(mpg ~ hp, df)
summary(fit)
str(fit)

ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg  <- predict(fit, new_hp)

predict(fit, new_hp)


##################################

my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)


####  Step 13 of 16 ####
# Скачайте набор данных - dataframe с двумя количественными переменными 
# (вспомните при необходимости, как задавать разделитель и другие параметры функции read.table), 
# постройте линейную регрессию, где - первая переменная - зависимая, вторая - независимая. 
# В ответ укажите значения регрессионных коэффициентов сначала intercept затем  slope.
# Десятичный разделитель - точка. В поле для ответа введите два числа, не округляйте значения, например;
# 12.434 6.2557
# У вас есть неограниченное число попыток.
# Время одной попытки: 5 mins

x <- read.table('/Users/zelimkhan/Downloads/dataset_11508_12 (2).txt')
fit <- lm(x[, 1] ~ x[, 2])
fit$coefficients

####  Step 14 of 16 ####
# Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. Только для бриллиантов класса Ideal 
# (переменная cut) c числом карат равным 0.46 (переменная carat) постройте линейную регрессию, 
# где в качестве зависимой переменной выступает price, в качестве предиктора - переменная  depth. 
# Сохраните коэффициенты регрессии в переменную fit_coef.
# Памятка:
# > fit <- lm(mpg ~ disp + wt, mtcars)
# > fit$coefficients # коэффициенты модели
# Это задание нужно решить, не используя цикл for().

library(ggplot2)

df <- diamonds
df <- subset(df, df$cut == "Ideal" & df$carat == 0.46)
fit <- lm(price ~ depth, df)
fit_coef <- fit$coefficients



####  Step 15 of 16 ####
# Напишите функцию regr.calc, которая на вход получает dataframe c двумя переменными.
# Если две переменные значимо коррелируют (p - уровень значимости для коэффициента корреляции Пирсона меньше 0.05), 
# то функция строит регрессионную модель, где первая переменная - зависимая, вторая - независимая. 
# Затем создает в dataframe новую переменную с назанием fit, где сохраняет предсказанные моделью значения зависимой переменной. 
# В результате функция должна возвращать исходный dataframe с добавленной новой переменной fit.
# Если две переменные значимо не коррелируют, то функция возвращает строчку "There is no sense in prediction"
# Примеры работы функции:
# > my_df = iris[,1:2] # на вход подаем данные iris только с переменными Sepal.Length и Sepal.Width
# > regr.calc(iris[,1:2]) # переменные значимо не коррелируют 
# [1] "There is no sense in prediction"
# > my_df = iris[,c(1,4)] # на вход подаем данные iris только с переменными Sepal.Length и Petal.Width
# > regr.calc(my_df) # переменные значимо коррелируют 

#                Sepal.Length     Petal.Width         fit
#
       1            5.1                0.2          4.955345
       2            4.9                0.2          4.955345
       3            4.7                0.2          4.955345
       .             .                  .           .
       .             .                  .           .

# Обратите внимание, при проверке вашей функции на вход будут подаваться данные с различными именами колонок.
# Ваша функция должна корректно работать в независимости от имен переменных.
# Перед тем как сдавать решение убедитесь, что ваша функция работает корректно на разных данных, с разными именами колонок.

x <- mtcars      
crt <- cor.test(x[, 1], x[, 2])
crt$p.value < 0.05
fit <- lm(x[, 1] ~ x[, 2], x)
x$fit <- fit$fitted.values


regr.calc <- function(x) {
  crt <- cor.test(x[, 1], x[, 2])
  if(crt$p.value < 0.05) {
  fit <- lm(x[, 1] ~ x[, 2], x)
  x$fit <- fit$fitted.values
  return(x)
  } else {
    return("There is no sense in prediction")
  }
}       
       
regr.calc(x)
       
####  Step 15 of 16 Памятка ####
# Постройте scatterplot по данным iris, сохранив его в переменную my_plot : 
# Ось X - переменная Sepal.Width
# Ось Y -  переменная Petal.Width
# Цвет точек - переменная Species
# Также добавьте линейное сглаживание для каждой группы наблюдений по переменной Species.
# Если Вы все сделали правильно должен получиться следующий график:

library(ggplot2)
my_plot <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, col = factor(Species))) + 
           geom_point(size = 3) +
           geom_smooth(method = lm)
                    




 ####  Step 16 of 16 Памятка ####
cor.test(mtcars$mpg, mtcars$disp) # Расчет корреляции Пирсона 
cor.test(~ mpg + disp, mtcars) # запись через формулу
cor.test(mtcars$mpg, mtcars$disp, method = "spearman") # Расчет корреляции Спирмена 
cor.test(mtcars$mpg, mtcars$disp, method = "kendall") # Расчет корреляции Кендала 
cor(iris[, -5]) # построение корреляционной матрицы
fit <- lm(mpg ~ disp, mtcars) # построение линейной регрессии 
fit$coefficients # коэффициенты регрессии 
fit$fitted.values # предсказанные значения зависимой переменной 
# При наличии одинаковых значений в переменных расчет непараметрических корреляций будет сопровождаться предупреждением 
# о невозможности рассчитать точное значение p - value.
# Если в ваших данных есть одинаковые наблюдения, но вы хотите рассчитать непараметрическую корреляцию, используйте функцию spearman_test  из пакета coin

library(coin)
spearman_test(~ mpg + disp, mtcars)

# Обратите внимание на различия в графиках. То что в первом aes() будет распространяться на все слои. 
# А то, что в aes() конкретного geom - только на него.

ggplot(mtcars, aes(mpg, disp, col = factor(am)))+
         geom_point()+
         geom_smooth()
       
ggplot(mtcars, aes(mpg, disp))+
         geom_point(aes(col = factor(am)))+
         geom_smooth()
       
ggplot(mtcars, aes(mpg, disp))+
         geom_point()+
         geom_smooth(aes(col = factor(am)))