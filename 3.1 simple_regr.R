remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

library(ggplot2)
library(psych)
library(dplyr)
library(psych)


#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("")
setwd("")


# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»

#### Step 1,2: Contens  _______________________________________________________________________________________________________ ####
# В этом уроке мы научимся рассчитывать коэффициенты корреляции и познакомимся с простой линейной регрессией.
# Ссылка на скрипт урока:
# Корреляция https://stepic.org/media/attachments/lesson/11508/cortest.R
# Регрессия https://stepic.org/media/attachments/lesson/11508/simple_regr.R


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


####  Step 12 of 16 ####
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

####  Step 13 of 16 ####
# Скачайте набор данных - dataframe с двумя количественными переменными (вспомните при необходимости, как задавать разделитель и другие параметры функции read.table), постройте линейную регрессию, где - первая переменная - зависимая, вторая - независимая. В ответ укажите значения регрессионных коэффициентов сначала intercept затем  slope.
# Десятичный разделитель - точка. В поле для ответа введите два числа, не округляйте значения, например;
# 12.434 6.2557
# У вас есть неограниченное число попыток.
# Время одной попытки: 5 mins

####  Step 14 of 16 ####
# Воспользуемся уже знакомыми данными diamonds из библиотеки ggplot2. Только для бриллиантов класса Ideal 
# (переменная cut) c числом карат равным 0.46 (переменная carat) постройте линейную регрессию, 
# где в качестве зависимой переменной выступает price, в качестве предиктора - переменная  depth. 
# Сохраните коэффициенты регрессии в переменную fit_coef.
# Памятка:
# > fit <- lm(mpg ~ disp + wt, mtcars)
# > fit$coefficients # коэффициенты модели
# Это задание нужно решить, не используя цикл for().


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

##### 3.2 ####
x <- test_data <- read.csv("https://stepic.org/media/attachments/course/129/fill_na_test.csv")
####  Step 5 of 15 ####
# не правильная функция 
fill_na <- function(x) {
  fit <- lm(y ~ x_1 + x_2, x)
  x$y_full <- predict(fit, subset(x, y != "NA"))
  x$y_full <- ifelse(is.na(x$y), x$y_full, x$y)
  return(x)
}

# не правильная функция
fill_na <- function(x) {
  fit <- lm(y ~ x_1 + x_2, x)
    x$y_full <- ifelse(is.na(x$y), predict(fit, x), x$y)
  return(x)
}

fill_na(x)

####  Step 6 of 15 ####
mtcars <- mtcars
names(mtcars)
which(names(mtcars) %in% с(mtcars$wt, mtcars$mpg, mtcars$disp, mtcars$drat, mtcars$hp)
df <- mtcars[, c(mtcars$wt, mtcars$mpg, mtcars$disp, mtcars$drat, mtcars$hp)]
df <- mtcars[, c(mtcars$wt, mtcars$mpg, mtcars$disp, mtcars$drat, mtcars$hp)]
which(mtcars, с(mtcars$wt, mtcars$mpg, mtcars$disp, mtcars$drat, mtcars$hp)

df <- select(mtcars, wt, mpg, disp, drat, hp)
#1
model <- lm(wt ~ wt + mpg + disp + drat + hp, df)
summary(model)
Multiple R-squared:  0.8584,	Adjusted R-squared:  0.8374

#2
model <- lm(wt ~ wt + mpg + disp + drat, df)
summary(model)
Multiple R-squared:  0.8407,	Adjusted R-squared:  0.8236    

#3
model <- lm(wt ~ wt + mpg + disp + hp, df)
summary(model)
Multiple R-squared:  0.858,	Adjusted R-squared:  0.8428 

#4
model <- lm(wt ~ wt + mpg + drat + hp, df)
summary(model)
Multiple R-squared:  0.7804,	Adjusted R-squared:  0.7568 

#5
model <- lm(wt ~ wt + disp + drat + hp, df)
summary(model)
Multiple R-squared:  0.8038,	Adjusted R-squared:  0.7828 

#6
model <- lm(wt ~ mpg + disp + drat + hp, df)
summary(model)
Multiple R-squared:  0.8584,	Adjusted R-squared:  0.8374


###

df1 <- mtcars[,c("wt", "mpg", "disp", "drat", "hp")]
fit_full <- lm(wt ~ ., data = df1)
optimal_fit <-  step(fit_full, direction = 'backward')
opt_summary <- summary(optimal_fit)
attr(as.formula(opt_summary), "term.labels")


### 

summary(lm(wt ~ mpg + disp + drat + hp, df))

#Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.364821   0.963395   4.531 0.000107 ***
  mpg         -0.079981   0.024787  -3.227 0.003272 ** 
  disp         0.005085   0.001318   3.857 0.000645 ***
  drat        -0.054984   0.206363  -0.266 0.791922    
hp          -0.003519   0.001914  -1.839 0.076967 . 

summary(lm(wt ~ mpg + disp + hp, df)) #убрал drat как не значащую

#Coefficients:
Estimate Std. Error t value Pr(>|t|)    
(Intercept)  4.200313   0.727164   5.776 3.35e-06 ***
  mpg         -0.082135   0.023040  -3.565  0.00133 ** 
  disp         0.005245   0.001155   4.543 9.67e-05 ***
  hp          -0.003702   0.001757  -2.107  0.04416 *
  
  
  
### 
# ссылка на коммент ниже, кот-й помог решить без перебора вариантов:
# https://stepik.org/lesson/11509/step/6?discussion=82172&reply=82441&unit=2532
# подробней:
# 1) установить пакет leaps
install.packages('leaps')
library(leaps)
b <- regsubsets(wt~., data = df, nbest = 4)
# #(все комбинации для 4-х независимых переменных + Intercept)
plot(b, scale = "adjr2")
# 4) выбираем независимые переменные из верхнего (наибольшее adjr2) значения графика и подставляем 
их в модель:
model <- lm(wt~mpg+disp+hp,df)
# Ну и можно заглянуть в конец урока - файла (я не догадался ( ), тогда через 
fit_full  <-  lm(wt ~., df)  # регрессия по всем независимым переменным
optimal_fit <-  step(fit_full, direction = 'backward')


###

> vars <- c("mpg", "disp", "drat", "hp",
            "mpg + disp", "mpg + drat", "mpg + hp",
            "disp + drat", "disp + hp", "drat+ hp",
            "mpg + disp + drat", "mpg + drat + hp",
            "mpg + disp + hp", "disp + drat + hp",
            "mpg + disp + drat + hp")
> adj.r.sqrt <- sapply(vars, function(i) summary(lm(paste0("wt ~ ", i), df))$adj.r.squared)
> names(adj.r.sqrt)[which.max(adj.r.sqrt)]

# Много тут писалось о том, что надо идти либо по stepwise forward, либо по stepwise backward подходам, 
# но эти подходы являются "жадными" и потому не гарантируют оптимального решения. 
# С текущим знанием языка R, а также с помощью интернетов пришёл к такому "перебору". 
# Его, разумеется, можно и улучшить, но задачу свою выполнил.

df <- mtcars[,c("wt", "mpg", "disp", "drat", "hp")]
cols <- c("mpg", "disp", "drat", "hp")
max_adj_r <- 0
els <- ""

for(n in 1:length(cols)){
  for(xs in combn(c("mpg", "disp", "drat", "hp"), n, simplify = F)){
    frml = paste(xs, collapse = ' + ')
    lin_mod = lm(eval(paste0("wt ~ ", frml)), df)
    sum = summary(lin_mod)
    print(paste(frml, sum$adj.r.squared))
    if(sum$adj.r.squared > max_adj_r){
      max_adj_r <- sum$adj.r.squared
      els <- frml
    }
  }
}
print("")
print(paste("Max adj. R^2 is", max_adj_r, "for elements", els))




###
install.packages("olsrr")
library(olsrr)
m <- lm(wt ~ ., data = df)
ols_step_both_p(m)
