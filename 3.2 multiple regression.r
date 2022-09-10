remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

library(ggplot2)
library(psych)
library(dplyr)
library(readxl)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")#

yt <- read.csv2("yt.csv")
yt <- read_xlsx("plan.xlsx")


# multiple linear regression
#

# numeric predictors

fit <- lm(Fertility ~ Examination + Catholic, data = swiss)
summary(fit)


fit2 <- lm(Fertility ~ Examination*Catholic, data = swiss)
summary(fit2)


confint(fit2)

# yt

ytFit <- lm(duration ~ kind_tz, yt)
summary(ytFit)

aggregate(duration ~ kind_tz, yt, mean)

names(yt)

ytFut <- lm(duration ~ ., yt)


##### Step 5 of 15 #####
# Напишите функцию fill_na, которая принимает на вход данные с тремя переменными:
# x_1  -  числовой вектор
# x_2 - числовой вектор
# y - числовой вектор с пропущенными значениями.
# Теперь — самое интересное. На первом этапе, используя только наблюдения, в которых нет пропущенных значений, 
# мы построим регрессионную модель (без взаимодействий), где  y — зависимая переменная, x_1 и x_2 — независимые переменные.
# Затем, используя построенную модель, мы заполним пропущенные значения предсказаниями модели.
# Функция должна возвращать dataframe c новой переменной  y_full. Сохраните в нее переменную y, 
# в которой пропущенные значения заполнены предсказанными значениями построенной модели.

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
# В переменной df сохранен subset данных mtcars только с переменными "wt", "mpg", "disp", "drat", "hp". 
# Воспользуйтесь множественным регрессионным анализом, чтобы предсказать вес машины (переменная "wt"). 
# Выберите такую комбинацию независимых переменных (из "mpg", "disp", "drat", "hp"), чтобы значение R^2 adjusted было наибольшим. 
# Взаимодействия факторов учитывать не надо. 
# Выполните все операции по сравнению моделей на вашем компьютере.
# В поле для ответа сохраните в переменную  model регрессионную модель с оптимальной комбинацией предикторов!


df <- select(df, wt, mpg, disp, drat, hp)
      model <- lm(wt ~, df)
model <- lm(wt ~  mpg + disp + hp, df)


#2
df1 <- mtcars[,c("wt", "mpg", "disp", "drat", "hp")]
fit_full <- lm(wt ~ ., data = df1)
summary(fit_full)
optimal_fit <-  step(fit_full, direction = 'backward')
opt_summary <- summary(optimal_fit)
attr(as.formula(opt_summary), "term.labels")


#3
vars <- c("mpg", "disp", "drat", "hp",
                "mpg + disp", "mpg + drat", "mpg + hp",
                "disp + drat", "disp + hp", "drat+ hp",
                "mpg + disp + drat", "mpg + drat + hp",
                "mpg + disp + hp", "disp + drat + hp",
                "mpg + disp + drat + hp")
adj.r.sqrt <- sapply(vars, function(i) summary(lm(paste0("wt ~ ", i), df))$adj.r.squared)
names(adj.r.sqrt)[which.max(adj.r.sqrt)]
      
#4
# ссылка на коммент ниже, кот-й помог решить без перебора вариантов:
# https://stepik.org/lesson/11509/step/6?discussion=82172&reply=82441&unit=2532
# подробней:
#  1) установить пакет leaps
library(leaps)
b <- regsubsets(wt~., data = df, nbest = 4)
#(все комбинации для 4-х независимых переменных + Intercept)
plot(b, scale = "adjr2")
# выбираем независимые переменные из верхнего (наибольшее adjr2) значения графика и подставляем их в модель:
model <- lm(wt ~ mpg + disp + hp, df)
# Ну и можно заглянуть в конец урока - файла (я не догадался ( ), тогда через 
fit_full  <-  lm(wt ~., df)  # регрессия по всем независимым переменным
optimal_fit <-  step(fit_full, direction = 'backward')
      
#5
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
      
# categorical predictors

hist(swiss$Catholic, col = 'red')

swiss$religious <- ifelse(swiss$Catholic > 60, 'Lots', 'Few')
swiss$religious <- as.factor(swiss$religious)

fit3 <- lm(Fertility ~ Examination + religious, data = swiss)
summary(fit3)

fit4 <- lm(Fertility ~ religious*Examination, data = swiss)
summary(fit4)



####  Step 7 of 15 ####

# Воспользуйтесь встроенным датасетом attitude, чтобы предсказать рейтинг (rating) по переменным complaints и critical. 
# Каково t-значение для взаимодействия двух факторов?
# Разделителем целой и дробной части в ответе должна быть запятая!

df7 <- attitude
names(df7)
fit <- lm(rating ~ complaints * critical, df7)
summary(fit)



# plots

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility)) + 
  geom_point() + 
  geom_smooth(method = 'lm')

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point() 

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth()

ggplot(swiss, aes(x = Examination, y = Fertility, col = religious)) + 
  geom_point()  + 
  geom_smooth(method = 'lm')


#

fit5 <- lm(Fertility ~ religious*Infant.Mortality*Examination, data = swiss)
summary(fit5)


# model comparison

rm(swiss)
swiss <- data.frame(swiss)

fit_full <- lm(Fertility ~ ., data = swiss)
summary(fit_full)

fit_reduced1 <- lm(Fertility ~ Infant.Mortality + Examination + Catholic + Education, data = swiss)
summary(fit_reduced1)

anova(fit_full, fit_reduced1)

fit_reduced2 <- lm(Fertility ~ Infant.Mortality + Education + Catholic + Agriculture, data = swiss)
summary(fit_reduced2)

anova(fit_full, fit_reduced2)


# model selection

optimal_fit <-  step(fit_full, direction = 'backward')
summary(optimal_fit)




####  Step 13 of 15 ####

# В этом примере будем работать с хорошо вам известным встроенным датасетом mtcars. 
# Переменная am говорит о том, какая коробка передач используется в машине: 0 - автоматическая, 1 - ручная.
# Сделаем эту переменную факторной.
mtcars <- mtcars
str(mtcars)
mtcars$am <- factor(mtcars$am, labels = c('Automatic', 'Manual'))
# Теперь постройте линейную модель, в которой в качестве зависимой переменной выступает расход топлива (mpg), 
# а в качестве независимых - вес машины (wt) и коробка передач (модифицированная am), а также их взаимодействие. 
# Выведите summary этой модели.
# Что отражает значение intercept в данной модели?

fit13 <- lm(mpg ~ wt * am, mtcars)
summary(fit13)


ggplot(mtcars, aes(y = mpg, x = wt, col = am)) +
  geom_point() +
  geom_smooth(method = 'lm')

####  Step 14 of 15 ####

####  Step 15 of 15 ####

# Визуализируйте взаимодействие переменных wt и am, дополнив код, приведённый в задании:
# Ось x - переменная wt
# Ось y - переменная mpg
# Цвет регрессионных прямых - переменная am
# У вас должно получиться следующее изображение:

mtcars$am <- factor(mtcars$am)

# теперь строим график
my_plot <- ggplot(mtcars, aes(y = mpg, x = wt, col = am)) +
           geom_smooth(method = 'lm')


