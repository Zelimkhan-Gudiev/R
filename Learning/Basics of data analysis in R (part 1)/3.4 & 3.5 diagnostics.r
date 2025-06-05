remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

install.packages('QuantPsyc')
install.packages('gvlma')

library(ggplot2)
library(psych)
library(dplyr)
library(readxl)
library(QuantPsyc)
library(gvlma)


#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")#

yt <- read.csv2("yt.csv")
yt$X <- NULL
yt <- subset(yt, select = - c(X, ktd))
yt <- read_xlsx("plan.xlsx")


#### regression diagnostics ####
# В этом уроке мы разберемся с проблемой диагностики регрессионной модели!
# Скрипт урока:
# https://stepic.org/media/attachments/lesson/11510/diagnostics.R

# 
library(ggplot2)

data(swiss)
str(swiss)



#### relationships between all variables ####
pairs(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point()


#### Outliers ####

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth(method = 'lm')


#### Normality of variables distributions ####

ggplot(swiss, aes(x = Examination)) + 
  geom_histogram()

ggplot(swiss, aes(x = Education)) + 
  geom_histogram()

ggplot(swiss, aes(x = log(Education))) +
       geom_histogram(binwidth = 0.3, colour = 'black', fill = 'white')



####  Step 6 of 9 ####

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041,
               0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255,
               0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 
               0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

ggplot(my_vector, aes(x = my_vector)) + 
  geom_histogram()


ggplot() + aes(my_vector) + 
  geom_histogram(binwidth=0.04, colour="black", fill="white")



hist(my_vector)
shapiro.test(my_vector)

hist(log(my_vector))
shapiro.test(log(my_vector))

hist(sqrt(my_vector))
shapiro.test(sqrt(my_vector))

hist(1/(my_vector))
shapiro.test(1/(my_vector))

# можно оценивать нормальность по Q-Q plot
qqnorm(my_vector, pch = 1, frame = FALSE)
qqline(my_vector, col = "steelblue", lwd = 2)


#### Step 7 of 9  ####
# Функция scale() позволяет совершить стандартизацию вектора, то есть делает его среднее значение равным нулю, 
# а стандартное отклонение - единице (Z-преобразование).
# Стандартизованный коэффициент регрессии (\betaβ) можно получить, если предикторы и зависимая переменная стандартизованы.
# Напишите функцию, которая на вход получает dataframe с двумя количественными переменными, а возвращает стандартизованные 
# коэффициенты для регрессионной модели, в которой первая переменная датафрейма выступает в качестве зависимой, 
# а вторая в качестве независимой
# Примеры работы функции.
beta.coef(mtcars[,c(1,3)])
7.036582e-17 -8.475514e-01

beta.coef(swiss[,c(1,4)])
3.603749e-16 -6.637889e-01 


x <- mtcars[,c(1,3)]
beta.coef <- function(x) {
  x <- as.data.frame(scale(cbind(x[, 1], x[,2 ])))
  fit <- lm(x[, 1] ~  x[, 2], x)$coefficients
  return(fit)
}
beta.coef(x)


#### Step 8 of 9  ####
# То, что вы только что сделали, можно сделать с помощью функции lm.beta из библиотеки QuantPsyc! :)
install.packages('QuantPsyc')
library(QuantPsyc)

lm.beta(lm(x[, 1] ~  x[, 2], x))


#### Step 9 of 9  ####
# Напишите функцию normality.test, которая получает на вход dataframe с количественными переменными, 
# проверяет распределения каждой переменной на нормальность с помощью функции shapiro.test. 
# Функция должна возвращать вектор с значениями p - value, полученного в результате проверки на нормальность каждой переменной.
# Названия элементов вектора должны совпадать с названиями переменных. 

#
normality.test <- function(x) {
  shapiro <-  sapply(x, function(x) shapiro.test(x))[2,]
  return(as.vector(shapiro))
}

normality.test(x)

#
normality.test  <- function(x){    
  return(sapply(x, FUN =  shapiro.test)['p.value',])}

#
normality.test <- function(x) {
  apply(x, 2, function (i) shapiro.test(i)$p.value)
}

#
normality.test  <- function(x){
  fb <- function(x){
    return(shapiro.test(x)$p.value)
  }
  return(sapply(x, FUN = fb))
}

#

normality.test <- function(x){
  vector = c()
  for (i in 1:length(x[1,])){
    res <- shapiro.test(x[,i])$p.value
    vector <- append(vector,res)  
  }
  names(vector) <- names(x[1,])
  return(vector)
}

#
normality.test  <- function(x){
  x1 <- sapply(x, shapiro.test)
  x1[2,]
}


#### 3.2 ####

#### linearity ####

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth()

#
lm1 <- lm(Education ~ Examination, swiss)
summary(lm1)


swiss$Examination_squared <- (swiss$Examination)^2

lm2 <- lm(Education ~ Examination + Examination_squared, swiss)
summary(lm2)


anova(lm2, lm1)


swiss$lm1_fitted <- lm1$fitted
swiss$lm2_fitted <- lm2$fitted
swiss$lm1_resid <- lm1$resid
swiss$lm2_resid <- lm2$resid
swiss$obs_number <- 1:nrow(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point(size = 3) + 
  geom_line(aes(x = Examination, y = lm1_fitted), col = 'red', lwd=1) +
  geom_line(aes(x = Examination, y = lm2_fitted), col = 'blue', lwd=1)

# У меня почему-то не заработало: 
ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3) + geom_hline(y=0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_hline(y=0, col = 'red', lwd = 1)

# Пришлось написать:
ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) +
  geom_point(size = 3) + geom_hline(yintercept = 0, col = 'red', lwd = 1)


ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_hline(aes(yintercept = 0), col = 'red', lwd = 1)

# Можно даже без второй aes:
ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_hline(yintercept = 0, col = 'red', lwd = 1)


#### independence of errors ####

ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth()


#### Homoscedasticity ####

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3)


#### Step 5 of 8 ####
# Функция gvlma() из библиотеки gvlma позволяет получить оценку выполнения основных допущений линейной регрессии. 
# В качестве аргумента она принимает объект, в который сохранена модель. Можно задать формулу модели прямо в функции gvlma. 
# Чтобы увидеть основные статистики, нужно выполнить команду summary для объекта, созданного с помощью функции gvlma.
# Например,
x <- gvlma(fit)
# или
x <- gvlma(Y ~ X, data = mydata)
summary(x)

# Загрузите себе прикреплённый к этому степу датасет и постройте регрессию, предсказывающую DV по IV. 
# Установите библиотеку gvlma и проверьте, удовлетворяется ли в этой модели требование гомоскедастичности. 
# Введите в поле ответа p-значение для теста гетероскедастичности.
# Данные: https://stepic.org/media/attachments/lesson/12088/homosc.csv

df5 <- read.csv('https://stepic.org/media/attachments/lesson/12088/homosc.csv')
fit5 <- lm(DV ~ IV, df5)
summary(fit5)
x <- gvlma(fit5)
x <- gvlma(DV ~ IV, df5)
summary(x)
# 1) Global Stat <-  Являются ли отношения между вашими предсказателями X и Y  линейными?. Отклонение нулевой (p <.05) указывает на 
# нелинейную связь между одним или несколькими вашими X и Y

# 2) Skewness <- Является ли ваше распределение искаженным положительно или отрицательно, что требует преобразования, чтобы 
# соответствовать предположению о нормальности? Отклонение нулевого значения (p <.05) указывает на то, что вы, вероятно, должны 
# преобразовать свои данные.

# 3) Kurtosis <-  проверка на выбросы (остроконечность) что требует трансформации, чтобы соответствовать предположению о нормальности?
# Отклонение нулевого значения (p <.05) указывает на то, что вы, вероятно, должны преобразовать свои данные.

# 4) Link function <- Является ли ваша зависимая переменная по-настоящему непрерывной или категоричной? 
# Отклонение нулевого (p <.05) означает, что вы должны использовать альтернативную форму обобщенной линейной модели 
# (например, логистическая или биномиальная регрессия).

# 5) Heteroscedasticity <- Является ли вариация остатков вашей модели постоянной в диапазоне X (предположение о гомосексуализме)? 
# Отклонение нулевого значения (p <.05) указывает на то, что ваши остатки являются гетероседикальными и, следовательно, 
# непостоянными в диапазоне X. Ваша модель лучше/хуже при прогнозировании для определенных диапазонов ваших шкал X.


#
# Не понятно, почему предложено использовать малопопулярную библиотеку gvlma, когда R-гугл (Rseek.org), например, 
# предлагает (https://www.r-bloggers.com/how-to-detect-heteroscedasticity-and-rectify-it/) использовать NCV test (car) 
# или Breush Pagan Test (lmtest).
# Другая проблема - я лично тесты прохожу на работе, R  у меня тут нет и я использую онлайн-версию (https://try.jupyter.org/), 
# в которой уже предустановлены все наиболее важные и используемые пакеты. Так вот, пакет car (а значит и NCV test) там предустановлен, 
# а вот пакет gvlma естественно нет. Я бы понял, если бы это был курс по статистике и тут бы рассказывалась разница между выполнением 
# самих тестов. Но нет - все тесты даются как "черный ящик" и мы смотрим лишь на само p-значение. Таким образом, почему выбран 
# именно малоизвестный пакет gvlma вместо де-факто "стандартного" cars совершенно неясно, так как иные возможности 
# глобального теста gvlma в задании никак не затрагиваются.
# Результаты полученные NCV тестом естественно заданием не принимаются. Ставлю дизлайк.



#### Errors Normally distributed ####

ggplot(swiss, aes(x = lm1_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm1$residuals)
qqline(lm1$residuals)

shapiro.test(lm1$residuals)


ggplot(swiss, aes(x = lm2_resid)) + 
  geom_histogram(binwidth = 4, fill = 'white', col = 'black')

qqnorm(lm2$residuals)
qqline(lm2$residuals)

shapiro.test(lm2$residuals)



#### Step 7 of 8 ####
# Напишите функцию resid.norm, которая тестирует распределение остатков от модели на нормальность при помощи функции shapiro.test 
# и создает гистограмму при помощи функции ggplot() с красной заливкой "red", если распределение остатков значимо отличается 
# от нормального (p < 0.05), и с зелёной заливкой "green" - если распределение остатков значимо не отличается от нормального.
# На вход функция получает регрессионную модель. Функция возвращает переменную, в которой сохранен график ggplot.
# В поле для ответа не нужно создавать никаких дополнительных объектов, только напишите функцию  resid.norm.
# Для создания гистограммы при помощи функции ggplot требуется dataframe, где хранится переменная. Обратите внимание на такие функции как:

fit <- fit5
shapiro.test(fit$residuals)$p.value
resid.norm <- function(fit) {
  shapiro <- shapiro.test(fit$residuals)$p.value
  if(shapiro < 0.05) {
    my_plot <-  ggplot(as.data.frame(fit$model), aes(x = fit$residuals)) +
                  geom_histogram(binwidth = 2, fill = 'red', col = 'white')
    return(my_plot)
  } else
    my_plot <-  ggplot(as.data.frame(fit$model), aes(x = fit$residuals)) +
      geom_histogram(binwidth = 2, fill = 'green', col = 'white')
  return(my_plot)
}

resid.norm(fit)

#1
resid.norm <- function(fit) {
  res <- fit$resid
  s <- shapiro.test(res)$p.value
  colorH <- ''
  ifelse(s < 0.05, colorH <- 'red', colorH <- 'green')
  my_plot <-  ggplot(as.data.frame(fit$model), aes(x = res)) +
      geom_histogram(binwidth = 2, fill = colorH, col = 'white')
    return(my_plot)
}

#2
resid.norm <- function(fit) {
  shapiro <- shapiro.test(fit$residuals)$p.value
  colorH <- ''
  ifelse(shapiro < 0.05, colorH <- 'red', colorH <- 'green')
  my_plot <-  ggplot(as.data.frame(fit$model), aes(x = fit$residuals)) +
    geom_histogram(binwidth = 2, fill = colorH, col = 'white')
  return(my_plot)
}


#### Step 8 of 8 ####

# Ещё одной проблемой регрессионных моделей может стать мультиколлинеарность - ситуация, когда предикторы очень сильно коррелируют между собой. 
# Иногда корреляция между двумя предикторами может достигать 1, например, когда два предиктора - это одна и та же переменная, 
# измеренная в разных шкалах (x1 - рост в метрах, x2 - рост в сантиметрах)  
# Проверить данные на мультиколлинеарность можно по графику pairs() и посчитав корреляцию между всеми предикторами c помощью функции cor.

# Напишите функцию high.corr, которая принимает на вход датасет с произвольным числом количественных переменных и возвращает вектор 
# с именами двух переменных с максимальным абсолютным значением коэффициента корреляции .
# Примеры работы функции:
high.corr(swiss)
# [1] "Examination" "Education"
x1 <- rnorm(30) # создадим случайную выборку
x2 <- rnorm(30) # создадим случайную выборку
x3  <- x1 + 5 # теперь коэффициент корреляции x1 и x3 равен единице
my_df <- data.frame(var1 = x1, var2 = x2, var3 = x3)
high.corr(my_df)
# [1] "var1" "var3"
# Вам могут понадобиться следующие функции: which, dimnames, colnames, rownames, diag, abs. Посмотрите справку по ним.
# Подсказки: Далеко не всегда 1 == 1 есть ТRUE! Смотри закрепленный комментарий или можно почитать о проблеме здесь.


high.corr <- function (x){
  t <- x[,sapply(x,  is.numeric)] # очищаем наш дата фрейм от не числовых данных
  r <- cor(t) # выводим таблицу в попарными коэффициентами корреляции Пирсона
  r <- upper.tri(r,diag = F) * r # оставляем в таблице только элементы из верхней диагонали, так как ниижние элементы такие же и они не нужны
  mr <- r[which.max(abs(r))] # находим максимальный коэф Пирсона в нашей урезанной таблице
  ind <- which(r==mr, arr.ind=TRUE) # сюда пишем индексы Строки и Колонки в которых содержится наш максимальный коэф Пирсона mr
  print <- c(rownames(r)[ind[,"row"]],  colnames(r)[ind[,"col"]]) # выводим вектор с названием Строки и Колонки где лежит наш mr
  return(print)
}
high.corr(x)

#
high.corr <- function(x){    
  cr <- cor(x)    
  diag(cr) <- 0    
  return(rownames(which(abs(cr)==max(abs(cr)), arr.ind=T)))}


high.corr <- function(x){
  x1 <- cor(x)
  diag(x1) <- 0
  x2 <- which.max(abs(x1))
  x31 <- ifelse(x2 %% length(x1[1,]) == 0, x2 / length(x1[1,]),
                x2 %/% length(x1[1,]) + 1)
  x32 <- ifelse(x2 %% length(x1[1,]) == 0, length(x1[1,]),
                x2 %% length(x1[1,]))
  return (c(colnames(x1)[x31], rownames(x1)[x32]))
  
}
