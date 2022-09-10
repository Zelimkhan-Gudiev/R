remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

library(ggplot2)
library(psych)
library(dplyr)
library(readxl)
install.packages('QuantPsyc')
library(QuantPsyc)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")#

yt <- read.csv2("yt.csv")
yt <- read_xlsx("plan.xlsx")
#
# regression diagnostics
# 

library(ggplot2)

data(swiss)
str(swiss)



# relationships between all variables
pairs(swiss)

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point()


# Outliers

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth(method = 'lm')


# Normality of variables distributions

ggplot(swiss, aes(x = Examination)) + 
  geom_histogram()

ggplot(swiss, aes(x = Education)) + 
  geom_histogram()



####  Step 6 of 9 ####

my_vector <- c(0.027, 0.079, 0.307, 0.098, 0.021, 0.091, 0.322, 0.211, 0.069, 0.261, 0.241, 0.166, 0.283, 0.041,
               0.369, 0.167, 0.001, 0.053, 0.262, 0.033, 0.457, 0.166, 0.344, 0.139, 0.162, 0.152, 0.107, 0.255,
               0.037, 0.005, 0.042, 0.220, 0.283, 0.050, 0.194, 0.018, 0.291, 0.037, 0.085, 0.004, 0.265, 0.218, 
               0.071, 0.213, 0.232, 0.024, 0.049, 0.431, 0.061, 0.523)

ggplot(my_vector, aes(x = my_vector)) + 
  geom_histogram()


ggplot() + aes(my_vector) + 
  geom_histogram(binwidth=0.05, colour="black", fill="white")



hist(my_vector)
shapiro.test(my_vector)

hist(log(my_vector))
shapiro.test(log(my_vector))

hist(sqrt(my_vector))
shapiro.test(sqrt(my_vector))

hist(1/(my_vector))
shapiro.test(1/(my_vector))
# linearity 

ggplot(swiss, aes(x = Examination, y = Education)) + 
  geom_point() + 
  geom_smooth()

#
# можно оценивать нормальность по Q-Q plot
qqnorm(my_vector, pch = 1, frame = FALSE)
qqline(my_vector, col = "steelblue", lwd = 2)

#### ####

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


ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3) + geom_hline(y=0, col = 'red', lwd = 1)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3) + geom_hline(y=0, col = 'red', lwd = 1)


# independence of errors

ggplot(swiss, aes(x = obs_number, y = lm1_resid)) + 
  geom_point(size = 3) + geom_smooth()

ggplot(swiss, aes(x = obs_number, y = lm2_resid)) + 
  geom_point(size = 3) + geom_smooth()


# Homoscedasticity

ggplot(swiss, aes(x = lm1_fitted, y = lm1_resid)) + 
  geom_point(size = 3)

ggplot(swiss, aes(x = lm2_fitted, y = lm2_resid)) + 
  geom_point(size = 3)


# Errors Normally distributed

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

normality.test <- function(x) {
  shapiro1 <- shapiro.test(x[, 1])$p.value
  shapiro2 <- shapiro.test(x[, 2])$p.value
  pvalue <- cbind(shapiro1, shapiro2)
  vec <- paste(names(x), as.vector(pvalue))
  return(as.vector(vec))
}

normality.test(x)

shapiro1 <- shapiro.test(x[, 1])$p.value
shapiro2 <- shapiro.test(x[, 2])$p.value
