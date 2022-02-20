remove(list = ls())
remove()

?mtcars

df <- mtcars
str(df)

# Изменяем ти переменных с numeric на factor
df$vs  <- factor(df$vs, labels = c("V", "S"))
df$am  <- factor(df$am, labels = c("Auto", "Manual"))

# Расчитываем описательные статистики
median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

# Результаты расчетов сохраняем в переменные (чтобы каждый раз не пересчитывать)
mean_disp <- mean(df$disp)

# Можно расчитывть описательные статистики только для переменных, которые отвечают определенным условиям
mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == 'V'])
sd(df$hp[df$cyl !=3 & df$am == 'Auto'])


# Вновь вернемся к данным mtcars. Рассчитайте среднее значение времени разгона (qsec)
# для автомобилей, число цилиндров (cyl) у которых не равняется 3 и показатель 
# количества миль на галлон топлива (mpg) больше 20.
# Получившийся результат (среднее значение) сохраните в переменную result.


result <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])


# Функция aggregate() расичтывает описательные статистика для сабсета данных

mean_hp_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean)
colnames(mean_hp_vs) <- c('VS', 'Mean HP')
aggregate(hp ~ vs, df, mean)

aggregate(hp ~ vs+am, df, mean)
aggregate(. ~ vs+am, df, mean)
aggregate(x = df$hp, by = list(df$vs, df$am), FUN = mean)

aggregate(x = df[, -c(8, 9)], by = list(df$am), FUN = median) 

aggregate(df[, c(1,3)], by = list(df$am, df$vs), FUN = sd)
str(df)
aggregate(cbind(mpg, disp) ~ am + vs, df, sd)

cbind(df$mpg, df$disp)

my_stats <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd)


# шаг 7 из 15 
# При помощи функции aggregate рассчитайте стандартное отклонение переменной hp (лошадиные силы)
# и переменной disp (вместимости двигателя)  у машин с автоматической и ручной коробкой передач. 
# Полученные результаты (результаты выполнения функции aggregate) сохраните в переменную descriptions_stat.

descriptions_stat <- aggregate(cbind(hp, disp) ~ am, df, sd)

descriptions_stat1 <- aggregate(x = df[, c(4, 3)], by = list(df$am), FUN = sd)

# Функция describe из пакета psyh

install.packages('psych')
install.packages('ggplot2')
library(psych)

describe(df)
describe(x = df)
describe(df[, -c(8, 9)])

descr <- describe(df)

descr2 <- describeBy(x = df[, -c(8, 9)], group = df$vs)
df[c(8, 9)]
descr2$V
descr2$S

descr21 <- describeBy(x = df[, - c(8, 9)], group = df$vs, mat = T, digits = 1)
descr3 <- describeBy(df[, -c(8, 9)], group = df$vs, mat = T, digits = 1, fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), mat = T, digits = 1, 
           fast = T)

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, 
           fast = T)


# Пропущенные значения
is.na(yt$reason)
sum(is.na(yt))

### Шаг 11 из 15 
# Воспользуемся встроенными данными airquality. В новую переменную сохраните subset исходных данных, 
# оставив наблюдения только для месяцев 7, 8 и 9.
# При помощи функции aggregate рассчитайте количество непропущенных наблюдений по переменной 
# Ozone в 7, 8 и 9 месяце. Для определения количества наблюдений используйте функцию length().

# Результат выполнения функции aggregate сохраните в переменную result.

# Подсказки:

#  1. Не забудьте сделать subset, чтобы отобрать наблюдения только по нужным месяцам, 
# вам может пригодиться следующая конструкция:

x <- 5
x %in% c(3, 4, 5)
# [1] TRUE

#  2. Для подсчета числа непропущенных наблюдений воспользуйтесь записью с помощью формулы, 
# при которой пропущенные значения не учитываются:
#  aggregate(y ~ x + z , data, FUN)

air <- subset(airquality, Month%in%c(7, 8, 9))
result <- aggregate(air$Ozone ~ Month, air, length)

### Шаг 12 из 15 
# Примените функцию describeBy к количественным переменным данных airquality, 
# группируя наблюдения по переменной Month.  Чему равен коэффициент асимметрии (skew) 
# переменной Wind в восьмом месяце?
# В графу с ответом требуется ввести только число. Десятичный разделитель - запятая: например 12,6

str(airquality)
describeBy(airquality, group = airquality$Month)
describeBy(airquality, airquality$Month == 8)[['TRUE']]['Wind','skew']
describeBy(airquality$Wind, airquality$Month == 8)$'TRUE'['skew']
skew(airquality$Wind[airquality$Month == 8])

### Шаг 13 из 15 
# Обратимся к встроенным данным iris. Соотнесите значения стандартного отклонения переменных.

iris
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)
#
describe(iris)['sd']
#
SD(iris)
#
subset(describe(iris), select = sd)
#
describe(iris)
#
describe(iris[,-c(5)])["sd"]
#
for (v in 1:4) {sd_v[v] <-  sd(iris[,v])}
#
z <- describe(x = iris[1:4], fast = T)
zz <- subset(z, select = sd)
#
sapply(iris[1:4], sd)
aggregate(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ 1, iris, sd)


### Шаг 14 из 15 
# В данных iris расположите по убыванию значения медиан количественных переменных в группе virginica.

iris
function()
median(iris$Sepal.Length[iris$Species == 'virginica'])
median(iris$Sepal.Width[iris$Species == 'virginica'])
median(iris$Petal.Length[iris$Species == 'virginica'])
median(iris$Petal.Width[iris$Species == 'virginica'])
#
aggregate(x= iris[-5], by = list(iris$Species), FUN=median)
names(sort(aggregate(x= iris[-5], by = list(iris$Species), FUN=median)[3,-1], dec=T))
#
names(sort(aggregate(. ~ Species, iris[iris$Species =='virginica',], median), d=T))
#
sort(sapply(iris[iris$Species =='virginica',][-5], FUN=median))
#
sort(aggregate(. ~ iris$Species, iris[,1:4], subset=iris$Species %in% 'virginica', median), decreasing = T)
#
describeBy(iris, group = iris$Species)$'virginica'['median']
#
aggregate(. ~ iris$Species, iris, median)
#
describe(iris[iris$Species == 'virginica', 1:4])['median']
#
describeBy(iris[,1:4], group = iris$Species)$virginica['median']


?replace