remove(list = ls())
remove()

#### Шаг 1 из 15. В этом уроке мы научимся рассчитывать основные описательные статистики в R. ####


### Скрипт урока можно скачать по ссылке:
### https://stepic.org/media/attachments/lesson/11479/Descriptive%20statistics.R



#### Шаг 3 из 15. Выберем данные для анализа mtcars, изменим тип переменных с numeric на factor и дадим имена уровням фактора. ####


?mtcars # Данный дата фрейм (ДФ) хорош тем, что в нем есть различные типы переменных
df <- mtcars
# посмотрим структуру данных.Помимо количества наблюдений (строк) и переменных (столбцов) показывает еще тип переменнных в ДФ
str(df)
# переменныt с numeric на factor указаны как numeric, что не есть хорошо.
# Изменяем ти переменных с numeric на factor
df$vs  <- factor(df$vs, labels = c("V", "S")) # labels это название уровней фактора. Когда присваеваем имена, то указывем через вектор "с" (labels = c("V", "S"))
df$am  <- factor(df$am, labels = c("Auto", "Manual"))



#### Шаг 4 из 15. Расчитываем описательные статистики для отдельных переменных. ####


median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)


# Результаты расчетов можно сохранять переменные (чтобы каждый раз не пересчитывать)
mean_disp <- mean(df$disp)

### С помощью знаний о том как обращаться к элементам вектора, то можно расчитывть описательные статистики 
### для любой подгруппы (subset) наших наблюдений (только для наблюдений, переменные которых отвечают определенным условиям)
# К элементам вектора df$mpg можно обращаться с помощью индексов df$mpg[1], df$mpg[c(1, 2, 3)], df$mpg[c(1:3)], а можно с 
# помощью логического условия mean(df$mpg[df$cyl == 6])
mean(df$mpg[df$cyl == 6])

mean(df$mpg[df$cyl == 6 & df$vs == 'V']) # можно указывать сложные условия "&", "|", "!="
sd(df$hp[df$cyl !=3 & df$am == 'Auto'])

##### Шаг 5 из 15. Задача #####
# Вновь вернемся к данным mtcars. Рассчитайте среднее значение времени разгона (qsec)
# для автомобилей, число цилиндров (cyl) у которых не равняется 3 и показатель 
# количества миль на галлон топлива (mpg) больше 20.
# Получившийся результат (среднее значение) сохраните в переменную result.


result <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])



##### Шаг 6 из 15. Функция aggregate() расичтывает > 1 (несколько) описательных статистик (ОС) для  > 1 переменной (сабсета данных) и разделяет наблюдения по значению заданной переменной #####
# Функция aggregate() рачитывает некторые ОС некоторого сабсета данных
# aggregate(x = df$hp объект который будет разбиваться на группы, для которых будут расчитываться ОС.
# Это может быть одна переменная (вектор значений) или несколько , 
# by = list(df$vs) лист группирующих переменных
# FUN = mean) функция, которую мы хотим применить.
# ... есть еще доп. аргументы (как сделать сабсет, что делать с пропущ. значениями)

#v1
mean_hp_vs <- aggregate(x = df$hp, by = list(df$vs), FUN = mean) # результат функции выводится в ДФ
colnames(mean_hp_vs) # показывает имена переменных в ДФ colnames(df)
colnames(mean_hp_vs) <- c('VS', 'Mean HP') # добавим заголовки в ДФ. 
# Функция colnames(mean_hp_vs) показывает имена переменных в ДФ colnames(df)

# v2 aggregate(x = df$hp, by = list(df$vs), FUN = mean)можно указать проще v2
aggregate(hp ~ vs, df, mean) # Сразу формируется с заголовками. df нужно указывать источник данных 

aggregate(hp ~ vs+am, df, mean) # группировка по двум переменным vs+am. 
# Вышеуказанную функцию можно записать по другому
aggregate(x = df$hp, by = list(df$vs, df$am), FUN = mean) # v2

# В вышеуказаннух примерах мы использовали тоько одну количественную переменную для расчета ОС, 
# но можно расчитать сразу для нескольких переменных с учетом разделения их на группы
aggregate(x = df[, -c(8, 9)], by = list(df$am), FUN = median)  # расчет ОС для всех переменных кроме 8 и 9 колонки

aggregate(. ~ vs+am, df, mean) # . - расчитываем для всех строк?

aggregate(df[, c(1,3)], by = list(df$am, df$vs), FUN = sd) # расчет ОС для всех 1 и 3 переменной с группировкой по двум переменным
# Вышеуказанную функцию можно записать по другому
aggregate(cbind(mpg, disp) ~ am + vs, df, sd) # v2
cbind(df$mpg, df$disp) # cbind делает таблицу с двумя колонками

my_stats <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd) # результат можно сохранять в переменные.


#### шаг 7 из 15. Задача #### 
# При помощи функции aggregate рассчитайте стандартное отклонение переменной hp (лошадиные силы)
# и переменной disp (вместимости двигателя)  у машин с автоматической и ручной коробкой передач. 
# Полученные результаты (результаты выполнения функции aggregate) сохраните в переменную descriptions_stat.

descriptions_stat <- aggregate(cbind(hp, disp) ~ am, df, sd) #v1
descriptions_stat1 <- aggregate(x = df[, c(4, 3)], by = list(df$am), FUN = sd) #v2
descriptions_stat <- aggregate(. ~ mtcars$am,mtcars[,c(3,4)],sd) #v3


#### шаг 8 из 15. Функция describe (из пакета psyh) расчитывает базовые ОС ####

install.packages('psych')
install.packages('ggplot2')
library(psych)

describe(x = df) #v1 "vs*", "am*" звездочки это предупреждение о том, что для качественных переменных ОС не расчитываются
describe(df) #v2
describe(df[, -c(8, 9)]) "vs*", "am*" # исключим качественные переменные

descr <- describe(df)


#### шаг 9 из 15. Функция describeBy (из пакета psyh) расчитывает ОС по группам (аналог aggregate) ####
descr2 <- describeBy(x = df[, -c(8, 9)], group = df$vs) # По умолчнию результат функции describeBy сохраняется в переменную с типом List
# descr2 это лист с двумя элементами
descr2$V
descr2$S
descr21 <- describeBy(x = df[, - c(8, 9)], group = df$vs, mat = T) # если добавить "mat = T", то результат 
# выполения ф. будет сохраняться в переменную с типом ДФ (descr21 это ДФ)
descr21 <- describeBy(x = df[, - c(8, 9)], group = df$vs, mat = T, digits = 1) # аргумент digits = 1 округляет 
# значения переменной до 1 знака после запятой

descr3 <- describeBy(df[, -c(8, 9)], group = df$vs, mat = T, digits = 1, fast = T) # аргумент fast = T расчитывает меньшее количество ОС

df[c(8, 9)] # качественные переменные vs  am

describeBy(df$qsec, group = list(df$vs, df$am), mat = T, digits = 1, 
           fast = T) # в большинесте ф. можно не писать x = df$qsec, а указать сразу df$qsec без х.
# аргуменнт group = list(df$vs, df$am) расчитывает ОС для гррупп

describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, 
           fast = T) # тоже самое только не в формате в ДФ (без mat = T)



#### шаг 10 из 15. Работа с пропущенными значениями (na). Некотрые ф. ведут себя по разному в зависимости от наличия пропущенных значений ####

is.na(df$mpg)
sum(is.na(df$mpg))
sum(is.na(df))

df$mpg[1:10] <- NA # удалим первые 10 значений переменной df$mpg
mean(df$mpg) # по умолчнию ф. mean не расчитывает ср. значение, т.к. имеются na.
mean(df$mpg, na.rm = T) # в большинстве стат. тестов есть агрумент, который спрашивает, что делать с na
# na.rm = T удаляет пропущенные значения.

aggregate(mpg ~ am, df, sd) # ф. aggregate по умолчанию игнориррует na.

describeBy(df$mpg ~ am) # ф. aggregate по умолчанию игнориррует na, если использовать 
# агрумент na.rm = T, то ф. удалит при расчете все наблюдения где имеются na


#### Шаг 11 из 15 ####
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

# v1
air <- subset(airquality, Month%in%c(7, 8, 9))
result <- aggregate(air$Ozone ~ Month, air, length)

# v2
ar <- subset(airquality,airquality[,5] %in% c(7,8,9))
result <-aggregate(Ozone ~ Month  , ar, length)

# v3
monthes <- subset(airquality, Month > 6)
result <- aggregate(Ozone ~ Month, monthes, length)

# v3
result <- aggregate(Ozone ~ Month, subset(airquality, Month %in% c(7,8,9)), length)

#### Шаг 12 из 15 ####
# Примените функцию describeBy к количественным переменным данных airquality, 
# группируя наблюдения по переменной Month.  Чему равен коэффициент асимметрии (skew) 
# переменной Wind в восьмом месяце?
# В графу с ответом требуется ввести только число. Десятичный разделитель - запятая: например 12,6
str(airquality)

describeBy(airquality, group = airquality$Month) # v1
describeBy(airquality, airquality$Month == 8)[['TRUE']]['Wind','skew'] # v2
describeBy(airquality$Wind, airquality$Month == 8)$'TRUE'['skew'] # v3
skew(airquality$Wind[airquality$Month == 8]) # v4

#### Шаг 13 из 15 ####
# Обратимся к встроенным данным iris. Соотнесите значения стандартного отклонения переменных.
iris

# v1
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Width)

describe(iris)['sd'] # v2
describe(iris)$sd # не очень понятно, что к чему относитися
sd(iris) # v3
subset(describe(iris), select = sd) # v4
describe(iris) # v5
describe(iris[,-c(5)])["sd"] # v6
for (v in 1:4) {sd_v[v] <-  sd(iris[,v])} # v7
# v8
z <- describe(x = iris[1:4], fast = T)
zz <- subset(z, select = sd)
# v9
sapply(iris[1:4], sd)
aggregate(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ 1, iris, sd)


#### Шаг 14 из 15 ####
# В данных iris расположите по убыванию значения медиан количественных переменных в группе virginica.
iris

# v1
median(iris$Sepal.Length[iris$Species == 'virginica'])
median(iris$Sepal.Width[iris$Species == 'virginica'])
median(iris$Petal.Length[iris$Species == 'virginica'])
median(iris$Petal.Width[iris$Species == 'virginica'])

# v2
aggregate(x= iris[-5], by = list(iris$Species), FUN=median)
names(sort(aggregate(x= iris[-5], by = list(iris$Species), FUN=median)[3,-1], dec=T))

# v3
names(sort(aggregate(. ~ Species, iris[iris$Species =='virginica',], median), d=T))

# v4
sort(sapply(iris[iris$Species =='virginica',][-5], FUN=median))

# v5
sort(aggregate(. ~ iris$Species, iris[,1:4], subset=iris$Species %in% 'virginica', median), decreasing = T)

# v6
describeBy(iris, group = iris$Species)$'virginica'['median']

# v7
aggregate(. ~ iris$Species, iris, median)

# v8
describe(iris[iris$Species == 'virginica', 1:4])['median']

# v9
describeBy(iris[,1:4], group = iris$Species)$virginica['median']


?replace