remove(list = ls())

#### Contens ####

# В этом уроке мы научимся рассчитывать основные описательные статистики в R.
# Скрипт урока можно скачать по ссылке:
# https://stepic.org/media/attachments/lesson/11479/Descriptive%20statistics.R



#### Packages and librarys ####

install.packages("dplyr")
library(dplyr)
install.packages("psych")
library(psych)



#### Step 2: Data preprocessing _______________________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")

?mtcars

df  <- mtcars
yt <- read.csv2("yt.csv")
# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»


str(df)

df$vs  <- factor(df$vs, labels = c("V", "S")) # изменим тип у переменной df$vs с numeric на factor. labels = c("V", "S") 
# присваеваем названия урровням фактора.
df$am  <- factor(df$am, labels = c("Auto", "Manual")) # Обратить внимание, что когда мы присваеваем имена мы пишем labels 
# равнятеся вектору labels = c("Auto", "Manual")



#### Step 3: Descriptive statistics (расчитываем описательные статистики для определенных перменных) __________________________________________ ####

median(df$mpg)
mean(df$disp)
sd(df$hp)
range(df$cyl)

mean_disp  <- mean(df$disp)

# можно расчитыват описательные статистики любого подмножества дата фрейма

mean(df$mpg[df$cyl == 6]) # среднее значение df$mpg у авто, количество цилиндров которых равно 6
mean(df$mpg[df$cyl == 6 & df$vs == "V"]) # среднее значение df$mpg у авто, количество цилиндров которых равно 6 и тип двигателя V
sd(df$hp[df$cyl != 3 & df$am == "Auto"]) # стандартное отклонение у авто, количество цилиндров которых не равно 3 и тип КПП равно автоматическая



#### yt (Descriptive statistics) ______________________________________________________________________________________________________________ ####

sum(is.na(yt$duration))
mean(yt$duration, na.rm = T)
sd(yt$duration, na.rm = T)

bad_duration <- yt$ktd[yt$duration > (mean(yt$duration, na.rm = T) + sd(yt$duration, na.rm = T))]
bad_numb_ret_depir <- yt$ktd[yt$numb_ret_depir > (mean(yt$numb_ret_depir, na.rm = T) + sd(yt$numb_ret_depir, na.rm = T))]
bad_numb_ret_oiv <- yt$ktd[yt$numb_ret_oiv > (mean(yt$numb_ret_oiv, na.rm = T) + sd(yt$numb_ret_oiv, na.rm = T))]

subset(yt, duplicated(c(bad_duration, bad_numb_ret_depir, bad_numb_ret_oiv)), select = ktd)
?"duplicate"
duplicate


mean(yt$duration[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])
sd(yt$duration[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])

mean(yt$numb_ret_depir[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])
sd(yt$numb_ret_depir[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])

mean(yt$numb_ret_oiv[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])
sd(yt$numb_ret_oiv[yt$deputy == "Чурсина Мария Вячеславовна" & yt$reason != 'Дорожная карта и план по стандартизации'])

## _________________________________________________________________ ##
mean(yt$duration[yt$deputy == "Гудиев Зелимхан Куйраевич"])
sd(yt$duration[yt$deputy == "Гудиев Зелимхан Куйраевич"])

mean(yt$numb_ret_depir[yt$deputy == "Гудиев Зелимхан Куйраевич"])
sd(yt$numb_ret_depir[yt$deputy == "Гудиев Зелимхан Куйраевич"])

mean(yt$numb_ret_oiv[yt$deputy == "Гудиев Зелимхан Куйраевич"])
sd(yt$numb_ret_oiv[yt$deputy == "Гудиев Зелимхан Куйраевич"])

## _________________________________________________________________ ##

mean(yt$duration[yt$deputy == "Магамгазиев Расул Висхаджиевич"])
sd(yt$duration[yt$deputy == "Магамгазиев Расул Висхаджиевич"])

mean(yt$numb_ret_depir[yt$deputy == "Магамгазиев Расул Висхаджиевич"])
sd(yt$numb_ret_depir[yt$deputy == "Магамгазиев Расул Висхаджиевич"])

mean(yt$numb_ret_oiv[yt$deputy == "Магамгазиев Расул Висхаджиевич"])
sd(yt$numb_ret_oiv[yt$deputy == "Магамгазиев Расул Висхаджиевич"])

## _________________________________________________________________ ##




#### Step 4 of 15. Задача _____________________________________________________________________________________________________________________ ####
# Вновь вернемся к данным mtcars. Рассчитайте среднее значение времени разгона (qsec) для автомобилей,
# число цилиндров (cyl) у которых не равняется 3 и показатель количества миль на галлон топлива (mpg) больше 20.
# Получившийся результат (среднее значение) сохраните в переменную result.

# Вариант 1
df <- mtcars
result <- mean(df$qsec[df$cyl != 3 & df$mpg > 20])

# Вариант 2
result = mean(mtcars$qsec['cyl' != 3 & 'mpg' > 20])



#### yt (Step 4 of 15). Задача ________________________________________________________________________________________________________________ ####

mean(yt$duration[yt$year_plan_st == 2020], na.rm = T)
sd(yt$duration[yt$year_plan_st == 2020], na.rm = T)
mean(yt$duration[yt$year_plan_st == 2021], na.rm = T)
sd(yt$duration[yt$year_plan_st == 2021], na.rm = T)

     
#### Step 5: Aggregation (расчитывает некоторые описательные статистики (ОС) для некоторого подмножества данных) ______________________________ ####

?aggregate

### расчитаем ОС для одной переменной ###

mean_hp_vs  <- aggregate(x = df$hp, by = list(df$vs), FUN = mean) # В ДФ созданном по итогам выполнения функции переменные не имеют наименований
# x = df$hp - подмножество для расчета ОС
# by = list(df$vs) - указываем переменную для разбиения на группы
# FUN = mean - указываем фукцию, которую мы хотим применить

# В ДФ mean_hp_vs у переменным нет наименований 
colnames(mean_hp_vs)  <- c("VS", "Mean HP") # укажем наименования переменных

### Вышеуказанные операции можно соращать в сокращенном виде (в виде формулы).###
aggregate(hp ~ vs, df, mean) # В ДФ созданном по итогам выполнения функции переменные имеют наименовани
#  hp - подмножество для расчета ОС
# ~ vs - указываем переменную для разбиеня на группы
# df - указываем ДФ
# mean - указываем фукцию, которую мы хотим применить

aggregate(hp ~ vs + am, df, mean) # разбиваем с учетом двух переменных vs + am
aggregate(x = df$hp, by = list (df$vs, df$am), FUN = mean) # тоже самое в не сокращенной версии


### расчитаем ОС для более одной переменной не в сокращенном виде ###
aggregate(x = df[,-c(8,9)], by = list(df$am), FUN = median) # расчитаем ОС не для одной переменной как выше, а для всех количественныз переменных
aggregate(df[,c(1,3)], by = list(df$am, df$vs), FUN = sd) # расчитаем ОС для 1 и 3 переменной с разбиением на группы по тиипу КПП (df$am) и типу двинателя (df$vs)

### расчитаем ОС для более одной переменной в сокращенном виде ###

aggregate(cbind(mpg, disp) ~ am + vs, df, sd)
my_stats  <- aggregate(cbind(mpg, disp) ~ am + vs, df, sd) # cbind(mpg, disp) создает лист(?) из двух векторов (переменных)



#### yt (Aggregation) __________________________________________________________________________________________________________________________ ####

aggregate(yt[, c("duration", "numb_ret_depir", "numb_ret_oiv")], by = list(yt$deputy), FUN = mean)
aggregate(cbind(duration, numb_ret_depir, numb_ret_oiv) ~ deputy, yt, mean)

aggregate(cbind(duration, numb_ret_depir, numb_ret_oiv) ~ deputy, subset(yt, 
          reason %in% c('План по стандартизации', 'Поручение ДЭПиР или руководства')), mean)



#### Step 7 of 15 _____________________________________________________________________________________________________________________________ ####
# При помощи функции aggregate рассчитайте стандартное отклонение переменной hp (лошадиные силы) 
# и переменной disp (вместимости двигателя)  у машин с автоматической и ручной коробкой передач. 
# Полученные результаты (результаты выполнения функции aggregate) сохраните в переменную descriptions_stat.

# Вариант 1
descriptions_stat <- aggregate(cbind(hp, disp) ~ am, mtcars, sd)
# Вариант 2
descriptions_stat <- aggregate(. ~ mtcars$am, mtcars[, c(3, 4)], sd)
# Вариант 3
descriptions_stat = aggregate(x = mtcars[, c('hp', 'disp')], by = list(mtcars$am), FUN = sd)
# Вариант 4
descriptions_stat <- aggregate(x = mtcars[, c(3, 4)], by = list(mtcars$am), FUN = sd)



#### yt (Step 7 of 15) ________________________________________________________________________________________________________________________ ####

aggregate(cbind(duration, numb_ret_depir, numb_ret_oiv) ~ deputy, yt, sd)

#### Step 8 of 15: Library "psych". Функция describe (расчитывает базовые ОС) ####
# install.packages("psych")
# library(psych)

?describe
describe(x = df) # ОС для всех переменных, которые содержатся в df
descr  <- describe(x = df[,-c(8,9)]) # ОС для всех переменных, которые содержатся в df, кроме двух качественных переменных под номером 8 и 9


#### yt (describe) ____________________________________________________________________________________________________________________________ ####
describe(yt[, c('duration', 'numb_ret_depir', 'numb_ret_oiv')])



#### Step 9 of 15: Library "psych". Функция describeBy (расчитывает базовые ОС по группам) ####


descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs) # ОС для грруппы с V образным двигателем и для группы с S образным двигателем
# результат выполнения describeBy сохраняется в лист в двумя элементами descr2
descr2$V
descr2$S

# если указать mat = T, результат выполнения describeBy сохраняется в ДФ
# если указать digits = 1, то значения ОС будут окрруглены до 1 знака после запятой
descr2  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1)

# если указать fast = T, то расчитывается меньший набор ОС
descr3  <- describeBy(x = df[,-c(8,9)], group = df$vs, mat = T, digits = 1, fast = T)

# можно расчитывать ОС для групп, сформированным по двум переменным group = list(df$vs, df$am)
describeBy(df$qsec, group = list(df$vs, df$am), digits = 1, 
           fast = T)

#### yt (describeBy)___________________________________________________________________________________________________________________________ ####

describeBy(cbind(yt$duration, yt$numb_ret_depir, yt$numb_ret_oiv), group = yt$deputy)
describeBy(x = yt[, c("duration", "numb_ret_depir", "numb_ret_oiv")], group = yt$deputy)



#### Step 10 of 15: NA values  ________________________________________________________________________________________________________________ ####
# некотрые функции ведут себя по разному в зависимости от того имеются ли в переменной пропущенные значения.

is.na(df) # функция is.na на вход принимает вектор, а на выход логический вектор с указанием того является какое-нибудь значение пропущенным TRUE
sum(is.na(df)) # показывает количетво пропущенных значений в векторе или ДФ

df$mpg[1:10]  <- NA # укажем, что первые 10 значений df$mpg это пропущенные значения

mean(df$mpg) # ф. mean по умолчанию не может расчитать ср. значение, т.к. имеются пропущенные значения
mean(df$mpg, na.rm = T) # если указать na.rm = T, то ф. mean уберет пропущенные значения и расчитает ср. значение для оставшихся данных

aggregate(mpg ~ am, df, sd) # aggregate по умолчанию игнорирует na

describe( ) # describe по умолчанию игнорирует na
describe(na.rm = ) # если в ф. describe указать na.rm = T, то ф. удалит все строчки для расчета всех ОС (это очень экстримально)

#### yt (Step 10 of 15: NA values) ____________________________________________________________________________________________________________ ####

is.na(yt$duration)
sum(is.na(yt$duration))



#### Step 11 of 15 ____________________________________________________________________________________________________________________________ ####
# Воспользуемся встроенными данными airquality. В новую переменную сохраните subset исходных данных, 
# оставив наблюдения только для месяцев 7, 8 и 9.
# При помощи функции aggregate рассчитайте количество непропущенных наблюдений по переменной Ozone 
# в 7, 8 и 9 месяце. Для определения количества наблюдений используйте функцию length().
# Результат выполнения функции aggregate сохраните в переменную result.
# Подсказки:
# 1. Не забудьте сделать subset, чтобы отобрать наблюдения только по нужным месяцам, вам может пригодиться следующая конструкция:
# > x <- 5
# > x %in% c(3, 4, 5)
# [1] TRUE
# 2. Для подсчета числа непропущенных наблюдений воспользуйтесь записью с помощью формулы,
# при которой пропущенные значения не учитываются: aggregate(y ~ x + z , data, FUN)

# Вариант 1
air <- subset(airquality, Month%in%c(7, 8, 9))
result <- aggregate(air$Ozone ~ Month, air, length)

# Вариант 2
result <- aggregate(Ozone ~ Month, airquality, subset = Month %in% c(7,8,9), length) 

# Вариант 3
ar <- subset(airquality,airquality[,5] %in% c(7,8,9))
result <-aggregate(Ozone ~ Month  , ar, length)

# Вариант 4
monthes <- subset(airquality, Month > 6)
result <- aggregate(Ozone ~ Month, monthes, length)

# Вариант 5
result <- aggregate(Ozone ~ Month, subset(airquality, Month %in% c(7,8,9)), length)

# Вариант 6
result <- aggregate(Ozone ~ Month, subset(airquality, Month %in% c(7:9)), length)

# Вариант 7
subset <- airquality[airquality$Month == 7 | airquality$Month == 8 | airquality$Month == 9,]
result <- aggregate(Ozone ~ Month, subset, length)

# Вариант 8
library(dplyr)
result <- airquality %>% 
  filter(Month %in% c(7, 8, 9), !is.na(Ozone)) %>% 
  group_by(Month) %>% 
  summarize(qOzone = length(Ozone))

# Вариант 9
result <- aggregate (Ozone ~ Month, subset(airquality, Month >= 7 & Month <= 9), length)

# Вариант 10
sub1 <- subset(airquality, Month %in% c(7,8,9)) 
result <- aggregate(x = sub1$Ozone ,by = list(sub1$Month), FUN = function(x) length(na.omit(x))) 

# Вариант 11
result <- aggregate(Ozone~Month, airquality[airquality$Month %in% (7:9),], length)

# Вариант 12
month_789 <-  subset(airquality, Month %in% 7:9)
result <- aggregate(Ozone ~ Month, month_789, length)



#### yt Step 11 of 15 _________________________________________________________________________________________________________________________ ####

result <- aggregate(Ozone ~ Month, airquality, subset = Month %in% c(7,8,9), length) 
road_map <- aggregate(duration ~ deputy, yt, subset = reason %in% 'Дорожная карта и план по стандартизации', length) 
  
subset(yt, reason %in% c('Дорожная карта и план по стандартизации', 'Дорожная карта'))



#### Step 12 of 15 ____________________________________________________________________________________________________________________________ ####
# Примените функцию describeBy к количественным переменным данных airquality, группируя наблюдения по переменной Month. 
# Чему равен коэффициент асимметрии (skew) переменной Wind в восьмом месяце?
# В графу с ответом требуется ввести только число. Десятичный разделитель - запятая: например 12,6

str(airquality)
describeBy(yt$duration, yt$teamleader)

# Вариант 1
describeBy(airquality, group = 'Month') # 0.04

# Вариант 2
describeBy(airquality, airquality$Month == 8)[['TRUE']]['Wind','skew']
# Вариант 3
describeBy(airquality$Wind, airquality$Month == 8)$'TRUE'['skew']
# Вариант 4
describeBy(airquality$Wind, group=airquality$Month == 8)
# Вариант 5
describeBy(x = airquality, na.rm=T, group = airquality$Month, mat = T)
# Вариант 6
describeBy(x = subset(airquality, Month==8)$Wind, group = subset(airquality, Month == 8)$Month, digits = 1)
# Вариант 7
subset(describeBy(airqualityWind, group = airqualityWind,group=airqualityMonth == 8, mat = T), group1 == 'TRUE')['skew']
# Вариант 8
round(skew(airquality$Wind[airquality$Month == 8], na.rm = T),2)
# Вариант 9
describeBy(airquality,group = list(airquality$Month))
# Вариант 10
aggregate(Wind ~ Month, subset(airquality, Month %in% c(8)), skew)
# Вариант 11
d3 <- describeBy(x = airquality, group = airquality$Month)
d3$`8`[3, "skew"]
# Вариант 12
aggregate(Wind ~ Month, airquality, skew)
# Вариант 13
skew <- describeBy(airquality$Wind, airquality$Month==8)$'TRUE'['skew'][1,1]
# Вариант 13
df1 <- airquality
z <- describeBy(x = df1, group = df1$Month)
z[["8"]]
# Вариант 14
df <- airquality
df2 <- describeBy(df, group = df$Month, mat = T)
# Вариант 15
tmp <- describeBy(airquality, group = airquality$Month, mat = T)
View(tmp)
# Вариант 16
describeBy(x = airquality[, -c(5,6)], group = airquality$Month, mat = T)
# Вариант 17
a <- airquality
b <- describeBy(x = a[ ,-5], group = a$Month, mat = T)
b[14, 'skew']


#### yt (Step 12 of 15) _______________________________________________________________________________________________________________________ ####




#### Step 13 of 15 ____________________________________________________________________________________________________________________________ ####
# Обратимся к встроенным данным iris. Соотнесите значения стандартного отклонения переменных.
# Вариант 1
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Length)
# Вариант 2
describe(iris)['sd']
# Вариант 3
subset(describe(iris), select = sd)
# Вариант 4
z <- describe(x = iris[1:4], fast = T)
zz <- subset(z, select = sd)
# Вариант 5
describe(iris)
# Вариант 6
sapply(iris[1:4], sd)
# Вариант 7
aggregate(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width ) ~ 1, iris, sd)
# Вариант 8
sapply(iris[1:4], FUN=sd)
# Вариант 9
SD(iris)
# Вариант 10
describeBy(x=iris, group = iris$Sepal.Length & iris$Sepal.Width)
# Вариант 11
apply(iris[,1:4], 2, sd)
# Вариант 12
describe(iris[,-c(5)])["sd"]
# Вариант 13
for (v in 1:4) {sd_v[v] <-  sd(iris[,v])}
# Вариант 14
arrange(describe(iris, digits = 2)[ ,'sd', drop=F])
# Вариант 15

aggregate(iris$Sepal.Length ~ Species, iris, sd)


#### yt (Step 13 of 15) _______________________________________________________________________________________________________________________ ####






#### Step 14 of 15 ____________________________________________________________________________________________________________________________ ####
# В данных iris расположите по убыванию значения медиан количественных переменных в группе virginica.

# Вариант 1
describe(iris)["median"]
describe(yt)["median"]
# Вариант 2
sd(iris$Sepal.Length)
sd(iris$Sepal.Width)
sd(iris$Petal.Length)
sd(iris$Petal.Length)
# Вариант 3

# Вариант 4

# Вариант 5
# Вариант 6
# Вариант 7
# Вариант 8
# Вариант 9
# Вариант 10
# Вариант 11
# Вариант 12

# Вариант 13
# Вариант 14
# Вариант 15


#### yt (Step 14 of 15) _______________________________________________________________________________________________________________________ ####






#### Step 15 of 15 ____________________________________________________________________________________________________________________________ ####
# В переменной my_vector сохранен вектор с пропущенными значениями. Вам нужно создать новый вектор fixed_vector, в котором все 
# пропущенные значения вектора my_vector будут заменены на среднее значение по имеющимся наблюдениям.
# При этом исходный вектор оставьте без изменений!
# Напоминаю, переменная my_vector уже создана, сразу начинайте работать с ней. Перед тем, как сдавать решение, вы можете 
# потренироваться на различных примерах. Ниже небольшой код, который может создать случайный вектор (выборка из 
# нормального распределения) с пропущенными значениями.

my_vector <- rnorm(30)
hist(my_vector)
my_vector[sample(1:30, 10)] <- NA # на десять случайных позиций поместим NA

# Задача для самостоятельной работы:
# Изучите справку по функции replace. Вызвать справку можно исполнив команду:
?replace
# Попробуйте решить это задание при помощи этой функции.

# Вариант 1
fixed_vector <- replace(my_vector, is.na(my_vector), mean(my_vector, na.rm = T))
# Вариант 2
fixed_vector <- ifelse(is.na(my_vector), mean(my_vector, na.rm = T), my_vector)
# Вариант 3
fixed_vector <- my_vector
fixed_vector[is.na(fixed_vector)] <- mean(fixed_vector, na.rm=TRUE)
# Вариант 4
fixed_vector <- replace(my_vector, my_vector %in% NA, mean(my_vector,na.rm = T))
# Вариант 5
fixed_vector <- replace(my_vector, list = is.na(my_vector), values = mean(my_vector, na.rm = T))
# Вариант 6
fixed_vector <- replace(my_vector,which(my_vector %in% NA),mean(my_vector[! my_vector %in% NA]))
# Вариант 7
m <- mean(my_vector, na.rm = T)
fixed_vector <-my_vector
fixed_vector[which(c(is.na(my_vector)))] <- m
# Вариант 8
na_pos <- is.na( my_vector )
fixed_vector <- my_vector
fixed_vector[na_pos] <- mean( my_vector[!na_pos])
# Вариант 9
fixed_vector <- my_vector
fixed_vector[is.na(my_vector)] <- mean(my_vector[!is.na(my_vector)])

#### yt (Step 15 of 15) _______________________________________________________________________________________________________________________ ####

