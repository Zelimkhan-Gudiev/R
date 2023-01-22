remove(list = ls())
rm()

#### 1 of 12 ####
# В этом уроке мы:
# 1) узнаем, как лучше всего хранить табличные данные;
# 2) познакомимся с тонкостями импорта данных в сессию R;
# 3) рассмотрим типичные этапы предобработки данных;
# 4) исследуем места обитания редких птиц.

# Дата фрейм и эффективная работа с ним - это хребет не только этого курса, но, пожалуй, и всего языка в целом. 
# Это достаточно сложная структура, сочетающая в себе сильные стороны вектора, матрицы и списка. 
# Крайне важно почувствовать себя в условиях, "приближенных к боевым", именно поэтому я уделяю
# особое внимание практической работе с дата фреймом.

#### Общий глоссарий для этого урока: ####
?data.frame
?str
?rownames, ?colnames, ?dimnames, ?nrow, ?ncol, ?dim
?subset, ?rbind, ?cbind, ?merge
?read.table (?read.csv, ?read.delim)
?write.table (?write.csv, ?write.delim)
?complete.cases, ?na.omit
?write.table (?write.csv)

#### Step 2 of 12 ####
## Дата фрейм 
# Дата фрейм (Data frame) - двумерная таблица с данными (Excel spreadsheets, SQL - таблица)
# Де-факто дата фрейм это стандартный способ хранения данных в формате "наблюдения/переменные": строки соответствуют наблюдениям, а столбцы - переменным.
# Дата фрейм наследует свойства матрицы (прямоугольная форма) и списка (переменные могут быть разных типов).

## Создание дата фреймов
df <- data.frame(x = 1:4, y = LETTERS[1:4], z = c(T, F))
# --dplyr--
library(dplyr)
df1 <- data_frame(x = 1:4, y = LETTERS[1:4], z = c(T, F))
data_frame(df)
# --
# Функция str - сводка об объекте
str(df)

## Именна переменных
df <- data.frame(x = 1:4, 
                 y = LETTERS[1:4], 
                 z = c(T, F),
                 row.names = c("Alpha", "Bravo", "Charlie", "Delta"))
rownames(df)
colnames(df)
dimnames(df)

## Размерности
nrow(df)
ncol(df)
dim(df)
c(nrow(df), ncol(df))
# Две важные особенности 
length(df) # length() возвращает количество слолбцов (переменных), а не общее количество элементов. 
# Это происходит потому-что дата фрейм (data frame) это список (list) уложенный по столбцам, а функция length 
# в случае ее применнеия по отношению к списку (list) возвращает количество элементов списка
length(list(x = 1:4, y = LETTERS[1:4]))

names(df) # names() возвращает имена слолбцов (переменных). В случае применения фунций names и colnames по отношению к дата фрейму (data frame),
# они возврашают один и тот же результат. Функция names  в случае ее применнеия по отношению к списку (list) возвращает имена элементов списка
names(list(x = 1:4, y = LETTERS[1:4]))
# Чтобы получить общее количество элементов в дата фрейме необходимо умножить ncol() на nrow()

## Индексация дата фрейм (data frame)
# Как для матрицы
df[3:4, -1] # положительная и отрицательная индексация
df[c(F, T), c("z", "x")] # логическая индексация и индексация по именам

df[, 1] # Если обратится только к одной переменной при помощи индексации с квадратными скобками [], то произойдет тоже самое, что ии с матриицей, 
# а именно схлопнется размеррность.
df[, 1, drop = FALSE] # Если мы не хотим, чтобы размерность схлопывалась, то мы необходимо указывать аргумент drop со значением FALSE

# Как для списка
df$z
df[[3]]
df[["z"]]
df['z']
## Фильтры по условию
df[df$x > 2, ]
subset(df, x > 2) # В функции subset не нужно дублировать наименование дата фрейма при указании переменных
subset(df, x > 2, select = c(x, z)) 

## Комбиниррование data frame
# Функции cbind и rbind работают, как для матриц:
rbind(df, data.frame(x = 5:6, # при применении rbind необходимо, чтобы именна у дата фреймов совподали в точности
                     y = c("K", "Z"),
                     z = TRUE,
                     row.names = c("Kappa", "Zulu")))


cbind(df, data.frame(season = c("Summer", "Autumn", "Winter", "Spring"), # при применении сbind необходимо, чтобы количество переменных 
                                temp = c(20, 5, -10, 5)))                #  у дата фреймов совподало в точности

## Более сложный случай комбиниррование data frame, а именно комбинирование по ключю (## Комбиниррование data frame: merge)
df
df_salary <- data.frame(x = c(3, 2, 6, 1), salary = c(100, 100, 300, 500))
merge(df, df_salary, by = "x")

library(dplyr)
left_join(df, df_salary, by = "x" )
# Для тех, кто знаком с SQL это inner join
# Остальные типы (left, right, outer, crossjoin) легко найти на stackoverflow по запросу "r joins"

## Комменты
## Формально, data.frame — это именованный список (см. typeof(data.frame)),  все элементы которого имеют одинаковую длину и который имеет атрибут
# row.names. Пример:
n <- 10
l <- list(rnorm(n), runif(n))
names(l) <- c("V1", "V2")
class(l) <- "data.frame"
rownames(l) <- 1:n
# Этот способ создания таблицы намного эффективнее (быстрее) чем стандартный data.frame, но тут отсутствуют всевозможные проверки «от дурака», 
# что может приводить к неожиданным результатам, если вы точно не контролируете входные данные.

##
#vector
mtcars$mpg #vector
mtcars[, 1] #vector
mtcars[[1]] #vector
mtcars[['mpg']] #vector

#df
mtcars['mpg'] #df
mtcars[1] #df
mtcars[, 1, drop = F] #df

#рассмотреть 1-3 строки по условию mtcars[2] == 6, не используя имена переменных
subset(mtcars[1:3, ], mtcars[1:3, 2] == 6, select = c(1)) # df
subset(mtcars[1:3, 1], mtcars[1:3, 2] == 6) # vector

#выводить 1 столбец по условию cyl == 6
subset(mtcars, cyl == 6, select = c(1)) #df
mtcars[mtcars$cyl == 6, 1, drop = F] # df
mtcars[mtcars$cyl == 6, 1] # vector

#### Step 3 of 12 ####
#### Step 4 of 12 ####
# И матрица, и дата фрейм имеют прямоугольную форму, но дата фрейм может содержать данные разных типов. 
# Так, если у нас есть матрица mat и дата фрейм df, то операция as.data.frame(mat) всегда будет работать корректно.
# А что насчёт обратной операции, as.matrix(df)?
# P.S. Стоит иметь в виду, что в R есть функция под названием df (плотность распределения Фишера).
# При создании переменной с названием df происходит т.н. маскировка функции df одноимённой переменной. 
# Восстановить доступ к функции df можно либо удалив переменную (rm(df)), либо воспользовавшись записью stats::df.
# P.P.S. Механизм маскировки действует тем же образом для любой коллизии имён, df просто наиболее частый случай. Попробуйте! 

stats::df()

#### Step 5 of 12 ####
# Анализ данных - это далеко не всегда заумные академические модели в вакууме. Иногда на основе простых манипуляций можно сделать 
# разумные выводы и облегчить принятие повседневных решений.
# Дата фрейм attitude - встроенный массив данных, содержащий рейтинг департаментов одной финансовой компании, составленный сотрудниками.
# Представьте, что вы хотите устраиваться как раз в эту компанию, и дата фрейм (совершенно случайно!) оказался в вашем распоряжении. 
# Вы решили, что самое главное для вас - это возможность учиться новому (learning). Возьмите 5 топовых департаментов по этому показателю. 
# Из этого набора вам более всего подойдёт тот департамент, который имеет наибольшую сумму баллов по трём показателям: реакция 
# на жалобы работников (complaints), надбавки в зависимости от результатов работы (raises) и возможность продвижения (advance).
# Какой же департамент вам выбрать? Напишите его номер XX (номер строки в дата фрейме).
###----
# With
rm(list = ls())
Num <- c(100, 100, 100, 100, 100)
Cost <- c(1200, 1300, 1400, 1500, 1600)

data_A <- data.frame(Num, Cost, stringsAsFactors = FALSE)
str(data_A)

with(data_A, Num * Cost)
with(data_A, Cost / Num)

# Within
within(data_A, Product <- Num * Cost)
within(data_A, Q <- Cost / Num)
###----
#
df <- attitude
df[with(df, order(- df$learning)), ]
df <- arrange(df, - learning) # head(df)

df <- head(df[order(- df$learning), ], 5)
df <- df[c("learning", "complaints", "raises", "advance")]
df <- within(df, sum_three <- complaints + raises + advance)
df <- df[order(- df$sum_three), ]
# 2
#встроенный массив
a <- attitude
head(a)
#сортируем (- по убыванию) по столбцы learning и выбираем первые 5 строк
b <- a[head(order(-a$learning), 5) ,]
#считаем суммы других показателей
b$sm <- b$complaints + b$raises + b$advance
#сортируем и смотрим имя строки для первого
rownames(b[head(order(- b$sm), 1), ])

#
names(which.max(rowSums(attitude[order(attitude$learning, decreasing = T),][1:5, ] [c("complaints", "raises", "advance")])))
#
bestLearning <- subset(attitude, learning >= sort(attitude$learning, T)[5])[1:5, ]
bestReq <- apply(bestLearning[c("complaints", "raises", "advance")], 1, sum)
names(which.max(bestReq))

#
n <- rownames(attitude[order(attitude$learning, decreasing = TRUE), ])[1:5]
which.max(rowSums(attitude[n, c(2, 5, 7)]))
#
# Решение без использования каких-либо функций, не присутствовавших в минувших глоссариях.
# Найдём пять топовых департаментов по показателю learning:
top5 <- sort(attitude$learning, decreasing = T)[1:5]
# Найдём индексы (номера строк) этих департаментов:
row_number <- which(attitude$learning >= min(top5))
# Выведем из дата-фрейма attitude только интересующие нас строки:
new_table <- rbind(attitude[row_number, ])
# Вычислим суммы по показателям complaints, raises и advance. Создастся именованный вектор, в качестве имён будут выступать номера
# строк исходного дата-фрейма:
top_sums <- rowSums(subset(new_table, select = c(complaints,raises,advance)))
# Найдём максимальное значение в векторе top_sums и выведем его имя:
names(which.max(top_sums))
# Ответ получен.
# Решение в одну строчку, оно же развёрнутое вышеизложенное:
names(which.max(rowSums(subset(rbind(attitude[which(attitude$learning >= min(sort(attitude$learning, decreasing = T)[1:5])),]), select = c(complaints,raises,advance)))))

#
library(dplyr)
df <- attitude %>%
  arrange(desc(learning)) %>%
  slice(1:5) %>%
  mutate(my_sum = complaints + raises + advance) %>%
  filter(my_sum == max(my_sum))






#
# Поделюсь иным коротким способом решения данной задачи:
# 1) гуглим как найти сумму столбцов в дата фрейме, переписываем код, получаем:
attitude$myRowSums = rowSums(attitude[, c(2, 5, 7)])
# у нас дополнительный столбец, отражающий суммы трех необходимых показателей
# 2) гуглим как отсортировать по определенному столбцу, находим:
attitude[order(attitude$learning), ]
# В целом аккуратное и красивое решение задачи

#
attitude_1 <- head(attitude[order(attitude$learning, decreasing = TRUE),],5)
attitude_2 <- cbind(attitude_1, "sum" = attitude_1$complaints + attitude_1$omplaints + attitude_1$raises + attitude_1$advance)
rownames(attitude_2[which.max(attitude_2$sum), , drop = FALSE])

#
names(which.max(rowSums(attitude[order(attitude$learning, decreasing = T), ][1:5, c(2, 5, 7)])))

#### Step 6 of 12 ####
#### Step 7 of 12 ####
# Вернёмся ненадолго к дата фрейму attitude. Какими из нижеуказанных способов можно выбрать только те строки, которые соответствуют
# департаментам с рейтингом (rating) ниже пятидесяти, при этом сохранив все столбцы, кроме rating?
# Убедитесь, что вы понимаете, почему работает (не работает) каждый из способов!
# P.S. Попробуйте сначала отметить верные ответы, а потом их проверять: так интереснее.

subset(attitude, rating < 50, -rating)                         # ok
subset(sel = -rating, sub = rating < 50, attitude)             # ok
attitude[attitude$rating < 50, -"rating"]
attitude[attitude$rating < 50, names(attitude) != "rating"]    # ok
attitude[rating < 50, names(attitude) != "rating"]
#### Step 8 of 12 ####
# Визуальная инспекция данных -- важный этап предварительного анализа данных. Она позволяет заметить очевидные несоответствия и аномалии в данных,
# и, что немаловажно, "почувствовать" структуру дата фрейма. Какие в нём есть переменные? Что они означают? 
# В каких единицах измеряются? Какой у них диапазон значений? 
# Используйте вспомогательные функции, о которых я говорил, чтобы разглядеть встроенный дата фрейм с названием quakes. 
# Сопоставьте значения из этого дата фрейма и их описания.
# Подсказки:
# описание дата фрейма есть в справке: ?quakes
# медиана -- одна из описательных статистик; медиана и среднее -- разные вещи!
df <- quakes
head(df)
tail(df)
summary(df)
library(psych)
describe(df)


#### Step 9 of 12 ####
df <- read.csv("/Users/zelimkhan/Desktop/Data/GitHub/DF/Antonov/avianHabitat.csv")


#### Step 10 of 12 ####df
#### Step 11 of 12 ####
#### Step 12 of 12 ####


iris
