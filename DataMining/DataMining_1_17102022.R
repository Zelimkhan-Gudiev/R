# Лабороторная работа от 17.10.2022
remove(list = ls())

library("psych")
library(zoo)
library(dplyr)
library(vroom)
library(readxl)

## Я не смого считать данные ?!
myData <- read.csv("C:/Users/GudievZK/Desktop/GitHub/DF/iris_example1.csv", dec = ",", row.names = FALSE) # ошибка!!!
tb <- read.table("C:/Users/GudievZK/Desktop/GitHub/DF/iris_example.csv")
tb <- vroom(file = "C:/Users/GudievZK/Desktop/GitHub/DF/iris_example1.csv", delim = "\t")
read_excel("C:/Users/GudievZK/Desktop/GitHub/DF/iris_example1.csv")
tb <- read.table(file = "C:/Users/GudievZK/Desktop/GitHub/DF/iris_example.csv", dec=",", sep="\t", header = TRUE)
##________________________________
myData <- as_data_frame(iris) # Так файл, предоставленный преподователем корректно не считывается, то подгрузим дата сет iris, 
                              # который поставляется вместе с R. Для удобства будем использовать функции из пакета dplyr.


#### 1.3 Проверьте, есть ли наблюдения, в которых значения по какому-либо параметру отсутствуют, и выведите список этих значений. ####

sum(is.na(myData)) # NA отсутствуют, т.к. я использую дата сет iris, который поставляется вместе с R.

myData[c(1, 3, 51), c(2, 4)] <- NA  # заменим некоторые данные на NA для того чтобы выполнять задания, 
                                    # предусмотренные лабороторной работой

myDataNA <- myData  # сохраним дата фрейм с 6 NA, т.к. он нам понадобится для выполнения заданий, 
# предусмотренные лабороторной работой.

sum(is.na(myDataNA)) # 6 NA

myData <- as_data_frame(iris) # сохраним в myData исходные данные без пропусков, т.к. он возможно нам понадобится для выполнения заданий, 
# предусмотренных лабороторной работой.

## Посмотрим в каких строках и столбцах имеются NA

myDataNA[!complete.cases(myDataNA),] # пропуски имеются в трех строках и в двух столбцах (Sepal.Width Petal, Petal.Width) #
# Строки с NA
which(apply(myDataNA, 1, anyNA)) # номера строк с NA: 1, 3, 51
# Столбцы с NA
names(which(colSums(is.na(myDataNA)) > 0)) # NA содержатся в переменных Sepal.Width и Petal.Width
colSums(is.na(myDataNA)) # 3 NA в переменной Sepal.Width и 3 NA в переменной Petal.Width


# Примите решение, что, на ваш взгляд, целесообразно в данном случае сделать с пропущенными значениями.
# В частности, подставьте вместо отсутствующих значений среднее с помощью функции na.aggregate либо удалите значения с пропусками.

myDataNA[] <- lapply(myDataNA, na.aggregate) # заменим NA на средние значения
sum(is.na(myData)) # 0 NA

# Проверим
mean(myDataNA$Sepal.Width, na.rm = T) # среднее значение переменной Sepal.Width = 3.052381 
mean(myDataNA$Petal.Width, na.rm = T) # среднее значение переменной Petal.Width = 1.211565 
myDataNA[c(1, 3, 51), c('Sepal.Width','Petal.Width')] # проверим подставились ли даннае значения на место NA


# Кратко ответьте на следующие вопросы.
#  1)	Что является элементарной единицей в этом наборе данных?
# Ответ: один цветок

#  2)	Определите вид данных: одномерные, двумерные, многомерные?
dim(myData) # Ответ: двумерные данные.

#  3)	Определите какие из этих переменных количественные, а какие качественные?
str(myData) 
# Ответ: переменная Species - качественная (фактор с тремя градациями), все остальные переменные - количественные (numeric)
# library(dplyr)
colnames(select_if(myData, is.numeric)) # количественные переменные: "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
colnames(select_if(myData, is.factor)) #  качественные переменные: "Species"

#  4)	Какие из этих переменных номинальные? Какие порядковые?
# Ответ: номиинальные переменны: "Species"; порядковые переменные в данных отсутствуют.
#  5)	Это временной ряд или данные об одном временном срезе?
# Ответ: это данные об одном временном срезе ?!
#  6)	Укажите, какие операции можно применять к каждой из переменных
# Ответ: По отношению к количественным переменным можно применять все математические операции. 
        # По отношению к качественным переменным могут применены только эквивалентность, принадлежность множеству и другие операции над множествами
#  7)	Сформулируйте (в общих терминах), на какие вопросы можно найти ответы при детальном анализе набора данных такого типа.
# Ответ:
summary(myDataNA) # Ответы на вопросы о минимальных и максимальных значениях и мерах центральной тенденции
describe(myDataNA) # Ответы на вопросы о мерах центральной тенденции и мерах изменчивости
table(myDataNA$Species) # 

# Как сформулировать этот на этот вопрос в общих теримних, а не перечислять все операции, которые можно выполнить по отношению к данному дата фрейму ?!

# Проверьте количество наблюдений и переменных.
dim(myData) # 150 наблюдений и 50 переменных
# Проверьте названия переменных.
names(myData) # "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species" 
# Проверьте структуру данных. В частности, количественные переменные должны иметь атрибут «числовая», 
# качественные – «factor», а также являются ли данные таблицей или фреймом.
str(myData)
colnames(select_if(myData, is.numeric)) # количественные: "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
colnames(select_if(myData, is.factor)) #  качественные: "Species"
typeof(myData) # "list"
class(myData)  # "data.frame"
attributes(myData)

# Рассчитайте описательные статистики.
summary(myData)

# Рассчитайте винзорированную (триммированную) среднюю по каждой переменной и сопоставьте это значение с исходным средним и
# медианным значением. Сделайте выводы. 

as.data.frame(winsor.mean(select_if(myDataNA, is.numeric), trim = 0.2, na.rm = TRUE)) # винзорированные (триммированные) средние по каждой переменной
describe(myDataNA[, -5])[c('mean', 'median')] # мода и медина по каждой переменной

# Сравнение средней с винзорированной (триммированной) средний по каждой переменной
describe(myDataNA[, -5])['mean'] - as.data.frame(winsor.mean(select_if(myDataNA, is.numeric), trim = 0.2, na.rm = TRUE))
# Сравнение медианы с винзорированной (триммированной) средний по каждой переменной
describe(myDataNA[, -5])['median'] - as.data.frame(winsor.mean(select_if(myDataNA, is.numeric), trim = 0.2, na.rm = TRUE))
# Ответ: ?!


# Проверьте данные на наличие выбросов по каждой переменной, присвоив ей буквенное значение (x1, х2, х3…). ?!

# Данный кусок кода у меня не работает
x1 <- myDataNA[, 1]
boxplot.stats(x1)$out # почему не работает?
boxplot(x1)

x2 <- myDataNA[, 2]
boxplot.stats(x2)$out # почему не работает?
boxplot.stats(myDataNA$Sepal.Width)$out
boxplot(x2)

x3 <- myData[, 3]
boxplot.stats(x3)$out # почему не работает?
boxplot.stats(myDataNA$Petal.Length)$out # выбросов нет
boxplot(x3)

x4 <- myData[, 4]
boxplot.stats(x4)$out # почему не работает?
boxplot.stats(myDataNA$Petal.Width)$out # выбросов нет
boxplot(x4)
#________________________________________________________________________
# Так как вышеприведенный код местами не работает, проверка на наличие выбосов будет выполнена следующим образом
boxplot.stats(myDataNA$Sepal.Length)$out # выбросов нет
boxplot(myDataNA$Sepal.Length) 

boxplot.stats(myDataNA$Sepal.Width)$out # выбросы: 4.4, 4.1, 4.2, 2.0
boxplot(myDataNA$Sepal.Width) 

boxplot.stats(myDataNA$Petal.Length)$out # выбросов нет
boxplot(myDataNA$Petal.Length)

boxplot.stats(myDataNA$Petal.Width)$out # выбросов нет
boxplot(myDataNA$Petal.Width)


# Значения, которые являются выбросами для всех переменных:
df <- data.frame(x1, x2, x3, x4) # Проверка df == data.frame(myDataNA$Sepal.Length, myDataNA$Sepal.Width, myDataNA$Petal.Length, myDataNA$Petal.Width)

rm(x1, x2, x3, x4)

# Данный кусок кода у меня не работает ?!
attach(df)
(a <- which(x1 %in% boxplot.stats(x1)$out)) # почему не работает?
(b <- which(x2 %in% boxplot.stats(x2)$out)) # почему не работает?
(c <-  which(x3 %in% boxplot.stats(x3)$out)) # почему не работает?
(d <-  which(x4 %in% boxplot.stats(x4)$out)) # почему не работает?
detach(df)
#________________________________________________________________________
# Так как вышеприведенный код местами не работает, индексы выбросов будут найдены следующим образом
a <- which(myDataNA$Sepal.Length %in% boxplot.stats(myDataNA$Sepal.Length)$out)
b <- which(myDataNA$Sepal.Width %in% boxplot.stats(myDataNA$Sepal.Width)$out)
c <-  which(myDataNA$Petal.Length %in% boxplot.stats(myDataNA$Petal.Length)$out)
d <-  which(myDataNA$Petal.Width %in% boxplot.stats(myDataNA$Petal.Width)$out)

oulier_list1 <- Reduce(intersect, list(a, b, c, d))
oulier_list2 <- Reduce(union, list(a, b, c, d)) # Индексы значений, которые являются выбросами хотя бы одной переменной: 16, 33, 34, 61
   
# ?!
# Выделите выбросы с помощью кластерного анализа. Напомним, что в кластерном анализе методом К-средних данные группируются 
# на основе близкого расположения к плотным скоплениям. Выбросами, поэтому, будут считаться наблюдения, 
# находящиеся в относительном удалении от таких скоплений.

# Для выделения выбросов необходимо убрать, при ее наличии, качественную переменную, записав данные под новым именем:
myDataNA_num <- myDataNA[, 1:4]
# Проведите кластеризацию методом К-средних, выделив три кластера:
kmeans.result <- kmeans(myDataNA_num, centers = 3) # ?! kmeans

# Отобразите центры кластеров:
kmeans.result$centers

# ?!
# Рассчитайте расстояния от объектов до центров кластеров:
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((myDataNA_num - centers) ^ 2))

# Возьмите несколько (например, 5) наибольших получившихся расстояний.
outliers <- order(distances, decreasing=T)[1:5]
print(outliers)
print(myDataNA[outliers, ])

# Проверьте с помощью графика, можно ли считать эти наблюдения выбросами:
plot(myDataNA[, c("Sepal.Length", "Sepal.Width")], pch ="o", col = kmeans.result$cluster, cex = 0.3)
points(kmeans.result$centers[, c("Sepal.Length", "Sepal.Width")], col = 1:3, pch = 8, cex = 1.5)
points(myDataNA[outliers, c("Sepal.Length", "Sepal.Width")], pch = "+", col = 4, cex = 1.5)
# Ответ: да, указанные наблюдения можно счиать выбросами, т.к они находятся довольно далеко от центра кластеров.

# Кратко опишите, какие наблюдения являются аномальными:
# Ответ: 
# - только по одной переменной;
oulier_list2 <- Reduce(union, list(a,b,c,d)) # Значения, которые являются выбросом для хотя бы одной переменной: 16, 33, 34, 61
# - сразу по всем переменным.
oulier_list1 <- Reduce(intersect, list(a,b,c,d)) # выбросы по всем переменным отсутствуют

# Выдвиньте разумные предположения о том, чем могли быть вызваны эти аномальные значения. 
# Ответ: ?!

# Сделайте выводы о том, что, в данном случае, целесообразно предпринять относительно выбросов.
# Ответ: ?!

# Представьте данные в виде: (где y, x1, x2,… – количественные переменные из данных) 
# блочных диаграмм: 
boxplot(myDataNA[, 1:4])


# гистограмм: 
hist(x) # ?!

# попарной матрицы диаграмм рассеяния:
pairs(~y+x1+x2+x3) # ?!

# ?!
# тепловой диаграммы:
distMatrix <- as.matrix(dist(myData[,1:4]))
heatmap(distMatrix) 

# ?!
# уровневой диаграммы
library(lattice)
levelplot(x1 ~ x2 * x3, myData, cuts=9, col.regions=grey.colors(10)[10:1])

# ?!
# контурной диаграммы (диаграммы высот) (также в библиотеке lattice)
filled.contour(volcano, color=terrain.colors, asp=1, plot.axes=contour(volcano, add=T))

# ?!
# поверхности многомерного распределения
persp(volcano, theta=25, phi=30, expand=0.5, col="lightblue")

# ?!
# графика параллельных координат на основе переменных #1-4 (по порядку слева направо из данных) в качестве числовых, 
# и переменной #5 (IRISTYPE) в качестве группировочной (библиотека MASS).

library(MASS)
parcoord(myData[1:4], col=myData[, 5])
