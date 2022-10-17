remove(list = ls())

library("psych")
library(zoo)
library(dplyr)

myData <- read.csv("C:/Users/GudievZK/Desktop/GitHub/DF/iris_example1.csv", dec = ",", row.names = FALSE) # ошибка!!!
myData <- iris 


#### 1.3 Проверьте, есть ли наблюдения, в которых значения по какому-либо параметру отсутствуют, и выведите список этих значений. ####

sum(is.na(myData)) # NA отсутствуют

myData[c(1, 3, 51), c(2, 4)] <- NA # заменим некоторые данные на NA 
is.na(myData)
sum(is.na(myData)) # 6 NA
myDataNA <- myData # сохраним дата фрейм с пропусками, т.к. мы его использовать вместо исходного файла

myData[!complete.cases(myData),] # ропуски имеются на пересечении строк 1, 3 и 51 и столбцов Sepal.Width Petal, Petal.Width

# Примите решение, что, на ваш взгляд, целесообразно в данном случае сделать с пропущенными значениями.
# В частности, подставьте вместо отсутствующих значений среднее с помощью функции na.aggregate либо удалите значения с пропусками.

myData[] <- lapply(myDataNA, na.aggregate)
sum(is.na(myData)) # 0 NA

# Кратко ответьте на следующие вопросы.
#  1)	Что является элементарной единицей в этом наборе данных?
# Ответ: один цветок
#  2)	Определите вид данных: одномерные, двумерные, многомерные?
dim(myData)
# Ответ: двумерные данные.
#  3)	Определите какие из этих переменных количественные, а какие качественные?
str(myData)
# library(dplyr)
colnames(select_if(myData, is.numeric)) # "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width" 
colnames(select_if(myData, is.factor)) #  "Species"
# Ответ: "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  
#  4)	Какие из этих переменных номинальные? Какие порядковые?
# Ответ:
#  5)	Это временной ряд или данные об одном временном срезе?
# Ответ:
#  6)	Укажите, какие операции можно применять к каждой из переменных
# Ответ:
#  7)	Сформулируйте (в общих терминах), на какие вопросы можно найти ответы при детальном анализе набора данных такого типа.
# Ответ:

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
# Рассчитайте винзорированную (триммированную) среднюю по каждой переменной и сопоставьте это значение с исходным средним 
# медианным значением. Сделайте выводы. 

as.data.frame(winsor.mean(select_if(myData, is.numeric), trim = 0.2, na.rm = TRUE))
describe(myDataNA[, -5])[c('mean', 'median')]


# Проверьте данные на наличие выбросов по каждой переменной, присвоив ей буквенное значение (x1, х2, х3…).
x1 <- myData[, 1]
boxplot.stats(x1)$out
boxplot(x1)

x2 <- myData[, 2]
boxplot.stats(x2)$out
boxplot(x2)

x3 <- myData[, 3]
boxplot.stats(x3)$out
boxplot(x3)

x4 <- myData[, 4]
boxplot.stats(x4)$out
boxplot(x4)

# Значения, которые являются выбросами для всех переменных:
df <- data.frame(x1, x2, x3, x4)
rm(x1, x2, x3, x4)

attach(df)
(a <- which(x1 %in% boxplot.stats(x1)$out))
(b <- which(x2 %in% boxplot.stats(x2)$out))
(c <-  which(x3 %in% boxplot.stats(x3)$out))
(d <-  which(x4 %in% boxplot.stats(x4)$out))
detach(df)
oulier_list1 <- Reduce(intersect, list(a,b,c,d))
oulier_list2 <- Reduce(union, list(a,b,c,d)) # Значения, которые являются выбросом для хотя бы одной переменной: 16, 33, 34, 61
   
# Выделите выбросы с помощью кластерного анализа. Напомним, что в кластерном анализе методом К-средних данные группируются 
# на основе близкого расположения к плотным скоплениям. Выбросами, поэтому, будут считаться наблюдения, 
# находящиеся в относительном удалении от таких скоплений.

# Для выделения выбросов необходимо убрать, при ее наличии, качественную переменную, записав данные под новым именем:
myData <- myData[, 1:4]
# Проведите кластеризацию методом К-средних, выделив три кластера:
sum(is.na(myData))
kmeans.result <- kmeans(df, centers = 3)

# Отобразите центры кластеров:
kmeans.result$centers

# Рассчитайте расстояния от объектов до центров кластеров:
centers <- kmeans.result$centers[kmeans.result$cluster, ]
distances <- sqrt(rowSums((myData - centers) ^ 2))

# Возьмите несколько (например, 5) наибольших получившихся расстояний.
outliers <- order(distances, decreasing=T)[1:5]
print(outliers)
print(myData[outliers, ])

plot(myData[, c("Sepal.Length", "Sepal.Width")], pch ="o", col = kmeans.result$cluster, cex = 0.3)
points(kmeans.result$centers[, c("x1", "x2")], col = 1:3, pch = 8, cex = 1.5)
points(myData[outliers, c("Sepal.Length", "Sepal.Width")], pch = "+", col = 4, cex = 1.5)


# Кратко опишите, какие наблюдения являются аномальными:
# Ответ: 
rownames(myData[outliers, ]) # Перечень аномальных наблюений "99",  "58", "94", "61", "119"
# - только по одной переменной;
oulier_list2 <- Reduce(union, list(a,b,c,d)) # Значения, которые являются выбросом для хотя бы одной переменной: 16, 33, 34, 61
# - сразу по всем переменным.
oulier_list1 <- Reduce(intersect, list(a,b,c,d)) # выбросы по всем переменным отсутствуют
# Выдвиньте разумные предположения о том, чем могли быть вызваны эти аномальные значения. 
# Сделайте выводы о том, что, в данном случае, целесообразно предпринять относительно выбросов.
# Представьте данные в виде: (где y, x1, x2,… – количественные переменные из данных)

# Представьте данные в виде: (где y, x1, x2,… – количественные переменные из данных)

# Представьте данные в виде: (где y, x1, x2,… – количественные переменные из данных) блочных диаграмм: 
boxplot(x)
# гистограмм: 
hist(x)
# попарной матрицы диаграмм рассеяния:
pairs(~y+x1+x2+x3)
# тепловой диаграммы:
distMatrix <- as.matrix(dist(myData[,1:4]))
heatmap(distMatrix)
# уровневой диаграммы
library(lattice)
levelplot(x1 ~ x2 * x3, myData, cuts=9, col.regions=grey.colors(10)[10:1])
# контурной диаграммы (диаграммы высот) (также в библиотеке lattice)
filled.contour(volcano, color=terrain.colors, asp=1, plot.axes=contour(volcano, add=T))
# поверхности многомерного распределения
persp(volcano, theta=25, phi=30, expand=0.5, col="lightblue")
# графика параллельных координат на основе переменных #1-4 (по порядку слева направо из данных) в качестве числовых, и переменной #5 (IRISTYPE) в качестве группировочной (библиотека MASS).
library(MASS)
parcoord(myData[1:4], col=myData[, 5])
