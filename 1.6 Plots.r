
remove(list = ls())
rm()

#### Packages and librarys ____________________________________________________________________________________________________ ####

install.packages("dplyr")
library(dplyr)
install.packages("psych")
library(psych)
install.packages("ggplot2")
library(ggplot2)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")

df  <- mtcars
yt <- read.csv2("yt.csv")
# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»

str(yt)

yt_f_names <- c('reason', 'year_plan_st', 'kvartal', 'stage', 'executor_ac', 'teamleader', 'deputy', 'contract', 
                'pcp', 'criteria', 'f2', 'method', 'tegs')
yt[, yt_f_names] <- lapply(yt[, yt_f_names], factor)

write.csv2(yt, "yt.csv")

#### Step 1: Base graphs ______________________________________________________________________________________________________ ####

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))


### 1) hist
hist(df$mpg, breaks = 20, xlab = "MPG", main ="Histogram of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

### 2) boxplot
boxplot(df$mpg[df$am == "Auto"], df$mpg[df$am == "Manual"], ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)

boxplot(mpg ~ am, df, ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)

### 3) plot
plot(density(df$mpg), xlab = "MPG", main ="Density of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

plot(df$mpg, df$hp, xlab = "MPG", ylab ="HP" , main ="MPG and HP", pch = 22)

plot(~ mpg + hp, df)


#### yt (Step 1: Base graphs) _________________________________________________________________________________________________ ####

### 1) hist
hist(yt$duration)
hist(yt$duration, breaks = 20)
hist(yt$numb_ret_depir, breaks = 20)
hist(yt$numb_ret_oiv, breaks = 5)

### 2) boxplot
boxplot(duration ~ deputy, yt, ylab = 'Длительность, раб.дн.', xlab = 'Заместитель РПО', main = "Длительность разработки",
        col = 'green', cex.lab = 1.3, cex.axis = 1.3)

table(yt$deputy)

### 3) plot
plot(yt$time_depir, yt$numb_ret_depir)
cor.test(yt$time_depir, yt$numb_ret_depir)

plot(density(yt$duration), xlab = "Длительность", main ="Density of Длительность", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

plot(yt$duration, yt$numb_ret_depir, xlab = "Длительность", ylab = "Количество возвратов от ДЭПиР", 
     main = "Взаимосвязсь между длительностью и количеством возвратов от ДЭПиР", pch = 20)
cor.test(yt$duration, yt$numb_ret_depir)

plot(yt$duration, yt$numb_ret_oiv, xlab = "Длительность", ylab = "Количество возвратов от ОИВ", 
main = "Взаимосвязсь между длительностью и количеством возвратов от ОИВ", pch = 20)
cor.test(yt$duration, yt$numb_ret_oiv)

plot(~ duration + numb_ret_depir, yt)

#Step 2, 3: Library ggplot2 _________________________________________________________________________________________________ ####

ggplot(df, aes(x = mpg))+
  geom_histogram(fill = "white", col = "black", binwidth = 2)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  ggtitle("MPG histogram")

ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot()+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG dotplot")


ggplot(df, aes(x = mpg))+
  geom_density(fill = "red")

ggplot(df, aes(x = mpg, fill = am))+
  geom_density(alpha = 0.5)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG density plot")


ggplot(df, aes(x = am, y = hp, fill = vs))+
  geom_boxplot()+
  xlab("Transmission type")+
  ylab("Gross horsepower")+
  scale_fill_discrete(name="Engine type")+
  ggtitle("Gross horsepower and engine type")


ggplot(df, aes(x = mpg, y = hp, size = qsec))+
  geom_point()+
  xlab("Miles/(US) gallon")+
  ylab("Gross horsepower")+
  scale_size_continuous(name="1/4 mile time")+
  ggtitle("Miles/(US) gallon and Gross horsepower")


my_plot  <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()

my_plot2  <- ggplot(df, aes(x = am, y = hp, fill = vs))

my_plot2 + geom_boxplot()

#### yt (Step 2, 3: Library ggplot2) _________________________________________________________________________________________ ####

ggplot(yt, aes(x = duration)) +
  geom_histogram(fill = "white", col = "black", binwidth = 20) +
  xlab("Длительность утверждения КТД, раб. дни") + 
  ylab("Количество КТД") +
  ggtitle("Гистограмма длительности утверждения КТД")
  
ggplot(yt, aes(x = duration, fill = kind_tz)) +
  geom_dotplot(binwidth = 15)+
  xlab("Длительность") +
  ylab("Количество КТД")+
  scale_fill_discrete("Вид ТЗ") +
  ggtitle("Гистограмма длительности")

ggplot(yt, aes(x = duration, fill = kind_tz)) +
  geom_density(alpha = 0.5) +
  xlab("Длительность") + 
  ylab("Количество КТД")+
  scale_fill_discrete("Вид ТЗ") +
  ggtitle("Гистограмма длительности")
  
ggplot(yt, aes(x = numb_ret_oiv)) + 
  geom_density(fill = "red")

ggplot(yt, aes(x = numb_ret_depir, fill = kind_tz)) +
  geom_density(alpha = 0.5) +
  xlab("Количестово возвратов от ДЭПиР") + 
  ylab("Количество КТД")+
  scale_fill_discrete("Вид ТЗ") +
  ggtitle("Количество возрвратов от ДЭПиР")


plot_durarion_other_var <- ggplot(yt, aes(x = duration, y = numb_ret_depir, col = kind_tz, size = numb_ret_oiv)) +
  geom_point() + 
  xlab("Длительность") +
  ylab("Количество возвратов от ДЭПиР") +
  scale_color_discrete("Вид ТЗ") +
  scale_size_continuous("Количество возрвратов от ОИВ") +
  ggtitle('Взаимосвязь длительности разработки, количество возрвратов от ДЭПиР, вида ТЗ и количество возрвратов от ОИВ')
  

ggplot(yt, aes(x = duration, y = numb_ret_depir, col = kind_tz, size = numb_ret_oiv)) +
  geom_point() + 
  xlab("Длительность") +
  ylab("Количество возвратов от ДЭПиР") +
  ggtitle('Взаимосвязь длительности разработки, количество возрвратов от ДЭПиР, вида ТЗ и количество возрвратов от ОИВ')


#Step 4: exercise _________________________________________________________________________________________________ ####

# При помощи функции ggplot() или boxplot() постройте график boxplot, используя встроенные в R данные airquality.
# По оси x отложите номер месяца, по оси y — значения переменной Ozone.
# На графике boxplot отдельными точками отображаются наблюдения, отклоняющиеся от 1 или 3 квартиля больше чем 
# на полтора межквартильных размаха. Сколько таких наблюдений присутствует в сентябре (месяц №9)?
# Обратите внимание, что для корректного отображения графика ggplot ожидает факторную переменную по оси x.

airquality
str(airquality)
airquality$Month <- factor(airquality$Month)

# 1
ggplot(airquality, aes(x = Month, y = Ozone)) +
  geom_boxplot()
#
ggplot(yt, aes(x = kind_tz, y = duration)) +
  geom_boxplot()

# 2
boxplot (Ozone ~ Month, airquality)
# 3
ggplot(na.exclude(airquality), aes(x = as.factor(Month), y = Ozone))+
  geom_boxplot(fill = "red", alpha = 0.5)+
  xlab("Month")


#Step 4: exercise _________________________________________________________________________________________________ ####

# Используем знакомые нам данные mtcars. 
# Нужно построить scatterplot с помощью ggplot из ggplot2, по оси x которого будет mpg, 
# по оси y - disp, а цветом отобразить переменную (hp).
# Полученный график нужно сохранить в переменную plot1. Таким образом в ответе должен быть скрипт:
# plot1 <- ggplot(data, aes())+
# geom_****()

plot1 <- ggplot(mtcars, aes(x = mpg, y = disp, col = hp)) + 
  geom_point()




#Step 4: exercise _________________________________________________________________________________________________ ####

# Укажите, при помощи какого варианта кода мы можем построить следующий график по данным iris:
# Гистограмма распределения переменной Sepal.Length, в которой цвет заполнения столбцов гистограммы 
# зависит от значения переменной Species.
# Возможно несколько вариантов правильного ответа. 

ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(fill = Species)) # right
ggplot(iris, aes(Sepal.Length)) + geom_histogram(aes(col = Species))
ggplot(iris, aes(Sepal.Length, col = Species)) + geom_histogram()
ggplot(iris, aes(Sepal.Length)) + geom_histogram(fill = Species)
ggplot(iris, aes(Sepal.Length, fill = Species)) + geom_histogram() # right

#Step 5: exercise _________________________________________________________________________________________________ ####

# Студент Ярослав очень любит строить графики в R. Основываясь на данных iris он хочет построить следующий график:
# Scatterplot (диаграмма рассеивания), где по оси X будет отложена переменная Sepal.Length,  по оси Y переменная  
# Sepal.Width. За цвет точек будет отвечать переменная  Species, а за размер точек переменная Petal.Length.
# Ярослав написал следующую команду
  ggplot(aes(Sepal.Length, Sepal.Width, col = Species)) +
  geom_point(iris, size = Petal.Length)
# Однако построить желаемый график не удается! Укажите, какие ошибки совершил Ярослав и попробуйте 
# построить данный график самостоятельно.
# Возможно несколько правильных ответов.

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, col = Species, size = Petal.Length)) + 
  geom_point()


