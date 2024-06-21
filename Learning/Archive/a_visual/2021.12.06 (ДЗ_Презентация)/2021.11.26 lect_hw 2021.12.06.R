library(ggplot2)

# Создать файл исходных данных reg_1
reg_1 <- read.csv2("InItial_Data_1.csv")
reg_1
View(reg_1)


# Создать систему координат
g <- ggplot(data = reg_1, mapping = aes(x = cpi_19, y =  wage_19))
g


# Построить диаграмму рассеяния
g <- ggplot(data = reg_1,
            mapping = aes(x = wage_19,
                          y = expen_19))

g+geom_point()


# Построить диаграмму рассеяния с маркировкой по категориальной переменной 
g <- ggplot(data = reg_1,
            mapping = aes(x = wage_19,
                          y = expen_19, color=tip_1))

g+geom_point()


# Группы выделяются размером точек (1-й вариант)
g <- ggplot(data = reg_1,
            mapping = aes(x = wage_19,
                          y = expen_19, size=tip_1))

g+geom_point()


# Группы выделяются размером точек (2-й вариант)
g <- ggplot(data = reg_1,
            mapping = aes(x = wage_19,
                          y = expen_19, size=gdp_c_18))

g+geom_point()


# Диаграмма рассеяния (цвет/ увеличение размера точек)
g <- ggplot(data = reg_1,
            mapping = aes(x = wage_19,
                          y = expen_19, color=tip_1))

g+geom_point(size=4)


# Сглаживание
# Разобраться с сглаживанием?!
g <- ggplot(data = reg_1,
            mapping = aes(x = wage_19, y=expen_19))

g + geom_smooth()


# Сглаживание для каждой группы регионов (по категориальной переменной typ_1)
g <- ggplot(data = reg_1)+
  geom_smooth(mapping = aes(x = wage_19, y=expen_19, linetype=tip_1, color=tip_1), se=FALSE, size=2)
g

# Зависимость уровня инфляции (ИПЦ) от темпов экономического роста (gdp_g_18) по группам регионов,
# различных по уровню экономического развития
g <- ggplot(data = reg_1)+
  geom_smooth(mapping = aes(x = gdp_g_18, y=cpi_19, linetype=tip_1, color=tip_1), se=FALSE, size=2)
g

# Сглаживание_метод "lm"
g <- ggplot(data = reg_1,
            mapping = aes(x = wage_19,
                          y=expen_19))
g + geom_smooth(method = 'lm',  col="purple")+geom_point()


# Сглаживание_метод "gam"
g <- ggplot(data = reg_1,
            mapping = aes(x = wage_19,
                          y=expen_19))
g + geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 4), col="purple")+geom_point()


# Сглаживание_метод "loess"
g <- ggplot(data = reg_1,
            mapping = aes(x = wage_19,
                          y = expen_19,color="purple"))
g + geom_point(color = "blue") +
  geom_smooth(method = "loess")







# РАБОТА С ПРОПУСКАМИ ДАННЫХ


## Установить пакет VIM
library(VIM)


### Узнать, в каких столбцах (по каким переменным)имеются пропущенные значения
names(which(sapply(reg_1, anyNA)))

## График распределения пропущенных значений
miss_plot<-aggr(reg_1[,c('cost_hous_19','prop_helth_20')])
miss_plot
?aggr()


## Для того, чтобы уместились названия переменных на графике, создадим объект reg_1_a,
## содержащий эти переменные, придав им короткие названия

reg_1_a <- reg_1[,c('cost_hous_19','prop_helth_20')]
names(reg_1_a) <- c('c_h_19', 'p_h_20')
aggr(reg_1_a)

# Построить график совместного распределения пропущенных значений
?marginplot
marginplot(reg_1[,c('cost_hous_19','prop_helth_20')])




#### РАБОТА С ПРОПУЩЕННЫМИ ДАННЫМИ

## Установить пакет DMwR


## подключить библиотеку DMwR2
library(DMwR2)


## Установить номера строк с числом пропущенных значений больше 1 %

manyNAs(reg_1,0.01)


### Способ 1- Удалить строки, в которых число пропущенных значений больше 1 %
reg_1_b <- reg_1[-manyNAs(reg_1,0.01),]
View(reg_1_b)
? manyNAs


### Способ 2 - Замена пропущенных значений путем вменения значения медианы (или средней)


## Установить пакет caret
library(caret)


## Установить число пропусков по переменным cost_hous_19 и prop_helth_20
sum(is.na(reg_1$cost_hous_19))
sum(is.na(reg_1$prop_helth_20))


## Вменяем значение медианы пропускам (по обоим столбцам)
proc<-preProcess(reg_1[,c('cost_hous_19','prop_helth_20')],method='medianImpute')

reg_1_c<-reg_1
reg_1_c[,c('cost_hous_19','prop_helth_20')]<- predict(proc,reg_1_c[,c('cost_hous_19','prop_helth_20')])


## Установить число пропусков по переменным cost_hous_19 и prop_helth_20 после вменения медианы

sum(is.na(reg_1_c$cost_hous_19))
sum(is.na(reg_1_c$prop_helth_20))

### Способ 3 - Замена пропущенных значений методом кластерного анализа (k ближайших соседей)

## загрузить пакет RANN
install.packages("RANN")
library(caret)
proc_1<-preProcess(reg_1[,c('cost_hous_19','prop_helth_20')],method='knnImpute')

reg_1_d<-reg_1
reg_1_d[,c('cost_hous_19','prop_helth_20')]<- predict(proc_1,reg_1_d[,c('cost_hous_19','prop_helth_20')])


## Сравнить описательную статистику по переменным с вмененными значениями
summary (reg_1_c[,c('cost_hous_19','prop_helth_20')]) 
summary (reg_1_d[,c('cost_hous_19','prop_helth_20')])
# По итогам срравнения видно, что описательная статистика в reg_1_d (method='knnImpute') сильно отличается от данных reg_1_c. Есть подозрение, что при проведении (method='knnImpute') выполнятеся стандартизация 

# Выполнить действие, обратное стандартизации

mean(reg_1$cost_hous_19,na.rm=TRUE)
sd(reg_1$cost_hous_19,na.rm=TRUE)
reg_1_d_c_h_19 <- reg_1_d$cost_hous_19*20.28058+52.11807
# Что означает данный занк "$"?
summary(reg_1_d_c_h_19)

head(reg_1_d_c_h_19)

# объединили строки с 252 по 256
reg_1_d_c_h_19 <- reg_1_d$cost_hous_19 * sd(reg_1$cost_hous_19, na.rm = TRUE) + 
  mean(reg_1$cost_hous_19, na.rm = TRUE)

## Построить гистограммы
hist(reg_1_c$cost_hous_19)
hist(reg_1_d_c_h_19)


### 2021.12.06

install.packages("ggplot2")
library(ggplot2)
read.csv(initial_Data_2.csv)
