remove(list = ls())
rm()

?iris
df  <- iris

str(df)

df1  <- subset(df, Species != "setosa")
str(df1)
table(df1$Species)

hist(df1$Sepal.Length)

ggplot(df1, aes(x =Sepal.Length ))+
  geom_histogram(fill = "white", col = "black", binwidth = 0.4)+
  facet_grid(Species ~ .)

ggplot(df1, aes(Sepal.Length, fill = Species ))+
  geom_density(alpha = 0.5)

ggplot(df1, aes(Species, Sepal.Length))+
  geom_boxplot()

shapiro.test(df1$Sepal.Length)

shapiro.test(df1$Sepal.Length[df1$Species == "versicolor"])
shapiro.test(df1$Sepal.Length[df1$Species == "virginica"])

#The same result in one line
by(df1$Sepal.Length, INDICES = df1$Species, shapiro.test)

bartlett.test(Sepal.Length  ~ Species, df1)


t.test(Sepal.Length  ~ Species, df1)
test1  <- t.test(Sepal.Length  ~ Species, df1)

str(test1)
test1$p.value

t.test(Sepal.Length  ~ Species, df1, var.equal = T)

t.test(df1$Sepal.Length, mu = 8)

t.test(df1$Petal.Length, df1$Petal.Width, paired = T)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               width = 0.1)+
  stat_summary(fun.y = mean, geom = "point", size = 4)

ggplot(df1, aes(Species, Sepal.Length))+
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", 
               size = 2)


?wilcox.test

test2  <- wilcox.test(Petal.Length ~ Species, df1)
pv  <- test2$p.value

ggplot(df1, aes(Species, Petal.Length))+
  geom_boxplot()


wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest  <- wilcox.test(df1$Petal.Length, df1$Petal.Width, paired = T)

paired_wtest$p.value


#### Step 10 of 16 ####
# Задача про свинок!
  
# Воспользуемся еще одним встроенным набором данных в R  - ToothGrowth. Данные позволяют исследовать рост зубов 
# у морских свинок в зависимости от дозировки витамина C и типа потребляемых продуктов.
# Сравните среднее значение длины зубов свинок, которые потребляли апельсиновый сок (OJ) с дозировкой 0.5 миллиграмм,
# со средним значением длины зубов свинок, которые потребляли аскорбиновую кислоту (VC) с дозировкой 2 миллиграмма. 
# Значение t - критерия сохраните в переменную t_stat.

tg <- ToothGrowth
write.csv2(tg, "tg.csv")
tg <- subset(tg, supp == 'OJ' & dose == 0.5 | supp == 'VC' & dose == 2.0)
t_stat <- t.test(len ~ supp, tg)$stat

ggplot(tg, aes(x = len)) +
  geom_histogram(fill = 'white', col = 'black', binwidth = 2) +
  facet_grid(supp ~ .)



#### Step 11 of 16 ####
# Скачайте данные, посвященные влиянию различного типа лечения на показатель артериального давления. 
https://stepic.org/media/attachments/lesson/11504/lekarstva.csv﻿
# По всем испытуемым сравните показатель давления до начала лечения (Pressure_before) с показателем давления 
# после лечения (Pressure_after) при помощи t - критерия для зависимых выборок. 
# В поле для ответа укажите значение t - критерия.
# (В качестве десятичного разделителя используйте запятую, например: 123,54)

lek <- read.csv('lekarstva.csv')

t.test(lek$Pressure_before, lek$Pressure_after, paired = T)



#### Step 15 of 16 ####
getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF")
ds <- read.table("dataset_11504_15.txt")
str(ds)
ds$V2 <- as.factor(ds$V2)

bartlett.test(ds$V1, ds$V2) # if p-value > 0.05 it mean that we don't have reason to refuse H0 about equal variances 
bartlett.test(V1 ~ V2, ds)  # we need to use t.test. if p-value < 0.05 we will refuse H0 and use wilcox.test

t.test(ds$V1, ds$V2, var.equal = T)
t.test(V1 ~ V2, ds, var.equal = T)
wilcox.test(ds$V1, ds$V2)

#
boxplot(V1 ~ V2, ds)


#### Step 16 of 16 ####

ds16 <- read.table("dataset_11504_16.txt")
t.test(ds16$V1, ds16$V2, var.equal = F) # if we find is a difference significant, we need to print three numbers: 
         # first variable's mean; second variable's mean; p-value. If we don't find
         # deference significant, we need to print following text: "The difference is not significant"




