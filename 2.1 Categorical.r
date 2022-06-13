remove(list = ls())
rm()

#### Packages and librarys ____________________________________________________________________________________________________ ####

install.packages('dplyr')
library(dplyr)

install.packages("ggplot2")
library(ggplot2)

install.packages("tidyverse")
library(dplyr)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")
df <- read.csv('grants.csv')
yt <- read.csv2("yt.csv")
names(yt)
# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»

#### Step 1,2: Contens  _______________________________________________________________________________________________________ ####
# В этом уроке мы научимся анализировать номинативные данные в R!
# Ссылка на скрипт урока: https://stepic.org/media/attachments/lesson/11502/categorical.R
# Ссылка на данные урока: https://stepic.org/media/attachments/lesson/11502/grants.csv



#### Step 3:Categorical data __________________________________________________________ ####

df <- read.csv("grants.csv")

str(df)


df$status <- as.factor(df$status)
levels(df$status) <- c("Not funded", "Funded")

df$status <- factor(df$status, labels = c("Not funded", "Funded"))


#### Step 3: 1d Table __________________________________________________________ ####
t1 <- table(df$status)
t1

dim(t1)

#### Step 3: yt (1d Table) __________________________________________________________ ####
table(yt$deputy)
table(yt$kind_tz)


#### Step 4: 2d Table __________________________________________________________ ####
t2 <- table(df$status, df$field)
t2
t2 <- table(status = df$status, field = df$field)
t2
dim(t2)

prop.table(t2)

prop.table(t2, 1)
prop.table(t2, 2)

prop.table(t2, 2) * 100
round(prop.table(t2, 2),2)
round(prop.table(t2, 2) *100, 1)

#### Step 4: yt (2d Table) __________________________________________________________ ####

yt_t1 <- table('Заместитель' = yt$deputy, 'Уровень наихудших КТД' = yt$top_worst_ktd)
yt_t1 <- as.data.frame(yt_t1)

dim(yt_t1)

prop.table(yt_t1, 1)
prop.table(yt_t1, 2)

round(prop.table(yt_t1, 1), 1)
round(prop.table(yt_t1, 2) * 100, 1)

# 
yt$ktd[yt$top_worst_ktd == '1']
yt$ktd[yt$top_worst_ktd == '2']

yt <- yt %>%
  mutate(tru = case_when(
    startsWith(ktd, "П") ~ "Поставка товра",
    startsWith(ktd, "В") ~ "Выполнение работ",
    startsWith(ktd, "О") ~ "Оказание услуг",
  ))



# Корректировка yt$deputy
yt$ktd[yt$deputy == '']
yt$teamleader[yt$ktd == "Поставка химических веществ"]
yt$teamleader[yt$ktd == "Поставка мебели детской и ученической"]
yt$teamleader[yt$ktd == "Поставка учебного оборудования для музыкального класса, закупаемого в рамках проекта «Техносфера современной школы»"]

yt$deputy[yt$deputy == ''] <- 'Чурсина Мария Вячеславовна'
yt$deputy[yt$ktd == ("Поставка химических веществ" | "Поставка мебели детской и ученической" 
  | "Поставка учебного оборудования для музыкального класса, закупаемого в рамках проекта «Техносфера современной школы»"] <- 'Чурсина Мария Вячеславовна'

yt$deputy[yt$ktd == 'Оказание услуг по техническому обслуживанию и текущему ремонту объектов коллекторного хозяйства'] <- 'Гудиев Зелимхан Куйраевич'


# ТРУ
yt$ktd[yt$ktd == "Разработка проектно-сметной документации на устройство сетей электроснабжения и электроосвещения"] <- "Выполнение работ по разработке проектно-сметной документации на устройство сетей электроснабжения и электроосвещения"


grep("Поставка", yt$ktd, value = T)
grep("Оказание", yt$ktd, value = T)
grep("Выполнение", yt$ktd, value = T)


for (i in 1:nrow(yt)) {
  if (grep("Поставка", yt$ktd[i], value = T)) {
    yt$tru[i] <- "Потавка товара"
  } else if (grep("Оказание", yt$ktd[i], value = T)) {
    yt$tru[i] <- "Оказание услуг"
  } else yt$tru[i] <- "Выполнение работ"
}



write.csv2(yt, "yt.csv")

#### Step 4: 3d Table __________________________________________________________ ####
t3 <- table(Years = df$years_in_uni, Field = df$field, Status = df$status)
t3

dim(t3)

#### Step 4: yt (3d Table) __________________________________________________________ ####

yt_t1 <- table('Заместитель' = yt$deputy, 'Уровень наихудших КТД' = yt$top_worst_ktd)
yt_t1 <- as.data.frame(yt_t1)

dim(yt_t1)

prop.table(yt_t1, 1)
prop.table(yt_t1, 2)

yt_t2 <- table('ТРУ' = yt$tru, 'Уровень наихудших КТД' = yt$top_worst_ktd)
yt_t2 <- as.data.frame(yt_t2)


#### Step 5: Exircises __________________________________________________________ ####

# К частям таблицы можно обращаться так же, как и к матрицам.
# HairEyeColor - таблица с данными, встроенными в R. Посмотрите на неё в R. Команда 
dimnames(HairEyeColor)
# позволит нам посмотреть, какие измерения есть в этой таблице и как они называются. Например, чтобы обратиться к части таблицы, 
# в которой хранятся данные только о мужчинах, нам нужно выполнить следующую команду: 
HairEyeColor[ , ,'Male']
HairEyeColor[ , "Blue",'Male'] # обращение к части таблицы, в которой хранятся данные только о мужчинах с голубыми глазами

# Ваша задача в переменную red_men сохранить долю рыжеволосых (Red) от общего числа голубоглазых мужчин.
# Обратите внимание, что нужны не проценты, а просто доля, то есть десятичная дробь  (например, не 10%, а 0.1).
# Дополните код, чтобы получить верный ответ.    
HairEyeColor <- HairEyeColor
red_men <- prop.table(HairEyeColor[, , "Male"], 2)
round(red_men, 3)
prop.table(HairEyeColor["Red", , "Male"], 3)

dim(HairEyeColor)

#
red_man <- HairEyeColor['Red', 'Blue', 'Male']
blue_eyes_man <- HairEyeColor[, "Blue", 'Male']
red_man_sum <- sum(red_man)
blue_eyes_man_sum <- sum(blue_eyes_man)
red_men <- red_man_sum / blue_eyes_man_sum
#
prop.table(HairEyeColor[, , 'Male'], 2)['Red', 'Blue']
#
prop.table(HairEyeColor[,'Blue','Male'])['Red']
#
prop.table(HairEyeColor[ , 'Blue' ,'Male'])[['Red']]
#
male <- HairEyeColor[ , ,'Male']
str <- prop.table(male, 2)
str[3,2]
#
prop.table(HairEyeColor[,2,1], )[3]
#
as.vector(prop.table(HairEyeColor[,'Blue',],2))[3]
#
prop.table(HairEyeColor[, , 'Male'], 2)['Red', 'Blue']
prop.table(HairEyeColor[,'Blue','Male'])['Red']
prop.table(HairEyeColor[ ,"Blue", "Male"])[3]
sum(HairEyeColor['Red','Blue','Male'])/sum(HairEyeColor[,'Blue','Male'])
# 1. HairEyeColor  является 3-х мерным массивом размерностью Hair - Eye - Sex str(HairEyeColor)
# 2. когда мы в [ ] указываем, например, [, , Male] (HairEyeColor[ , , "Male"]) (т.е. 3-ю размерность), то получаем матрицу Hair - Eye.
# 3. когда мы в [ ] указываем, например, [, Blue, Male] (HairEyeColor[ ,"Blue", "Male"]) (т.е. 2 и 3-ю размерности), 
# то получаем табличку значений  Hair
# 4. После этого мы подаем таблички на ввод ф-ции prop.table(), которая возвращает, соответственно, одно-или двух мерную таблицу.
# 5. Теперь остается выбрать необходимую нам колонку (['Red']) или пересечение рядов и колонок в этой таблице (['Red', 'Blue'])
# 6. Вместо человеческих наименований можно использовать порядковые номера строк и столбцов [3]


#### Step 6: Exircises __________________________________________________________ ####
# С таблицами, как и с матрицами, можно совершать разные арифметические операции, например, суммировать все элементы таблицы.
# Напишите число зеленоглазых женщин в наборе данных HairEyeColor.
sum(HairEyeColor[, "Green", "Female"])

#
#
#
#### plots __________________________________________________________ ####

barplot(t1)
barplot(t2)
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"))
barplot(t2, legend.text = TRUE, args.legend = list(x = "topright"), beside = TRUE)

mosaicplot(t2)


#### yt (plots) __________________________________________________________ ####

barplot(yt_t1)
barplot(yt_t1, legend.text = T, args.legend = list(x = "topright"), beside = T)


#### Step 8: Exircises __________________________________________________________ ####
# Постройте столбчатую диаграмму распределения цвета глаз по цвету волос только у женщин из
# таблицы HairEyeColor. По оси X должен идти цвет волос, цвет столбиков должен отражать цвет глаз. По оси Y - количество наблюдений.
# Чтобы построить столбчатую диаграмму в ggplot, вам нужно подключить нужный пакет, 
# затем преобразовать таблицу HairEyeColor в data frame:
  mydata <- as.data.frame(HairEyeColor)
dim(mydata)
# Постройте график на основе предложенного кода, сохранив его в переменную obj.
# Укажите, чему равен аргумент data, что должно находиться в aes(). Изучите справку по geom_bar(), чтобы узнать, 
# чему должен равняться аргумент position для отображения цвета глаз в виде соседних столбиков, также вам может быть полезна 
# эта памятка (https://ggplot2.tidyverse.org/reference/geom_bar.html). Там же вы найдёте ответ на вопрос, за что отвечает аргумент stat.
# С помощью scale_fill_manual мы говорим графику, что мы хотим, чтобы он использовал указанные нами цвета.
# Дополните предложенный код:


mydata <- as.data.frame(HairEyeColor[, , "Female"])
obj <- ggplot(data = mydata, aes(x = Hair, y = Freq, fill = Eye)) + 
        geom_bar(stat = "identity", position = "dodge") + 
        scale_fill_manual(values = c("Brown", "Blue", "Darkgrey", "Darkgreen"))

##

ggplot(data = yt, aes(x = top_worst_ktd, y = 'Freq', fill = deputy)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("Brown", "Blue", "Darkgrey", "Darkgreen"))



#У себя на компьютере вы можете визуализировать полученный график, исполнив 'obj'. 
# В случае, если все сделано правильно, он будет выглядеть так (обратите внимание на название осей и легенды):
# Прежде чем отправить код на проверку, выполните его на своем компьютере, чтобы избежать лишних ошибок.
# При ошибке, обратите внимание на содержание feedback.

#### Binomial Test __________________________________________________________ ####

# 
binom.test(x = 5, n = 20, p = 0.5)
binom.test(t1)

#### yt (Binomial Test) __________________________________________________________ ####




#### Chi-Square __________________________________________________________ ####
t1
chisq.test(t1)

chi <- chisq.test(t1)
chi$exp
chi$obs


t2
chisq.test(t2)


#### yt (Chi-Square) __________________________________________________________ ####

chisq.test(yt_t2)


#### Fisher's Exact Test __________________________________________________________ ####

fisher.test(t2)


#### yt (Fisher's Exact Test) __________________________________________________________ ####

#### Step 12: Exircises __________________________________________________________ ####

df12 <-  HairEyeColor["Brown", , "Female"]
t12 <- table(df12)
chisq.test(df12)


#### Step 13: Exircises __________________________________________________________ ####

diamonds
# 1
df13 <- as.data.frame(diamonds[c('cut', 'color')])
chi <- chisq.test(x = df13$cut, y = df13$color)
main_stat <- chi$statistic

#2

diamods_table <- table(diamonds$cut, diamonds$color)
chi_result <- chisq.test(diamods_table) 
main_stat <- chi_result$statistic

#3
main_stat <- chisq.test(diamonds$cut, diamonds$color)$statistic

#### Step 14: Exircises __________________________________________________________ ####
#1
df14 <- as.data.frame(diamonds[, c('price', 'carat')])
df14$factor_price <- ifelse(diamonds$price >= mean(diamonds$price), 1, 0)
df14$factor_carat <- ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0)
df14_1 <- as.data.frame(df14[, c('factor_price', 'factor_carat')])
main_stat <- chisq.test(df14_1$factor_price, df14_1$factor_carat)$statistic

#2
diamonds$factor_price <- factor(ifelse(diamonds$price >= mean(diamonds$price), 1, 0))    
diamonds$factor_carat <- factor(ifelse(diamonds$carat >= mean(diamonds$carat), 1, 0))    
main_stat_master <- chisq.test(diamonds$factor_price, diamonds$factor_carat)$statistic

#3
main_stat <- chisq.test(as.integer(diamonds$price >= mean(diamonds$price)), 
                        as.integer(diamonds$carat >= mean(diamonds$carat)))$statistic
#4
main_stat <- chisq.test(table(
  diamonds$price >= mean(diamonds$price),
  diamonds$carat >= mean(diamonds$carat)
))$statistic

#5
dd <- list()
dd$factor_price <- diamonds$price >= mean(diamonds$price)
dd$facotr_carat <- diamonds$carat >= mean(diamonds$carat)
main_stat <- chisq.test(table(dd))$statistic


#### Step 15: Exircises __________________________________________________________ ####
#1
df15 <- mtcars
df15$am <- factor(df15$am)
df15$vs <- as.factor(df15$vs)
fish <- fisher.test(df15$am, df15$vs)
fisher_test <- fish$p.value
#2
tbl = table(mtcars$am, mtcars$vs)    
fit  <- fisher.test(tbl)    
fisher_test_master = fit$p.value
#3
fisher_test <- fisher.test(mtcars$vs, mtcars$am)$p
#4
fisher_test <- fisher.test(mtcars$am, mtcars$vs)$p.value
#5
fisher_test <- fisher.test(mtcars$am, mtcars$vs)[1]
#
ft <- fisher.test(as.factor(mtcars$am), as.factor(mtcars$vs))
fisher_test <- ft$p.value

#### Step 16: Exircises __________________________________________________________ ####
  

