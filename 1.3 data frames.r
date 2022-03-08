remove(list = ls())

#### Contens ####
# Этот урок посвящён работе с data frame. Вначале мы посмотрим, как загружать готовые датасеты в R 
# с помощью функции read.table (read.csv). Чтобы функция сработала, убедитесь, что: 
# 1) указано расширение загружаемого файла
# 2) название файла взято в кавычки
# 3) загружаемый файл находится в рабочей директории. 

# Если файл находится не в рабочей директории, есть три способа решить эту проблему: 
# 1) изменить рабочую директорию (в RStudio: Session -> Set Working Directory -> Choose Directory...)
# 2) переместить файл в актуальную рабочую директорию
# 3) указать полный путь к файлу, например: read.csv("C:/Users/username/documents/evals.csv")

#Скрипт для урока можно скачать по ссылке: https://stepic.org/media/attachments/lesson/11481/data%20frames.R.
# Датасет, с которым мы будем работать: https://stepic.org/media/attachments/lesson/11481/evals.csv.


####  Reading data #### 

?read.table
?read.csv

mydata <- read.csv('evals.csv')

####  Summaries #### 

head(mydata) # комманда head по умолчанию выводит шесть первых строк
head(mydata, 3) # через запятую можно указать сколько строк выводить

tail(my_data) # комманда tail по умолчанию выводит шесть последних строк
tail(my_data, 7) # через запятую можно указать сколько строк выводить

View(my_data)
str(my_data) # показывает внутреннюю структуру объекта R
summary(my_data)

a <- names(mydata)

summary(yt$duration) #
summary(yt$teamleader) #
hist(yt$duration) #


# Variabyt_2# Variables

b <- mydata$score
yt$teamleader

rm(b)
mean(mydata$score)
mean(yt$teamleader) # не работает 

summary(mydata$score)
summary(yt[, c('numb_ret_depir', 'numb_ret_oiv', 'duration')])

mydata$score * 2

mydata$ten_point_scale <- mydata$score * 2



summary(mydata$ten_point_scale)

mydata$new_varible <- 0
yt_2$new_varible <- 1:nrow(yt_2)
mydata$number <- 1:nrow(mydata)
summary(mydata$number)

nrow(mydata)
ncol(mydata)


#### Subsetting ####

mydata$score[1:10]

mydata[1,1]
mydata[c(2,193,225),1]
mydata[101:200,1]

mydata[5,]
mydata[,1] == mydata$score

mydata[,2:5]
head(mydata[,2:5])


#### Subsetting with condition ####

mydata$gender
mydata$gender == 'female'
head(mydata[mydata$gender == 'female',1:3])

head(subset(mydata, gender == 'female'))
head(subset(mydata, score > 3.5))

yt_21 <- subset(yt_2, teamleader == "Осипенкова Елена Владимировна")
yt_22 <- subset(yt_2, numb_ret_depir > 5)
yt_22 <- subset(yt_2, numb_ret_depir > 5 & deviat_numb_ret_oiv < 5)

#### rbind, cbind ####

mydata2 <- subset(mydata, gender == 'female')
mydata3 <- subset(mydata, gender == 'male')
mydata4 <- rbind(mydata2, mydata3)

mydata5 <- mydata[,1:10]
mydata6 <- mydata[,11:24]
mydata7 <- cbind(mydata6, mydata5)

