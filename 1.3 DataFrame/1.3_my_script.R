remove(list = ls())

# Reading data

?read.table()
?read.csv

my_data <- read.csv('https://stepik.org/media/attachments/lesson/11481/evals.csv')

head(my_data) # комманда head по умолчанию выводит шесть первых строк
head(my_data, 7) # через запятую можно указать сколько строк выводить

tail(my_data) # комманда tail по умолчанию выводит шесть последних строк
tail(my_data, 7) # через запятую можно указать сколько строк выводить

View(my_data)
str(my_data) # показывает внутреннюю структуру объекта R
summary(my_data)

a <- names(my_data)

# Variables (Обращение к переменным)

b <- my_data$score

mean(my_data$score)
summary(my_data$score)

my_data$score*2

my_data$ten_point_scale <- my_data$score*2 # Если после знака $ указать не существующую переменную,
# то она будет создана. 

summary(my_data$ten_point_scale)

my_data$new_variable <- 0
my_data$number <- 1:nrow(my_data)
summary(my_data$number)
nrow(my_data)
ncol(my_data)


# Subsetting

my_data$score[1:10]
my_data[1,1]
my_data[c(2,193,225),1]
my_data[101:200,1]

my_data[5,]
my_data[,1] == my_data$score

my_data[,2:5]
View(my_data[,2:5])
View(head(my_data[,2:5]))




# Subsetting with condition

my_data$gender
my_data$gender == 'female'
my_data[,1]
my_data[my_data$gender =='female', 1]
my_data[my_data$gender == 'female', 1:3]
View(my_data[my_data$gender == 'female', c(1:3)])
head(my_data[my_data$gender == 'female', 1:3])

subset(my_data, gender == 'female')
View(subset(my_data, gender == 'female'))
head(subset(my_data, gender == 'female'))

subset(my_data, score > 3.5)
View(subset(my_data, score > 3.5))
head(subset(my_data, score > 3.5))

subset(my_data, score < 3.5)
View(subset(my_data, score < 3.5))
head(subset(my_data, score < 3.5))

subset(my_data, score == 3.5)
View(subset(my_data, score == 3.5))
head(subset(my_data, score = 3.5))

# rbind, cdind

my_data2 <- subset(my_data, gender == 'female')
my_data3 <- subset(my_data, gender == 'male')
my_data4 <- rbind(my_data2, my_data3)

my_data5 <- my_data[, 1:10]
my_data6 <- my_data[, 11:24]
my_data7 <- cbind(my_data5, my_data6)

library(help = 'datasets')
mtcars
data(mtcars)
?mtcars

mydata <- mtcars


### Step 7 из 11
# Вариант 1
mtcars$even_gear <- ?ifelse (mtcars$gear %% 2 == 1, 0, 1)

# Варинат 2
mtcars$even_gear1 <- (mtcars$gear+1) %% 2 

# Вариант 3
mtcars$even_gear2[mtcars$gear%%2 == 0] <- 1
mtcars$even_gear2[mtcars$gear%%2 == 1] <- 0

# Вариант 4
data('mtcars')
a <- mtcars$gear
even <- subset(mtcars, a%%2 == 0)
even$even_gear <- 1

uneven <- subset(mtcars, a%%2 == 1)
uneven$even_gear <- 0

mtcars <- rbind(even, uneven)


### Step 8 из 11
# Вариант 1
mpg_4.1 <- mtcars$mpg[mtcars$cyl == 4]

# Вариант 2
s <-subset(mtcars, cyl==4)
mpg_4.2 <- s$mpg

# Вариант 3
mpg_4 <- mtcars[mtcars$cyl == 4, "mpg"]


### Step 9 из 11
# Вариант 1
mini_mtcars <- mtcars[c(3,7,10,12,32),]

# Вариант 2
mini_mtcars <- mtcars[c(3, 7, 10, 12, nrow(mtcars)), ]

# Вариант 3
mydata <- mtcars
s3 <- subset(mydata[3,])
s7 <- subset(mydata[7,])
s10 <- subset(mydata[10,])
s12 <- subset(mydata[12,])
s32 <- subset(mydata[nrow(mydata),])
mini_mtcars1 <- rbind(s3, s7, s10, s12, s32)

### Step 11 из 11
new_data <- subset(mtcars, cyl !=3 & qsec > mean(qsec))























