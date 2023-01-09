remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####
library(ggplot2)
library(psych)
library(dplyr)
library(readxl)
library(ROCR)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")#

yt <- read.csv2("yt.csv")
yt <- read_xlsx("plan.xlsx")
data(diamonds)


# step 1 lapply 
apply(array, margin, ...)

lapply(list, function) # lapply apply the function to the elements of list

my_list <- list(x = c(rnorm(30), NA), y = rnorm(10))
str(my_list)

# Approach with cycle
for (i in 1:length(my_list)) {
  print(mean(my_list[[i]], na.rm = T))
}

# Approach with lapply
lapply(my_list, mean)
lapply(my_list, mean, na.rm = T)
lapply(my_list, function(x) x * 2)

sapply(my_list, range, na.rm = T, simplify = F)
sapply(my_list, range, na.rm = T, simplify = T)

#### Step 3 of 16 ####
# Напишите функцию positive_sum, которая получает на вход dataframe с произвольным количеством числовых переменных. 
# Основная задача функции - найти сумму положительных значений в каждой переменной и сохранить их в список.
# Рассмотрим пример работы функции на небольшом примере:

test_data <- data.frame(matrix(rnorm(30), ncol = 6))
#
positive_sum <- function(test_data) {lapply(test_data, function(x) {sum(x[!is.na(x) & x > 0])})}
positive_sum(test_data)
#
positive_sum <- function(d) {lapply(d, function(x) sum(x[x > 0], na.rm = T))}
#
positive_sum <-  function(d){
  lapply(d, FUN = function(x) sum(subset(x, x > 0)))
}
#
positive_sum <- function(df){
  lapply(df, function(x) sum(x[x > 0 & !is.na(x)]))
}
#
positive_sum <-  function(test_data){
  test_data[test_data < 0] <- 0
  lapply(test_data, sum, na.rm = T)
}
#
positive_sum <-  function(x){
  pos_sum <- apply(x, 2, function(x) sum(x[!is.na(x) & x > 0]))
  return(as.list(pos_sum))
  
}

#### step 4 of 16 ####

cars <- c("Mazda", "Volga", "Merc")
car <- "Mazda RX4" 

grepl(car, cars)
grepl(cars, car) #!

## Approach with cycle
# 1
for (i in cars) {
  print(grepl(i, car))
}

#2

for (i in cars) {
  if (grepl(i, car)) {
    print(My_car <- i)
  }
}

## We will can do easier all the above

cars[sapply(cars, function(x) grepl(x, car))]
cars[lapply(cars, function(x) grepl(x, car))] # error

cars_grep <- sapply(cars, function(x) grepl(x, car))
typeof(cars_grep)
is.vector(cars_grep)

car_grep <- sapply(car, function(x) grepl(x, cars))
typeof(car_grep)
is.matrix(car_grep)

cars_grep1 <- lapply(cars, function(x) grepl(x, car))
typeof(cars_grep1)
is.vector(cars_grep1)
is.list(cars_grep1)

car_grep1 <- lapply(car, function(x) grepl(x, cars))
typeof(car_grep1)
is.vector(car_grep1)
is.list(car_grep1)


#### Step 5 of 16  ####
# Обратите внимание на следующее выражение, которое очень часто будет вам помогать при работе с данными:
# давайте напишем команду, которая отбирает только количественные колонки в данных:
iris_num <- iris[sapply(iris, is.numeric)]

# Готово! sapply(iris, is.numeric) возвращает вектор логических значений, который мы и используем для индексации.
# Этот пример также иллюстрирует идею, что lapply и sapply можно применять к dataframe. Так как dataframe - это в том числе и список.
# Например, результат команды:
sapply(iris[1:4], sd)
# эквивалентна результату:
apply(iris[1:4], 2, sd)

 # так как каждая колонка dataframe - это и есть элемент списка, то функция lapply и sapply
# возвращает результат применения некоторой функции к каждой колонке данных!
# Но тут есть одно но!
# Как вы помните, apply производит все опперации именно над матрицами, поэтому если вы отправите в apply dataframe 
# с разными типами данных, то R сначала приведет все колонки к одному типу, чтобы получилась матрица, 
# т.к. в матрице могут храниться данные только одного типа! 
# Это в свою очередь может привести к неожиданному результату:
sapply(iris, is.numeric)

apply(iris, 2, is.numeric)

# По результатам команды apply можно подумать, что в данных нет количественных переменных! Дело в том, что перед тем как 
# применить функцию is.numeric, сначала данные iris были переведены в матрицу,
# а все переменные переведены в строки, как в наиболее общий тип данных. 
# В результате получаем для каждой колонки FALSE.
# Вот такой вот тонкий момент, о котором нужно помнить, применяя функцию apply к data.frame. 
# В свою очередь с sapply и lapply такого не случится, т.к. в этом случае мы по очереди применим требуемую функцию к каждой колонке данных, 
# как к каждому элементу списка!

#### step 6 of 16  tapply, aggregate, by ####
tapply(mtcars$mpg, mtcars$am, function(x) mean(x))
aggregate(mpg ~ am, mtcars, function(x) mean(x))
aggregate(mpg ~ am, mtcars, mean)

# by группирует не вектор в отличие от вышеуказанных функций, а целый датафрейм по какой-то переменной
by(iris[1:4], iris$Species, colMeans)

tapply(iris[1:4], iris$Species, mean) # error
aggregate(. ~ Species, iris, mean)

by(iris[1:4], iris$Species, 
   function(x) sapply(x, 
                      function(col) shapiro.test(col)$p.value))

aggregate(. ~ Species, iris, function(x) shapiro.test(x)$p.value)








#### step 7 of 16 vapply ####

vapply(list, function, FUN.VALUE = type, ...)
vapply(mtcars, mean, FUN.VALUE = numeric(1))
sapply(mtcars, mean)

mapply(rep, c(1, 2, 3, 4), c(1, 2, 3, 4))
rep(c(1, 2, 3, 4), c(1, 2, 3, 4))
rep(1, 3)

x <- c(20, 25, 13)
m <- c(0, 1, 2)
s <- c(3, 5, 6)
mapply(rnorm, x, m, s)



#### Step 9 of 16  ####
# Давайте рассмотрим один небольшой пример работы с функцией mapply. Я оговорился, что она довольно специфична, однако иногда она все-таки 
# оказывается довольно полезной.
# Допустим у нас есть матрица размером 100 на 200:
m <- matrix(rnorm(100 * 200), nrow = 100)
# И мы хотим присвоить имена строчкам и столбикам в этой матрице по принципу:
# row_1, row_2, row_3, ..., row_100 - для строк
# col_1, col_2, col_3, ..., col_200 - для колонок
# Тогда мы могли бы сгенерировать список данными именами следующим образом:
m_names <- mapply(paste, list("row", "col"), list(1:100, 1:200), sep = "_")
str(m_names)
dimnames(m) <- m_names
head(m)

p1 <- paste(a, b, sep = " ")

#### Step 10 of 16  ####
# Хотелось бы рассмотреть еще один подводный камень применения функций семейства apply к dataframe.
# Предположим, мы решили написать простенькую функцию для расчета стандартного отклонения количественных переменных в данных.
get_sd <- function(x){
  num_var <- sapply(x, is.numeric) # x[sapply(x, is.numeric)]
  sapply(x[, num_var], sd)
}

# Казалось бы, все логично и работает на различных примерах:
get_sd(iris)
# Но в нашем коде скрыта серьезная уязвимость!) Предположим, у нас есть набор данных, 
# в котором только одна количественная переменная:
my_df <- data.frame(x = 1:10, y = letters[1:10])
get_sd(my_df)

# Что вообще только что произошло? Дело в том, что существуют различные способы обращения к колонкам dataframe:
my_df[1]     # получим dataframe
my_df[[1]]   # получим вектор
my_df[, 1]   # получим вектор
# В случае, если у нас только одна количественная переменная, обращение x[, num_var] вернет колонку в виде вектора, 
# а sapply применит функцию sd к каждому наблюдению вместо того, чтобы применить ко всей переменной.
# Таким образом, если вы хотите применить какую-либо функцию к неизвестному заранее числу колонок в данных, 
# лучше используйте такую индексацию типа: my_df[col_index]. То есть:

get_sd <- function(x){
num_var <- sapply(x, is.numeric)
sapply(x[num_var], sd)
}
7et_sd(my_df)



#### Step 11 of 16  ####
# короткое решение с использованием регулярного выражения https://habr.com/ru/post/545150/
my_names <- function(test_data, names){
  
  # слепляем все слова в одну строку, разделив их символом "|",
  # который на языке регулярных выражений означает "или",
  # т.е. grepl("HPS7|HPS4", dataset$name) будет искать совпадение в dataset$name
  # c "HPS7" или "HPS4"
  names_line <- paste0(names, collapse = "|")
  
  # возвращаем датасет со строками, в которых нашлись с какими-то из имен,
  # т.е. где команда grepl(names_line, dataset$name) вернула TRUE
  return(test_data[grepl(names_line, test_data$name),])
}

# более длинное решение без регулярных выражений 
my_names_2 <- function(dataset, names){
  
  # получаем список с результатами поочередного применения имен из names к 
  # колонке с именами в датасете
  l <- lapply(names, grepl, test_data$name)
  
  # складываем поэлементно все листы, помня, что значение TRUE = 1, а значение 
  # FALSE = 0,  т.е. после сложения в том елементе, где хотя бы один раз было TRUE
  # в итоговом векторе на этом месте будет значение отличное от 0
  sum_res <- Reduce("+", l)
  
  # возвращаем те строки датасета, у которых соответсвующие значения суммарного 
  # вектора отлично от 0
  return(test_data[sum_res > 0,])
}

# тестирование
dataset <- as.data.frame(list(name = c("p4@HPS1", "p7@HPS2", "p4@HPS3", "p7@HPS4", "p7@HPS5", "p9@HPS6", "p11@HPS7", "p10@HPS8", "p15@HPS9"), 
                                expression = c(118.84, 90.04, 106.6, 104.99, 93.2, 66.84, 90.02, 108.03, 111.83)))


names = c("HPS5", "HPS6", "HPS9", "HPS2", "HPS3", "HPS7", "HPS4", "HPS8")
names = c("HPS5", "HPS6")
my_names(test_data, names)
my_names_2(test_data, names)

#
my_names <- function (dataset, names){    
  dataset[as.numeric(lapply(names, function(x) which(grepl(x, dataset$name)))),]}

#
my_names <- function (dataset, names){    
  gs=gsub('^.*\\@','',dataset[,1])    
  return(dataset[gs %in% names,])}

#
my_names <- function (dataset, names) {
  dataset[sapply(names, grep, dataset$name), ]
}

#
my_names <- function (dataset, names) dataset[grepl(paste(names,collapse = "|"), dataset$name),]

#
my_names <- function (dataset, names) {
  dataset[gsub(".*@", "", dataset$name) %in% unique(names), ]
}



#### Step 12 of 16  ####
t <- test_data
find_outliers <- function(t){
  t %>%
    # делаем факторами все нечисловые колонки
    mutate_if(Negate(is.numeric), factor) %>%
    # группируем по колонкам-факторам
    group_by_if(is.factor) %>%
    # рассчитываем по группам критические значения и сравниваем значение х с
    # критическими значениями для групп 
    # переводим получившиеся TRUE/FALSE в integer
    mutate(is_outlier = as.integer(x > mean(x) + 2 * sd(x) | 
                                     x < mean(x) - 2 * sd(x))
    ) 
}

find_outliers(test_data)

mutate_if(t, Negate(is.numeric), factor)

test_data <- as.data.frame(unclass(read.csv("https://stepic.org/media/attachments/course/724/hard_task.csv")), 
                           stringsAsFactors = TRUE) %>% as_tibble()
str(test_data)

colnames(select_if(test_data, is.factor)) %>% paste(, sep = "+")

aggregate(. ~ factor_2 + factor_3, test_data, mean)

test_data %>% mutate(is_outlier = ifelse())





#### Step 13 of 16  ####
#### Step 14 of 16  ####
#### Step 15 of 16  ####
#### Step 16 of 16  ####


remove(list = ls())
nds <- 0.2

V1

one_thing_without_nds <- 4567.87
quantity_of_things <- 10
summ_of_nds <- 9135.74
one_thing_with_nds <- 5481.44
one_thing_without_nds*quantity_of_things

one_thing_with_nds/100*20
one_thing_without_nds/100*20
nds_for_one_thing <- 913.574
nds_for_one_thing*10

one_thing_without_nds_v2 <- 4567.87
summ_of_nds <- 9135.74
one_thing_with_nds <- 5481.44
one_thing_without_nds*quantity_of_things

V2


c <- letters

