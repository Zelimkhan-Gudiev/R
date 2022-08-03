remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####



#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")


# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»

#### Step 1,2: Contens  _______________________________________________________________________________________________________ ####
# В этом уроке мы научимся создавать собственные функции в R!
# Ссылка на скрипт урока.
# Ссылка на данные урока.

my_calc  <- function(x, y){
  s  <- x + y
  return(s)
}

my_calc(x = 10, y = 15)
result  <- my_calc(10, 15)

my_calc_2  <- function(x, y){
  s  <- x + y
  d  <- x - y
  return(c(s, d))
}

my_calc_2(10, 15)


my_calc_3  <- function(x, y, z){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3(1, 2, 3)

my_calc_3  <- function(x, y, z = 10){
  s  <- x + y + z
  d  <- x - y - z
  return(c(s, d))
}

my_calc_3(1, 2)


############

distr1  <- rnorm(100)
hist(distr1)
distr1[1:30]  <- NA
distr1[is.na(distr1)]  <- mean(distr1, na.rm = T)

my_na_rm  <- function(x){
  if (is.numeric(x)){
    stat_test  <- shapiro.test(x)
    if (stat_test$p.value > 0.05){
      x[is.na(x)]  <- mean(x, na.rm = T)
      print("NA values were replaced with mean")
    } else{
      x[is.na(x)]  <- median(x, na.rm = T)
      print("NA values were replaced with median")
    }
    return(x)
  } else{
    print("X is not numeric")
  }
}

d1  <- rnorm(2000)
d2  <- runif(2000)

d1[1:10]  <- NA
d2[1:10]  <- NA

d1  <- my_na_rm(d1)
head(d1)

d2  <- my_na_rm(d2)
head(d2)
rm(my_na_rm)
source('/Users/zelimkhan/Desktop/Data/GitHub/my_na_rm.R')


#### Step 7 of 11 https://stepik.org/lesson/11507/step/7?auth=login&thread=solutions&unit=2530 ####
# Напишите функцию, которая выводит номера позиций пропущенных наблюдений в векторе.
# На вход функция получает числовой вектор с пропущенными значениями. 
# Функция возвращает новый вектор с номерами позиций пропущенных значений.
# Подсказка: чтобы проверить является ли наблюдение NA, воспользуйтесь функцией is.na(), 
# кстати, функция векторизирована, и аргументом может служить вектор произвольной длинны.
# Запись x == NA ни к чему осмысленному не приведет. Т.к. если x это NA, то команда x == NA также вернет NA, а не TRUE!
# > my_vector <- c(1, 2, 3, NA, NA)
# > NA.position(my_vector)
# [1] 4 5

my_vector <- c(1, 2, 3, NA, NA)
which(is.na(my_vector) == T)


NA.position(my_vector)



# 
NA.position <- function(x) {
  which(is.na(my_vector))
  return(which(is.na(my_vector)))
}

#
NA.position <- function(x){
  temp <- is.na(x)
  ans <- c()
  
  for (i in 1:length(temp)){
    if (temp[i] == T){
      ans <- c(ans, i)
    }
  }
  return(ans)
}

#
NA.position <- function(x){
  new_vector <- c()
  a <- 1
  for(i in 1:length(x)){
    if(is.na(x[i])) {
      new_vector[a] <- i
      a <- a+1
    }
  }
  return(new_vector)
}

#
NA.position <- function(my_vector){
  if (any(is.na(my_vector))){
    res <- which(is.na(my_vector))
  } else {
    res <- ("NAs not found in this vector")
  }
  print(res)
}

#
NA.position <- function(x){(1:length(x))[is.na(x)]}

#
NA.position <- function(x){
  # put your code here  
  j <- 1
  b <- c()
  for (i in 1:length(x)){
    if (is.na(x[i])){
      b[j] <- i 
      j <- j + 1
    } else {
    }
  }
  print(b)
}

#
NA.position <- function(x){
  return((1:length(x))[is.na(x)])
}

#
NA.position <- function(x){    
  which(is.na(x))}

#
NA.position <- function(x){
  df <- data.frame(number = c(1:length(x)), varx = x)
  subset(df, is.na(varx))$number 
}

#
NA.position <- function(x){
  ex <- 1:length(x)
  return(ex[is.na(x)])  
}

#
NA.position = function(my_vector) {  
  x = list()
  counter = 0
  counter_num = 0
  for (i in is.na(my_vector)) {
    counter = counter + 1
    if (i == T) {
      counter_num = counter_num + 1
      x[counter_num] = counter
    }
    x = unlist(x)
  } 
  return(x)
}

#
NA.position <- NA.position<- function(x){
  which(x%in% NA)
}

#
NA.position <- function(x){
  xna <- is.na(x)
  nann <- c()
  for (i in (1:length(xna)))
    if (xna[i]) nann <- c(nann, i) 
  return(nann)
  
}

#
NA.position <- function(x){
  c <- which(is.na(x))
  return(c)
}

#
NA.position <- function(x){
  n <- length(x)
  prv <- 1:n
  mm <- is.na(x)
  print(prv[mm])
  
}

#
NA.position <- function(x){
  c(1:length(x))[is.na(x)]
}



#### Step 8 of 11 https://stepik.org/lesson/11507/step/8?auth=login&thread=solutions&unit=2530 ####
# Напишите функцию NA.counter для подсчета пропущенных значений в векторе.
# На вход функция  NA.counter должна принимать один аргумент - числовой вектор. 
# Функция должна возвращать количество пропущенных значений.
# my_vector <- c(1, 2, 3, NA, NA)
# NA.counter(my_vector)
# [1] 2 


NA.counter <- function(x) {
  sumNa <- sum(is.na(x))
  return(sumNa)
}

distr8 <- rnorm(100)
distr8[1:10] <- NA

NA.counter(distr8)

#
NA.counter <- function(x){    
  return(sum(is.na(x)))}

#
NA.counter <- function(x){
  length(which(is.na(x) ))
}

#
NA.counter <- function(x){
  counter <- 0
  for(i in 1:length(x)){
    if(is.na(x[i])) {
      counter <- counter+1
    }
  }
  return(counter)
}

#
NA.counter <- function(x){
  r <- ifelse(is.na(x), 1, 0)
  sum(r)
}

#
NA.counter <- function(x){
  length(is.na(x)[x==TRUE])
}

#
NA.counter <- function(c){
  out <- which(is.na(c))
  number <- NROW(out)
  return(number)
}

#
function(x){return(length(x)-length(na.omit(x)))}

#
NA.counter <- function(x){
  c <- na.omit(x)
  b <- which.max(which(c%in%c))
  g <- which.max(which(x%in%x))
  return(g-b)
}



#### Step 9 of 11 ####

read_data  <- function(){
  df  <- data.frame()
  number  <<- 0
  for (i in dir(pattern = "*.csv")){
    temp_df  <- read.csv(i, stringsAsFactors = F)
    df  <- rbind(temp_df, df)
    number <<- number + 1
    }
  print(paste(as.character(number), "files were combined"))
  return(df)
}

# Advanced method without for loop

read_data_advanced <- function(){
    df <- do.call(rbind, lapply(dir(pattern = "*.csv"), 
                                read.csv, stringsAsFactors = F))
    return(df)
}

df  <- data.frame(x = factor(1:5))
df1  <- data.frame(x = factor(7:8))
str(df)
str(df1)

df3  <- rbind(df, df1)
str(df3)
table(df3$x)



#### Step 10 of 11 ####

# Напишите функцию filtered.sum, которая на вход получает вектор с пропущенными, положительными и отрицательными значениями
# и возвращает сумму положительных элементов вектора.
# filtered.sum(c(1, -2, 3, NA, NA))
# [1] 4 

vector10 <- c(-3, NA, -1, 0, 1, 2, 3)
filtered.sum(vector10)

####
filtered.sum <- function(x) {
  sumP <- sum(x[x > 0], na.rm = T)
  return(sumP)
}

#
filtered.sum <- function(x) {
  return(sum(x[x > 0], na.rm = T))
}

#
filtered.sum <- function(x){
  sum(ifelse(x > 0 & !is.na(x), x, 0))
}

#
filtered.sum <- function(x) {
  sum(replace(x, x < 0 | is.na(x), 0))
}

#
filtered.sum <- function(x) sum(x[x>0 & !is.na(x)])

#
filtered.sum <- function(x) {
  return(sum(x[which(is.numeric(x) & x > 0)]))
}

#
filtered.sum <- function(x) {
  summ = 0
  for (i in 1:length(x)){
    if ((is.na(x[i]) == FALSE) & (x[i] > 0)) {
      summ = summ + x[i]}
  }
  return(summ)
}

#### Step 11 of 11 ####
# Задача для героев!
# Напишите функцию outliers.rm, которая находит и удаляет выбросы. 
# Для обнаружения выбросов воспользуемся самым простым способом, с которым вы не раз встречались, используя график Box plot. 
# Выбросами будем считать те наблюдения, которые отклоняются от 1 или 3 квартиля больше чем на 1,5 *  IQR,
# где  IQR  - межквартильный размах.
# На вход функция получает числовой вектор x. Функция должна возвращать модифицированный вектор x с удаленными выбросами. 
# Ссылка на видео с объяснением, как на графике box-plot отображаются выбросы:
# https://stepic.org/lesson/%D0%9A%D0%B2%D0%B0%D1%80%D1%82%D0%B8%D0%BB%D0%B8-%D1%80%D0%B0%D1%81%D0%BF%...
# Полезные функции:
# IQR(x) - рассчитывает межквартильный размах вектора x
# quantile(x, probs = c(0.25, 0.75)) - рассчитывает первый и третий квартиль вектора x   

distr11 <- rnorm(100, mean = 50, sd = 5)
sd(distr11)

outliers.rm(v111)

#v1
outliers.rm <- function(x) {
  x <- ifelse(x < 1.5 * IQR(x) - quantile(x, probs = 0.25) | x > 1.5 * IQR(x) + quantile(x, probs = 0.75), 0, x)
  x <- x[x != 0]
  return(x)
}

#v2
outliers.rm <- function(x) {
  x <- x[x > 1.5 * IQR(x) - quantile(x, probs = 0.25) & x < 1.5 * IQR(x) + quantile(x, probs = 0.75)]
  return(x)
}
outliers.rm(v111)

#v3
outliers.rm <- function(x){
  out_vec <- boxplot.stats(x)$out
  return(x[-which(x %in% out_vec)])
}
outliers.rm(v111)

#4
outliers.rm <- function(x){
  x1 <- x[-which(x>(as.numeric(quantile(x, probs = c(0.25, 0.75))[2] + (1.5*IQR(x)))))]
  x2 <- x[-which(x<(as.numeric(quantile(x, probs = c(0.25, 0.75))[1] - (1.5*IQR(x)))))]
  x <- x[-which(x %in% c(x1, x2))]
  return(x)
}
outliers.rm(v111)

#5
outliers.rm <- function(x){
  x1 <- x[which(x<(as.numeric(quantile(x, probs = c(0.25, 0.75))[2] + (1.5*IQR(x)))))]
  x2 <- x[which(x>(as.numeric(quantile(x, probs = c(0.25, 0.75))[1] - (1.5*IQR(x)))))]
  x <- c(x1, x2)
  return(x)
}
outliers.rm(v111)

#6
outliers.rm <- function(x){
  mod_x <<- c()
  q1 <- quantile(x,probs=c(0.75))
  q2 <- quantile(x,probs=c(0.25))
  for(i in 1:length(x)){
        if (x[i]<(q1+1.5*IQR(x)) & x[i]>(q2-1.5*IQR(x))) {
      mod_x <<- append(mod_x, x[i])
      return(x)
    }
  }
}
#
outliers.rm <- function(x){
  mod_x <<- c()
  q1 <- quantile(x,probs=c(0.75))
  q2 <- quantile(x,probs=c(0.25))
  iq <- 1.5*IQR(x)
  for(i in 1:length(x)){
    if (x[i]<=(q1+iq) & x[i]>=(q2-iq)) {
      mod_x <<- append(mod_x, x[i])
      return(x)
    }
  }
}

#
v111 <- as.numeric(unlist(strsplit("16.3 8.13 22.92 19.84 22.64 22.43 27.66 12.06 21.19 24.08 12.89 16.82 27.1 15.11 30.0 21.75 17.99 20.48 25.77 18.49 16.68 22.2 17.17 11.9 17.48 7.37 18.84 27.57 21.24 26.33 19.56 17.11 11.29 12.1 16.95 15.8", " ")))

# sample input
v111 <- c(-0.73, 0.08, 1.04, 0.09, 1.01, 6.11, -1.69, -1.08, -0.47, 0.82, -0.25, 13.91, -6.39, 0.19, 0.69, 1.95, -0.01, 0.27, 6.52, -0.82, -0.41, 0.93, -68.37, -2.97, 1.36, 3.7, -0.78, -0.42, -3.59, 0.19)
# sample output
v1111 <- c(-0.73, 0.08, 1.04, 0.09, 1.01, -1.69, -1.08, -0.47, 0.82, -0.25, 0.19, 0.69, 1.95, -0.01, 0.27, -0.82, -0.41, 0.93, -2.97, 1.36, -0.78, -0.42, 0.19)
























v111_1 <- v111[-which(v111>(as.numeric(quantile(v111, probs = c(0.25, 0.75))[2] + (1.5*IQR(v111)))))]
v111_2 <- v111[-which(v111<(as.numeric(quantile(v111, probs = c(0.25, 0.75))[1] - (1.5*IQR(v111)))))]
v111 <- v111[-which(v111 %in% c(v111_1))]
v111 <- v111[-which(v111 %in% c(v111_2))]

