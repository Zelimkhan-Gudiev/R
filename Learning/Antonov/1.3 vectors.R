remove(list = ls())
rm()

#### 1 of 11 ####
# В этом уроке мы:
# 1) рассмотрим ключевой объект языка -- вектор;
# 2) посмотрим, какие есть удобные возможности для создания численных векторов;
# 3) узнаем про типы векторов и правила их приведения;
# 4) увидим, как действует принцип векторизации.
# Вектора и векторизация -- это краеугольный камень языка R и ключ к написанию красивого и эффективного кода. 
# Идеи, которые лежат в основе урока, редко встречаются в других языках программирования, так что будьте внимательны!

?c, ?":"
?seq, ?rep
?print
?all.equal
?typeof
?is.logical, ?as.logical, etc.
?length, ?names

#### 2 of 11 ####
x <- c(5, 8)
y <- c(x, 1, c(3, 4), x, NA)

5:9
3:-1
seq(1, 2, by = 0.25)
seq(3, 4, length.out = 5)
seq(3, 4, length = 5)

rep(1:3, times = 3)
rep(1:3, each = 4)
rep(1:3, length.out = 5)
rep(1:3, length = 5)

#### 3 of 11 ####

#### 4 of 11 ####


#### 5 of 11 ####

# Traditions , traditions...
hello <- "Hello world"
print(hello)
hello

# Lost in translation
c(1, 2, 3)
с(1, 2, 3) # russian "c". Error in с(1, 2, 3) : could not find function "с"

# Important caveat
0.1 + 0.1 == 0.2             # TRUE
0.1 + 0.05 == 0.15           # FALSE
all.equal(0.1 + 0.05, 0.15)  # TRUE
# R FAQ 7.31
# google "R why are these numbers not equal"
# https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal

# Non-trivial sequence: "all thirds and sevenths"
u <- seq(0, 1, by = 1/3)
v <- seq(0, 1, by = 1/7)
c(u, v)
help.search('sort')
w <- sort(c(u, v))
# google "remove duplicate elements from vector"
w <- unique(w)


#### 6 of 11 ####
# Вместе мы решили задачу про 1/3 и 1/7. Напишите функцию get_fractions, которая принимает на вход два числа, m и n, 
# и возвращает аналогичный вектор, содержащий все дроби вида {i/m, i = 0, 1, ..., m} и  {j/n, j = 0, 1, ..., n}. 
# Вектор не должен содержать повторов. И -- сюжетный поворот -- должен быть упорядочен в порядке убывания.
# Подсказки:
#  проверить функцию можно подстановкой m=3, n=7: должен получиться вектор из моего примера, только задом наперёд;
# в R огромное количество функций, а у многих функций есть вспомогательные аргументы.

get_fractions <- function(m, n) {
  x <- seq(0, 1, 1/m)
  y <- seq(0, 1, 1/n)
  z <- sort(c(x, y), decreasing = T)
  z <- unique(z)
  return(z)
}


get_fractions(3, 7)

# 
get_fractions <- function(m, n) {
  sort(unique(c(0:m/m,0:n/n)), d = T)
}

#
get_fractions <- function(m, n) {
  sort(unique(c(seq(0, 1, 1 / m), seq(0, 1, 1 / n))), decreasing = TRUE)
}

#
get_fractions <- function(m, n) {
  vect <- c(0:m/m, 0:n/n) # задаем вектор всех чисел 0, 1/m, 2/m,...m, 1, 0, 1/n,...,(n-1)/n, 1
  vect_unique <- unique(vect) # убираем повторяющиеся значения
  vect_sorted_unique <- sort(vect_unique, decreasing = TRUE) # располагаем элементы по убыванию
  return(vect_sorted_unique)
}

#### 7 of 11 Типы векторов ####
#  Вектор может содержать данные только одного типа, поэтому вектора в документации часто назыают атомарными векторами.
# Основные типы векторов:
# 1) logical (TRUE/FALSE)
# 2) integer (целые числа)
# 3) numeric/double (числа с плавающей точкой (вещественные числа?!))
# 4) complex (комплексные числа)
# 5) character (строки)
# 6) raw (байтове последовательности)

# тип вектора можно определить с помошью typeof() и is.* (где * это любой из типов вектора)

## Приведение типов.
# Естественное приведение типов. 
# Естественным считается приведение типов слева на право по цепочке
# logical-integer-double-character

b <- c(FALSE, 1.5)
typeof(b) # double. Если данное приведение типов будет выполенно в процессе выполения программы, то никого предупреждения не последует. 
# Вектор b будет корректно создан и тип у него будет double. Это значит, что FALSE естественным образом 
# будет приведен к 0 и добавлено к значению 1.5 в векторе b
b <- c(5, b, "abc")
typeof(b) # character

# Принудитильное приведение типов
# Принудительное приведение типов осуществляется с помошью функций семейства as.* (где * это любой из типов вектора)

as.numeric(b) # приведем строковый вектор b (character) к типу integer
as.double(b)
as.integer(b) # ! происходит округление

## Длина вектора
x <- 1:100
length(x)
length(x) <- 4 # Можно принудительно изменить длину вектора
length(x) <- 7 # Можно принудительно изменить длину вектора

## Имена элементов вектора
# Есть два способа присвавать имена элементам вектора
# v1 
a <- c(uno = 1, dos = 2, 'universal answer' = 42, 99)
names(a)
# v2
names(a) <- c("one", "two", "forty two", "ninety nine")

names(a) <- NULL # удаляет вектор имен


#### 8 of 11 ####
#### 9 of 11 ####
# Давайте экспериментировать с типами! Функции семейства as.* дают возможность устраивать форменные безобразия. 
# Установите соответствие между вызовами слева и результатами справа.
# Попробуйте пройти это задание, не пользуясь консолью R. 
-1:1  #  -1  0  1
as.logical(-1:1) # TRUE FALSE TRUE
as.character(as.logical(-1:1)) # "TRUE"  "FALSE" "TRUE" 
as.numeric(as.logical(-1:1)) # 1 0 1
as.character(as.numeric(as.logical(-1:1))) # "1" "0" "1"
as.numeric(as.character(as.logical(-1:1))) # NA NA NA



#### 10 of 11 ####
# Я упоминал, что большинство имён из букв и цифр в R являются валидными, то есть такими, которые можно использовать в качестве имён 
# переменных или имён names() в векторах. Попробуйте придумать способ, с помощью которого можно проверять валидность имени 
# и с его помощью отметьте только валидные имена из предложенного списка.
# P.S. Строго описанные правила именования есть в R FAQ и на stackoverflow. Кто ищёт, тот всегда найдёт!
# P.P.S. Пожалуйста, никогда не именуйте переменные похожим образом. Это учебный пример!

next <- 'a' # Error in next <- 1 : invalid (NULL) left side of assignment
xXx_TerM1Nat0R_xXx <- 1 # ok
2b.or.not.2b <- 1 # Error: unexpected symbol in "2b.or.not.2b"
.hidden <- 1
function <- 1 # Error: unexpected assignment in "function <-"
:smiley: <- 1 # Error: unexpected ':' in ":"
super_long_name_in_fact_so_long_i_cannot_stop_typing_please_help <- 1 # ok


ls(all.names = TRUE)
?make.names
make.names("function") # "function."





#### 11 of 11 ####



