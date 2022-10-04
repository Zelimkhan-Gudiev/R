remove(list = ls())
rm()

#### 1 of 11 ####
# В этом уроке мы:
# рассмотрим условные операторы и циклы;
# ужаснёмся, как легко "повесить" R, если забыть о векторизации;
# перестанем называть пакеты библиотеками;
# узнаем об установке и поддержке пакетов.

# Условия и циклы всегда так или иначе присутствуют в реализациях хоть сколько-нибудь сложных алгоритмов. 
# Для того, чтобы в этом убедиться, достаточно посмотреть в исходный код популярных пакетов. 
# Вы ведь не успели забыть, что исходный код R и всех его пакетов полностью открыт?

?if, ?ifelse, ?switch
?Logic
?repeat, ?while, ?for
?installed.packages(), ?library(), ?require()
?install.packages(), ?update.packages()
? sessionInfo()


#### Step 2 of 10 ####

## if, else, ifelse
# if (conditions) {do something} else {do onother thing}


## switch
switch("factorial",
       sum = 5 + 5,
       product = 5 * 5,
       factorial = factorial(5),
       0)

switch("sum",
       sum = 5 + 5,
       product = 5 * 5,
       factorial = factorial(5),
       0)

## repeat
i <- 0 # Создадим переменную i 
repeat {
  i <- i + runif(1) # Будем добавлять к переменной i случайное значение от 0 до 1 
  print(i)
  if (i > 5) break # до тех пор пока значение i не превысит 5. Когда значение i превысит 5, 
                   # то при помощи условного оператора if и ключевого слова break выйдим из цикла.
}

## while
# while работает похожим образом
i <- 2^14
while(i > 100) {
  i <- i/2
  print(i)
}


## for

for (i in 1:8) {
  if (i %% 2 == 0) 
    print(i)
}

for (i in letters) {
  if (i == 'b') next
  if (i == "d") break
  print(i)
}

for (i in 1:5)  i # Внутри цикла for необходимо принудительно вызывать функцию print

## for против векторизации
v <- 1:1e5
system.time({
  x <- 0
  for (i in v) x[i] <- sqrt(v[i])
})

v <- 1:1e5
x <- 0
for (i in v) {
  x[i] <- sqrt(v[i])
  print(x[i])
}

system.time({
  y <- sqrt(v)
})

identical(x, y)

#### Step 3 of 10 ####
#### Step 4 of 10 ####
# Выполните в своей сессии следующие команды (про функцию set.seed я расскажу чуть далее):
set.seed(1337)
x <- runif(1e6, min = -1, max = 1)
length(x[x > - 0.2 & x < 0.3])
x[x > -0.2 & x < 0.3]
#
subset(x, x > -0.2 & x < 0.3)
#
y <- x[x > -0.2 & x < 0.3]
length(y)
length(which(x > -0.2 & x < 0.3))

# Теперь в вашем распоряжении вектор x длиной один миллион. Мне крайне интересно, сколько среди них чисел в диапазоне (-0.2, 0.3). 
# Для определённости не включая границы интервала. Впрочем, кто знаком с теорией вероятностей, тот знает, что это уточнение несущественно.
# P.S. Задача-то простецкая вышла: всегда можно распечатать x на принтере и посчитать вручную.

#### Step 5 of 10 ####
# Вам не нравятся ни монетка, ни "камни-ножницы-бумага"? Хорошо, давайте играть в "Монополию", только у меня нет кубика. Напишите его сами!
# Функция dice_roll(n) должна выдавать n независимых бросков игрального кубика. Допустимые значения, разумеется, в диапазоне от 1 до 6.
# Только учтите, что если кубик нечестный (не все грани выпадают с равной вероятностью), я это проверю и играть с вами не сяду!


dice_roll(5)

dice_roll <- function(n) {
  x<-floor(runif(n, 1, 7))
  return(x)
}

#
dice_roll <- function(n) {
  x = runif(n)
  ifelse(x > 5/6, '1',
         ifelse(x > 4/6, '2',
                ifelse(x > 3/6, '3',
                       ifelse(x > 2/6, '4',
                              ifelse(x > 1/6, '5', '6')))))
}

#
dice_roll <- function(n) {
  sample.int(6, n, replace = TRUE)
}

#
dice_roll <- function(n) {
  x <- runif(10)
  ifelse(x > 5/6, 1,
         ifelse(x > 4/6, 2,
                ifelse(x > 3/6, 3,
                       ifelse(x > 2/6, 4,
                              ifelse(x > 1/6, 5, 6
                              )
                       )
                )
         )
  )
}

#
dice_roll <- function(n){
  round(runif(n, 0.5, 6.5))
}

#
dice_roll <- function(n) {
  x <- runif(n, min = 1, max = 7)
  x=as.integer(x)
  return(print(x))
}



#### Step 6 of 10 ####

#### Step 7 of 10 ####
install.packages('randtoolbox')
library(randtoolbox)
help(randtoolbox)
#### Step 8 of 10 ####
# Я уже говорил, что некоторые пакеты зависят друг от друга. Таким образом, все пакеты в CRAN выстраиваются в дерево зависимостей 
# (поглядеть на этого монстра можно здесь). Все имеющиеся зависимости для каждого пакета хранятся в специальном файле под названием DESCRIPTION.
# Чтобы получить справку по пакету целиком, можно воспользоваться командой вида
help(package = "xts")
# Первая ссылка в списке и есть DESCRIPTION. В поле Depends указаны те пакеты, без которых данный пакет работать не будет.
# Пакет stats поставляется вместе с R, а в CRAN есть пакеты quantmod, xts и zoo. 
# Установите недостающие пакеты, подключите их и, пользуясь полем Depends﻿, укажите иерархию зависимостей этих пакетов друг от друга, 
# начиная с самого "независимого".

install.packages('stats')
library(stats)
help(package = "stats")

install.packages('zoo')
library(zoo)
help(package = "zoo") # Depends: R (>= 3.1.0), stats

install.packages('xts')
library(xts)
help(package = "xts") # Depends: zoo (>= 1.7-12)

install.packages('quantmod')
library(quantmod)
help(package = "quantmod") # Depends: R (>= 3.2.0), xts(>= 0.9-0), zoo, TTR(>= 0.2), methods


#### Step 9 of 10 ####
# Поизучайте повнимательнее вывод функции sessionInfo()﻿. Что он содержит?
sessionInfo()

#### Step 10 of 10 ####











