remove(list = ls())
rm()

#### 1 of 11 ####
# В этом уроке мы:
# В этом уроке мы
# 1) рассмотрим правила переписывания для векторов;
# 2) перечислим типы индексирования;
# 3) познакомимся с эффективными приёмами доступа и замены элементов вектора;
# 4) вместе решим несколько хитрых упражнений.

# Это ключевой урок на первой неделе: здесь я рассказываю о том, как сделать работу с векторами проще и приятнее. 
# А ещё здесь больше всего очков. Сюда можно возвращаться в течение всего курса за полезными функциями и примерами использования.
Recycling rules
?"+", ?Arithmetic, ?"["
?all, ?any
?which, ?which.min, ?which.max
?attributes
?sample, ?runif


#### Step 2 of 13 ####
### Правила переписывания (recycling) ###
# Как работает арифметика на векоторах разной длины?
# 1) Длина результата равна длине большего из векторов.
# 2) Меньший вектор дублируется (переписывается) несколько раз, чтобы длина переписанного вектора совпала с длиной большего вектора.
# 3) Если длина большего вектора не делится на цело на длину меньшего вектора, выдается предупреждение.
1:5 + 0:1


### Доступ к элементам вектора ###
x <- seq(10, 100, by = 10)
# Положительные индексы
x[1]
x[3:4]
x[c(8, 7, 3, 6:8, x[1])]
# Отрицательные индексы
x[-5]
x[-c(2:6)]
x[c(-3, -5, -length(x), -5)]
# Логическое индексиррование. Логические элементы соответвующие значению TRUE
x[rep(c(TRUE, FALSE), 5)]
x[c(TRUE, FALSE)]
x[x > 77 & x < 99]

# Индексация по имени
# Есть два способа присвавать имена элементам вектора
# v1 
a <- c(uno = 1, dos = 2, 'universal answer' = 42, 99)
names(a)
# v2
names(a) <- c("one", "two", "forty two", "ninety nine")

names(a) <- NULL # удаляет вектор имен

a[c("two", "one", "forty two", "f")]
a[c(2, 1, 42)]

# Функция all и any
all(x < 200)
all(x > 200)
any(x > 20)
any(x < 20)
any(x > 150)
any(x < 15)

# Функция which

which(x >= 50) # Возвращает индексы элементов
which.min(x)
which.max(x)

# Атрибуты объектов
# length (длина) - это свойство объектов, оно есть всегда
# Атрибуты (names, dimnames, dim ...) могут присутсвовать, если это необходимо
x <- c(5, 3, 9)
names(x) <- c("V", "III", "IX")
attr(x, "author") <- "Ceasar"
attributes(x)
attributes(x) <-  NULL
attributes(x)


#### Step 3 of 13 ####
# Чем больше вы знаете разнообразных функций для работы с векторами, тем больше существует вариантов для решения простых задач. 
# Пусть имеется вектор x произвольного типа. Какая конструкция может быть использована, чтобы получить содержимое вектора x без каждого седьмого элемента?
# P.S. Давайте предполагать, что понятие "каждый седьмой" имеет смысл, то есть элементов в векторе хотя бы семь.
x <- 1:21


x[-(seq(7, length(x), by = 7))]
x[1:length(x) %% 7 > 0]  # 1/7, 2/7, .. 7/7
x[1:length(x) %% 7 != 0]
x[c(rep(T, 6), F)]
x[-(1:floor(length(x)/7) * 7)]

#### Step 4 of 13  ####
# Я дал целый набор функций для работы с векторами, и некоторые названия похожи, а функции могут быть принципиально разными. 
# Чтобы не запутаться, давайте наведём порядок. 
# Строки -- высказывания о функциях, столбцы -- сами функции. Если сразу ответить трудно, поэкспериментируйте в консоли на каких-нибудь простых векторах.
# Подсказки: 
# Есть ли отношения вида "больше-меньше" для строкового типа? Оказывается, да: строки можно упорядочить в так называемом лексикографическом порядке. 
# Поэтому "самая большая строка" -- это не значит самая длинная, это последняя в упорядоченном наборе строк.
# Векторизованность в самом широком смысле означает поэлементное действие: функция f векторизована, если f(v) есть результат применения 
# f к каждому элементу вектора v. Это не всегда означает, что длина результата равна длине v.
# Вот набор векторов, на котором можно попробовать действие функций:
a <- 5
b <- 1:10
c <- 22:19
d <- c("A", "BBB", "Z")
e <- c("1", "99", "HI")
f <- c(TRUE, FALSE)
max(5)
max(1:10)
max(22:19)
max(c("A", "BBB", "Z"))
max(c("1", "99", "HI"))
max(c(TRUE, FALSE))

which(5)
which(1:10)
which(22:19)
which(c("A", "BBB", "Z"))
which(c("1", "99", "HI"))
which(c(TRUE, FALSE))

which.max(5)
which.max(1:10)
which.max(22:19)
which.max(c("A", "BBB", "Z"))
which.max(c("1", "99", "HI"))
which.max(c(TRUE, FALSE))

#
check <- function(x) {
  print(max(x))
  print(which.max(x))
  print(which(x))
}

check(5)
check(1:10)
check(22:19)
check(c("A", "BBB", "Z"))
check(c("1", "99", "HI"))
check(c(TRUE, FALSE))



#### Step 5 of 13  ####
# Сложнейший вопрос: если в R сложить два вектора, имеющих длину 4 и 6, то результат будет вектором длины ...
# Ответ: 6


#### Step 6 of 13  ####
# fizz-buzz, imperative style 
y <- vector(mode = "character", length = 100)
y <- character(100)

for (i in 1:100) {
  if (i %% 15 == 0) {
    y[i] <- "fizz buzz"
  } else if (i %% 3 == 0) {
    y[i] <- "fizz"
  } else if (i %% 5 == 0) {
    y[i] <- y[i] <- "buzz"
  } else {
    y[i] <- i
  }
}
y

# fizz-buzz, vector-oriented style
x <- 1:100
z <- 1:100
z[x %% 5 == 0] <- "buzz"
z[x %% 3 == 0] <- "fizz"
z[x %% 15 == 0] <- "fizz buzz"
all(z == y)

#### Step 7 of 13  ####
# Я зашифровал для вас послание. Вот оно: 1, 23, 5, 19, 15, 13, 5
# Один из встроенных массивов, о котором я упоминал в лекциях, поможет вам расшифровать эпитет, применимый к языку R. Регистр неважен.
letters[c(1, 23, 5, 19, 15, 13, 5)]


#### Step 8 of 13  ####
# Geometric progression
x <- 2 ^ (0:10)
log2(x)

# Some randomness
set.seed(42)
x <- sample(1:100, 50)

# Neighbors with greatest diff
x[-1]
x[-length(x)]
x[-1] - x[-length(x)]
k <- which.max(abs(x[-1] - x[-length(x)]))
x[c(k, k + 1)]
x[c(19, 20)]

# Multiple min/max
x <- sample(1:100, 50, replace = T)
sort(x)
min(x)
which.min(x)
which(x == min(x))

# Packing into a function
maxdiff <- function(x) {
  y <- abs(x[-1] - x[-length(x)])
  k <- which(y == max(y))
  print("First neighbor(s):")
  print(x[k])
  print("Second neighbor(s):")
  print(x[k + 1])
  print("Maximum absolute diff is:")
  print(max(y))
}

xx <- sample(1:100, 1e4, replace = T)
maxdiff(xx)

#### Step 9 of 13  ####
# Пусть вектор называется нестрого возрастающим, если каждый следующий элемент в нём не меньше, чем предыдущий. 
# Точно так же, вектор назовём нестрого убывающим, если каждый следующий элемент в нём не больше, чем предыдущий.
# Напишите функцию, которая принимает один аргумент (числовой вектор) и возвращает TRUE, если 
# вектор обладает свойством нестрогой монотонности, то есть является либо несторого возрастающим, либо нестрого убывающим. 
# В противном случае функция возвращает FALSE.
# Пример.  x <- c(0, 0, 3, 4, 4, 8) нестрого возрастает, возвращаем TRUE. y <- c(3:0, 1) был бы нестрого убывающим, 
# если бы не последняя единица, поэтому возвращаем FALSE.
x <- c(0, 0, 3, 4, 4, 8)
y <- c(3:0, 1) 


is_monotone(x)

#
is_monotone <- function(x) {
  i <- seq(1:(length(x) - 1))
  if (all(x[i + 1] >= x[i]) | all(x[i + 1] <= x[i])){
    TRUE
  } else {
    FALSE
  }
}
#
is_monotone <- function(x) {
return(all(x[-length(x)] <= x[-1]) | all(x[-length(x)] >= x[-1]))
}

#
is_monotone <- function(x) {
  all(x == sort(x) | x == -sort(-x))
}
#
is_monotone <- function(x) {
  all(diff(x) >= 0) || all(diff(x) <= 0)
}
#
is_monotone <- function(x) {
  return(all(x[-length(x)] - x[-1] >= 0) | all(x[-length(x)] - x[-1] <= 0))
}
#
is_monotone <- function(x) {
  diffs <- x[-1] - x[-length(x)]
  all(diffs >= 0) || all(diffs <= 0)
}
#
is_monotone <- function(x) {
  diff(range(sign(diff(x)))) < 2
}
#
is_monotone <- function(x) {
  y <- x[-1] - x[-length(x)]
  return(all(y >= 0) | all(y <= 0))
}

#
is_monotone <- function(x) {
  a = x[-1]-x[-length(x)]
  print(all(a >= 0) | all(a <= 0))
}
#
is_monotone <- function(x) {
  if (min(x) == max(x)) {
    print(TRUE)
  } else if (all(x[-1] - x[-length(x)] >= 0) | all(x[-1] - x[-length(x)] <= 0)) {
    print(TRUE)
  } else {
    print(FALSE)}}
#
is_monotone <- function(x) {
  all(x[-1] >= x[-length(x)]) || all(x[-1] <= x[-length(x)])
}
#
is_monotone <- function(x) {
  y <- x[-1] - x[-length(x)]
  return(max(y) * min(y) >= 0)
}
#
is_monotone <- function(x) {
  x1 <- ifelse(diff(x) >= 0,1,0)
  y1 <- ifelse(diff(x) <= 0,1,0)
  if ((mean(x1) == 1) | (mean(y1) == 1)) print(TRUE) else print(FALSE)
}
#
is_monotone <- function(x) {
  return(min(x[-1] >= x[-length(x)]) || min(x[-1] <= x[-length(x)]))
}
#
is_monotone <- function(x) {
  return(min(x[-1] >= x[-length(x)])||min(x[-1] <= x[-length(x)]))
  
}
#
# Решил сэкономить на одном вызове `diff`. По идее так должно быть быстрее, вот скрипт для проверки.
# Он генерирует вектор длины 10, 100, 1000, ... и для каждой длины сравнивает две функции:
is_monotone1 <- function(x) {
  x <- diff(x)
  all(x >= 0) || all(x <= 0)
}

is_monotone2 <- function(x) {
  all(diff(x) >= 0) || all(diff(x) <= 0)
}

require(microbenchmark)
set.seed(42)
lapply(setNames(nm = 10^(1:6)),
       function(n) {
         x <- sample.int(n, replace = TRUE)
         microbenchmark(is_monotone1(x), is_monotone2(x))
       })




Подсказки: 
# "Не меньше" = "больше либо равно".
# обратите внимание на постоянный вектор, например rep(0, 10). Является ли он нестрого возрастающим? А нестрого убывающим?
  
#### Step 10 of 13  ####
# Четыре способа индексирования векторов -- это как времена года: какие-то мы любим больше, какие-то меньше, но они все равноправны.
# Сопоставьте перечисленные способы и наиболее подходящие к ним характеристики.


#### Step 11 of 13  ####
# Разбавим курс ложечкой комбинаторики. Пусть у нас есть n предметов, из которых нужно выбрать k штук.
# Известнейшая комбинаторная формула  ("Цэ из эн по ка") задаёт количество всевозможных сочетаний.
# Похожий вид имеет и количество сочетаний с повторениями (мультикомбинаций).
# Запрограммируйте оба этих значения в виде функции, зависящей от n и k. Аргумент with_repetitions будет отвечать за вариант подсчёта: 
# если он FALSE, то пусть считается количество сочетаний, а если TRUE, то сочетаний с повторениями.
# Подсказки:
# если вам необходима вспомогательная функция, её также можно определить. Назовите её как угодно.
# в имеющейся декларации функции combin_count указано with_repetitions = FALSE. Это аргумент по умолчанию:
# оно будет подставлено, если вызвать функцию без указания with_repetitions, т.е. combin_count(10, 5) в точности эквивалентно combin_count(10, 5, FALSE).
# подсчёт факториалов "в лоб" сопряжён с опасностью переполнения целочисленного типа. В данном случае 
# я не буду проверять корректную работу при больших значениях n, оставим этот аспект за скобками.
# если вы не знаете, как считать число сочетаний с повторениями, обратитесь к Википедии.

combin_count <- function(n, k, with_repretitions = FALSE) {
  if (with_repretitions == FALSE) {
    factorial(n)/(factorial(k)*factorial(n-k))
  } else {
    factorial(n+k-1)/(factorial(k)*factorial(n-1))
  }
}

#
combin_count <- function(n, k, with_repretitions = FALSE) {
  ifelse(with_repretitions,choose(n+k-1, k),choose(n, k))
}

#
combin_count <- function(n, k, with_repretitions = FALSE) {
  if (with_repretitions) {
    return(prod(c((k+1):(n+k-1)))/prod(c(1:(n-1))))
  } else {
    return(prod(c((k+1):n))/prod(c(1:(n-k))))
  }  
}
#
combin_count <- function(n, k, with_repretitions = FALSE) {
  if (with_repretitions) n <- n + k - 1
  if (k == 0 || k == n) return(1)
  if (k == 1 || k == n - 1) return(n)  
  if ((n - k + 1) <= k) k <- (n - k)
  
  x <- c((n - k + 1):n)
  for(i in k:2) {
    kmax <- max(which(x %% i == 0))
    x[kmax] <- x[kmax] %/% i
  }
  prod(x)
}
#
combin_count <- function(n, k, with_repretitions = FALSE) {
  ifelse(with_repretitions, return(choose(n + k - 1, n - 1)), return(choose(n, k)))
}




combin_count(10, 5, T)

#### Step 12 of 13  ####
#### Step 13 of 13  ####










