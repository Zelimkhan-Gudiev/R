remove(list = ls())
rm()

#### 1 of 11 ####
# В этом уроке мы:
# 1) выберем красную таблетку и увидим, как устроена матрица;
# 2) рассмотрим, как хранить данные различных типов и длин при помощи списка;
# 3) познакомимся с семейством функций apply;
# 4) попытаемся угадать, кто скрывается за маской анонимной функции.
# Не устаю напоминать про то, что важно самостоятельно пощупать новые объекты. Создавайте какие угодно вектора, матрицы и списки в своей сессии, 
# применяйте к ним функции, о которых я рассказываю, проверяйте на них свои решения задач.

# Общий глоссарий для этого урока:
  
?matrix
?dim, ?rownames, ?colnames
?rbind, ?cbind, ?apply
?rowSums, ?rowMeans, ?colSums, ?colMeans
?list, ?unlist
?"[" (?"[[", ?"$")
?lapply, ?sapply
Partial matching, ?"..." (ellipsis)
?diag


#### Step 2 of 14 ####
# Матрица - это двумерный массив данных одного типа. По сути это вектор уложенный по столбцам.
# Создание матрицы
matrix(1:6, nrow = 2, ncol = 3)
# Избавимся от ненужной избыточности:
matrix(1:6, nrow = 2)
matrix(1:6, ncol = 3)
matrix(1:6, nrow = 2, byrow = T)
matrix(7:8, nrow = 2, ncol = 5)
# Атрибут dim
# Единственным отличием вектора от матрицы заключается в наличии у последней атрибута dim
m <- matrix(1:6, ncol = 3)
dim(m)
c(nrow(m), ncol(m))
dim(m) <- NULL
dim(m) <- c(2, 3)


# Арифметические операторы
# Арифметические операции с матрицами действуют поэлементно, с учетом правил переписывания
m1 <- matrix(1:4, nrow = 2)
m2 <- matrix(c(1, 2, 2, 3), nrow = 2)
m1 + m2
m1 + 5
m1 * 2
m1 * m2
# Умножение в смысле линейной алгебры
m1 %*% m2

# Идексироване матриц
# Для индексиорвания матриц действуют те же правила, что и для индексирования векторов, но с учетом размерности
m <- matrix(1:10, ncol = 5)
m[1, 3]
m[2, ]
m[, 4]
m[1, ] <- 0
m[, -5] <- 11:18


# Cхлопывание размерности
m <- matrix(1:10, ncol = 5)
ind <- c(1, 3, 5)
m[, ind]
ind <- 3
m[, ind]
m[, ind, drop = F]

# Именованые матрицы rownames/colnames
m <- matrix(1:10, ncol = 5)
rownames(m) <- c("row1", "row2")
colnames(m) <- paste0("column", 1:5)

m["row1", c("column2", "column4"), drop = F]

# Присоединение матриц rbind/cbind
rbind(m1, m2)
cbind(m1, m2)

# Аргумент ... (Ellipsis)
# Новый аргумент ... позволяет передовать любое количесво объектов
cbind(m1, m2, c(5, 3), m2[, 1], m1 * 3, cbind(m2, m1))

# Другие примеры функций с ... (Ellipsis): c, paste, paste0, sum.

# Применение функций к матрице: apply
m <- matrix(1:25, 5)
f <- function(x) sum(x ^ 2)
# У функции apply есть три аргумента: массив (матрица), индекс (1 - по строкам, 2 - по столбцам), функция.
apply(m, 1, f)
apply(m, 2, f)

apply(m, 1:2, function(i) if (i > 13) i else 13) # Если небходимо применить функцию к каждому элементу (а не к строркам или столбцам),
                                                # то необходимо в качестве второго аргумента указать и строки и столбцы

# Все то же самое можно сделать проще с помощью логического индексирования на матрице
m[m <= 13] <- 13
m

# rowSums, rowMeans, colSums, colMeans
# Наиболее часто встречаемые операции по строкам и столбцам - sum и mean
m <- matrix(1:25, 5)
rowSums(m)

all.equal(rowSums(m), apply(m, 1, sum))
all.equal(colMeans(m), apply(m, 2, mean))


#### Step 3 of 14 ####
# Если матрица mat имеет размерность m на n (например, mat <- matrix(0, m, n)), то какой объект будет возвращён после выполнения следующих операций?
m <- 2
n <- 3
mat <- matrix(0, m, n)

mat[m, n] # Вектор длины 1, содержащий элемент в правом нижнем углу (mat[m, n] <- 6)
mat[n, m] # Скорее всего, ошибочная запись: ошибки не будет только при m=n
mat[m, ]  # Вектор, содержащий строку (ряд) номер m (mat[m, ] <- 1)
mat[m, , drop = FALSE] # Матрица, состоящая из одной строки (ряда) номер m (mat[m, , drop = FALSE] <- 1)
mat[, n, drop = F] # Матрица, состоящая из одного столбца (колонки) номер n
mat[, n, drop = TRUE] # Вектор, содержащий столбец (колонку) номер n
mat > 5 # Матрица m на n логического типа по условию
mat[mat > 5] # Вектор, содержащий все значения по условию (возможно, пустой)


#### Step 4 of 14 ####
# Предположим, что у нас есть целочисленный вектор v и число n. Наша задача — найти позицию элемента в векторе, 
# который ближе всего к числу n. При этом если таких элементов несколько, необходимо указать все позиции.
# Напишите функцию, которая принимает на вход вектор и число и возвращает вектор индексов, отвечающих указанному условию.
# Индексы должны быть выстроены по возрастанию.
# Пример. Пусть 
v <- c(5, 2, 7, 7, 7, 2, 0, 0)
n <- 1
length(v)
# Ответом будет вектор: 2 6 7 8
# Подсказки: "ближе всего" означает минимальную разницу между числами; не забудьте про модуль!

find_closest <- function(v, n) {
  ind_min <- which(abs(v - n) == min(abs(v - n)))
  return(ind_min)
}

find_closest(v, n)
#
find_closest <- function(v, n) {
  w <- abs(v-n)
  names(w) <- 1:length(v)
  as.integer(names(w[w == min(w)]))
}

#
find_closest <- function(v, n) {
  (function(dist) {
    which(dist == min(dist)) 
    }) (dist <- abs(v - n))
}
#
find_closest <- function(v, n) {
  d <- abs(v - n)
  return(which(d == min(d)))
}
#
find_closest <- function(v, n){
  a <- sapply(v, function(x) abs(n - x))
  which(min(a) == a)
}

#### Step 5 of 14 ####
# bind diag matrix
m1 <- matrix(1:12, nrow = 3)
m2 <- matrix(1:15, ncol = 3)


bind_diag <- function(m1, m2, fill) {
  m3 <- matrix(fill,
               nrow = nrow(m1) + nrow(m2),
               ncol = ncol(m1) + ncol(m2))
  m3[1:nrow(m1), 1:ncol(m1)] <- m1
  m3[nrow(m1) + 1:nrow(m2), ncol(m1) + 1:ncol(m2)] <- m2
m3
}
#
bind_diag<-function(m1, m2, fill) {
  cbind(
    rbind(m1, matrix(fill, nrow(m2), ncol(m1))),
    rbind(matrix(fill, nrow(m1), ncol(m2)), m2))
}

bind_diag(m1, m2, fill = NA)

#### Step 6 of 14 ####
# Построим зиккурат! Напишите функцию, которая принимает одно целое число n, а возвращает “ступенчатую” матрицу, состоящую из n этажей. 
# Этажи нумеруются с первого, ширина каждой ступеньки равна одной строке или столбцу.
# Пример. Пусть n=4, тогда необходимо вернуть матрицу вида
1   1   1   1   1   1   1
1   2   2   2   2   2   1
1   2   3   3   3   2   1
1   2   3   4   3   2   1
1   2   3   3   3   2   1
1   2   2   2   2   2   1
1   1   1   1   1   1   1
# Подсказка: обратите внимание на случай n=1! Зиккурат в этом случае будет матрицей 1 на 1, высотой 1.

build_ziggurat <- function(n) {
  m2 <- t(m1 <- matrix(c(1:(n - 1), n, (n - 1):1), 2 * n - 1, 2 * n - 1))
  print((m1 + m2 - abs(m1 - m2)) / 2)
}
n <- 4

build_ziggurat(4)

#
build_ziggurat <- function(n) {
  d = n*2-1
  # создадим 4 ступенчатые матрицы, направленные в разные стороны
  m1 = matrix(1:d, d, d)
  m2 = matrix(1:d, d, d, byrow = T)
  m3 = matrix(d:1, d, d)
  m4 = matrix(d:1, d, d, byrow = T)
  
  # найдем минимум для каждой позиции из четырех матриц
  pmin(m1, m2, m3, m4)
}
#
build_ziggurat <- function(n) {
  d <- n * 2 - 1
  m <- matrix(0, d, d)
  for (i in 1:n) {
    i2 <- d - i + 1
    m[i:i2, i:i2] <- i
  }
  m
}
#
build_ziggurat <- function(n) {
  v <- c(1:(n - 1), n, (n - 1):1)  # пример: 1 2 3 2 1
  N <- 2 * n - 1
  # матрица из повторяющихся векторов
  mat1 <- matrix(rep(v, N), N, N)
  # та же матрица в траспонированном ("повернутом") виде
  mat2 <- matrix(rep(v, N), N, N, byrow=T)
  # смешение матриц дает пирамиду
  # модуль позволяет "убрать" избыточные диагональные элементы
  return((mat1 + mat2 - abs(mat1 - mat2)) / 2)
}
#
build_ziggurat <- function(n) {
  d <- n * 2 - 1
  outer(1:d, 1:d, function(x, y) {
    x <- n - abs(n - x)
    y <- n - abs(n - y)
    pmin(x, y)
  })
}
#
build_ziggurat <- function(n) {
  if (n == 1) {return(matrix(1))
  } else {
    x <- c(1:n, (n - 1):1)
    m <- matrix(x, 2 * n-1, 2 * n-1)
    n <- matrix(x, nrow = 2 * n - 1, ncol = 2 * n - 1, byrow = T)
    l <- (m[] + n[] - abs(m[] - n[])) / 2
    return(l)
  }
}
#
build_ziggurat <- function(n) {
  size <- n *2-1
  temp <- matrix(NaN, size, size)
  pmin(n-abs(n - row(temp)), n-abs(n-col(temp)))
}
#
build_ziggurat <- function(n, level = 1) {
  m <- matrix(level, nrow = n * 2 - 1, ncol = n * 2 - 1)
  if (n > 1) m[2:((n-1) * 2),2:((n-1) * 2)] <- build_ziggurat(n-1, level + 1)
  return(m)
}
#
build_ziggurat <- function(n) {
  n2 <- n * 2 - 1
  m <- matrix(0L, ncol = n2, nrow = n2)
  n - pmax(abs(n - row(m)), abs(n - col(m)))
}
#
build_ziggurat <- function(n) {
  m <- matrix(0, n * 2 - 1, n * 2 - 1)
  for (i in (n - 1):0) {
    m[(n - i):(n + i), (n - i):(n + i)] <- n - i
  }
  return(m)
}
#
build_ziggurat <- function(n) {
  #Выводим в отдельную матрицу со столбцами "row" и "col" все индексы матрицы размерности зиккурата
  z <- which(matrix(1, n * 2 - 1, n * 2 - 1) == 1, arr.ind = T)
  #В каждой строке меняем значения, превышающее заданное число n
  z <- ifelse(z > n, 2 * n - z, z)
  #В каждой строке оставляем только наименьшее
  z <- ifelse(z[,1] >=z [, 2], z[, 2], z[, 1])
  #Итоговый вектор превращаем в матрицу (зиккурат)
  dim(z) <- c(2 * n - 1, 2 * n - 1)
  #Zиккурат
  z
}
#
build_ziggurat <- function(n) {
  m <- matrix(1, nrow = 2 * n - 1, ncol = 2 * n - 1)
  for(i in 1:n){
    m[i:(2 * n - i), i:(2 * n - i)] <- i
  }
  m
}
#
build_ziggurat <- function(n) {
  if (n == 1) {return(matrix(1, 1, 1))}
  
  
  k <- matrix(0, 2 * n - 1, 2 * n - 1)
  k[n, n] <- n
  
  for (i in 1:(n - 1)) {
    k[n + i, n + i] <-  n - i
    k[n - i, n - i] <-  n - i
    k[n - i, n + i] <-  n - i
    k[n + i, n - i] <-  n - i
}
#
build_ziggurat <- function(n) {
    m1 <- matrix(c(1:n , (n - 1):1), nrow = (2 * n - 1), ncol = (2 * n - 1))
    return((m1 + t(m1) - abs(m1 - t(m1))) / 2)
}
#### Step 7 of 14 ####
#### Step 8 of 14 ####
#### Step 9 of 14 ####
#### Step 10 of 14 ####
#### Step 11 of 14 ####
#### Step 12 of 14 ####
#### Step 13 of 14 ####
#### Step 14 of 14 ####
