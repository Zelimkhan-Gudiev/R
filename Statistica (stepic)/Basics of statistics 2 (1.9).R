# Практические задания по анализу данных в R.

#### 1 of 8 ####
# Напишите функцию NA_position, которая получает на вход два числовых вектора одинаковой длины. 
# Функция должна возвращать TRUE, если позиции NA в обоих векторах совпадают или пропущенных значений вообще нет, 
# и значение FALSE, если позиции NA в векторах не совпадают.  

NA_position  <- function(x, y){
  all(is.na(x) == is.na(y))
}

v1  <- c(1, 2, 3)
v2  <- c(3, 4, 6)
NA_position(v1, v2)

v1  <- c(1, 2, NA)
v2  <- c(3, 4, NA)
NA_position(v1, v2)


v1  <- c(NA, 2, NA)
v2  <- c(3, 4, NA)
NA_position(v1, v2)

#### 2 of 8 ####
x <- mtcars[,c("am", "vs")]

table(x)

smart_test <- function(x) {
  x1 <- table(x)
  if(any(x1 < 5)) {
    fish <- fisher.test(x1)
    fish$p.value
  } else {
    chis <- chisq.test(x1)
    unlist(c(chis$statistic, chis$parameter, chis$p.value))
  }
}

smart_test(x)
