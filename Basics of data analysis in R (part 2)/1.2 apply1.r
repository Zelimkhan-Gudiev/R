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


# steps 2 - 3 for vs apply 


str(diamonds)

min_size <- numeric(nrow(diamonds))
for (i in 1:nrow(diamonds)){
  min_size[i] <-  min(diamonds[i, 8:10])
}

min_size_2 <- apply(diamonds[, 8:10], 1, min)


# steps 4 and 7 apply function
?apply(array, margin, ...)

apply(X, MARGIN, FUN, ...)

d <- matrix(rnorm(30), nrow = 5)

apply(d, MARGIN = 1, FUN = sd)
apply(d, MARGIN = 2, FUN = sd)

apply(mtcars, 2, sd)
apply(mtcars, 1, sd)

s <- apply(d, MARGIN = 2, FUN = sd)
range(1:10)

my_range <- apply(d, MARGIN = 2, FUN = range)
my_range


# step 8 apply advanced
outliers_count <- function(x){
  otliers <- x[abs(x - mean(x)) > 2 * sd(x)]
  if (length(otliers) > 0){
    return(otliers)
  } else {
    return("There are no otliers")
  }
}

iris_num <- iris[, 1:4]

iris_outliers <- apply(iris_num, 2, outliers_count)
str(iris_outliers)


#### Step 5 of 9  ####
# ¬ переменной my_df сохранен dataframe с произвольным числом количественных переменных. 
# ѕри помощи функции apply найдите максимальное значение в каждой строке. 
# —охраните результат (вектор максимальных значений) в переменную row_max.

row_max <- apply(my_df, MARGIN = 1, FUN = max)

#### Step 6 of 9  ####
# ¬ переменной my_df сохранен dataframe с произвольным числом количественных переменных. 
# –ассчитайте медиану дл€ всех столбцов с количественными переменными. 
# ¬ переменную col_median сохраните вектор полученных значений.

col_median <- apply(my_df, MARGIN = 2, FUN = median)



