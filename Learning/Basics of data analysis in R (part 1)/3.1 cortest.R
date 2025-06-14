remove(list = ls())
rm()

#### Packages and librarys. ____________________________________________________________________________________________________ ####

library(ggplot2)
library(psych)
library(dplyr)
library(psych)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")
setwd("/Users/zelimkhan/Desktop/Data/GitHub/DF/")

yt <- read.csv2("yt.csv")
df  <- mtcars

# ����� ��������� ������� � RStudio ���� csv (��� ���������) ����������:
# 1.	��� �� Windows � ��� ���������� ���������� ��� ����� �CSV (����������� -�������)�
# 2.	��� �� Mac � ��� ���������� ���������� ��� ����� �CSV UTF-8 (����������� -�������)�

#### Step 1,2: Contens  _______________________________________________________________________________________________________ ####
# � ���� ����� �� �������� ������������ ������������ ���������� � ������������ � ������� �������� ����������.
# ������ �� ������ �����:
# ���������� https://stepic.org/media/attachments/lesson/11508/cortest.R
# ��������� ???https://stepic.org/media/attachments/lesson/11508/simple_regr.R




cor.test(x = df$mpg, y = df$hp)
fit  <- cor.test(x = df$mpg, y = df$hp)

cor.test(~ mpg + hp, df)

str(fit)

fit$p.value

plot(x = df$mpg, y = df$hp)

ggplot(df, aes(x = mpg, y = hp, col = factor(cyl)))+
  geom_point(size = 5)+
  facet_grid(. ~ am)


###########################################

df  <- mtcars
df_numeric  <- df[, c(1,3:7)]


pairs(df_numeric)

cor(df_numeric)

fit  <- corr.test(df_numeric)
fit$r
fit$p
fit$adjust

#### yt  ####

pairs(yt[, c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 
             'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
             'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')])

cor(yt[, c('numb_ret_depir', 'numb_ret_oiv', 'time_plan', 'time_ac', 'time_rev_oiv', 
           'time_rev_depir', 'time_vn_sogl', 'time_depir', 'time_oiv', 
           'time_prep_rg', 'time_rg', 'time_mrg', 'time_eaist', 'duration')])

####  Step 5 of 16 ####

# �������� ������� corr.calc, ������� �� ���� �������� data.frame � ����� ��������������� �����������, 
# ������������ ����������� ���������� ������� � ���������� ������ �� ���� ��������: ����������� ���������� � p - ������� ����������.
# ������ ������ �������:
# > corr.calc( mtcars[, c(1,5)] )  # �� ���� ������ ������ mtcars ������ � ����������� mpg � drat
# [1] 0.6811719078 0.0000177624
# > corr.calc( iris[,1:2] ) # �� ���� ������ ������ iris ������ � ����������� Sepal.Length � Sepal.Width
# [1] -0.1175698 0.1518983
# ��� ��������� ������� �������� ��������, ��� �������� ���������� ��������� dataframe ����� ���� �������������. 
# ������ ������� � ������, ��� ��� ������ �������� �� �����  dataframe � ����� ��������������� ����������� ��� � ������� ����.
# �� �������� ���������� ���������� library(psych), ���� ������ ������������ �� ��� ������� ���� ������.

# ������������ 
str(df)
typeof(df$disp)
df$disp <- as.numeric(df$disp)
df$hp <- as.numeric(df$hp)
crt <- cor.test(df[, 'disp'], df[, 'hp'])
crt <- cor.test(df[, 3], df[, 4])
str(crt)
crt$estimate

#### How to Fix: �x� must be numeric in R (Error in cor.test.default(df[, c(3, 4)]) : 'x' must be a numeric vector) ####
# Example 1: Error in vector �x� must be numeric
# vector creation
x <- c("61", "4", "21", "67", "89", "2")
# display vector
print(x)
# plotting hist
hist(x)
# To solve this error we will convert the vector elements into numeric data using as.numeric() methods.
x <- c("61", "4", "21", "67", "89", "2")
print(x)
res <- as.numeric(x)
hist(res)

# Example 2: Error in dataframe �x� must be numeric
# Create data for chart
val <-data.frame("num"=c("77","55","80","60"),
                 "course"=c('DSA','C++','R','Python'))

print(val)
hist(val[,1])
# To solve this error we will convert the dataframe element into numeric data using as.numeric() methods
val <-data.frame("num"=c(77,55,80,60),
                 "course"=c('DSA','C++','R','Python'))
print(val)
hist(val[,1])


#1
corr.calc <- function(x){
  crt <- cor.test(x[, 1], x[, 2])
  return(c(crt$estimate, crt$p.value))
}

corr.calc(df[, c(3, 4)])

#2
corr.calc <- function(x){    
  fit  <- cor.test(x[[1]], x[[2]])
  r <- fit$estimate
  p <- fit$p.value
  return(c(r, p))}
corr.calc(df[, c(3, 4)])

#3
corr.calc <- function(df){
  # put your code here  
  fit <- cor.test(~., df)
  return(c(fit$estimate, fit$p.value))
}

#4
corr.calc <- function(x){
  cor.test(x = x[, 1], y = x[, 2])[c("estimate", "p.value")]
}

#5
corr.calc <- function(x){
  
  ct<- psych::corr.test(x)
  return(c(ct$r[1,2], ct$p[1,2]))
  
}

#6
library(psych)
corr.calc <- function(x){
  fit  <- corr.test(x)
  return(c(fit$r[2], fit$p[2]))
}

#8



####  Step 6 of 16 ####
# �������� ������� filtered.cor ������� �� ���� �������� data.frame �  ������������ ����������� ���������� 
# (��� ���������������, ��� � ����� ������ �����), ������������ ������������ ���������� ������� ����� ����� ������
# �������������� ���������� � ���������� ���������� �� ������ �������� ������������ ����������. 
# (�� ���� ������� ����� ������� -0.9, ���� ��� ���������� �� ������  ����������).

# ������������� ������� � data.frame ���� �� ���� �������������� ����������.

# �������� ��������: ��� �������� ����� ������� �� ���� ����� ���������� ������ � ���������� ������� �������. 
# ���� ������� ������ ��������� �������� ���������� �� ���� ����������. ����� ���, ��� ������� �������, ���������, 
# ��� ���� ������� �������� ��������� �� ������ ������, � ������� ������� �������. 
# ���� �� ������ ������������ ������� corr.test �� �������� ��������� ���������y psych.
# ������ ��� ����������:
# https://stepic.org/media/attachments/lesson/11504/step6.csv
# Remove Non-Numeric Columns https://datamining.togaware.com/survivor/Remove_Non_Numeric.html

# �� ��������� ��������� � �������� ��������� apply � ��������� ����� - Advanced R, �� �� ������ ������� ������� � apply � sapply. ��� ������� ������ ������, ��� ������� ����� �����������.
# �������� �������� �� ������� which.max()
# �������� �������� �� ����������� diag(matrix) <- n???
# ���� �� ��������� ������, ???'x' must be a numeric vector, ???������ � ������ �������� �� ������ �������������� ����������.

colnames(df)
typeof(df)
lapply(df, class)
typeof(yt)
is.numeric(yt$name)
is.numeric(yt$duration)
lapply(df, class)
yt_numeric <- yt[, sapply(yt, is.numeric)]
pairs(yt_numeric)

#
library(dplyr)
xNum <- select_if(x, is.numeric)
pairs(xNum)


x <- df

filtered.cor <- function(x){
  library(dplyr)
  library(psych)
  x <- select_if(x, is.numeric)
  crt <- corr.test(x)$r
  diag(crt) <- 0
  crt[which.max(abs(crt))]
}

filtered.cor(test_data)

test_data <- as.data.frame(list(V5 = c("b", "b", "b", "b", "b", "b", "b", "b"), 
                                V4 = c("r", "r", "r", "r", "r", "r", "r", "r"), 
                                V2 = c(-0.3, -0.1, -0.5, 0.3, 0.4, 1.4, -0.7, 1.8),
                                V6 = c("e", "e", "e", "e", "e", "e", "e", "e"), 
                                V3 = c("k", "k", "k", "k", "k", "k", "k", "k"), 
                                V1 = c(0.6, 0.6, 0.3, -1.2, -0.4, -0.2, -0.3, 0.5)))

test_data <- select_if(test_data, is.numeric)

####  Step 7 of 16 ####
# �������� ������� smart_cor, ������� �������� �� ���� dataframe � ����� ��������������� �����������. 
# ��������� � ������� ����� ������-�����, ��� ������ � ����� ���������� ����������� ����������� �������������.
# ���� ���� �� � ����� ������� ������������� ���������� ���������� �� ����������� (p - value ������ 0.05), 
# �� ������� ������ ���������� ����������� ���������� ��������. (�������� ������ �� ������ ��������).
# ���� � ����� �������� ������������� ���������� �� ����������� ������� �� ����������, 
# �� ������� ������ ���������� ����������� ���������� �������.
# > test_data  <- read.csv("https://stepik.org/media/attachments/course/129/test_data.csv")
# > smart_cor(test_data)
# [1] -0.1031003

names(df)
df  <- select_if(df, is.numeric)
smart_cor <- function(x) {
  shapiro1 <- shapiro.test(x[, 1])[2]
  shapiro2 <- shapiro.test(x[, 2])$p.value
  if(shapiro1 < 0.05 | shapiro2 < 0.05) {
    corSpear <- cor.test(x[, 1], x[, 2], method = "spearman")
    return(corSpear$estimate)
  } else {
    corPear <- cor.test(x[, 1], x[, 2], method = "pearson")
    return(corPear$estimate)
  }
}


####  Step 12 of 16 ####
# �������� ����� ������ - dataframe � ����� ��������������� ����������� (��������� ��� �������������, ��� �������� ����������� � ������ ��������� ������� read.table), ��������� �������� ���������, ��� - ������ ���������� - ���������, ������ - �����������. � ����� ������� �������� ������������� ������������� ������� intercept �����  slope.
# ���������� ����������� - �����. � ���� ��� ������ ������� ��� �����, �� ���������� ��������, ��������;
# 12.434 6.2557
# � ��� ���� �������������� ����� �������.
# ����� ����� �������: 5 mins


####  Step 13 of 16 ####
# ������������� ��� ��������� ������� diamonds �� ���������� ggplot2. ������ ��� ����������� ������ Ideal (���������� cut) c ������ ����� ������ 0.46 (���������� carat) ��������� �������� ���������, ��� � �������� ��������� ���������� ��������� price, � �������� ���������� - ����������  depth. ��������� ������������ ��������� � ���������� fit_coef.
# �������:
# > fit <- lm(mpg ~ disp + wt, mtcars)
# > fit$coefficients # ������������ ������
# ��� ������� ����� ������, �� ��������� ���� for().


####  Step 14 of 16 ####
# �������� ������� regr.calc, ������� �� ���� �������� dataframe c ����� �����������.
# ���� ��� ���������� ������� ����������� (p - ������� ���������� ��� ������������ ���������� ������� ������ 0.05), �� ������� ������ ������������� ������, ��� ������ ���������� - ���������, ������ - �����������. ����� ������� � dataframe ����� ���������� � �������� fit, ��� ��������� ������������� ������� �������� ��������� ����������. � ���������� ������� ������ ���������� �������� dataframe � ����������� ����� ���������� fit.
# ���� ��� ���������� ������� �� �����������, �� ������� ���������� ������� "There is no sense in prediction"
# ������� ������ �������:
# > my_df = iris[,1:2] # �� ���� ������ ������ iris ������ � ����������� Sepal.Length � Sepal.Width
# > regr.calc(iris[,1:2]) # ���������� ������� �� ����������� 
# [1] "There is no sense in prediction"
# > my_df = iris[,c(1,4)] # �� ���� ������ ������ iris ������ � ����������� Sepal.Length � Petal.Width
# > regr.calc(my_df) # ���������� ������� ����������� 

        Sepal.Length        Petal.Width        fit


1         5.1                   0.2           4.955345
2         4.9                   0.2           4.955345
3         4.7                   0.2           4.955345
.           .                    .                .
.           .                    .                .

# �������� ��������, ��� �������� ����� ������� �� ���� ����� ���������� ������ � ���������� ������� �������. 
# ���� ������� ������ ��������� �������� � ������������� �� ���� ����������.
# ����� ��� ��� ������� ������� ���������, ��� ���� ������� �������� ��������� �� ������ ������, � ������� ������� �������.


####  Step 15 of 16 ####
# ��������� scatterplot �� ������ iris, �������� ��� � ���������� my_plot : 
# ��� X - ���������� Sepal.Width
# ��� Y -  ���������� Petal.Width
# ���� ����� - ���������� Species
# ����� �������� �������� ����������� ��� ������ ������ ���������� �� ���������� Species.
# ���� �� ��� ������� ��������� ������ ���������� ��������� ������:


yt$reason
yt[yt$reason == '���� �� ��������������']




length("�������������� ��������� ��� ������� ����� ������� � ������������ ��������.")
getwd()
list.files()  
sd
var
mean
