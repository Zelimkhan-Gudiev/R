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

# ����� ��������� ������� � RStudio ���� csv (��� ���������) ����������:
# 1.	��� �� Windows � ��� ���������� ���������� ��� ����� �CSV (����������� -�������)�
# 2.	��� �� Mac � ��� ���������� ���������� ��� ����� �CSV UTF-8 (����������� -�������)�

#### Step 1,2: Contens  _______________________________________________________________________________________________________ ####
# � ���� ����� �� �������� ������������ ������������ ���������� � ������������ � ������� �������� ����������.
# ������ �� ������ �����:
# ���������� https://stepic.org/media/attachments/lesson/11508/cortest.R
# ��������� ???https://stepic.org/media/attachments/lesson/11508/simple_regr.R


df  <- mtcars
df_numeric  <- df[,c(1,3:7)]

fit  <- lm(mpg ~ hp, df)
summary(fit)
str(fit)

ggplot(df, aes(hp, mpg))+
  geom_point(size = 5)+
  geom_smooth(method = "lm")+
  facet_grid(.~cyl)

ggplot(df, aes(hp, mpg))+
  geom_smooth(method = "lm", se = F)+
  facet_grid(.~cyl)

fitted_values_mpg  <- data.frame(mpg = df$mpg, fitted = fit$fitted.values)

new_hp <- data.frame(hp = c(100, 150, 129, 300))
new_hp$mpg  <- predict(fit, new_hp)

predict(fit, new_hp)


##################################

my_df  <- mtcars
my_df$cyl  <- factor(my_df$cyl, labels = c("four", "six", "eight"))
fit  <- lm(mpg ~ cyl, my_df)


####  Step 13 of 16 ####
# �������� ����� ������ - dataframe � ����� ��������������� ����������� 
# (��������� ��� �������������, ��� �������� ����������� � ������ ��������� ������� read.table), 
# ��������� �������� ���������, ��� - ������ ���������� - ���������, ������ - �����������. 
# � ����� ������� �������� ������������� ������������� ������� intercept �����  slope.
# ���������� ����������� - �����. � ���� ��� ������ ������� ��� �����, �� ���������� ��������, ��������;
# 12.434 6.2557
# � ��� ���� �������������� ����� �������.
# ����� ����� �������: 5 mins

x <- read.table('/Users/zelimkhan/Downloads/dataset_11508_12 (2).txt')
fit <- lm(x[, 1] ~ x[, 2])
fit$coefficients

####  Step 14 of 16 ####
# ������������� ��� ��������� ������� diamonds �� ���������� ggplot2. ������ ��� ����������� ������ Ideal 
# (���������� cut) c ������ ����� ������ 0.46 (���������� carat) ��������� �������� ���������, 
# ��� � �������� ��������� ���������� ��������� price, � �������� ���������� - ����������  depth. 
# ��������� ������������ ��������� � ���������� fit_coef.
# �������:
# > fit <- lm(mpg ~ disp + wt, mtcars)
# > fit$coefficients # ������������ ������
# ��� ������� ����� ������, �� ��������� ���� for().

library(ggplot2)

df <- diamonds
df <- subset(df, df$cut == "Ideal" & df$carat == 0.46)
fit <- lm(price ~ depth, df)
fit_coef <- fit$coefficients



####  Step 15 of 16 ####
# �������� ������� regr.calc, ������� �� ���� �������� dataframe c ����� �����������.
# ���� ��� ���������� ������� ����������� (p - ������� ���������� ��� ������������ ���������� ������� ������ 0.05), 
# �� ������� ������ ������������� ������, ��� ������ ���������� - ���������, ������ - �����������. 
# ����� ������� � dataframe ����� ���������� � �������� fit, ��� ��������� ������������� ������� �������� ��������� ����������. 
# � ���������� ������� ������ ���������� �������� dataframe � ����������� ����� ���������� fit.
# ���� ��� ���������� ������� �� �����������, �� ������� ���������� ������� "There is no sense in prediction"
# ������� ������ �������:
# > my_df = iris[,1:2] # �� ���� ������ ������ iris ������ � ����������� Sepal.Length � Sepal.Width
# > regr.calc(iris[,1:2]) # ���������� ������� �� ����������� 
# [1] "There is no sense in prediction"
# > my_df = iris[,c(1,4)] # �� ���� ������ ������ iris ������ � ����������� Sepal.Length � Petal.Width
# > regr.calc(my_df) # ���������� ������� ����������� 

#                Sepal.Length     Petal.Width         fit
#
       1            5.1                0.2          4.955345
       2            4.9                0.2          4.955345
       3            4.7                0.2          4.955345
       .             .                  .           .
       .             .                  .           .

# �������� ��������, ��� �������� ����� ������� �� ���� ����� ���������� ������ � ���������� ������� �������.
# ���� ������� ������ ��������� �������� � ������������� �� ���� ����������.
# ����� ��� ��� ������� ������� ���������, ��� ���� ������� �������� ��������� �� ������ ������, � ������� ������� �������.

x <- mtcars      
crt <- cor.test(x[, 1], x[, 2])
crt$p.value < 0.05
fit <- lm(x[, 1] ~ x[, 2], x)
x$fit <- fit$fitted.values


regr.calc <- function(x) {
  crt <- cor.test(x[, 1], x[, 2])
  if(crt$p.value < 0.05) {
  fit <- lm(x[, 1] ~ x[, 2], x)
  x$fit <- fit$fitted.values
  return(x)
  } else {
    return("There is no sense in prediction")
  }
}       
       
regr.calc(x)
       
####  Step 15 of 16 ������� ####
# ��������� scatterplot �� ������ iris, �������� ��� � ���������� my_plot : 
# ��� X - ���������� Sepal.Width
# ��� Y -  ���������� Petal.Width
# ���� ����� - ���������� Species
# ����� �������� �������� ����������� ��� ������ ������ ���������� �� ���������� Species.
# ���� �� ��� ������� ��������� ������ ���������� ��������� ������:

library(ggplot2)
my_plot <- ggplot(iris, aes(x = Sepal.Width, y = Petal.Width, col = factor(Species))) + 
           geom_point(size = 3) +
           geom_smooth(method = lm)
                    




 ####  Step 16 of 16 ������� ####
cor.test(mtcars$mpg, mtcars$disp) # ������ ���������� ������� 
cor.test(~ mpg + disp, mtcars) # ������ ����� �������
cor.test(mtcars$mpg, mtcars$disp, method = "spearman") # ������ ���������� �������� 
cor.test(mtcars$mpg, mtcars$disp, method = "kendall") # ������ ���������� ������� 
cor(iris[, -5]) # ���������� �������������� �������
fit <- lm(mpg ~ disp, mtcars) # ���������� �������� ��������� 
fit$coefficients # ������������ ��������� 
fit$fitted.values # ������������� �������� ��������� ���������� 
# ��� ������� ���������� �������� � ���������� ������ ����������������� ���������� ����� �������������� ��������������� 
# � ������������� ���������� ������ �������� p - value.
# ���� � ����� ������ ���� ���������� ����������, �� �� ������ ���������� ����������������� ����������, ����������� ������� spearman_test  �� ������ coin

library(coin)
spearman_test(~ mpg + disp, mtcars)

# �������� �������� �� �������� � ��������. �� ��� � ������ aes() ����� ���������������� �� ��� ����. 
# � ��, ��� � aes() ����������� geom - ������ �� ����.

ggplot(mtcars, aes(mpg, disp, col = factor(am)))+
         geom_point()+
         geom_smooth()
       
ggplot(mtcars, aes(mpg, disp))+
         geom_point(aes(col = factor(am)))+
         geom_smooth()
       
ggplot(mtcars, aes(mpg, disp))+
         geom_point()+
         geom_smooth(aes(col = factor(am)))


####  3.4 Step 9 of 9 ####
# �������� ������� normality.test, ������� �������� �� ���� dataframe � ��������������� �����������, ���������
# ������������� ������ ���������� �� ������������ � ������� ������� shapiro.test. ������� ������ ����������
# ������ � ���������� p - value, ����������� � ���������� �������� �� ������������ ������ ����������. 
# �������� ��������� ������� ������ ��������� � ���������� ����������. 
# ������ ������ �������:
# normality.test(iris[,-5])
# Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
# 1.018116e-02 1.011543e-01 7.412263e-10 1.680465e-08
x <- iris[, -5]

normality.test <- function(x) {
  shapiro <-  sapply(x, function(x) shapiro.test(x))[2,]
  return(as.vector(shapiro))
}

normality.test(x)



install.packages('gvlma')
library(gvlma)
