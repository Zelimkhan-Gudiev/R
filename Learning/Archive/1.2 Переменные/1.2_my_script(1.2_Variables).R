remove(list = ls())

##### Step 1.2

my_var1 <- 42
my_var2 <- 35.25

my_var1 + 100

my_var1 + my_var2 - 12

my_var3 <- my_var1^2 + my_var2^2

my_var3 > 200
my_var3 > 30099
my_var1 == my_var2
my_var1 != my_var2

my_new_var <- my_var1 == my_var2

1 : 67
my_vector1 <- 1:67
my_vector2 <- c(-32, 45, 67, 12.78, 129, 0, -65)

my_vector1[1]
my_vector1[3]
my_vector2[2]
my_vector2[c(1,2,3)]
1:3
my_vector2[1:3]

my_vector2[c(1,5,6,7,10)]

my_vector1 + 10
my_vector2 + 56

my_vector2 == 0
my_vector1 > 30

x <- 23

my_vector1 > 23
my_vector1 > x
x == 23

my_vector2 > 0
my_vector2[my_vector2 > 0]
my_vector2[my_vector2 < 0]
my_vector2[my_vector2 == 0]

my_vector1[my_vector1 > 20 & my_vector1 < 30]

positive_numbers <- my_vector2[my_vector2 > 0]

v1 <-  c(165, 178, 180, 181, 167, 178, 187, 167, 187)
mean_v1 <- mean(v1)
v1[v1 > mean_v1]

greater_than_mean <- v1[v1 > mean_v1]


age <- c(16, 18, 22, 27)
is_maried <- c(F, F, T, T)
name <- c("Olga", "Maria", "Nastya", "Polina")

data <- list(age, is_maried, name)
View(data)
data[[1]][1]
data[[2]][3]

df <- data.frame(Name = name, Age = age, Status = is_maried)
View(df)
head(df)

typeof(df)
