remove(list = ls())


#### Introduction ####
# В этом уроке познакомимся с синтаксисом циклов в R. #
# Скрипт для урока можете найти по ссылке: https://stepic.org/media/attachments/lesson/11478/conditions.R #
# Датасет: https://stepic.org/media/attachments/lesson/11481/evals.csv. #



#### Подгтовка данных ####
mydata <-  read.csv("/Users/zelimkhan/Desktop/evals.csv") # не работает
mydata <- read.csv('evals.csv') 

#### if ####

a <- -10

# два варианта
if (a > 0) {
  print('positive')
} else('not positive') 
# после else не обязательно использовать фигурные скобки, но тогда мы можем выполнить только одну комманду


# два варианта + действие (a + 1)
if (a > 0) {
  print('positive')
} else {
  print('not positive')
  print(a + 1)
}


# три варианта
if (a > 0) {
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero')

#### ifelse  ####
# ifelse позволяет записать вышеуказанную конструкцию в одну строку
a <- 10
a <- c(1, -1)

ifelse(a > 0, 'positive', 'not positive')


#### for ####
# цикл for позволяет выполнять много раз рутинную операцию
for (i in 1:10) {
  print(i)
}

for(i in 1:nrow(yt)) {
  print(yt$ktd)
}

for(i in 1:nrow(yt)) {
  print(yt$ktd[i])
}


for (i in 1:nrow(mydata)) {
  print(mydata$score[i])
}


#### for + if ####

# вывод mydata$score[i] по i in 1:nrow(mydata) если mydata$gender[i] == 'male'
for (i in 1:nrow(mydata)) {
  if (mydata$gender[i] == 'male') {
    print(mydata$score[i])
  }
}

for(i in 1:nrow(yt)) {
  if(yt$numb_ret_depir[i] > 5) {
    print(yt$name[i])
  }
}

for(i in 1:nrow(yt_2)) {
  if(yt_2$numb_ret_depir[i] > 3) {
    ksk <- data.frame(yt_2$name[i], yt$teamleader[i], yt$numb_ret_depir[i])
  }
}


#### for + if VS ifelse ####

mydata$quality <- rep(NA, nrow(mydata))

for (i in 1:nrow(mydata)) {
  if (mydata$score[i] > 4){
    mydata$quality[i] <- 'good'
  } else mydata$quality[i] <- 'bad'
}

mydata$quality2 <- ifelse(mydata$score > 4, 'good', 'bad')


### while оператор while выполняет действие до тех пор пока не будет выполнено какое-либо условие

i <- 1

while (i < 51) {
  print(mydata$score[i])
  i <- i + 1
}

#### Step 4 из 8 ####
# Создайте новую числовую переменную  new_var в данных mtcars, которая содержит единицы в строчках, 
# если в машине не меньше четырёх карбюраторов (переменная "carb") или больше шести цилиндров (переменная "cyl"). 
# В строчках, в которых условие не выполняется, должны стоять нули.

mtcars <- mtcars

# Вариант 1
mtcars$new_var <- ifelse((mtcars$carb >= 4 | mtcars$cyl > 6), '1', '0')
yt_2$score <- ifelse((yt_2$numb_ret_depir >= 4 | yt$numb_ret_oiv >= 4), 'Bad', 'Norm')

# Вариант 2
for (i in 1:nrow(mtcars)){
  if (mtcars$carb[i] >= 4  |  mtcars$cyl[i] > 6){
    mtcars$new_var1[i] = 1
  }
  else mtcars$new_var1[i] = 0
}

# Вариант 3
mtcars$new_var2 <- rep(NA, nrow(mtcars))
for (i in 1:nrow(mtcars)) {
  if (mtcars$carb[i] >= 4) {
    mtcars$new_var2[i] <- 1
  } else if (mtcars$cyl[i] >6){
    mtcars$new_var2[i] <- 1
  } else mtcars$new_var2[i] <- 0}



#### Step 5 из 8 ####
#В уже существующей переменной my_vector сохранен вектор из 50 чисел.
# Решите задачу используя конструкцию:
#  if () {
#  } else {
#  } 
#Если среднее значение вектора my_vector больше 20, в переменную result сохраните "My mean is great",  
#если среднее значение my_vector меньше или равно 20 то в переменную result сохраните  строку "My mean is not so great".

if (mean(my_vector) > 20) {
  result <- 'My mean is great'
} else {
  result <- 'My mean is not so great'
}


#### Step 6 из 8 ####
# И так ваша задача создать переменную good_months и сохранить в нее число пассажиров только в тех месяцах, 
# в которых это число больше, чем показатель в предыдущем месяце.
# Важный момент! В R оператор : для создания последовательности имеет приоритет над арифметическими действиями. 
# Таким образом, если у вас есть переменная i, равная 10, и вы хотите создать вектор от 1 до i - 1, 
# воспользуйтесь скобками, чтобы указать последовательность действий.

ap <- data.frame(AirPassengers)

# Вариант 1
vector1 <- as.vector(AirPassengers)
vector2 <- c(vector1[length(vector1)],vector1[-length(vector1)])
diffs <- vector1-vector2
output = vector1[diffs>0]
good_months <- output

# Вариант 2
AP_minus1 <- AirPassengers[1:143]
AP <- AirPassengers[2:144]

good_months1 <- AP[AP - AP_minus1 > 0]

# Вариант 3
ap$good_months3 <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]] 

# Вариант 4. Пример решения с циклом:
  
good_months <- c()    
index <- 1    
for (i in 2:length(AirPassengers)) {    
  if (AirPassengers[i]>AirPassengers[i-1]){    
    good_months[index] <- AirPassengers[i]    
    index <- index + 1    
  }    
}

ap$good_months <- good_months[index]
# Вариант 3. Пример правильного решения без цикла:
  
  good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]] 











