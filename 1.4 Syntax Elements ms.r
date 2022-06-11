remove(list = ls())


#### Introduction _____________________________________________________________________________________________________________________________ ####
# В этом уроке познакомимся с синтаксисом циклов в R. #
# Скрипт для урока можете найти по ссылке: https://stepic.org/media/attachments/lesson/11478/conditions.R #
# Датасет: https://stepic.org/media/attachments/lesson/11481/evals.csv. #



#### Data preparation (Подготовка данных) _____________________________________________________________________________________________________ ####

mydata <-  read.csv("/Users/zelimkhan/Desktop/evals.csv") # не работает
mydata <- read.csv('evals.csv') 
yt <- read.csv2('yt.csv') # Отступление



#### if _______________________________________________________________________________________________________________________________________ ####


# принимает на вход условие (одно логическое значение), что выдает на выходе TRUE or FALSE
# оператор if: 1) принимает на вход условие; 2) затем проверяет выполняется ли данное условие;
# 3) если выполняестя, то выполняет некоторую комманду; 4) если условие не выполняется то выполняет другую комманду 
a <- -10

# два варианта
if (a > 0) { # 1) принимает на вход условие if (a > 0); 2) затем проверяет выполняется ли данное условие;
  print('positive') # 3) если выполняестя, то выполняет некоторую комманду print('positive');
} else('not positive') # 4) если условие не выполняется то выполняет другую комманду else('not positive');
# после else не обязательно использовать фигурные скобки, но тогда мы можем выполнить только одну комманду

# два варианта + действие (a + 1)
if (a > 0) {
  print('positive')
} else { # Если после else не использовать фигурные скобки { } то мы сможем выполнить только одно действие print('not positive'). Если хотим выполнить более одного действия, нужно использовать фигурные скобки { }
  print('not positive')
  print(a + 1) # + действие (a + 1)
}


# три варианта
if (a > 0) {
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero')


#### yt (if) __________________________________________________________________________________________________________________________________ ####
# Warning messages:
# В отличии от ifelse операторр if не позволяет работать с векторами произвольной длины.
# при выполнении нижеуказанной функции будет выдано только одно значение ("positive") для первого значения вектора a <- c(1, -1), т.е. для 1
if (yt$duration < mean(yt$duration, na.rm = T)) {
  print('dur_less_than_mean')
} else if (yt$duration > mean(yt$duration, na.rm = T)) {
  print('dur_more_than_mean')
} else print('dur_equal_mean')


#### ifelse ___________________________________________________________________________________________________________________________________ ####


# ifelse позволяет записать вышеуказанную конструкцию в одну строку
a <- 10
ifelse(a > 0, 'positive', 'not positive')

a <- c(1, -1) # ifelse позволяет работать с векторами произвольной длины. В данном случае длина вектора а равняется двум a <- c(1, -1)
ifelse(a > 0, 'positive', 'not positive') # в данном случае ifelse выдает два значения 'positive' и 'not positive'

# В отличии от ifelse операторр if не позволяет работать с векторами произвольной длины.
# при выполнении нижеуказанной функции будет выдано только одно значение ("positive") для первого значения вектора a <- c(1, -1), т.е. для 1
a <- c(1, -1)
if (a > 0) {
  print('positive')
} else if (a < 0) {
  print('negative')
} else print('zero') 


#### yt (felse) _______________________________________________________________________________________________________________________________ ####
# error
ifelse(yt$duration <= mean(yt$duration, na.rm = T), "dur less than mean/dur equal mean ","dur more than mean")
less_or_equal_or_more_than_mean <- ifelse(yt$duration <= mean(yt$duration, na.rm = T), "dur less than mean/dur equal mean","dur more than mean")
length(less_or_equal_or_more_than_mean[less_or_equal_or_more_than_mean == 'dur more than mean'])
length(less_or_equal_or_more_than_mean[less_or_equal_or_more_than_mean == "dur less than mean/dur equal mean"])
mean(yt$duration, na.rm = T)
length(yt[yt$duration == 130.2266])

# v2 # 
p1 <- ifelse(yt$duration < mean(yt$duration, na.rm = T), "dur less than mean","other")
length(p1[p1 == "dur less than mean"])
length(p1[p1 == "other"])
p2 <- ifelse(yt$duration > mean(yt$duration, na.rm = T), "dur more than mean", "other")
length(p2[p2 == "dur more than mean"])
length(p2[p2 == "other"])
p3 <- ifelse(yt$duration == mean(yt$duration, na.rm = T), "dur equal mean","other")
length(p3[p3 == "dur equal mean"])
length(p3[p3 == "other"])

yt$ktd[yt$duration == mean(yt$duration, na.rm = T)]




#### for ______________________________________________________________________________________________________________________________________ ####


# оператор for на вход должен получать вектор значений in 1:10
# цикл for позволяет выполнять много раз рутинную операцию (с каждым элементом вектора)
for (i in 1:10) {
  print(i)
}

# for делает следующее: i пробегает значения от 1 до количества строк в дата фрейме и выводит значения yt$duration из i строки
for(i in 1:nrow(yt)) {
  print(yt$duration[i])
}

for(i in 1:nrow(yt)) {
  print(yt$ktd[i])
}


for (i in 1:nrow(mydata)) {
  print(mydata$score[i])
}


#### for + if _________________________________________________________________________________________________________________________________ ####
# for + if принимает на вход вектор и условие

# вывод mydata$score[i] по i in 1:nrow(mydata) если mydata$gender[i] == 'male'

for (i in 1:nrow(mydata)) {           # for принимает на вход вектор. Вектор от 1 до количества строк в mydata
  if (mydata$gender[i] == 'male') {   # if принимает на вход вектор и условие. Если i значние mydata$gender[i] равно 'male'
    print(mydata$score[i])            # print(mydata$score[i] выводит i значние переменной score. Выводит значние переменной score для всех строк где значние mydata$gender[i] равно 'male' по всем строкам от 1 до nrow(mydata)
  }
}

for(i in 1:nrow(yt)) {
  if(yt$numb_ret_depir[i] > 5) {
    print(yt$name[i])
  }
}

for(i in 1:nrow(yt)) {
  if(yt$numb_ret_depir[i] > 3) {
    ksk <- data.frame(yt$name[i], yt$teamleader[i], yt$numb_ret_depir[i])
  }
}


#### for + if VS ifelse _______________________________________________________________________________________________________________________ ####

# Создадим новую переменную из значений, которые есть в других переменных

mydata$quality <- rep(NA, nrow(mydata)) # Создадим новую переменную quality с пустыми значениями и длиной nrow(mydata).
# Данная перменная будет отражать бинарное значение хороший преподователь или плохой на основе его оценки score

for (i in 1:nrow(mydata)) {             # задаем вектор, который должен пробегать оператор for
  if (mydata$score[i] > 4){             # задаем условия для операторра if. Проверяем в i строке mydata$score больше 4
    mydata$quality[i] <- 'good'         # указываем действие, которое необходимо выполнить если условие выполнено. Если в i строке значение mydata$score больше 4, то в этой i строке в столбце mydata$quality записываем значение 'good' 
  } else mydata$quality[i] <- 'bad'     # указываем действие, которое необходимо выполнить если условие не выполнено. Если в i строке значение mydata$score не больше 4, то в этой i строке в столбце mydata$quality записываем значение 'bad'
}

yt$quality <- rep(NA, nrow(yt))

for (i in 1:nrow(yt)) {
  if (yt$duration[i] > mean(yt$duration, na.rm = T)) {
    yt$quality[i] <- "dur more than mean"
  } else if (yt$duration[i] < mean(yt$duration, na.rm = T)) {
    yt$quality[i] <- "dur less than mean"
  } else yt$quality[i] <- "dur equal mean"
}
# 
length(yt$quality[yt$quality == "dur more than mean"])
length(yt$quality[yt$quality == "dur less than mean"])
length(yt$quality[yt$quality == "dur equal mean"])


# С помощью ifelse можно выполнить все вышеуказанное в одну строчку
mydata$quality2 <- ifelse(mydata$score > 4, 'good', 'bad') # mydata$score > 4 проверяет в каждой (i) строке mydata$score больше 4
# Если да, то в такой строке в столбце mydata$quality2 записываем значение 'good'
# Если нет, то в такой строке в столбце mydata$quality2 записываем значение 'bad'

# yt
yt$quality2 <-  ifelse(yt$duration > mean(yt$duration, na.rm = T), "dur more than mean", "dur less than mean/dur equal mean")



#### while ____________________________________________________________________________________________________________________________________ ####
# оператор while выполняет действие до тех пор пока не будет выполнено какое-либо условие

i <- 1

while (i < 51) {            # оператор while будет выполнять нижеуказанные действия до тех пор пока i < 51, т.е с 1 до 51-й строки
  print(mydata$score[i])    # если i < 51, то выводим значение столбца mydata$score и так до тех пор пока (i < 51)
  i <- i + 1                # если i < 51, то добавляем 1 к i (i + 1) и так до тех пор пока (i < 51)
}


#### Step 4 of 8. Задача ______________________________________________________________________________________________________________________ ####
# Создайте новую числовую переменную  new_var в данных mtcars, которая содержит единицы в строчках, 
# если в машине не меньше четырёх карбюраторов (переменная "carb") или больше шести цилиндров (переменная "cyl"). 
# В строчках, в которых условие не выполняется, должны стоять нули.

mtcars <- mtcars

# Вариант 1
mtcars$new_var <- ifelse((mtcars$carb >= 4 | mtcars$cyl > 6), '1', '0')
# yt
yt$score <- ifelse((yt$numb_ret_depir >= 4 | yt$numb_ret_oiv >= 4), 'Bad', 'Norm') # отсупление
yt$score2 <- ifelse((yt$numb_ret_depir >= mean(yt$numb_ret_depir)) | (yt$numb_ret_oiv >= mean(yt$numb_ret_depir)), 'Bad', 'Norm')
length(yt$score[yt$score == 'Bad'])
length(yt$score[yt$score == 'Norm'])
49+58

# Вариант 2
for (i in 1:nrow(mtcars)){
  if (mtcars$carb[i] >= 4  |  mtcars$cyl[i] > 6){
    mtcars$new_var1[i] = 1
  }
  else mtcars$new_var1[i] = 0
}

# yt
yt$quality3 <- rep(NA, nrow(yt))

for (i in i:nrow(yt)) {
  if (yt$duration[i] > mean(yt$duration, na.rm = T)) {
    yt$quality3[i] <- "dur more than mean"
  } else if (yt$duration[i] < mean(yt$duration, na.rm = T)) {
    yt$quality3[i] <- "dur less than mean"
  } else yt$quality3[i] <- "dur equal mean"
}

for (i in 1:nrow(yt)) {
  if (yt$duration[i] > mean(yt$duration, na.rm = T)) {
  yt$quality3[i] <- "dur more than mean"
 } else if (yt$duration[i] < mean(yt$duration, na.rm = T)) {
   yt$quality3[i] <- "dur less than mean"
 } else yt$quality3[i] <- "dur equal mean"
}

# Вариант 3
mtcars$new_var2 <- rep(NA, nrow(mtcars))
for (i in 1:nrow(mtcars)) {
  if (mtcars$carb[i] >= 4) {
    mtcars$new_var2[i] <- 1
  } else if (mtcars$cyl[i] >6){
    mtcars$new_var2[i] <- 1
  } else mtcars$new_var2[i] <- 0}



#### Step 5 of 8. Задача ______________________________________________________________________________________________________________________ ####
#В уже существующей переменной my_vector сохранен вектор из 50 чисел.
# Решите задачу используя конструкцию:
#  if () {
#  } else {
#  } 
# Если среднее значение вектора my_vector больше 20, в переменную result сохраните "My mean is great",  
# если среднее значение my_vector меньше или равно 20 то в переменную result сохраните  строку "My mean is not so great".

if (mean(my_vector) > 20) {
  result <- 'My mean is great'
} else {
  result <- 'My mean is not so great'
}


#### Step 6 of 8. Задача (Не понял) ___________________________________________________________________________________________________________ ####
# И так ваша задача создать переменную good_months и сохранить в нее число пассажиров только в тех месяцах, 
# в которых это число больше, чем показатель в предыдущем месяце.
# Важный момент! В R оператор : для создания последовательности имеет приоритет над арифметическими действиями. 
# Таким образом, если у вас есть переменная i, равная 10, и вы хотите создать вектор от 1 до i - 1, 
# воспользуйтесь скобками, чтобы указать последовательность действий.
#    > i <- 10
#    > 1 : i - 1 # так мы создадим последовательность от 1 до 10, а потом вычтем единицу из каждого элемента
#    [1] 0 1 2 3 4 5 6 7 8 9
#    > 1 : (i - 1) # а вот так мы создадим последовательность от 1 до i - 1, то есть от 1 до 9. 
#    [1] 1 2 3 4 5 6 7 8 9

ap <- data.frame(AirPassengers)
typeof(AirPassengers)

# Вариант 1
vector1 <- as.vector(AirPassengers)
vector2 <- c(vector1[length(vector1)],vector1[-length(vector1)])
diffs <- vector1-vector2
output <-  vector1[diffs>0]
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
  if (AirPassengers[i] > AirPassengers[i-1]){
    good_months[index] <- AirPassengers[i]
    index <- index + 1
  }
}
ap$good_months <- good_months[index]

# Вариант 3. Пример правильного решения без цикла:
  
  good_months <- AirPassengers[-1][AirPassengers[-1] > AirPassengers[-144]] 


#### Step 7 of 8. Задача (Не понял) __________________________________________________________________________________________________________  ####
# Задачка для супер героев, повышенной сложности!
  
# Для встроенных в R данных AirPassengers рассчитайте скользящее среднее с интервалом сглаживания равным 10. 
# Напечатайте получившийся результат (первым значением в выводе должно быть среднее для элементов 1:10, 
# во втором значении - среднее для элементов 2:11 и т.д., в последнем  - среднее для элементов 135 :144)
# Все полученные значения средних сохраните в переменную moving_average.
# Пример расчета для вектора из 11 элементов:
#    !Не отображается рисунок!
# Соответственно, для наших данных из 144 наблюдений должно получиться 135 средних (первые и последние 4 средних):
# 129.8 129.0 129.0 127.3 ... 483.6 489.2 486.5 490.6
  
# Важный момент! В R оператор : для создания последовательности имеет приоритет над арифметическими действиями. Таким образом, если у вас есть переменная i, равная 10, и вы хотите создать вектор от 1 до i - 1, воспользуйтесь скобками, чтобы указать последовательность действий.
# > i <- 10
# > 1 : i - 1 # так мы создадим последовательность от 1 до 10, а потом вычтем единицу из каждого элемента
# [1] 0 1 2 3 4 5 6 7 8 9
# > 1 : (i - 1) # а вот так мы создадим последовательность от 1 до i - 1, то есть от 1 до 9. 
# [1] 1 2 3 4 5 6 7 8 9
  
# Если вам потребуется создать вектор moving_average заранее, то есть несколько способов сделать это:
#   1. самый простой, но не очень правильный вариант - создать пустой вектор
#       moving_average <- c()
#   2. можно сразу создать вектор определенной длины и определенного типа:
#       moving_average <- numeric(135)
#       Такой вариант является более предпочтительным. Почему? Узнаем во второй части курса!)

# А для тех, кто уже выбрал путь воина и не хочет использовать цикл - советую познакомиться с функцией cumsum. 
# Подсказка: если у нас есть два вектора одинаковой длинны, то если из одного вектора вычесть второй вектор, 
# мы найдем разность для первых элементов векторов, затем для вторых и т.д.
# > x <- c(2, 4, 7)
# > y <- c(2, 3, 5)
# > x - y
# [1] 0 1 2
  
# Решение

# Вариант 1
    
  moving_average <- numeric(135) # создаем пустой числовой вектор из 135 элементов
  last_index <- length(AirPassengers) - 9
  for (i in 1:last_index) {
    end <- i + 9
    moving_average[i] <- mean(AirPassengers[i:end])
  }    

# Вариант 2
# Можно решить и без цикла при помощи разностей кумулятивных сумм!    
    
  n <- 10
  d <- AirPassengers
  cx <- c(0, cumsum(d))
  moving_average <- (cx[(n + 1):length(cx)] - cx[1:(length(cx) - n)]) / n
  
  
# Вариант 3
  moving_average <- numeric(135)
  for (i in 1:135) {
    moving_average[i] <- sum(AirPassengers[i:(i+9)])/10
  }
  
  
#### Step 8 of 8. Задача ______________________________________________________________________________________________________________________ ####
# У слушателей, которые уже знакомы с особенностями языка R, этот урок мог вызвать некоторые
# вопросы относительно применения конструкций for и while. Действительно, в R циклы for
# нужно использовать с большой аккуратностью. Почему? Вы найдете ответ во второй части нашего курса!
