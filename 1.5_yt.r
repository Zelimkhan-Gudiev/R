remove(list = ls())
remove()

# ЮТРЕК________________________________________________________________________________________________________
#### ЮТРЕК_______________________________________________________________________________________________________####
install.packages('ggplot2')
library(ggplot2)
install.packages('psych')
library(psych)

yt <- read.csv2("yt.csv")

# Посмотреть подозрительные данные
pdzr <- yt$ktd[yt$tegs == "Не полностью YouTrackная"]
#______________________________________________________________
yt <- subset(yt, tegs != "Не полностью YouTrackная")
yt <- yt[, -c("numb", "tegs" , "discription", "det_let_prot")]
yt <- yt[, -c(1, 15, 18, 19, 26, 27, 29)]
str(yt)
# Номера столбцов
which(names(yt) == 'numb')
which(names(yt) == 'tegs')
which(names(yt) == 'discription')
which(names(yt) == 'det_let_prot')
which(names(yt) == 'executor_depir')
which(names(yt) == 'deputy')
which(names(yt) == 'date_rev_ac')
which(names(yt) == 'date_rev_depir')
129 - 45

which(names(yt) == 'numb_ret_depir')
which(names(yt) == 'duration')
my_yt <- aggregate(cbind(duration, numb_ret_depir, numb_ret_oiv) ~ teamleader, yt, mean)
describe_yt <- describe(yt[, c(9, 11)])
describe_yt2 <- describeBy(x = yt[, c(9, 11, 44)], group = yt$teamleader, mat = T, digits = 2)





# Взаимосвязь между количеством возвравтов КТД от ДЭПиР и от ОИВ
depir_oiv <- ggplot(data = yt, mapping = aes(x = numb_ret_depir, y =  numb_ret_oiv, color=kind_tz))+
  labs(x= "Количество возвратов КТД на доработку по замечаниям ДЭПиР" , 
       y= "Количество возвратов КТД на доработку по замечаниям ОИВ", color= "Вид ТЗ",
       title = "Взаимосвязь между количеством возвравтов КТД от ДЭПиР и от ОИВ")
depir_oiv + geom_point() + geom_smooth(method = 'lm')

cor.test(yt$numb_ret_depir, yt$numb_ret_oiv)
cor.test(yt$numb_ret_depir, yt$numb_ret_oiv, method = 'spearman')



# Взаимосвязь между количеством возвравтов КТД от ДЭПиР и длительностью
depir_duration <- ggplot(data = yt, mapping = aes(x = numb_ret_depir, y =  duration, color=kind_tz))+
  labs(x= "Количество возвратов КТД на доработку по замечаниям ДЭПиР" , 
       y= "Длительность", color= "Вид ТЗ",
       title = "Взаимосвязь между количеством возвравтов КТД от ДЭПиР и длительностью")
depir_duration + geom_point() + geom_smooth(method = 'lm')

cor.test(yt$numb_ret_depir, yt$duration)

# Взаимосвязь между количеством возвравтов КТД от ОИВ и длительностью
oiv_duration <- ggplot(data = yt, mapping = aes(x = numb_ret_depir, y =  duration, color=kind_tz))+
  labs(x= "Количество возвратов КТД на доработку по ОИВ" , 
       y= "Длительность", color= "Вид ТЗ",
       title = "Взаимосвязь между количеством возвравтов ОИВ от ОИВ и длительностью")
oiv_duration + geom_point() + geom_smooth(method = 'lm')

cor.test(yt$numb_ret_oiv, yt$duration)

str(yt)
yt$ktd <- as.factor(yt$ktd)
yt$reason <- as.factor(yt$reason)
yt$year_plan_st <- as.factor(yt$year_plan_st)
yt$kvartal <- as.factor(yt$kvartal)
yt$kind_tz <- as.factor(yt$kind_tz)
yt$stage <- as.factor(yt$stage)
yt$executor_ac <- as.factor(yt$executor_ac)
yt$teamleader <- as.factor(yt$teamleader)
yt$deputy <- as.factor(yt$deputy)
yt$executor_depir <- as.factor(yt$executor_depir)
yt$contract <- as.factor(yt$contract)
yt$pcp <- as.factor(yt$pcp)
yt$criteria <- as.factor(yt$criteria)
yt$f2 <- as.factor(yt$f2)
yt$method <- as.factor(yt$method)
yt$tegs <- as.factor(yt$tegs)

# Номера столбцов
which(names(yt) == 'duration')
which(names(yt) == 'numb_ret_depir')
which(names(yt) == 'numb_ret_oiv')


# Пропущенные значения
is.na(yt$reason)
sum(is.na(yt))

yt$time_depir
depir_time <- sum(yt$time_depir, na.rm = T)
ac_time <- sum(yt$time_develop, na.rm = T) + sum(yt$time_rev_oiv, na.rm = T) + sum(yt$time_depir, na.rm = T) + 
  sum(yt$time_vn_sogl, na.rm = T)


