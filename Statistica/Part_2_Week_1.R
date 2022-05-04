remove(list = ls())

getwd()
setwd("/Users/zelimkhan/Desktop/Data/GitHub/Statistica/")

#### 1.7 Exercise 8 ####
#1
chisq.test(rbind(c(20, 15), c(11, 12), c(7, 9)))$p.value

#2
male <- c(20,11,7)
female <- c(15,12,9)
names <- c("TORM", "PRITORM", "NETORM")
bbb <- data.frame(male, female, row.names = names)
chi <- chisq.test(bbb)
attributes(chi)
chi$p.value

#3
c(20,11,7)
c(15,12,9)
c("TORM", "PRITORM", "NETORM")
data.frame(row.names = c("TORM", "PRITORM", "NETORM"), c(20,11,7), c(15,12,9))
chisq.test(data.frame(
                      row.names = c("TORM", "PRITORM", "NETORM"), 
                                 c(20,11,7), c(15,12,9)
                      )
          )$p.value


#### 1.7 Exercise 11 ####

#воссоздадим таблицу
patients <- rbind(c(18, 7), c(6, 13))
#подпишем строки и столбцы
colnames(patients) <- c("Yes", "No")
rownames(patients) <- c("Placebo", "Aspirin")
#вот график, который нам нужен
mosaicplot(patients, color=T, shade=T, ylab="Thrombosis", xlab="Group")
#а вот так можно в точности воспроизвести рисунок, который мы видели
mosaicplot(patients, color=T, shade=T, ylab="Thrombosis", xlab="Group", cex.axis=1, main="")

# Итого:
# - размер прямоугольников  соответствует количеству наблюдений.
# - цвет прямоугольников - величине значимости отклонения ожидаемых и наблюдаемых частот в этой ячейке.
# - если значения стандартизированных остатков больше 3х, можно считать, что в этой ячейке зафиксированы значимые отклонения.


#Кстати, давайте сдвинем наше распределение так, чтобы в нашем графике появились закрашенные ячейки.

#воссоздадим таблицу
patients2 <- rbind(c(25, 1), c(3, 30))
#подпишем строки и столбцы
colnames(patients2) <- c("Yes", "No")
rownames(patients2) <- c("Placebo", "Aspirin")
#вот наш график
mosaicplot(patients2, color=T, shade=T, ylab="Thrombosis", xlab="Group", cex.axis=1, main="")


####  1.9 Practice ####

####  1.9 Practice. Exercise 3 ####
# 1
smart_test <- function(x) {
  x <- as.table(x)
  if(any(x < 5)) {
    fish <- fisher.test(x)
    fish$p.value
  } else {
    chis <- chisq.test(x)
    c(chis$statistic, chis$parameter, chis$p.value)
  }
}

smart_test(df)
smart_test(df1)
df4 <- unlist(df)


# 2
smart_test <- function(x) {
   x <- table(x)
   if(any(x < 5)) {
    fish <- fisher.test(x)
    print(fish$p.value)
  } else {
    chis <- chisq.test(x)
    print(c(chis$statistic, chis$parameter, chis$p.value))
  }
}


table(df)
typeof(df4)
class(df)
df3 <- as.data.frame(df)

# Достаточно наблюдений в таблице
df <- table(mtcars[,c("am", "vs")])

# Недостаточно наблюдений в таблице
df1 <-  table(mtcars[1:20,c("am", "vs")])







