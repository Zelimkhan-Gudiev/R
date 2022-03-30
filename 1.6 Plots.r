
remove(list = ls())
rm()

#### Packages and librarys ____________________________________________________________________________________________________ ####

install.packages("dplyr")
library(dplyr)
install.packages("psych")
library(psych)
install.packages("ggplot2")
library(ggplot2)

#### Data preprocessing _______________________________________________________________________________________________________ ####

getwd()
setwd("C:/Users/GudievZK/Desktop/GitHub/DF/")

df  <- mtcars
yt <- read.csv2("yt.csv")
# Чтобы корректно открыть в RStudio файл csv (без закорючек) необходимо:
# 1.	Для ОС Windows – при сохранении установить тип файла «CSV (разделитель -запятая)»
# 2.	Для ОС Mac – при сохранении установить тип файла «CSV UTF-8 (разделитель -запятая)»


#### Step 1: Base graphs ______________________________________________________________________________________________________ ####

df$vs  <- factor(df$vs  , labels = c("V", "S"))
df$am  <- factor(df$am  , labels = c("Auto", "Manual"))

### 1) hist
hist(df$mpg, breaks = 20, xlab = "MPG", main ="Histogram of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

### 2) boxplot
boxplot(df$mpg[df$am == "Auto"], df$mpg[df$am == "Manual"], ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)

boxplot(mpg ~ am, df, ylab = "MPG", main ="MPG and AM", 
        col = "green", cex.lab = 1.3, cex.axis = 1.3)

### 3) plot
plot(density(df$mpg), xlab = "MPG", main ="Density of MPG", 
     col = "green", cex.lab = 1.3, cex.axis = 1.3)

plot(df$mpg, df$hp, xlab = "MPG", ylab ="HP" , main ="MPG and HP", pch = 22)

plot(~ mpg + hp, df)


#### yt (Step 1: Base graphs) _________________________________________________________________________________________________ ####

### 1) hist
hist(yt$duration)
hist(yt$numb_ret_depir)
hist(yt$numb_ret_oiv)

### 2) boxplot
boxplot(duration ~ deputy, yt, ylab = 'Длительность, раб.дн.', xlab = 'Заместитель РПО', main = "Длительность разработки",
        col = 'green', cex.lab = 1.3, cex.axis = 1.3)

table(yt$deputy)

### 3) plot
plot(yt$time_depir, yt$numb_ret_depir)



#Step 2, 3: Library ggplot2 _________________________________________________________________________________________________ ####

ggplot(df, aes(x = mpg))+
  geom_histogram(fill = "white", col = "black", binwidth = 2)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  ggtitle("MPG histogram")

ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot()+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG dotplot")


ggplot(df, aes(x = mpg))+
  geom_density(fill = "red")

ggplot(df, aes(x = mpg, fill = am))+
  geom_density(alpha = 0.5)+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG density plot")


ggplot(df, aes(x = am, y = hp, fill = vs))+
  geom_boxplot()+
  xlab("Transmission type")+
  ylab("Gross horsepower")+
  scale_fill_discrete(name="Engine type")+
  ggtitle("Gross horsepower and engine type")


ggplot(df, aes(x = mpg, y = hp, size = qsec))+
  geom_point()+
  xlab("Miles/(US) gallon")+
  ylab("Gross horsepower")+
  scale_size_continuous(name="1/4 mile time")+
  ggtitle("Miles/(US) gallon and Gross horsepower")


my_plot  <- ggplot(df, aes(x = mpg, y = hp, col = vs, size = qsec))+
  geom_point()

my_plot2  <- ggplot(df, aes(x = am, y = hp, fill = vs))

my_plot2 + geom_boxplot()

#### yt (Step 2, 3: Library ggplot2) _________________________________________________________________________________________ ####

ggplot(yt, aes(x = duration)) +
  geom_histogram(fill = "white", col = "black", binwidth = 20) +
  xlab("Длительность утверждения КТД, раб. дни") + 
  ylab("Количество КТД") +
  ggtitle("Гистограмма утверждения КТД")
  
ggplot(df, aes(x = mpg, fill = am))+
  geom_dotplot()+
  xlab("Miles/(US) gallon")+
  ylab("Count")+
  scale_fill_discrete(name="Transmission type")+
  ggtitle("MPG dotplot")

ggplot(yt, aes(x = numb_ret_depir)) +
  geom_dotplot(binwidth = 0.4)+
  xlab("Количество возвратов ДЭПиР") +
  ylab("Количество КТД")+
  scale_fill_discrete(name = yt$deputy) +
  ggtitle("Гистограмма возвратов ДЭПиР")
  
















