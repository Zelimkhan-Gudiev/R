data_1 <- read.csv2('data.csv')

names(data_1)

View(data_1)
head(data_1,10)


tail(data_1,15)


data_1$TD <- as.factor(data_1$TD)
data_1$Sex <- as.factor(data_1$Sex)
data_1$Add <- as.factor(data_1$Add)
data_1$Add1 <- as.factor(data_1$Add1)
data_1$Region <- as.factor(data_1$Region)
View(data_1)

names(which(sapply(data_1, anyNA)))

library(VIM)

miss_plot<-aggr(data_1[,c("Age", "St")])

library(caret)

summary (data_1[,c("Add1", "Age", "St")])


proc<-preProcess(data_1[,c("Add1", "Age", "St")],method='medianImpute')
data_1_c <- data_1
data_1_c[,c("Add1", "Age", "St")] <- predict(proc, data_1_c[,c("Add1", "Age", "St")])
names(which(sapply(data_1_c, anyNA)))

summary (data_1_c[,c("Add1", "Age", "St")])

cor(data_1_c[,c("Age", "St")])

g <- ggplot(data = data_1_c,
            mapping = aes(x = Age,
                          y=St))
g

g + geom_point(color = "blue") +
  geom_smooth(method = "loess")

g+geom_smooth(method = "lm", col="pink")+geom_point()

g+geom_smooth(method = "gam",formula = y ~s(x, bs = "cs", k=4),
                col="pink")+geom_point()


g <- ggplot(data = data_1_c,
            mapping = aes(x = Age,
                          y=St,
                          color=Add,
                          fill=Add))
g + geom_point() +
  geom_smooth(method = "lm")

p <- ggplot(data = data_1_c,
            mapping = aes(x = Add))
p + geom_bar()


p <- ggplot(data = data_1_c,
            mapping = aes(x = Add, fill=..prop..))
p + geom_bar(mapping = aes(y=..prop..,group=1))+
  scale_fill_gradient()

hist(data_1_c$Age, breaks=30)

p <- ggplot(data = data_1_c,
            mapping = aes(x = St))
p+geom_histogram(color='blue',fill="lightblue")

x<-data_1_c$St

p + geom_histogram(aes(y=..density..), color='blue',fill="lightblue") +
  stat_function(fun = dnorm, args=list(mean=mean(x), sd=sd(x)), color='red', size=2)

