install.packages("Rssa")
install.packages("hht")
install.packages("EMD")
install.packages("factoextra")
install.packages("factoextra", type="binary")

library(data.table)
library(EMD)
library(hht)
library(ggplot2)
library(Rssa)
library(factoextra)
library(dplyr)


myData <- read.csv2("C:/Users/GudievZK/Desktop/GitHub/DataMining/Савёловский.csv", encoding = "UTF-8")
x1 <- myData$HourlyPeoples_08_22
x2 <- myData$HourlyPeoples_08_10
x3 <- myData$HourlyPeoples_17_19
x4 <- myData$HourlyPeoples_22_08

df <- data.frame(x1, x2, x3, x4)

plot(myData$MonthlyPeoples)
plot(myData$DailyPeoples)

head(df)
long <- melt(setDT(df))

SUB.DATA = long %>% select("value",variable)

plot <- ggplot(data = SUB.DATA) +
  geom_line(aes(x = 1:dim(SUB.DATA)[1],
                y = get("value"),
                color = as.factor(variable))) +
            theme_minimal() + xlab("Time") + guides(color = guide_legend(title = "Трафик по времени суток")) +
            ylab("Число") + ggtitle(paste("Plot for ", "Савеловский", sep = ""))
plot



pca <- princomp(df, cor = TRUE, scores = TRUE)
summary(pca) 		#вывод результатов компонентной модели
scree <- fviz_eig(pca) 	#график каменистой осыпи
pca_ind <- fviz_pca_ind(pca, col.ind = "cos2", repel = TRUE)
pca_var <- fviz_pca_var(res.pca, col.var = "contrib", repel = TRUE)
pca_biplot <- fviz_pca_biplot(pca, repel = TRUE)


OUTPUT <-long %>% 
        group_by(variable) %>% 
        select("value",variable) %>% 
        summarize(median=median(get("value")),IQR=IQR(get("value")))

plot(OUTPUT$median)
lines(OUTPUT$median)

s <- ssa(long$value)
summary(s)
plot(s)
r <- reconstruct(s, groups = list(Trend = c(1, 4), Seasonality = c(2:3, 5:6)))
plot(r, add.original = TRUE)
plot(r, add.original = FALSE)

s2 <- ssa (long$value, L=240)

r <- reconstruct(s2, groups = list(Trend = c(1, 4), Seasonality = c(2:3, 5:6)))
plot(r, add.original = TRUE)
plot(r$Trend)
plot(r$Seasonality)

r2 <- reconstruct(s, groups = list(Trend = c(1, 4), Seasonality = c(2:3, 5:6)))
plot(r2, add.original = TRUE)
plot(r2$Trend)
plot(r2$Seasonality)

s3 <- ssa(long$value, L=2400)
r3 <- reconstruct(s3, groups = list(Trend = c(1, 4), Seasonality = c(2:3, 5:6)))
plot(r3$Trend)
plot(r3$Seasonality)